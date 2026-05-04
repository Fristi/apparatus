package apparatus

import apparatus.core.*
import cats.Applicative
import cats.implicits.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*

import java.time.Instant
import java.util.UUID

enum BankAccountState:
  case Uninitialized
  case Active(balance: BigDecimal)
  case Closed

  def evolve(ev: BankAccountEvent): BankAccountState =
    this match {
      case BankAccountState.Uninitialized =>
        ev match {
          case BankAccountEvent.Opened(_) => BankAccountState.Active(0)
          case _ => this
        }
      case BankAccountState.Active(balance) =>
        ev match {
          case BankAccountEvent.Deposited(amount, at) => BankAccountState.Active(balance + amount)
          case BankAccountEvent.Withdrawn(amount, at) => BankAccountState.Active(balance - amount)
          case BankAccountEvent.ClosedAccount(_) => BankAccountState.Closed
          case _ => this
        }
      case BankAccountState.Closed => this
    }

  def decide(cmd: BankAccountCommand): List[BankAccountEvent] = this match {
    case BankAccountState.Uninitialized => cmd match {
      case BankAccountCommand.Open(at) => List(BankAccountEvent.Opened(at))
      case _ => List(BankAccountEvent.Rejected("invalid command for current state"))
    }
    case BankAccountState.Active(balance) => cmd match {
      case BankAccountCommand.Deposit(amount, at) => List(if (amount <= 0) BankAccountEvent.Rejected("amount must be positive") else BankAccountEvent.Deposited(amount, at))
      case BankAccountCommand.Withdraw(amount, at) =>
        List(if (amount <= 0) BankAccountEvent.Rejected("amount must be positive") else if (amount > balance) BankAccountEvent.Rejected("insufficient funds") else BankAccountEvent.Withdrawn(amount, at))
      case BankAccountCommand.Close(at) => List(BankAccountEvent.ClosedAccount(at))
      case _ => Nil
    }
    case BankAccountState.Closed => List(BankAccountEvent.Rejected("invalid command for current state"))
  }

enum BankAccountCommand:
  case Open(at: Instant)
  case Deposit(amount: BigDecimal, at: Instant)
  case Withdraw(amount: BigDecimal, at: Instant)
  case Close(at: Instant)

enum BankAccountEvent:
  case Opened(at: Instant)
  case Deposited(amount: BigDecimal, at: Instant)
  case Withdrawn(amount: BigDecimal, at: Instant)
  case ClosedAccount(at: Instant)
  case Rejected(reason: String)

object BankAccountEvent:
  private def encode(ev: BankAccountEvent): String = ev match
    case BankAccountEvent.Opened(at)            => s"Opened|$at"
    case BankAccountEvent.Deposited(amount, at) => s"Deposited|$amount|$at"
    case BankAccountEvent.Withdrawn(amount, at) => s"Withdrawn|$amount|$at"
    case BankAccountEvent.ClosedAccount(at)     => s"ClosedAccount|$at"
    case BankAccountEvent.Rejected(reason)      => s"Rejected|$reason"

  private def decode(s: String): BankAccountEvent =
    s.split("\\|").toList match
      case "Opened" :: at :: Nil              => BankAccountEvent.Opened(Instant.parse(at))
      case "Deposited" :: amount :: at :: Nil => BankAccountEvent.Deposited(BigDecimal(amount), Instant.parse(at))
      case "Withdrawn" :: amount :: at :: Nil => BankAccountEvent.Withdrawn(BigDecimal(amount), Instant.parse(at))
      case "ClosedAccount" :: at :: Nil       => BankAccountEvent.ClosedAccount(Instant.parse(at))
      case "Rejected" :: rest                 => BankAccountEvent.Rejected(rest.mkString("|"))
      case _                                  => throw new Exception(s"Cannot decode event: $s")

  given Meta[BankAccountEvent] = Meta[String].imap(decode)(encode)

val bankAccount: Decider[BankAccountState, BankAccountCommand, List[BankAccountEvent]] =
  DeciderBuilder
    .seed[BankAccountState](BankAccountState.Uninitialized)
    .decide[BankAccountCommand, List[BankAccountEvent]](_.decide(_))
    .evolveList(_.evolve(_))

def transactionsProjection(id: UUID, repo: BankAccountTransactionRepository[ConnectionIO]): FSM[ConnectionIO, List[BankAccountEvent], Int] = 
  FSM.Basic(BaseMachineT.stateless[ConnectionIO, List[BankAccountEvent], Int] { evs => 
    evs.traverse {
      case BankAccountEvent.Deposited(amount, at) => repo.insertTransaction(id, Transaction(TransactionType.Deposit, amount, at))
      case BankAccountEvent.Withdrawn(amount, at) => repo.insertTransaction(id, Transaction(TransactionType.Withdrawal, amount, at))
      case _ => Applicative[ConnectionIO].pure(0)
    }
    .map(_.sum)
  })

enum TransactionType:
  case Deposit, Withdrawal

object TransactionType:
  given Meta[TransactionType] = Meta[String].imap(TransactionType.valueOf)(_.toString)

case class Transaction(transactionType: TransactionType, amount: BigDecimal, at: Instant)

object Transaction:
  given Read[Transaction] = Read.derived[Transaction]

object DoobieBankAccountTransactionRepository extends BankAccountTransactionRepository[ConnectionIO] {
  override def create(): ConnectionIO[Int] =
    sql"""
      CREATE TABLE IF NOT EXISTS transactions (
        aggregate_id     UUID        NOT NULL,
        transaction_type TEXT        NOT NULL,
        amount           NUMERIC     NOT NULL,
        at               TIMESTAMPTZ NOT NULL
      )
    """.update.run

  override def insertTransaction(id: UUID, tx: Transaction): ConnectionIO[Int] =
    sql"""
      INSERT INTO transactions (aggregate_id, transaction_type, amount, at)
      VALUES ($id, ${tx.transactionType}, ${tx.amount}, ${tx.at})
    """.update.run

  override def listTransactions(id: UUID): ConnectionIO[List[Transaction]] =
    sql"""
      SELECT transaction_type, amount, at
      FROM transactions
      WHERE aggregate_id = $id
      ORDER BY at ASC
    """.query[Transaction].to[List]
}

trait BankAccountTransactionRepository[F[_]]:
  def create(): F[Int]
  def insertTransaction(id: UUID, tx: Transaction): F[Int]
  def listTransactions(id: UUID): F[List[Transaction]]