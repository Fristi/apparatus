package apparatus.examples

import apparatus.core.Apparatus
import apparatus.core.machines.*
import cats.Applicative
import cats.implicits.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import zio.blocks.schema.Schema

import java.time.Instant
import java.util.UUID

sealed abstract class BankAccountError(msg: String) extends Exception(msg)
object BankAccountError:
  case object NotInitialized   extends BankAccountError("account not initialized")
  case object AlreadyOpen      extends BankAccountError("account already open")
  case object AlreadyClosed    extends BankAccountError("account is closed")
  case class  InvalidAmount(reason: String) extends BankAccountError(reason)
  case object InsufficientFunds extends BankAccountError("insufficient funds")

enum BankAccountState:
  case Uninitialized
  case Active(balance: BigDecimal)
  case Closed

  def evolve(ev: BankAccountEvent): BankAccountState =
    this match {
      case BankAccountState.Uninitialized =>
        ev match {
          case BankAccountEvent.Opened(_, _) => BankAccountState.Active(0)
          case _ => this
        }
      case BankAccountState.Active(balance) =>
        ev match {
          case BankAccountEvent.Deposited(_, amount, at) => BankAccountState.Active(balance + amount)
          case BankAccountEvent.Withdrawn(_, amount, at) => BankAccountState.Active(balance - amount)
          case BankAccountEvent.ClosedAccount(_, _) => BankAccountState.Closed
          case _ => this
        }
      case BankAccountState.Closed => this
    }

  def decide(cmd: BankAccountCommand): Either[BankAccountError, List[BankAccountEvent]] =
    this match {
      case BankAccountState.Uninitialized => cmd match {
        case BankAccountCommand.Open(id, at) => Right(List(BankAccountEvent.Opened(id, at)))
        case _                               => Left(BankAccountError.NotInitialized)
      }
      case BankAccountState.Active(balance) => cmd match {
        case BankAccountCommand.Deposit(id, amount, at) =>
          if amount <= 0 then Left(BankAccountError.InvalidAmount("amount must be positive"))
          else Right(List(BankAccountEvent.Deposited(id, amount, at)))
        case BankAccountCommand.Withdraw(id, amount, at) =>
          if amount <= 0 then Left(BankAccountError.InvalidAmount("amount must be positive"))
          else if amount > balance then Left(BankAccountError.InsufficientFunds)
          else Right(List(BankAccountEvent.Withdrawn(id, amount, at)))
        case BankAccountCommand.Close(id, at) => Right(List(BankAccountEvent.ClosedAccount(id, at)))
        case _                                => Left(BankAccountError.AlreadyOpen)
      }
      case BankAccountState.Closed => Left(BankAccountError.AlreadyClosed)
    }

sealed trait BankAccountCommand {
  val id: UUID
}

object BankAccountCommand:
  case class Open(id: UUID, at: Instant) extends BankAccountCommand
  case class Deposit(id: UUID, amount: BigDecimal, at: Instant) extends BankAccountCommand
  case class Withdraw(id: UUID, amount: BigDecimal, at: Instant) extends BankAccountCommand
  case class Close(id: UUID, at: Instant) extends BankAccountCommand

enum BankAccountEvent derives Schema:
  case Opened(id: UUID, at: Instant)
  case Deposited(id: UUID, amount: BigDecimal, at: Instant)
  case Withdrawn(id: UUID, amount: BigDecimal, at: Instant)
  case ClosedAccount(id: UUID, at: Instant)


val bankAccount: Decider[BankAccountState, BankAccountCommand, Either[BankAccountError, List[BankAccountEvent]]] =
  DeciderBuilder
    .seed[BankAccountState]("bank-account", BankAccountState.Uninitialized)
    .decide[BankAccountCommand, Either[BankAccountError, List[BankAccountEvent]]](_.decide(_))
    .evolveErrorList(_.evolve(_))

def transactionsProjection(repo: BankAccountTransactionRepository[ConnectionIO]): Apparatus[ConnectionIO, List[BankAccountEvent], Int] =
  Apparatus.closedMealy(ClosedMealy.stateless[ConnectionIO, List[BankAccountEvent], Int] { evs =>
    evs.traverse {
      case BankAccountEvent.Deposited(id, amount, at) => repo.insertTransaction(id, Transaction(TransactionType.Deposit, amount, at))
      case BankAccountEvent.Withdrawn(id, amount, at) => repo.insertTransaction(id, Transaction(TransactionType.Withdrawal, amount, at))
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
