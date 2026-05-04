package apparatus

import apparatus.core.*
import doobie.Meta

enum BankAccountState:
  case Uninitialized
  case Active(balance: BigDecimal)
  case Closed

  def evolve(events: List[BankAccountEvent]): BankAccountState =
    events.foldLeft(this)((acc: BankAccountState, ev: BankAccountEvent) => acc match {
      case BankAccountState.Uninitialized =>
        ev match {
          case BankAccountEvent.Opened => BankAccountState.Active(0)
          case _ => acc
        }
      case BankAccountState.Active(balance) =>
        ev match {
          case BankAccountEvent.Deposited(amount) => BankAccountState.Active(balance + amount)
          case BankAccountEvent.Withdrawn(amount) => BankAccountState.Active(balance - amount)
          case BankAccountEvent.ClosedAccount => BankAccountState.Closed
          case _ => acc
        }
      case BankAccountState.Closed => acc
    })

  def decide(cmd: BankAccountCommand): List[BankAccountEvent] = this match {
    case BankAccountState.Uninitialized => cmd match {
      case BankAccountCommand.Open => List(BankAccountEvent.Opened)
      case _ => List(BankAccountEvent.Rejected("invalid command for current state"))
    }
    case BankAccountState.Active(balance) => cmd match {
      case BankAccountCommand.Deposit(amount) => List(if (amount <= 0) BankAccountEvent.Rejected("amount must be positive") else BankAccountEvent.Deposited(amount))
      case BankAccountCommand.Withdraw(amount) =>
        List(if (amount <= 0) BankAccountEvent.Rejected("amount must be positive") else if (amount > balance) BankAccountEvent.Rejected("insufficient funds") else BankAccountEvent.Withdrawn(amount))
      case BankAccountCommand.Close => List(BankAccountEvent.ClosedAccount)
      case _ => Nil
    }
    case BankAccountState.Closed => List(BankAccountEvent.Rejected("invalid command for current state"))
  }

enum BankAccountCommand:
  case Open
  case Deposit(amount: BigDecimal)
  case Withdraw(amount: BigDecimal)
  case Close

enum BankAccountEvent:
  case Opened
  case Deposited(amount: BigDecimal)
  case Withdrawn(amount: BigDecimal)
  case ClosedAccount
  case Rejected(reason: String)

object BankAccountEvent:
  given Meta[BankAccountEvent] = Meta[String].timap {
    case "Opened" => Opened
    case "Closed" => ClosedAccount
    case s if s.startsWith("Deposited:") => Deposited(BigDecimal(s.drop(10)))
    case s if s.startsWith("Withdrawn:") => Withdrawn(BigDecimal(s.drop(10)))
    case s if s.startsWith("Rejected:") => Rejected(s.drop(9))
    case other => Rejected(s"unknown:$other")
  } {
    case Opened => "Opened"
    case ClosedAccount => "Closed"
    case Deposited(amt) => s"Deposited:$amt"
    case Withdrawn(amt) => s"Withdrawn:$amt"
    case Rejected(r) => s"Rejected:$r"
  }

val bankAccount: Decider[BankAccountState, BankAccountCommand, List[BankAccountEvent]] =
  Decider(
    state = BankAccountState.Uninitialized,
    decide = (cmd, state) => state.decide(cmd),
    evolve = (events, state) => state.evolve(events)
  )
