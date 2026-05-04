package apparatus

import apparatus.core.*
import doobie.Meta

enum BankAccountState:
  case Uninitialized
  case Active(balance: BigDecimal)
  case Closed

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
    case "Opened"                         => Opened
    case "Closed"                         => ClosedAccount
    case s if s.startsWith("Deposited:") => Deposited(BigDecimal(s.drop(10)))
    case s if s.startsWith("Withdrawn:") => Withdrawn(BigDecimal(s.drop(10)))
    case s if s.startsWith("Rejected:")  => Rejected(s.drop(9))
    case other                            => Rejected(s"unknown:$other")
  } {
    case Opened         => "Opened"
    case ClosedAccount  => "Closed"
    case Deposited(amt) => s"Deposited:$amt"
    case Withdrawn(amt) => s"Withdrawn:$amt"
    case Rejected(r)    => s"Rejected:$r"
  }

val bankAccount: Decider[BankAccountState, BankAccountCommand, List[BankAccountEvent]] =
  Decider(
    state = BankAccountState.Uninitialized,
    decide = (cmd, state) => (cmd, state) match
      case (BankAccountCommand.Open, BankAccountState.Uninitialized) =>
        List(BankAccountEvent.Opened)

      case (BankAccountCommand.Deposit(amt), BankAccountState.Active(_)) if amt <= 0 =>
        List(BankAccountEvent.Rejected("amount must be positive"))
      case (BankAccountCommand.Deposit(amt), BankAccountState.Active(_)) =>
        List(BankAccountEvent.Deposited(amt))

      case (BankAccountCommand.Withdraw(amt), BankAccountState.Active(_)) if amt <= 0 =>
        List(BankAccountEvent.Rejected("amount must be positive"))
      case (BankAccountCommand.Withdraw(amt), BankAccountState.Active(bal)) if bal < amt =>
        List(BankAccountEvent.Rejected("insufficient funds"))
      case (BankAccountCommand.Withdraw(amt), BankAccountState.Active(_)) =>
        List(BankAccountEvent.Withdrawn(amt))

      case (BankAccountCommand.Close, BankAccountState.Active(_)) =>
        List(BankAccountEvent.ClosedAccount)

      case _ =>
        List(BankAccountEvent.Rejected("invalid command for current state"))
    ,
    evolve = (events, state) => events.foldLeft(state) {
      case (BankAccountState.Uninitialized, BankAccountEvent.Opened)         => BankAccountState.Active(BigDecimal(0))
      case (BankAccountState.Active(bal), BankAccountEvent.Deposited(amt))   => BankAccountState.Active(bal + amt)
      case (BankAccountState.Active(bal), BankAccountEvent.Withdrawn(amt))   => BankAccountState.Active(bal - amt)
      case (BankAccountState.Active(_), BankAccountEvent.ClosedAccount)      => BankAccountState.Closed
      case (s, _)                                                             => s
    }
  )
