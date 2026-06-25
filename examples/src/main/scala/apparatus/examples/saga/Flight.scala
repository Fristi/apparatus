package apparatus.examples.saga

import apparatus.core.*
import apparatus.core.machines.*
import apparatus.examples.{BookingCorrelated, BookingFlow}
import cats.Applicative
import cats.implicits.*
import zio.blocks.schema.Schema

import java.time.LocalDate
import java.util.UUID

// ── Flight ────────────────────────────────────────────────────────────────────

final case class FlightQuery(from: String, to: String, fromDate: LocalDate, toDate: LocalDate)

enum FlightState {
  case Seed
  case Searching(bookingId: UUID, flow: BookingFlow)
  case AwaitingClearance(bookingId: UUID, flightNumber: String)
  case Reserved(bookingId: UUID)
  case Cancelled

  def decide(cmd: FlightCommand): List[FlightEvent] = this match
    case FlightState.Seed =>
      cmd match
        case FlightCommand.InitSearch(id, query, bookingId, flow) => List(FlightEvent.SearchStarted(id, bookingId, query, flow))
        case _                                                    => Nil
    case FlightState.Searching(bookingId, _) =>
      cmd match
        case FlightCommand.SelectFlight(id, flightNumber) => List(FlightEvent.Reserved(id, bookingId))
        case FlightCommand.NoFlightFound(id)            => List(FlightEvent.Failed(id, bookingId))
        case FlightCommand.RequestClearance(id, number) => List(FlightEvent.ClearanceRequired(id, bookingId, number))
        case _                                            => Nil
    case FlightState.AwaitingClearance(bookingId, _) =>
      cmd match
        case FlightCommand.VerifyClearance(id) => List(FlightEvent.Reserved(id, bookingId))
        case FlightCommand.RejectClearance(id) => List(FlightEvent.Failed(id, bookingId))
        case _                                 => Nil
    case FlightState.Reserved(bookingId) =>
      cmd match
        case FlightCommand.Cancel(id) => List(FlightEvent.Compensated(id, bookingId))
        case _                        => Nil
    case FlightState.Cancelled => Nil

  def evolve(ev: FlightEvent): FlightState = this match
    case FlightState.Seed =>
      ev match
        case FlightEvent.SearchStarted(_, bookingId, _, flow) => FlightState.Searching(bookingId, flow)
        case _                                                => this
    case FlightState.Searching(bookingId, _) =>
      ev match
        case FlightEvent.Reserved(_, _)           => FlightState.Reserved(bookingId)
        case FlightEvent.Failed(_, _)            => FlightState.Seed
        case FlightEvent.ClearanceRequired(_, _, number) => FlightState.AwaitingClearance(bookingId, number)
        case _                                   => this
    case FlightState.AwaitingClearance(bookingId, number) =>
      ev match
        case FlightEvent.Reserved(_, _) => FlightState.Reserved(bookingId)
        case FlightEvent.Failed(_, _)   => FlightState.Seed
        case _                          => this
    case FlightState.Reserved(_) =>
      ev match
        case FlightEvent.Compensated(_, _) => FlightState.Cancelled
        case _                             => this
    case FlightState.Cancelled => this
}

sealed trait FlightCommand:
  val id: UUID

object FlightCommand:
  case class InitSearch(id: UUID, query: FlightQuery, bookingId: UUID, flow: BookingFlow) extends FlightCommand
  case class SelectFlight(id: UUID, flightNumber: String)                                 extends FlightCommand
  case class NoFlightFound(id: UUID)                                                      extends FlightCommand
  case class RequestClearance(id: UUID, flightNumber: String)                             extends FlightCommand
  case class VerifyClearance(id: UUID)                                                    extends FlightCommand
  case class RejectClearance(id: UUID)                                                    extends FlightCommand
  case class Cancel(id: UUID)                                                             extends FlightCommand

enum FlightEvent extends BookingCorrelated derives Schema:
  case SearchStarted(id: UUID, bookingId: UUID, query: FlightQuery, flow: BookingFlow)
  case ClearanceRequired(id: UUID, bookingId: UUID, flightNumber: String)
  case Reserved(id: UUID, bookingId: UUID)
  case Failed(id: UUID, bookingId: UUID)
  case Compensated(id: UUID, bookingId: UUID)

trait FlightService[F[_]]:
  def searchFlight(query: FlightQuery): F[Option[String]]

final class DefaultFlightService[F[_]: Applicative] extends FlightService[F]:
  def searchFlight(query: FlightQuery): F[Option[String]] = Some("BA123").pure[F]

val flightDecider: Decider[FlightState, FlightCommand, List[FlightEvent]] =
  DeciderBuilder
    .seed[FlightState]("flight", FlightState.Seed)
    .decide[FlightCommand, List[FlightEvent]](_.decide(_))
    .evolveList(_.evolve(_))

def flightMachine[F[_]: Applicative](
                                      decider: Decider[FlightState, FlightCommand, List[FlightEvent]] = flightDecider
                                    ): Apparatus[F, FlightCommand, List[FlightEvent]] =
  Apparatus.aggregateMachine[F, FlightCommand, FlightEvent](decider, _.id)

def flightSearchConnector[F[_]: Applicative](service: FlightService[F]): Apparatus[F, FlightEvent, List[FlightCommand]] =
  Apparatus.closedMealy(ClosedMealy.stateless[F, FlightEvent, List[FlightCommand]] {
    case FlightEvent.SearchStarted(id, _, query, BookingFlow.Civilian) =>
      service.searchFlight(query).map {
        case Some(flightNumber) => List(FlightCommand.SelectFlight(id, flightNumber))
        case None               => List(FlightCommand.NoFlightFound(id))
      }
    case FlightEvent.SearchStarted(id, _, query, BookingFlow.Diplomat) =>
      service.searchFlight(query).map {
        case Some(flightNumber) => List(FlightCommand.RequestClearance(id, flightNumber))
        case None               => List(FlightCommand.NoFlightFound(id))
      }
    case _ => Nil.pure[F]
  })
