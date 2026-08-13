package apparatus.examples.saga

import apparatus.core.*
import apparatus.core.machines.*
import apparatus.examples.{BookingCorrelated, BookingFlow}
import cats.Applicative
import cats.implicits.*
import zio.blocks.schema.Schema

import java.time.LocalDate
import java.util.UUID


// ── Car ───────────────────────────────────────────────────────────────────────

final case class CarQuery(city: String, from: LocalDate, to: LocalDate)

enum CarState {
  case Seed
  case Searching(bookingId: UUID, flow: BookingFlow)
  case AwaitingLicenseCheck(bookingId: UUID, carModel: String)
  case Reserved(bookingId: UUID)
  case Cancelled

  def decide(cmd: CarCommand): List[CarEvent] = this match
    case CarState.Seed =>
      cmd match
        case CarCommand.InitSearch(id, query, bookingId, flow) => List(CarEvent.SearchStarted(id, bookingId, query, flow))
        case _                                               => Nil
    case CarState.Searching(bookingId, _) =>
      cmd match
        case CarCommand.SelectCar(id, _)              => List(CarEvent.Reserved(id, bookingId))
        case CarCommand.NoCarFound(id)                => List(CarEvent.Failed(id, bookingId))
        case CarCommand.RequestLicenseCheck(id, model) => List(CarEvent.LicenseCheckRequired(id, bookingId, model))
        case _                                        => Nil
    case CarState.AwaitingLicenseCheck(bookingId, _) =>
      cmd match
        case CarCommand.VerifyDriverLicense(id) => List(CarEvent.Reserved(id, bookingId))
        case CarCommand.RejectDriverLicense(id) => List(CarEvent.Failed(id, bookingId))
        case _                                  => Nil
    case CarState.Reserved(bookingId) =>
      cmd match
        case CarCommand.Cancel(id) => List(CarEvent.Compensated(id, bookingId))
        case _                     => Nil
    case CarState.Cancelled => Nil

  def evolve(ev: CarEvent): CarState = this match
    case CarState.Seed =>
      ev match
        case CarEvent.SearchStarted(_, bookingId, _, flow) => CarState.Searching(bookingId, flow)
        case _                                             => this
    case CarState.Searching(bookingId, _) =>
      ev match
        case CarEvent.Reserved(_, _) => CarState.Reserved(bookingId)
        case CarEvent.Failed(_, _)   => CarState.Seed
        case CarEvent.LicenseCheckRequired(_, _, model) => CarState.AwaitingLicenseCheck(bookingId, model)
        case _                       => this
    case CarState.AwaitingLicenseCheck(bookingId, model) =>
      ev match
        case CarEvent.Reserved(_, _) => CarState.Reserved(bookingId)
        case CarEvent.Failed(_, _)   => CarState.Seed
        case _                       => this
    case CarState.Reserved(_) =>
      ev match
        case CarEvent.Compensated(_, _) => CarState.Cancelled
        case _                          => this
    case CarState.Cancelled => this
}

sealed trait CarCommand:
  val id: UUID

object CarCommand:
  case class InitSearch(id: UUID, query: CarQuery, bookingId: UUID, flow: BookingFlow) extends CarCommand
  case class SelectCar(id: UUID, carModel: String)                                    extends CarCommand
  case class NoCarFound(id: UUID)                                                     extends CarCommand
  case class RequestLicenseCheck(id: UUID, carModel: String)                          extends CarCommand
  case class VerifyDriverLicense(id: UUID)                                            extends CarCommand
  case class RejectDriverLicense(id: UUID)                                            extends CarCommand
  case class Cancel(id: UUID)                                                         extends CarCommand

enum CarEvent extends BookingCorrelated derives Schema:
  case SearchStarted(id: UUID, bookingId: UUID, query: CarQuery, flow: BookingFlow)
  case LicenseCheckRequired(id: UUID, bookingId: UUID, carModel: String)
  case Reserved(id: UUID, bookingId: UUID)
  case Failed(id: UUID, bookingId: UUID)
  case Compensated(id: UUID, bookingId: UUID)

trait CarService[F[_]]:
  def searchCar(query: CarQuery): F[Option[String]]

final class DefaultCarService[F[_]: Applicative] extends CarService[F]:
  def searchCar(query: CarQuery): F[Option[String]] = Some("Tesla Model 3").pure[F]

val carDecider: Decider[CarState, CarCommand, List[CarEvent]] =
  DeciderBuilder
    .seed[CarState]("car", CarState.Seed)
    .decide[CarCommand, List[CarEvent]](_.decide(_))
    .evolveList(_.evolve(_))

def carMachine[F[_]: Applicative](
                                   decider: Decider[CarState, CarCommand, List[CarEvent]] = carDecider
                                 ): Apparatus[F, CarCommand, List[CarEvent]] =
  Apparatus.aggregateMachine[F, CarCommand, CarEvent](decider, _.id)

def carSearchConnector[F[_]: Applicative](service: CarService[F]): Apparatus[F, CarEvent, List[CarCommand]] =
  Apparatus.closedMealy(ClosedMealy.stateless[F, CarEvent, List[CarCommand]] {
    case CarEvent.SearchStarted(id, _, query, BookingFlow.Civilian) =>
      service.searchCar(query).map {
        case Some(carModel) => List(CarCommand.SelectCar(id, carModel))
        case None           => List(CarCommand.NoCarFound(id))
      }
    case CarEvent.SearchStarted(id, _, query, BookingFlow.Diplomat) =>
      service.searchCar(query).map {
        case Some(carModel) => List(CarCommand.RequestLicenseCheck(id, carModel))
        case None           => List(CarCommand.NoCarFound(id))
      }
    case _ => Nil.pure[F]
  })
