package apparatus.examples.saga

import apparatus.core.*
import apparatus.core.machines.*
import apparatus.examples.{BookingCorrelated, BookingFlow}
import cats.Applicative
import cats.implicits.*
import zio.blocks.schema.Schema

import java.time.LocalDate
import java.util.UUID


// ── Hotel ─────────────────────────────────────────────────────────────────────

final case class HotelQuery(city: String, from: LocalDate, to: LocalDate)

enum HotelState {
  case Seed
  case Searching(bookingId: UUID, flow: BookingFlow)
  case AwaitingBackgroundCheck(bookingId: UUID, hotelName: String)
  case Reserved(bookingId: UUID)
  case Cancelled

  def decide(cmd: HotelCommand): List[HotelEvent] = this match
    case HotelState.Seed =>
      cmd match
        case HotelCommand.InitSearch(id, query, bookingId, flow) => List(HotelEvent.SearchStarted(id, bookingId, query, flow))
        case _                                                   => Nil
    case HotelState.Searching(bookingId, _) =>
      cmd match
        case HotelCommand.SelectHotel(id, _)               => List(HotelEvent.Reserved(id, bookingId))
        case HotelCommand.NoHotelFound(id)                 => List(HotelEvent.Failed(id, bookingId))
        case HotelCommand.RequestBackgroundCheck(id, name) => List(HotelEvent.BackgroundCheckRequired(id, bookingId, name))
        case _                                             => Nil
    case HotelState.AwaitingBackgroundCheck(bookingId, _) =>
      cmd match
        case HotelCommand.VerifyBackgroundCheck(id) => List(HotelEvent.Reserved(id, bookingId))
        case HotelCommand.RejectBackgroundCheck(id) => List(HotelEvent.Failed(id, bookingId))
        case _                                      => Nil
    case HotelState.Reserved(bookingId) =>
      cmd match
        case HotelCommand.Cancel(id) => List(HotelEvent.Compensated(id, bookingId))
        case _                       => Nil
    case HotelState.Cancelled => Nil

  def evolve(ev: HotelEvent): HotelState = this match
    case HotelState.Seed =>
      ev match
        case HotelEvent.SearchStarted(_, bookingId, _, flow) => HotelState.Searching(bookingId, flow)
        case _                                               => this
    case HotelState.Searching(bookingId, _) =>
      ev match
        case HotelEvent.Reserved(_, _) => HotelState.Reserved(bookingId)
        case HotelEvent.Failed(_, _)   => HotelState.Seed
        case HotelEvent.BackgroundCheckRequired(_, _, name) => HotelState.AwaitingBackgroundCheck(bookingId, name)
        case _                         => this
    case HotelState.AwaitingBackgroundCheck(bookingId, name) =>
      ev match
        case HotelEvent.Reserved(_, _) => HotelState.Reserved(bookingId)
        case HotelEvent.Failed(_, _)   => HotelState.Seed
        case _                         => this
    case HotelState.Reserved(_) =>
      ev match
        case HotelEvent.Compensated(_, _) => HotelState.Cancelled
        case _                            => this
    case HotelState.Cancelled => this
}

sealed trait HotelCommand:
  val id: UUID

object HotelCommand:
  case class InitSearch(id: UUID, query: HotelQuery, bookingId: UUID, flow: BookingFlow) extends HotelCommand
  case class SelectHotel(id: UUID, hotelName: String)                                      extends HotelCommand
  case class NoHotelFound(id: UUID)                                                        extends HotelCommand
  case class RequestBackgroundCheck(id: UUID, hotelName: String)                           extends HotelCommand
  case class VerifyBackgroundCheck(id: UUID)                                               extends HotelCommand
  case class RejectBackgroundCheck(id: UUID)                                               extends HotelCommand
  case class Cancel(id: UUID)                                                              extends HotelCommand

enum HotelEvent extends BookingCorrelated derives Schema:
  case SearchStarted(id: UUID, bookingId: UUID, query: HotelQuery, flow: BookingFlow)
  case BackgroundCheckRequired(id: UUID, bookingId: UUID, hotelName: String)
  case Reserved(id: UUID, bookingId: UUID)
  case Failed(id: UUID, bookingId: UUID)
  case Compensated(id: UUID, bookingId: UUID)

trait HotelService[F[_]]:
  def searchHotel(query: HotelQuery): F[Option[String]]

final class DefaultHotelService[F[_]: Applicative] extends HotelService[F]:
  def searchHotel(query: HotelQuery): F[Option[String]] = Some("Grand Hotel").pure[F]

val hotelDecider: Decider[HotelState, HotelCommand, List[HotelEvent]] =
  DeciderBuilder
    .seed[HotelState]("hotel", HotelState.Seed)
    .decide[HotelCommand, List[HotelEvent]](_.decide(_))
    .evolveList(_.evolve(_))

def hotelMachine[F[_]: Applicative](
                                     decider: Decider[HotelState, HotelCommand, List[HotelEvent]] = hotelDecider
                                   ): Apparatus[F, HotelCommand, List[HotelEvent]] =
  Apparatus.aggregateMachine[F, HotelCommand, HotelEvent](decider, _.id)

def hotelSearchConnector[F[_]: Applicative](service: HotelService[F]): Apparatus[F, HotelEvent, List[HotelCommand]] =
  Apparatus.closedMealy(ClosedMealy.stateless[F, HotelEvent, List[HotelCommand]] {
    case HotelEvent.SearchStarted(id, _, query, BookingFlow.Civilian) =>
      service.searchHotel(query).map {
        case Some(hotelName) => List(HotelCommand.SelectHotel(id, hotelName))
        case None            => List(HotelCommand.NoHotelFound(id))
      }
    case HotelEvent.SearchStarted(id, _, query, BookingFlow.Diplomat) =>
      service.searchHotel(query).map {
        case Some(hotelName) => List(HotelCommand.RequestBackgroundCheck(id, hotelName))
        case None            => List(HotelCommand.NoHotelFound(id))
      }
    case _ => Nil.pure[F]
  })
