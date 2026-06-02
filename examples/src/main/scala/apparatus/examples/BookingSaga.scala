package apparatus.examples

import apparatus.core.*
import apparatus.core.machines.*
import apparatus.core.patterns.*
import cats.*
import cats.data.NonEmptySet
import cats.implicits.*
import cats.derived.*
import zio.blocks.schema.Schema
import java.time.LocalDate
import java.util.UUID

// ── Sub-aggregate IDs (fixed for this example) ───────────────────────────────

val flightId  = UUID.fromString("00000000-0000-0000-0000-000000000001")
val carId     = UUID.fromString("00000000-0000-0000-0000-000000000002")
val hotelId   = UUID.fromString("00000000-0000-0000-0000-000000000003")
val bookingId = UUID.fromString("00000000-0000-0000-0000-000000000004")

/** Booking saga service events carry aggregate `id` and saga `bookingId`. */
trait BookingCorrelated extends SagaCorrelated:
  def id: UUID
  def bookingId: UUID
  final def correlationId: UUID = bookingId

// ── Flight ────────────────────────────────────────────────────────────────────

final case class FlightQuery(from: String, to: String, fromDate: LocalDate, toDate: LocalDate)

enum FlightState {
  case Seed
  case Searching(bookingId: UUID)
  case Reserved(bookingId: UUID)
  case Cancelled

  def decide(cmd: FlightCommand): List[FlightEvent] = this match
    case FlightState.Seed =>
      cmd match
        case FlightCommand.InitSearch(id, query, bookingId) => List(FlightEvent.SearchStarted(id, bookingId, query))
        case _                                              => Nil
    case FlightState.Searching(bookingId) =>
      cmd match
        case FlightCommand.SelectFlight(id, _) => List(FlightEvent.Reserved(id, bookingId))
        case FlightCommand.NoFlightFound(id)   => List(FlightEvent.Failed(id, bookingId))
        case _                                 => Nil
    case FlightState.Reserved(bookingId) =>
      cmd match
        case FlightCommand.Cancel(id) => List(FlightEvent.Compensated(id, bookingId))
        case _                        => Nil
    case FlightState.Cancelled => Nil

  def evolve(ev: FlightEvent): FlightState = this match
    case FlightState.Seed =>
      ev match
        case FlightEvent.SearchStarted(_, bookingId, _) => FlightState.Searching(bookingId)
        case _                                          => this
    case FlightState.Searching(bookingId) =>
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
  case class InitSearch(id: UUID, query: FlightQuery, bookingId: UUID) extends FlightCommand
  case class SelectFlight(id: UUID, flightNumber: String)            extends FlightCommand
  case class NoFlightFound(id: UUID)                                 extends FlightCommand
  case class Cancel(id: UUID)                                        extends FlightCommand

enum FlightEvent extends BookingCorrelated derives Schema:
  case SearchStarted(id: UUID, bookingId: UUID, query: FlightQuery)
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

private def flightSearchConnector[F[_]: Applicative](service: FlightService[F]): Apparatus[F, FlightEvent, List[FlightCommand]] =
  Apparatus.closedMealy(ClosedMealy.stateless[F, FlightEvent, List[FlightCommand]] {
    case FlightEvent.SearchStarted(id, _, query) =>
      service.searchFlight(query).map {
        case Some(flightNumber) => List(FlightCommand.SelectFlight(id, flightNumber))
        case None               => List(FlightCommand.NoFlightFound(id))
      }
    case _ => Nil.pure[F]
  })

// ── Car ───────────────────────────────────────────────────────────────────────

final case class CarQuery(city: String, from: LocalDate, to: LocalDate)

enum CarState {
  case Seed
  case Searching(bookingId: UUID)
  case Reserved(bookingId: UUID)
  case Cancelled

  def decide(cmd: CarCommand): List[CarEvent] = this match
    case CarState.Seed =>
      cmd match
        case CarCommand.InitSearch(id, query, bookingId) => List(CarEvent.SearchStarted(id, bookingId, query))
        case _                                         => Nil
    case CarState.Searching(bookingId) =>
      cmd match
        case CarCommand.SelectCar(id, _) => List(CarEvent.Reserved(id, bookingId))
        case CarCommand.NoCarFound(id)   => List(CarEvent.Failed(id, bookingId))
        case _                           => Nil
    case CarState.Reserved(bookingId) =>
      cmd match
        case CarCommand.Cancel(id) => List(CarEvent.Compensated(id, bookingId))
        case _                     => Nil
    case CarState.Cancelled => Nil

  def evolve(ev: CarEvent): CarState = this match
    case CarState.Seed =>
      ev match
        case CarEvent.SearchStarted(_, bookingId, _) => CarState.Searching(bookingId)
        case _                                       => this
    case CarState.Searching(bookingId) =>
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
  case class InitSearch(id: UUID, query: CarQuery, bookingId: UUID) extends CarCommand
  case class SelectCar(id: UUID, carModel: String)                  extends CarCommand
  case class NoCarFound(id: UUID)                                   extends CarCommand
  case class Cancel(id: UUID)                                       extends CarCommand

enum CarEvent extends BookingCorrelated derives Schema:
  case SearchStarted(id: UUID, bookingId: UUID, query: CarQuery)
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

private def carSearchConnector[F[_]: Applicative](service: CarService[F]): Apparatus[F, CarEvent, List[CarCommand]] =
  Apparatus.closedMealy(ClosedMealy.stateless[F, CarEvent, List[CarCommand]] {
    case CarEvent.SearchStarted(id, _, query) =>
      service.searchCar(query).map {
        case Some(carModel) => List(CarCommand.SelectCar(id, carModel))
        case None           => List(CarCommand.NoCarFound(id))
      }
    case _ => Nil.pure[F]
  })

// ── Hotel ─────────────────────────────────────────────────────────────────────

final case class HotelQuery(city: String, from: LocalDate, to: LocalDate)


enum HotelState {
  case Seed
  case Searching(bookingId: UUID)
  case Reserved(bookingId: UUID)
  case Cancelled

  def decide(cmd: HotelCommand): List[HotelEvent] = this match
    case HotelState.Seed =>
      cmd match
        case HotelCommand.InitSearch(id, query, bookingId) => List(HotelEvent.SearchStarted(id, bookingId, query))
        case _                                             => Nil
    case HotelState.Searching(bookingId) =>
      cmd match
        case HotelCommand.SelectHotel(id, _) => List(HotelEvent.Reserved(id, bookingId))
        case HotelCommand.NoHotelFound(id)   => List(HotelEvent.Failed(id, bookingId))
        case _                                 => Nil
    case HotelState.Reserved(bookingId) =>
      cmd match
        case HotelCommand.Cancel(id) => List(HotelEvent.Compensated(id, bookingId))
        case _                       => Nil
    case HotelState.Cancelled => Nil

  def evolve(ev: HotelEvent): HotelState = this match
    case HotelState.Seed =>
      ev match
        case HotelEvent.SearchStarted(_, bookingId, _) => HotelState.Searching(bookingId)
        case _                                         => this
    case HotelState.Searching(bookingId) =>
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
  case class InitSearch(id: UUID, query: HotelQuery, bookingId: UUID) extends HotelCommand
  case class SelectHotel(id: UUID, hotelName: String)                 extends HotelCommand
  case class NoHotelFound(id: UUID)                                   extends HotelCommand
  case class Cancel(id: UUID)                                         extends HotelCommand

enum HotelEvent extends BookingCorrelated derives Schema:
  case SearchStarted(id: UUID, bookingId: UUID, query: HotelQuery)
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

private def hotelSearchConnector[F[_]: Applicative](service: HotelService[F]): Apparatus[F, HotelEvent, List[HotelCommand]] =
  Apparatus.closedMealy(ClosedMealy.stateless[F, HotelEvent, List[HotelCommand]] {
    case HotelEvent.SearchStarted(id, _, query) =>
      service.searchHotel(query).map {
        case Some(hotelName) => List(HotelCommand.SelectHotel(id, hotelName))
        case None            => List(HotelCommand.NoHotelFound(id))
      }
    case _ => Nil.pure[F]
  })

final case class BookingServices[F[_]](
  flight: FlightService[F],
  car:    CarService[F],
  hotel:  HotelService[F]
)

object BookingServices:
  def default[F[_]: Applicative]: BookingServices[F] =
    BookingServices(new DefaultFlightService[F], new DefaultCarService[F], new DefaultHotelService[F])

// ── Booking saga domain ───────────────────────────────────────────────────────

sealed trait BookingCommand:
  val id: UUID

final case class BookingSagaState(
  fromCity: String,
  toCity: String,
  fromDate: LocalDate,
  toDate: LocalDate
) derives Schema

object BookingCommand:
  case class Start(id: UUID, state: BookingSagaState) extends BookingCommand
  case class Compensate(id: UUID) extends BookingCommand
  case class Advance(id: UUID, step: BookingStep, phase: SagaPhase, result: SagaStepResult) extends BookingCommand

  val advancePrism: SagaAdvancePrism[BookingCommand, BookingStep] =
    new Prism[BookingCommand, (UUID, BookingStep, SagaPhase, SagaStepResult)] {
      override def getOption(input: BookingCommand): Option[(UUID, BookingStep, SagaPhase, SagaStepResult)] =
        input match {
          case BookingCommand.Advance(id, step, phase, result) => Some((id, step, phase, result))
          case _ => None
        }

      override def reverseGet(input: (UUID, BookingStep, SagaPhase, SagaStepResult)): BookingCommand =
        val (id, step, phase, result) = input
        BookingCommand.Advance(id, step, phase, result)
    }

enum BookingStep(val position: Int) derives Eq, Order, Show, Schema:
  case Hotel extends BookingStep(1)
  case Car extends BookingStep(2)
  case Flight extends BookingStep(3)

val bookingStepSagaStepCorrelationIdGenerator: SagaStepCorrelationIdGenerator[BookingStep] = new SagaStepCorrelationIdGenerator[BookingStep]:
  override def next(step: BookingStep): UUID = step match
    case BookingStep.Hotel => hotelId
    case BookingStep.Car => carId
    case BookingStep.Flight => flightId

val flightStep = new SagaStepAdapter[FlightCommand, FlightEvent, BookingStep, BookingSagaState] {
  override def step: BookingStep = BookingStep.Flight
  override def start(id: UUID, state: BookingSagaState, correlationId: UUID): FlightCommand =
    FlightCommand.InitSearch(id, FlightQuery(state.fromCity, state.toCity, state.fromDate, state.toDate), correlationId)
  override def compensate(id: UUID): FlightCommand = FlightCommand.Cancel(id)
  override def classify(event: FlightEvent): Option[(SagaPhase, SagaStepResult)] = event match {
    case FlightEvent.Reserved(_, _)     => Some(SagaPhase.Forward -> SagaStepResult.Completed)
    case FlightEvent.Failed(_, _)       => Some(SagaPhase.Forward -> SagaStepResult.Failed)
    case FlightEvent.Compensated(_, _)  => Some(SagaPhase.Compensation -> SagaStepResult.Completed)
    case _                              => None
  }
}

val carStep = new SagaStepAdapter[CarCommand, CarEvent, BookingStep, BookingSagaState] {
  override def step: BookingStep = BookingStep.Car
  override def start(id: UUID, state: BookingSagaState, correlationId: UUID): CarCommand =
    CarCommand.InitSearch(id, CarQuery(state.toCity, state.fromDate, state.toDate), correlationId)
  override def compensate(id: UUID): CarCommand = CarCommand.Cancel(id)
  override def classify(event: CarEvent): Option[(SagaPhase, SagaStepResult)] = event match {
    case CarEvent.Reserved(_, _)     => Some(SagaPhase.Forward -> SagaStepResult.Completed)
    case CarEvent.Failed(_, _)       => Some(SagaPhase.Forward -> SagaStepResult.Failed)
    case CarEvent.Compensated(_, _)  => Some(SagaPhase.Compensation -> SagaStepResult.Completed)
    case _                           => None
  }
}

val hotelStep = new SagaStepAdapter[HotelCommand, HotelEvent, BookingStep, BookingSagaState] {
  override def step: BookingStep = BookingStep.Hotel
  override def start(id: UUID, state: BookingSagaState, correlationId: UUID): HotelCommand =
    HotelCommand.InitSearch(id, HotelQuery(state.toCity, state.fromDate, state.toDate), correlationId)
  override def compensate(id: UUID): HotelCommand = HotelCommand.Cancel(id)
  override def classify(event: HotelEvent): Option[(SagaPhase, SagaStepResult)] = event match {
    case HotelEvent.Reserved(_, _)     => Some(SagaPhase.Forward -> SagaStepResult.Completed)
    case HotelEvent.Failed(_, _)       => Some(SagaPhase.Forward -> SagaStepResult.Failed)
    case HotelEvent.Compensated(_, _)  => Some(SagaPhase.Compensation -> SagaStepResult.Completed)
    case _                             => None
  }
}

val behavior: SagaBehavior[BookingCommand, BookingStep, BookingSagaState] = SagaBehaviorFactory(
  name = "booking",
  startCommandClass = classOf[BookingCommand.Start],
  compensateCommandClass = classOf[BookingCommand.Compensate],
  sagaIdExtractor = _.id,
  sagaStateExtractor = { case BookingCommand.Start(_, state) => state },
  prism = BookingCommand.advancePrism,
  steps = NonEmptySet.of(BookingStep.Hotel, BookingStep.Car, BookingStep.Flight),
  uuidGen = bookingStepSagaStepCorrelationIdGenerator
)

type BookingDecider = Decider[SagaState[BookingStep, BookingSagaState], BookingCommand, List[SagaEvent[BookingStep, BookingSagaState]]]

private def flightServiceFSM[F[_]: Applicative](
  services: BookingServices[F],
  machine:  Apparatus[F, FlightCommand, List[FlightEvent]]
): Apparatus[F, SagaEvent[BookingStep, BookingSagaState], List[BookingCommand]] =
  flightStep
    .rmap(
      flightStep.lmapOrEmpty(machine.feedback(flightSearchConnector(services.flight))),
      BookingCommand.advancePrism
    )
    .label("Flight service")

private def carServiceFSM[F[_]: Applicative](
  services: BookingServices[F],
  machine:  Apparatus[F, CarCommand, List[CarEvent]]
): Apparatus[F, SagaEvent[BookingStep, BookingSagaState], List[BookingCommand]] =
  carStep
    .rmap(
      carStep.lmapOrEmpty(machine.feedback(carSearchConnector(services.car))),
      BookingCommand.advancePrism
    )
    .label("Car service")

private def hotelServiceFSM[F[_]: Applicative](
  services: BookingServices[F],
  machine:  Apparatus[F, HotelCommand, List[HotelEvent]]
): Apparatus[F, SagaEvent[BookingStep, BookingSagaState], List[BookingCommand]] =
  hotelStep
    .rmap(
      hotelStep.lmapOrEmpty(machine.feedback(hotelSearchConnector(services.hotel))),
      BookingCommand.advancePrism
    )
    .label("Hotel service")

private def serviceNetwork[F[_]: Applicative](
  bookingServices: BookingServices[F],
  flight:          Apparatus[F, FlightCommand, List[FlightEvent]],
  car:             Apparatus[F, CarCommand, List[CarEvent]],
  hotel:           Apparatus[F, HotelCommand, List[HotelEvent]]
): Apparatus[F, SagaEvent[BookingStep, BookingSagaState], List[BookingCommand]] =
  flightServiceFSM[F](bookingServices, flight)
    .merge(carServiceFSM[F](bookingServices, car).merge(hotelServiceFSM[F](bookingServices, hotel)))

def saga[F[_]: Applicative](bookingServices: BookingServices[F]): Apparatus[F, BookingCommand, List[SagaEvent[BookingStep, BookingSagaState]]] =
  Apparatus.aggregateMachine[F, BookingCommand, SagaEvent[BookingStep, BookingSagaState]](behavior.decider, _.id)
    .feedback(serviceNetwork(bookingServices, flightMachine[F](), carMachine[F](), hotelMachine[F]()))

def sagaRerootedAtCar[F[_]: Applicative](
  booking:         BookingDecider,
  bookingServices: BookingServices[F]
): Apparatus[F, CarCommand, List[SagaEvent[BookingStep, BookingSagaState]]] =
  sagaRerootedAtCar(booking, bookingServices, flightMachine[F](), carMachine[F](), hotelMachine[F]())

def sagaRerootedAtCar[F[_]: Applicative](
  booking:         BookingDecider,
  bookingServices: BookingServices[F],
  flight:          Apparatus[F, FlightCommand, List[FlightEvent]],
  car:             Apparatus[F, CarCommand, List[CarEvent]],
  hotel:           Apparatus[F, HotelCommand, List[HotelEvent]]
): Apparatus[F, CarCommand, List[SagaEvent[BookingStep, BookingSagaState]]] =
  carStep
    .rmap(
      car.feedback(carSearchConnector(bookingServices.car)),
      BookingCommand.advancePrism
    )
    .andThen(
      Apparatus.aggregateMachine[F, BookingCommand, SagaEvent[BookingStep, BookingSagaState]](booking, _.id)
        .feedbackMany(serviceNetwork(bookingServices, flight, car, hotel))
    )
