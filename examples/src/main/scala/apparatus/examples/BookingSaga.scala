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

/** Booking saga service events carry aggregate `id` and saga `bookingId`. */
trait BookingCorrelated extends SagaCorrelated:
  def id: UUID
  def bookingId: UUID
  final def correlationId: UUID = bookingId

// ── Booking flow ──────────────────────────────────────────────────────────────

enum BookingFlow derives Eq, Schema:
  case Civilian
  case Diplomat

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

private def flightSearchConnector[F[_]: Applicative](service: FlightService[F]): Apparatus[F, FlightEvent, List[FlightCommand]] =
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

private def carSearchConnector[F[_]: Applicative](service: CarService[F]): Apparatus[F, CarEvent, List[CarCommand]] =
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

private def hotelSearchConnector[F[_]: Applicative](service: HotelService[F]): Apparatus[F, HotelEvent, List[HotelCommand]] =
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
  toDate: LocalDate,
  flow: BookingFlow = BookingFlow.Civilian
) derives Schema

object BookingCommand:
  case class Start(id: UUID, state: BookingSagaState) extends BookingCommand
  case class Compensate(id: UUID) extends BookingCommand
  case class Advance(id: UUID, step: BookingStep, phase: SagaPhase, result: SagaStepResult) extends BookingCommand

  val advanceCodec: SagaAdvanceCodec[BookingCommand, BookingStep] =
    SagaAdvanceCodec(
      {
        case BookingCommand.Advance(id, step, phase, result) => Some((id, step, phase, result))
        case _ => None
      },
      (id, step, phase, result) => BookingCommand.Advance(id, step, phase, result)
    )

enum BookingStep(val position: Int) derives Eq, Order, Show, Schema:
  case Hotel extends BookingStep(1)
  case Car extends BookingStep(2)
  case Flight extends BookingStep(3)

def bookingBehavior(
  uuidGen: SagaStepCorrelationIdGenerator[BookingStep] = SagaStepCorrelationIdGenerator.random
): SagaBehavior[BookingCommand, BookingStep, BookingSagaState] =
  SagaBehaviorFactory(
    name = "booking",
    startCommand = { case BookingCommand.Start(id, state) => (id, state) },
    compensateCommand = { case BookingCommand.Compensate(id) => id },
    commandCorrelationId = _.id,
    codec = BookingCommand.advanceCodec,
    steps = NonEmptySet.of(BookingStep.Hotel, BookingStep.Car, BookingStep.Flight),
    uuidGen = uuidGen
  )

val behavior: SagaBehavior[BookingCommand, BookingStep, BookingSagaState] = bookingBehavior()

val flightStep = new SagaStepAdapter[FlightCommand, FlightEvent, BookingStep, BookingSagaState] {
  override def step: BookingStep = BookingStep.Flight
  override def start(id: UUID, state: BookingSagaState, correlationId: UUID): FlightCommand =
    FlightCommand.InitSearch(id, FlightQuery(state.fromCity, state.toCity, state.fromDate, state.toDate), correlationId, state.flow)
  override def compensate(id: UUID): FlightCommand = FlightCommand.Cancel(id)
  override def classify(event: FlightEvent): Option[(SagaPhase, SagaStepResult)] =
    SagaStepAdapter.classifyReserveSearch(
      { case FlightEvent.Reserved(_, _)    => () },
      { case FlightEvent.Failed(_, _)      => () },
      { case FlightEvent.Compensated(_, _) => () }
    )(event)
}

val carStep = new SagaStepAdapter[CarCommand, CarEvent, BookingStep, BookingSagaState] {
  override def step: BookingStep = BookingStep.Car
  override def start(id: UUID, state: BookingSagaState, correlationId: UUID): CarCommand =
    CarCommand.InitSearch(id, CarQuery(state.toCity, state.fromDate, state.toDate), correlationId, state.flow)
  override def compensate(id: UUID): CarCommand = CarCommand.Cancel(id)
  override def classify(event: CarEvent): Option[(SagaPhase, SagaStepResult)] =
    SagaStepAdapter.classifyReserveSearch(
      { case CarEvent.Reserved(_, _)    => () },
      { case CarEvent.Failed(_, _)      => () },
      { case CarEvent.Compensated(_, _) => () }
    )(event)
}

val hotelStep = new SagaStepAdapter[HotelCommand, HotelEvent, BookingStep, BookingSagaState] {
  override def step: BookingStep = BookingStep.Hotel
  override def start(id: UUID, state: BookingSagaState, correlationId: UUID): HotelCommand =
    HotelCommand.InitSearch(id, HotelQuery(state.toCity, state.fromDate, state.toDate), correlationId, state.flow)
  override def compensate(id: UUID): HotelCommand = HotelCommand.Cancel(id)
  override def classify(event: HotelEvent): Option[(SagaPhase, SagaStepResult)] =
    SagaStepAdapter.classifyReserveSearch(
      { case HotelEvent.Reserved(_, _)    => () },
      { case HotelEvent.Failed(_, _)      => () },
      { case HotelEvent.Compensated(_, _) => () }
    )(event)
}

type BookingDecider = Decider[SagaState[BookingStep, BookingSagaState], BookingCommand, List[SagaEvent[BookingStep, BookingSagaState]]]

/** Sub-aggregate machines wired into the booking saga service network. */
final case class BookingMachines[F[_]](
  flight: Apparatus[F, FlightCommand, List[FlightEvent]],
  car:    Apparatus[F, CarCommand, List[CarEvent]],
  hotel:  Apparatus[F, HotelCommand, List[HotelEvent]]
)

object BookingMachines:
  def default[F[_]: Applicative]: BookingMachines[F] =
    BookingMachines(flightMachine[F](), carMachine[F](), hotelMachine[F]())

private def serviceFSM[F[_]: Applicative, Cmd, Evt <: SagaCorrelated](
  adapter:   SagaStepAdapter[Cmd, Evt, BookingStep, BookingSagaState],
  machine:   Apparatus[F, Cmd, List[Evt]],
  connector: Apparatus[F, Evt, List[Cmd]],
  label:     String
): Apparatus[F, SagaEvent[BookingStep, BookingSagaState], List[BookingCommand]] =
  adapter
    .adapt(machine.feedback(connector), BookingCommand.advanceCodec)
    .label(label)

private def serviceNetwork[F[_]: Applicative](
  services: BookingServices[F],
  machines: BookingMachines[F]
): Apparatus[F, SagaEvent[BookingStep, BookingSagaState], List[BookingCommand]] =
  serviceFSM(flightStep, machines.flight, flightSearchConnector(services.flight), "Flight service")
    .merge(serviceFSM(carStep, machines.car, carSearchConnector(services.car), "Car service"))
    .merge(serviceFSM(hotelStep, machines.hotel, hotelSearchConnector(services.hotel), "Hotel service"))

private def bookingOrchestratorLoop[F[_]: Applicative](
  booking:  BookingDecider,
  services: BookingServices[F],
  machines: BookingMachines[F]
): Apparatus[F, List[BookingCommand], List[SagaEvent[BookingStep, BookingSagaState]]] =
  Apparatus
    .aggregateMachine[F, BookingCommand, SagaEvent[BookingStep, BookingSagaState]](booking, _.id)
    .feedbackMany(serviceNetwork(services, machines))

private def serviceCommandLoop[F[_]: Applicative, Cmd, Evt <: SagaCorrelated](
  adapter:   SagaStepAdapter[Cmd, Evt, BookingStep, BookingSagaState],
  machine:   Apparatus[F, Cmd, List[Evt]],
  connector: Apparatus[F, Evt, List[Cmd]],
  loop:      Apparatus[F, List[BookingCommand], List[SagaEvent[BookingStep, BookingSagaState]]]
): Apparatus[F, Cmd, List[SagaEvent[BookingStep, BookingSagaState]]] =
  adapter
    .rmap(machine.feedback(connector), BookingCommand.advanceCodec)
    .andThen(loop)

def saga[F[_]: Applicative](
  bookingServices: BookingServices[F],
  sagaBehavior:    SagaBehavior[BookingCommand, BookingStep, BookingSagaState] = behavior
): Apparatus[F, BookingCommand, List[SagaEvent[BookingStep, BookingSagaState]]] =
  saga(bookingServices, sagaBehavior, BookingMachines.default[F])

def saga[F[_]: Applicative](
  bookingServices: BookingServices[F],
  sagaBehavior:    SagaBehavior[BookingCommand, BookingStep, BookingSagaState],
  machines:        BookingMachines[F]
): Apparatus[F, BookingCommand, List[SagaEvent[BookingStep, BookingSagaState]]] =
  bookingOrchestratorLoop(sagaBehavior.decider, bookingServices, machines).lmap(List(_))

/** Car sub-aggregate entry into the booking orchestrator (e.g. async license verification). */
def carSaga[F[_]: Applicative](
  bookingServices: BookingServices[F],
  sagaBehavior:    SagaBehavior[BookingCommand, BookingStep, BookingSagaState] = behavior
): Apparatus[F, CarCommand, List[SagaEvent[BookingStep, BookingSagaState]]] =
  carSaga(bookingServices, sagaBehavior, BookingMachines.default[F])

def carSaga[F[_]: Applicative](
  bookingServices: BookingServices[F],
  sagaBehavior:    SagaBehavior[BookingCommand, BookingStep, BookingSagaState],
  machines:        BookingMachines[F]
): Apparatus[F, CarCommand, List[SagaEvent[BookingStep, BookingSagaState]]] =
  serviceCommandLoop(
    carStep,
    machines.car,
    carSearchConnector(bookingServices.car),
    bookingOrchestratorLoop(sagaBehavior.decider, bookingServices, machines)
  )

/** Hotel sub-aggregate entry into the booking orchestrator (e.g. async background check). */
def hotelSaga[F[_]: Applicative](
  bookingServices: BookingServices[F],
  sagaBehavior:    SagaBehavior[BookingCommand, BookingStep, BookingSagaState] = behavior
): Apparatus[F, HotelCommand, List[SagaEvent[BookingStep, BookingSagaState]]] =
  hotelSaga(bookingServices, sagaBehavior, BookingMachines.default[F])

def hotelSaga[F[_]: Applicative](
  bookingServices: BookingServices[F],
  sagaBehavior:    SagaBehavior[BookingCommand, BookingStep, BookingSagaState],
  machines:        BookingMachines[F]
): Apparatus[F, HotelCommand, List[SagaEvent[BookingStep, BookingSagaState]]] =
  serviceCommandLoop(
    hotelStep,
    machines.hotel,
    hotelSearchConnector(bookingServices.hotel),
    bookingOrchestratorLoop(sagaBehavior.decider, bookingServices, machines)
  )

/** Flight sub-aggregate entry into the booking orchestrator (e.g. async clearance check). */
def flightSaga[F[_]: Applicative](
  bookingServices: BookingServices[F],
  sagaBehavior:    SagaBehavior[BookingCommand, BookingStep, BookingSagaState] = behavior
): Apparatus[F, FlightCommand, List[SagaEvent[BookingStep, BookingSagaState]]] =
  flightSaga(bookingServices, sagaBehavior, BookingMachines.default[F])

def flightSaga[F[_]: Applicative](
  bookingServices: BookingServices[F],
  sagaBehavior:    SagaBehavior[BookingCommand, BookingStep, BookingSagaState],
  machines:        BookingMachines[F]
): Apparatus[F, FlightCommand, List[SagaEvent[BookingStep, BookingSagaState]]] =
  serviceCommandLoop(
    flightStep,
    machines.flight,
    flightSearchConnector(bookingServices.flight),
    bookingOrchestratorLoop(sagaBehavior.decider, bookingServices, machines)
  )
