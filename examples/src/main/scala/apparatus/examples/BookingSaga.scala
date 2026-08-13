package apparatus.examples

import apparatus.core.*
import apparatus.core.machines.*
import apparatus.core.patterns.*
import apparatus.examples.saga.*
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

final case class BookingServices[F[_]](flight: FlightService[F], car: CarService[F], hotel: HotelService[F])

object BookingServices:
  def default[F[_]: Applicative]: BookingServices[F] =
    BookingServices(new DefaultFlightService[F], new DefaultCarService[F], new DefaultHotelService[F])

// ── Booking saga domain ───────────────────────────────────────────────────────

sealed trait BookingCommand:
  val id: UUID

final case class BookingSagaState(fromCity: String, toCity: String, fromDate: LocalDate, toDate: LocalDate, flow: BookingFlow) derives Schema

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
  services: BookingServices[F]
): Apparatus[F, SagaEvent[BookingStep, BookingSagaState], List[BookingCommand]] =
  serviceFSM(flightStep, flightMachine[F](), flightSearchConnector(services.flight), "Flight service")
    .merge(serviceFSM(carStep, carMachine[F](), carSearchConnector(services.car), "Car service"))
    .merge(serviceFSM(hotelStep, hotelMachine[F](), hotelSearchConnector(services.hotel), "Hotel service"))

private def bookingOrchestratorLoop[F[_]: Applicative](
  booking:  BookingDecider,
  services: BookingServices[F]
): Apparatus[F, List[BookingCommand], List[SagaEvent[BookingStep, BookingSagaState]]] =
  Apparatus
    .aggregateMachine[F, BookingCommand, SagaEvent[BookingStep, BookingSagaState]](booking, _.id)
    .feedbackMany(serviceNetwork(services))

private def serviceCommandLoop[F[_]: Applicative, Cmd, Evt <: SagaCorrelated](
  adapter:   SagaStepAdapter[Cmd, Evt, BookingStep, BookingSagaState],
  machine:   Apparatus[F, Cmd, List[Evt]],
  connector: Apparatus[F, Evt, List[Cmd]],
  loop:      Apparatus[F, List[BookingCommand], List[SagaEvent[BookingStep, BookingSagaState]]]
): Apparatus[F, Cmd, List[SagaEvent[BookingStep, BookingSagaState]]] =
  adapter
    .rmap(machine.feedback(connector), BookingCommand.advanceCodec)
    .andThen(loop)

def bookingEntrypoint[F[_]: Applicative](
  bookingServices: BookingServices[F],
  sagaBehavior:    SagaBehavior[BookingCommand, BookingStep, BookingSagaState] = behavior
): Apparatus[F, BookingCommand, List[SagaEvent[BookingStep, BookingSagaState]]] =
  bookingOrchestratorLoop(sagaBehavior.decider, bookingServices).lmap(List(_))

/** Car sub-aggregate entry into the booking orchestrator (e.g. async license verification). */
def carEntrypoint[F[_]: Applicative](
  bookingServices: BookingServices[F],
  sagaBehavior:    SagaBehavior[BookingCommand, BookingStep, BookingSagaState] = behavior
): Apparatus[F, CarCommand, List[SagaEvent[BookingStep, BookingSagaState]]] =
  serviceCommandLoop(
    carStep,
    carMachine[F](),
    carSearchConnector(bookingServices.car),
    bookingOrchestratorLoop(sagaBehavior.decider, bookingServices)
  )

/** Hotel sub-aggregate entry into the booking orchestrator (e.g. async background check). */
def hotelEntrypoint[F[_]: Applicative](
  bookingServices: BookingServices[F],
  sagaBehavior:    SagaBehavior[BookingCommand, BookingStep, BookingSagaState] = behavior
): Apparatus[F, HotelCommand, List[SagaEvent[BookingStep, BookingSagaState]]] =
  serviceCommandLoop(
    hotelStep,
    hotelMachine[F](),
    hotelSearchConnector(bookingServices.hotel),
    bookingOrchestratorLoop(sagaBehavior.decider, bookingServices)
  )

/** Flight sub-aggregate entry into the booking orchestrator (e.g. async clearance check). */
def flightEntrypoint[F[_]: Applicative](
  bookingServices: BookingServices[F],
  sagaBehavior:    SagaBehavior[BookingCommand, BookingStep, BookingSagaState] = behavior
): Apparatus[F, FlightCommand, List[SagaEvent[BookingStep, BookingSagaState]]] = {
  serviceCommandLoop(
    flightStep,
    flightMachine[F](),
    flightSearchConnector(bookingServices.flight),
    bookingOrchestratorLoop(sagaBehavior.decider, bookingServices)
  )
}