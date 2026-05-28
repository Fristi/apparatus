package apparatus.examples

import apparatus.core.*
import apparatus.core.machines.*
import apparatus.core.patterns.*
import cats.*
import cats.data.NonEmptySet
import cats.implicits.*
import cats.derived.*
import zio.blocks.schema.Schema
import java.util.UUID

// ── Sub-aggregate IDs (fixed for this example) ───────────────────────────────

val flightId  = UUID.fromString("00000000-0000-0000-0000-000000000001")
val carId     = UUID.fromString("00000000-0000-0000-0000-000000000002")
val hotelId   = UUID.fromString("00000000-0000-0000-0000-000000000003")
val bookingId = UUID.fromString("00000000-0000-0000-0000-000000000004")

// ── Flight ────────────────────────────────────────────────────────────────────

enum FlightState { case Idle, Reserved, Compensated, Failed }

sealed trait FlightCommand:
  val id: UUID
object FlightCommand:
  case class Reserve(id: UUID)    extends FlightCommand
  case class Compensate(id: UUID) extends FlightCommand

enum FlightEvent derives Schema:
  case Reserved(id: UUID)
  case Compensated(id: UUID)
  case CompensationFailed(id: UUID)
  case Failed(id: UUID)

def flightDecider(
  failsOnReserve:    Boolean    = false,
  failsOnCompensate: Boolean    = false
): Decider[FlightState, FlightCommand, List[FlightEvent]] =
  DeciderBuilder.seed(FlightState.Idle)
    .partiallyDecide[FlightCommand, FlightEvent]:
      case (FlightState.Idle,     cmd: FlightCommand.Reserve)    => if failsOnReserve    then List(FlightEvent.Failed(cmd.id))             else List(FlightEvent.Reserved(cmd.id))
      case (FlightState.Reserved, cmd: FlightCommand.Compensate) => if failsOnCompensate then List(FlightEvent.CompensationFailed(cmd.id)) else List(FlightEvent.Compensated(cmd.id))
    .evolveList:
      case (FlightState.Idle,     FlightEvent.Reserved(_))           => FlightState.Reserved
      case (FlightState.Idle,     FlightEvent.Failed(_))             => FlightState.Failed
      case (FlightState.Reserved, FlightEvent.Compensated(_))        => FlightState.Compensated
      case (FlightState.Reserved, FlightEvent.CompensationFailed(_)) => FlightState.Failed
      case (s, _)                                                    => s

// ── Car ───────────────────────────────────────────────────────────────────────

enum CarState { case Idle, Reserved, Compensated, Failed }

sealed trait CarCommand:
  val id: UUID
object CarCommand:
  case class Reserve(id: UUID)    extends CarCommand
  case class Compensate(id: UUID) extends CarCommand

enum CarEvent derives Schema:
  case Reserved(id: UUID)
  case Compensated(id: UUID)
  case CompensationFailed(id: UUID)
  case Failed(id: UUID)

def carDecider(
  failsOnReserve:    Boolean  = false,
  failsOnCompensate: Boolean  = false
): Decider[CarState, CarCommand, List[CarEvent]] =
  DeciderBuilder.seed(CarState.Idle)
    .partiallyDecide[CarCommand, CarEvent]:
      case (CarState.Idle,     cmd: CarCommand.Reserve)    => if failsOnReserve    then List(CarEvent.Failed(cmd.id))             else List(CarEvent.Reserved(cmd.id))
      case (CarState.Reserved, cmd: CarCommand.Compensate) => if failsOnCompensate then List(CarEvent.CompensationFailed(cmd.id)) else List(CarEvent.Compensated(cmd.id))
    .evolveList:
      case (CarState.Idle,     CarEvent.Reserved(_))           => CarState.Reserved
      case (CarState.Idle,     CarEvent.Failed(_))             => CarState.Failed
      case (CarState.Reserved, CarEvent.Compensated(_))        => CarState.Compensated
      case (CarState.Reserved, CarEvent.CompensationFailed(_)) => CarState.Failed
      case (s, _)                                              => s

// ── Hotel ─────────────────────────────────────────────────────────────────────

enum HotelState { case Idle, Reserved, Compensated, Failed }

sealed trait HotelCommand:
  val id: UUID
object HotelCommand:
  case class Reserve(id: UUID)    extends HotelCommand
  case class Compensate(id: UUID) extends HotelCommand

enum HotelEvent derives Schema:
  case Reserved(id: UUID)
  case Compensated(id: UUID)
  case CompensationFailed(id: UUID)
  case Failed(id: UUID)

def hotelDecider(
  failsOnReserve:    Boolean    = false,
  failsOnCompensate: Boolean    = false
): Decider[HotelState, HotelCommand, List[HotelEvent]] =
  DeciderBuilder.seed(HotelState.Idle)
    .partiallyDecide[HotelCommand, HotelEvent]:
      case (HotelState.Idle,     cmd: HotelCommand.Reserve)    => if failsOnReserve    then List(HotelEvent.Failed(cmd.id))             else List(HotelEvent.Reserved(cmd.id))
      case (HotelState.Reserved, cmd: HotelCommand.Compensate) => if failsOnCompensate then List(HotelEvent.CompensationFailed(cmd.id)) else List(HotelEvent.Compensated(cmd.id))
    .evolveList:
      case (HotelState.Idle,     HotelEvent.Reserved(_))           => HotelState.Reserved
      case (HotelState.Idle,     HotelEvent.Failed(_))             => HotelState.Failed
      case (HotelState.Reserved, HotelEvent.Compensated(_))        => HotelState.Compensated
      case (HotelState.Reserved, HotelEvent.CompensationFailed(_)) => HotelState.Failed
      case (s, _)                                                   => s

// ── Booking saga domain ───────────────────────────────────────────────────────

sealed trait BookingCommand:
  val id: UUID

object BookingCommand:
  case class Start(id: UUID) extends BookingCommand
  case class Advance(id: UUID, step: BookingStep, phase: SagaPhase, result: SagaStepResult) extends BookingCommand

  val advancePrism: SagaAdvancePrism[BookingCommand, BookingStep] = new Prism[BookingCommand, (BookingStep, SagaPhase, SagaStepResult)] {
    override def getOption(input: BookingCommand): Option[(BookingStep, SagaPhase, SagaStepResult)] = input match {
      case _: BookingCommand.Start => None
      case BookingCommand.Advance(_, step, phase, result) => Some((step, phase, result))
    }

    override def reverseGet(input: (BookingStep, SagaPhase, SagaStepResult)): BookingCommand =
      BookingCommand.Advance(bookingId, input._1, input._2, input._3)
  }

enum BookingStep(val position: Int) derives Eq, Order, Show, Schema:
  case Hotel extends BookingStep(1)
  case Car extends BookingStep(2)
  case Flight extends BookingStep(3)

val flightStep = new SagaStepAdapter[FlightCommand, FlightEvent, BookingStep] {
  override def step: BookingStep = BookingStep.Flight
  override def start: FlightCommand = FlightCommand.Reserve(flightId)
  override def compensate: FlightCommand = FlightCommand.Compensate(flightId)
  override def classify(event: FlightEvent): Option[(SagaPhase, SagaStepResult)] = event match {
    case FlightEvent.Reserved(_)           => Some(SagaPhase.Forward -> SagaStepResult.Completed)
    case FlightEvent.Failed(_)             => Some(SagaPhase.Forward -> SagaStepResult.Failed)
    case FlightEvent.Compensated(_)        => Some(SagaPhase.Compensation -> SagaStepResult.Completed)
    case FlightEvent.CompensationFailed(_) => Some(SagaPhase.Compensation -> SagaStepResult.Failed)
  }
}

val carStep = new SagaStepAdapter[CarCommand, CarEvent, BookingStep] {
  override def step: BookingStep = BookingStep.Car
  override def start: CarCommand = CarCommand.Reserve(carId)
  override def compensate: CarCommand = CarCommand.Compensate(carId)
  override def classify(event: CarEvent): Option[(SagaPhase, SagaStepResult)] = event match {
    case CarEvent.Reserved(_)           => Some(SagaPhase.Forward -> SagaStepResult.Completed)
    case CarEvent.Failed(_)             => Some(SagaPhase.Forward -> SagaStepResult.Failed)
    case CarEvent.Compensated(_)        => Some(SagaPhase.Compensation -> SagaStepResult.Completed)
    case CarEvent.CompensationFailed(_) => Some(SagaPhase.Compensation -> SagaStepResult.Failed)
  }
}

val hotelStep = new SagaStepAdapter[HotelCommand, HotelEvent, BookingStep] {
  override def step: BookingStep = BookingStep.Hotel
  override def start: HotelCommand = HotelCommand.Reserve(hotelId)
  override def compensate: HotelCommand = HotelCommand.Compensate(hotelId)
  override def classify(event: HotelEvent): Option[(SagaPhase, SagaStepResult)] = event match {
    case HotelEvent.Reserved(_)           => Some(SagaPhase.Forward -> SagaStepResult.Completed)
    case HotelEvent.Failed(_)             => Some(SagaPhase.Forward -> SagaStepResult.Failed)
    case HotelEvent.Compensated(_)        => Some(SagaPhase.Compensation -> SagaStepResult.Completed)
    case HotelEvent.CompensationFailed(_) => Some(SagaPhase.Compensation -> SagaStepResult.Failed)
  }
}


val behavior: SagaBehavior[BookingCommand, BookingStep] = SagaBehaviorFactory(
  startCommand = BookingCommand.Start(bookingId),
  prism = BookingCommand.advancePrism,
  steps = NonEmptySet.of(BookingStep.Hotel, BookingStep.Car, BookingStep.Flight)
)



type FlightDecider = Decider[FlightState, FlightCommand, List[FlightEvent]]
type CarDecider = Decider[CarState, CarCommand, List[CarEvent]]
type HotelDecider = Decider[HotelState,  HotelCommand,  List[HotelEvent]]
type BookingDecider = Decider[SagaState[BookingStep], BookingCommand, List[SagaEvent[BookingStep]]]

private def flightServiceFSM[F[_] : Applicative](flight: FlightDecider): Apparatus[F, SagaEvent[BookingStep], List[BookingCommand]] =
  flightStep.rmap(flightStep.lmapOrEmpty(Apparatus.aggregateMachine[F, FlightCommand, FlightEvent]("flight", flight, _.id)), BookingCommand.advancePrism).label("Flight service")

private def carServiceFSM[F[_] : Applicative](car: CarDecider): Apparatus[F, SagaEvent[BookingStep], List[BookingCommand]] =
  carStep.rmap(carStep.lmapOrEmpty(Apparatus.aggregateMachine[F, CarCommand, CarEvent]("car", car, _.id)), BookingCommand.advancePrism).label("Car service")

private def hotelServiceFSM[F[_] : Applicative](hotel: HotelDecider): Apparatus[F, SagaEvent[BookingStep], List[BookingCommand]] =
  hotelStep.rmap(hotelStep.lmapOrEmpty(Apparatus.aggregateMachine[F, HotelCommand, HotelEvent]("hotel", hotel, _.id)), BookingCommand.advancePrism).label("Hotel service")

private def makeServices[F[_] : Applicative](flight: FlightDecider, car: CarDecider, hotel: HotelDecider): Apparatus[F, SagaEvent[BookingStep], List[BookingCommand]] =
  flightServiceFSM[F](flight).merge(carServiceFSM[F](car).merge(hotelServiceFSM[F](hotel)))

def saga[F[_] : Applicative](
  flight: FlightDecider  = flightDecider(),
  car:    CarDecider    = carDecider(),
  hotel:  HotelDecider  = hotelDecider()
): Apparatus[F, BookingCommand, List[SagaEvent[BookingStep]]] =
  Apparatus.aggregateMachine[F, BookingCommand, SagaEvent[BookingStep]]("booking", behavior.decider, _.id)
    .feedback(makeServices[F](flight, car, hotel))

def sagaRerootedAtCar[F[_] : Applicative](
  booking:     BookingDecider,
  flight: FlightDecider  = flightDecider(),
  car:    CarDecider    = carDecider(),
  hotel:  HotelDecider  = hotelDecider()
): Apparatus[F, CarCommand, List[SagaEvent[BookingStep]]] =
  carStep
    .rmap(Apparatus.aggregateMachine[F, CarCommand, CarEvent]("car", car, _.id), BookingCommand.advancePrism)
    .andThen(Apparatus.aggregateMachine[F, BookingCommand, SagaEvent[BookingStep]]("booking", booking, _.id).feedbackMany(makeServices[F](flight, car, hotel)))
