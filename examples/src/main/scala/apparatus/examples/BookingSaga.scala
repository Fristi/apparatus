package apparatus.examples

import apparatus.core.*
import cats.*
import cats.data.NonEmptySet
import cats.implicits.*
import cats.derived.*

// ── Flight ────────────────────────────────────────────────────────────────────

enum FlightState { case Idle, Reserved, Compensated, Failed }
enum FlightCommand { case Reserve, Compensate }
enum FlightEvent { case Reserved, Compensated, CompensationFailed, Failed }

def flightDecider(
  failsOnReserve:    Boolean    = false,
  failsOnCompensate: Boolean    = false,
  initialState:      FlightState = FlightState.Idle
): Decider[FlightState, FlightCommand, List[FlightEvent]] =
  DeciderBuilder.seed(initialState)
    .partiallyDecide[FlightCommand, FlightEvent]:
      case (FlightState.Idle,     FlightCommand.Reserve)    => if failsOnReserve    then List(FlightEvent.Failed)             else List(FlightEvent.Reserved)
      case (FlightState.Reserved, FlightCommand.Compensate) => if failsOnCompensate then List(FlightEvent.CompensationFailed) else List(FlightEvent.Compensated)
    .evolveList:
      case (FlightState.Idle,     FlightEvent.Reserved)           => FlightState.Reserved
      case (FlightState.Idle,     FlightEvent.Failed)             => FlightState.Failed
      case (FlightState.Reserved, FlightEvent.Compensated)        => FlightState.Compensated
      case (FlightState.Reserved, FlightEvent.CompensationFailed) => FlightState.Failed
      case (s, _)                                                 => s

// ── Car ───────────────────────────────────────────────────────────────────────

enum CarState { case Idle, Reserved, Compensated, Failed }
enum CarCommand { case Reserve, Compensate }
enum CarEvent { case Reserved, Compensated, CompensationFailed, Failed }

def carDecider(
  failsOnReserve:    Boolean  = false,
  failsOnCompensate: Boolean  = false,
  initialState:      CarState = CarState.Idle
): Decider[CarState, CarCommand, List[CarEvent]] =
  DeciderBuilder.seed(initialState)
    .partiallyDecide[CarCommand, CarEvent]:
      case (CarState.Idle,     CarCommand.Reserve)    => if failsOnReserve    then List(CarEvent.Failed)             else List(CarEvent.Reserved)
      case (CarState.Reserved, CarCommand.Compensate) => if failsOnCompensate then List(CarEvent.CompensationFailed) else List(CarEvent.Compensated)
    .evolveList:
      case (CarState.Idle,     CarEvent.Reserved)           => CarState.Reserved
      case (CarState.Idle,     CarEvent.Failed)             => CarState.Failed
      case (CarState.Reserved, CarEvent.Compensated)        => CarState.Compensated
      case (CarState.Reserved, CarEvent.CompensationFailed) => CarState.Failed
      case (s, _)                                           => s

// ── Hotel ─────────────────────────────────────────────────────────────────────

enum HotelState { case Idle, Reserved, Compensated, Failed }
enum HotelCommand { case Reserve, Compensate }
enum HotelEvent { case Reserved, Compensated, CompensationFailed, Failed }

def hotelDecider(
  failsOnReserve:    Boolean    = false,
  failsOnCompensate: Boolean    = false,
  initialState:      HotelState = HotelState.Idle
): Decider[HotelState, HotelCommand, List[HotelEvent]] =
  DeciderBuilder.seed(initialState)
    .partiallyDecide[HotelCommand, HotelEvent]:
      case (HotelState.Idle,     HotelCommand.Reserve)    => if failsOnReserve    then List(HotelEvent.Failed)             else List(HotelEvent.Reserved)
      case (HotelState.Reserved, HotelCommand.Compensate) => if failsOnCompensate then List(HotelEvent.CompensationFailed) else List(HotelEvent.Compensated)
    .evolveList:
      case (HotelState.Idle,     HotelEvent.Reserved)           => HotelState.Reserved
      case (HotelState.Idle,     HotelEvent.Failed)             => HotelState.Failed
      case (HotelState.Reserved, HotelEvent.Compensated)        => HotelState.Compensated
      case (HotelState.Reserved, HotelEvent.CompensationFailed) => HotelState.Failed
      case (s, _)                                               => s

// ── Booking saga domain ───────────────────────────────────────────────────────

enum BookingCommand:
  case Start
  case Advance(step: BookingStep, phase: SagaPhase, result: SagaStepResult)

object BookingCommand {
  val advancePrism: SagaAdvancePrism[BookingCommand, BookingStep] = new Prism[BookingCommand, (BookingStep, SagaPhase, SagaStepResult)] {
    override def getOption(input: BookingCommand): Option[(BookingStep, SagaPhase, SagaStepResult)] = input match {
      case BookingCommand.Start => None
      case BookingCommand.Advance(step, phase, result) => Some((step, phase, result))
    }

    override def reverseGet(input: (BookingStep, SagaPhase, SagaStepResult)): BookingCommand =
      BookingCommand.Advance(input._1, input._2, input._3)
  }
}

enum BookingStep(val position: Int) derives Eq, Order, Show:
  case Hotel extends BookingStep(1)
  case Car extends BookingStep(2)
  case Flight extends BookingStep(3)

val flightStep = new SagaStepAdapter[FlightCommand, FlightEvent, BookingStep] {
  override def step: BookingStep = BookingStep.Flight
  override def start: FlightCommand = FlightCommand.Reserve
  override def compensate: FlightCommand = FlightCommand.Compensate
  override def classify(event: FlightEvent): Option[(SagaPhase, SagaStepResult)] = event match {
    case FlightEvent.Reserved => Some(SagaPhase.Forward -> SagaStepResult.Completed)
    case FlightEvent.Failed => Some(SagaPhase.Forward -> SagaStepResult.Failed)
    case FlightEvent.Compensated => Some(SagaPhase.Compensation -> SagaStepResult.Completed)
    case FlightEvent.CompensationFailed => Some(SagaPhase.Compensation -> SagaStepResult.Failed)
  }
}

val carStep = new SagaStepAdapter[CarCommand, CarEvent, BookingStep] {
  override def step: BookingStep = BookingStep.Car
  override def start: CarCommand = CarCommand.Reserve
  override def compensate: CarCommand = CarCommand.Compensate
  override def classify(event: CarEvent): Option[(SagaPhase, SagaStepResult)] = event match {
    case CarEvent.Reserved => Some(SagaPhase.Forward -> SagaStepResult.Completed)
    case CarEvent.Failed => Some(SagaPhase.Forward -> SagaStepResult.Failed)
    case CarEvent.Compensated => Some(SagaPhase.Compensation -> SagaStepResult.Completed)
    case CarEvent.CompensationFailed => Some(SagaPhase.Compensation -> SagaStepResult.Failed)
  }
}

val hotelStep = new SagaStepAdapter[HotelCommand, HotelEvent, BookingStep] {
  override def step: BookingStep = BookingStep.Hotel
  override def start: HotelCommand = HotelCommand.Reserve
  override def compensate: HotelCommand = HotelCommand.Compensate
  override def classify(event: HotelEvent): Option[(SagaPhase, SagaStepResult)] = event match {
    case HotelEvent.Reserved => Some(SagaPhase.Forward -> SagaStepResult.Completed)
    case HotelEvent.Failed => Some(SagaPhase.Forward -> SagaStepResult.Failed)
    case HotelEvent.Compensated => Some(SagaPhase.Compensation -> SagaStepResult.Completed)
    case HotelEvent.CompensationFailed => Some(SagaPhase.Compensation -> SagaStepResult.Failed)
  }
}


val behavior: SagaBehavior[BookingCommand, BookingStep] = SagaBehaviorFactory(
  startCommand = BookingCommand.Start,
  prism = BookingCommand.advancePrism,
  steps = NonEmptySet.of(BookingStep.Hotel, BookingStep.Car, BookingStep.Flight)
)


def flightServiceFSM[F[_] : Applicative](
                       flight: Decider[FlightState, FlightCommand, List[FlightEvent]] = flightDecider()
                     ): Apparatus[F, SagaEvent[BookingStep], List[BookingCommand]] =
  flightStep.rmap(flightStep.lmapOrEmpty(flight.toApparatus[F]("flight")), BookingCommand.advancePrism).label("Flight service")


def carServiceFSM[F[_] : Applicative](
  car: Decider[CarState, CarCommand, List[CarEvent]] = carDecider()
): Apparatus[F, SagaEvent[BookingStep], List[BookingCommand]] =
  carStep.rmap(carStep.lmapOrEmpty(car.toApparatus[F]("car")), BookingCommand.advancePrism).label("Car service")

def hotelServiceFSM[F[_] : Applicative](
  hotel: Decider[HotelState, HotelCommand, List[HotelEvent]] = hotelDecider()
): Apparatus[F, SagaEvent[BookingStep], List[BookingCommand]] =
  hotelStep.rmap(hotelStep.lmapOrEmpty(hotel.toApparatus[F]("hotel")), BookingCommand.advancePrism).label("Hotel service")

def makeServices[F[_] : Applicative](
  flight: Decider[FlightState, FlightCommand, List[FlightEvent]] = flightDecider(),
  car:    Decider[CarState,    CarCommand,    List[CarEvent]]    = carDecider(),
  hotel:  Decider[HotelState,  HotelCommand,  List[HotelEvent]]  = hotelDecider()
): Apparatus[F, SagaEvent[BookingStep], List[BookingCommand]] =
  flightServiceFSM(flight).merge(carServiceFSM(car).merge(hotelServiceFSM(hotel)))

def saga[F[_] : Applicative](
  flight: Decider[FlightState, FlightCommand, List[FlightEvent]] = flightDecider(),
  car:    Decider[CarState,    CarCommand,    List[CarEvent]]    = carDecider(),
  hotel:  Decider[HotelState,  HotelCommand,  List[HotelEvent]]  = hotelDecider()
): Apparatus[F, BookingCommand, List[SagaEvent[BookingStep]]] =
  behavior.decider.toApparatus[F]("booking") <-> makeServices[F](flight, car, hotel)

// ── Rerooted at car ───────────────────────────────────────────────────────────
//
// Entry point is a CarCommand. The caller rehydrates the booking orchestrator
// (via `behavior.decider.evolveFrom(events)`) and passes it as `booking`.
//
// The car machine is wrapped in a Stable node so that its state is shared
// between the entry-point carCore and the car service inside the feedback
// reactor.  After carCore processes the incoming CarCommand the car transitions
// to Reserved; that updated state is propagated by `>>>` into the feedback
// services, so compensation can be applied without manually pre-seeding.

def sagaRerootedAtCar[F[_] : Applicative](
  booking:     Decider[SagaState[BookingStep], BookingCommand, List[SagaEvent[BookingStep]]],
  flight:      Decider[FlightState, FlightCommand, List[FlightEvent]] = flightDecider(),
  car:         Decider[CarState,    CarCommand,    List[CarEvent]]    = carDecider(),
  hotel:       Decider[HotelState,  HotelCommand,  List[HotelEvent]]  = hotelDecider()
): Apparatus[F, CarCommand, List[SagaEvent[BookingStep]]] =
  carStep
    .rmap(car.toApparatus[F]("car"), BookingCommand.advancePrism)
    .andThen(booking.toApparatus[F]("booking").feedbackMany(makeServices[F](flight, car, hotel)))