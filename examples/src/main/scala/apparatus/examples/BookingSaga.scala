package apparatus.examples

import apparatus.core.*
import cats.*
import cats.data.NonEmptySet
import cats.implicits.*
import cats.derived.*

import scala.collection.immutable.SortedSet

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
  // -- Flight
  case MarkFlightComplete
  case MarkFlightFailed
  case MarkFlightCompensationComplete
  case MarkFlightCompensationFailed
  // -- Hotel
  case MarkHotelComplete
  case MarkHotelFailed
  case MarkHotelCompensationComplete
  case MarkHotelCompensationFailed
  // -- Car
  case MarkCarComplete
  case MarkCarFailed
  case MarkCarCompensationComplete
  case MarkCarCompensationFailed


enum BookingStep(val position: Int) derives Eq, Order, Show:
  case Hotel extends BookingStep(1)
  case Car extends BookingStep(2)
  case Flight extends BookingStep(3)

val behavior = new SagaBehavior[BookingCommand, BookingStep] {
  override def startCommand: BookingCommand = BookingCommand.Start
  override def steps: NonEmptySet[BookingStep] = NonEmptySet.of(BookingStep.Hotel, BookingStep.Car, BookingStep.Flight)
  override def compensationHandler: PartialFunction[BookingCommand, (BookingStep, SagaStepResult)] = {
    case BookingCommand.MarkFlightCompensationComplete => (BookingStep.Flight, SagaStepResult.Completed)
    case BookingCommand.MarkFlightCompensationFailed   => (BookingStep.Flight, SagaStepResult.Failed)
    case BookingCommand.MarkHotelCompensationComplete  => (BookingStep.Hotel,  SagaStepResult.Completed)
    case BookingCommand.MarkHotelCompensationFailed    => (BookingStep.Hotel,  SagaStepResult.Failed)
    case BookingCommand.MarkCarCompensationComplete    => (BookingStep.Car,    SagaStepResult.Completed)
    case BookingCommand.MarkCarCompensationFailed      => (BookingStep.Car,    SagaStepResult.Failed)
  }
  override def stepHandler: PartialFunction[BookingCommand, (BookingStep, SagaStepResult)] = {
    case BookingCommand.MarkFlightComplete => (BookingStep.Flight, SagaStepResult.Completed)
    case BookingCommand.MarkFlightFailed   => (BookingStep.Flight, SagaStepResult.Failed)
    case BookingCommand.MarkHotelComplete  => (BookingStep.Hotel,  SagaStepResult.Completed)
    case BookingCommand.MarkHotelFailed    => (BookingStep.Hotel,  SagaStepResult.Failed)
    case BookingCommand.MarkCarComplete    => (BookingStep.Car,    SagaStepResult.Completed)
    case BookingCommand.MarkCarFailed      => (BookingStep.Car,    SagaStepResult.Failed)
  }
}

val bookingDecider: Apparatus[Id, BookingCommand, List[SagaEvent[BookingStep]]] = Apparatus.Basic(behavior.decider.toBaseMachine)

// ── Services wiring ───────────────────────────────────────────────────────────

def flightServiceFSM(
  flight: Decider[FlightState, FlightCommand, List[FlightEvent]] = flightDecider()
): Apparatus[Id, SagaEvent[BookingStep], List[BookingCommand]] =
  (Apparatus.Basic(flight.toBaseMachine[Id]).label("Flight Decider")
    .lmapOrEmpty[SagaEvent[BookingStep]] {
      case SagaEvent.Booted(BookingStep.Flight, _)                | SagaEvent.StepStarted(BookingStep.Flight)         => FlightCommand.Reserve
      case SagaEvent.CompensationTriggered(BookingStep.Flight, _) | SagaEvent.CompensationStarted(BookingStep.Flight) => FlightCommand.Compensate
    }.label("Flight Event Router")
    .rmap(_.collect {
      case FlightEvent.Reserved           => BookingCommand.MarkFlightComplete
      case FlightEvent.Failed             => BookingCommand.MarkFlightFailed
      case FlightEvent.Compensated        => BookingCommand.MarkFlightCompensationComplete
      case FlightEvent.CompensationFailed => BookingCommand.MarkFlightCompensationFailed
    })
  ).label("Flight Service")

def carServiceFSM(
  car: Decider[CarState, CarCommand, List[CarEvent]] = carDecider()
): Apparatus[Id, SagaEvent[BookingStep], List[BookingCommand]] =
  (Apparatus.Basic(car.toBaseMachine[Id]).label("Car Decider")
    .lmapOrEmpty[SagaEvent[BookingStep]] {
      case SagaEvent.Booted(BookingStep.Car, _)                | SagaEvent.StepStarted(BookingStep.Car)         => CarCommand.Reserve
      case SagaEvent.CompensationTriggered(BookingStep.Car, _) | SagaEvent.CompensationStarted(BookingStep.Car) => CarCommand.Compensate
    }.label("Car Event Router")
    .rmap(_.collect {
      case CarEvent.Reserved           => BookingCommand.MarkCarComplete
      case CarEvent.Failed             => BookingCommand.MarkCarFailed
      case CarEvent.Compensated        => BookingCommand.MarkCarCompensationComplete
      case CarEvent.CompensationFailed => BookingCommand.MarkCarCompensationFailed
    })
  ).label("Car Service")

def hotelServiceFSM(
  hotel: Decider[HotelState, HotelCommand, List[HotelEvent]] = hotelDecider()
): Apparatus[Id, SagaEvent[BookingStep], List[BookingCommand]] =
  (Apparatus.Basic(hotel.toBaseMachine[Id]).label("Hotel Decider")
    .lmapOrEmpty[SagaEvent[BookingStep]] {
      case SagaEvent.Booted(BookingStep.Hotel, _)                | SagaEvent.StepStarted(BookingStep.Hotel)         => HotelCommand.Reserve
      case SagaEvent.CompensationTriggered(BookingStep.Hotel, _) | SagaEvent.CompensationStarted(BookingStep.Hotel) => HotelCommand.Compensate
    }.label("Hotel Event Router")
    .rmap(_.collect {
      case HotelEvent.Reserved           => BookingCommand.MarkHotelComplete
      case HotelEvent.Failed             => BookingCommand.MarkHotelFailed
      case HotelEvent.Compensated        => BookingCommand.MarkHotelCompensationComplete
      case HotelEvent.CompensationFailed => BookingCommand.MarkHotelCompensationFailed
    })
  ).label("Hotel Service")

def makeServices(
  flight: Decider[FlightState, FlightCommand, List[FlightEvent]] = flightDecider(),
  car:    Decider[CarState,    CarCommand,    List[CarEvent]]    = carDecider(),
  hotel:  Decider[HotelState,  HotelCommand,  List[HotelEvent]]  = hotelDecider()
): Apparatus[Id, SagaEvent[BookingStep], List[BookingCommand]] =
  flightServiceFSM(flight) merge carServiceFSM(car) merge hotelServiceFSM(hotel)

def saga(
  flight: Decider[FlightState, FlightCommand, List[FlightEvent]] = flightDecider(),
  car:    Decider[CarState,    CarCommand,    List[CarEvent]]    = carDecider(),
  hotel:  Decider[HotelState,  HotelCommand,  List[HotelEvent]]  = hotelDecider()
): Apparatus[Id, BookingCommand, List[SagaEvent[BookingStep]]] =
  bookingDecider.label("Booking Saga") <-> makeServices(flight, car, hotel)

// ── Rerooted at car ───────────────────────────────────────────────────────────
//
// Entry point is a CarCommand. The caller rehydrates the booking orchestrator
// (via `behavior.decider.evolveFrom(events)`) and passes it as `booking`.
//
// `carFeedback` is the car sub-decider used *inside* the feedback reactor for
// compensation — distinct from the entry-point `car` decider, because by the
// time this network runs the car booking step has already succeeded and the
// car decider must be pre-seeded at Reserved so it can process Compensate.

def sagaRerootedAtCar(
  booking:     Decider[SagaState[BookingStep], BookingCommand, List[SagaEvent[BookingStep]]],
  flight:      Decider[FlightState, FlightCommand, List[FlightEvent]] = flightDecider(),
  car:         Decider[CarState,    CarCommand,    List[CarEvent]]    = carDecider(),
  hotel:       Decider[HotelState,  HotelCommand,  List[HotelEvent]]  = hotelDecider()
): Apparatus[Id, CarCommand, List[SagaEvent[BookingStep]]] =

  // Translate the incoming CarCommand result into BookingCommands.
  val carCore: Apparatus[Id, CarCommand, List[BookingCommand]] =
    Apparatus.Basic(car.toBaseMachine[Id])
      .rmap(_.collect {
        case CarEvent.Reserved           => BookingCommand.MarkCarComplete
        case CarEvent.Failed             => BookingCommand.MarkCarFailed
        case CarEvent.Compensated        => BookingCommand.MarkCarCompensationComplete
        case CarEvent.CompensationFailed => BookingCommand.MarkCarCompensationFailed
      })

  // Booking orchestrator pre-seeded at the current saga state.
  val bookingDeciderAtCar: Apparatus[Id, BookingCommand, List[SagaEvent[BookingStep]]] =
    Apparatus.Basic(booking.toBaseMachine[Id])

  // carServiceFSM(carFeedback) handles compensation back to the car step if a
  // later step (e.g. flight) fails after car has already succeeded.
  carCore >>> bookingDeciderAtCar.feedbackMany(
    flightServiceFSM(flight) merge carServiceFSM(car.copy(state = CarState.Reserved)) merge hotelServiceFSM(hotel)
  )
