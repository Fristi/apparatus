package apparatus.core

import cats.*
import cats.data.NonEmptySet
import cats.implicits.*

// ── Flight ────────────────────────────────────────────────────────────────────

enum FlightState { case Idle, Reserved, Compensated, Failed }
enum FlightCommand { case Reserve, Compensate }
enum FlightEvent { case Reserved, Compensated, Failed }

def flightDecider(failsOnReserve: Boolean = false): Decider[FlightState, FlightCommand, List[FlightEvent]] =
  DeciderBuilder.seed(FlightState.Idle)
    .partiallyDecide[FlightCommand, FlightEvent]:
      case (FlightState.Idle, FlightCommand.Reserve)        => if failsOnReserve then List(FlightEvent.Failed) else List(FlightEvent.Reserved)
      case (FlightState.Reserved, FlightCommand.Compensate) => List(FlightEvent.Compensated)
    .evolveList:
      case (FlightState.Idle, FlightEvent.Reserved)        => FlightState.Reserved
      case (FlightState.Idle, FlightEvent.Failed)          => FlightState.Failed
      case (FlightState.Reserved, FlightEvent.Compensated) => FlightState.Compensated
      case (s, _)                                          => s

// ── Car ───────────────────────────────────────────────────────────────────────

enum CarState { case Idle, Reserved, Compensated, Failed }
enum CarCommand { case Reserve, Compensate }
enum CarEvent { case Reserved, Compensated, Failed }

def carDecider(failsOnReserve: Boolean = false): Decider[CarState, CarCommand, List[CarEvent]] =
  DeciderBuilder.seed(CarState.Idle)
    .partiallyDecide[CarCommand, CarEvent]:
      case (CarState.Idle, CarCommand.Reserve)        => if failsOnReserve then List(CarEvent.Failed) else List(CarEvent.Reserved)
      case (CarState.Reserved, CarCommand.Compensate) => List(CarEvent.Compensated)
    .evolveList:
      case (CarState.Idle, CarEvent.Reserved)        => CarState.Reserved
      case (CarState.Idle, CarEvent.Failed)          => CarState.Failed
      case (CarState.Reserved, CarEvent.Compensated) => CarState.Compensated
      case (s, _)                                    => s

// ── Hotel ─────────────────────────────────────────────────────────────────────

enum HotelState { case Idle, Reserved, Compensated, Failed }
enum HotelCommand { case Reserve, Compensate }
enum HotelEvent { case Reserved, Compensated, Failed }

def hotelDecider(failsOnReserve: Boolean = false): Decider[HotelState, HotelCommand, List[HotelEvent]] =
  DeciderBuilder.seed(HotelState.Idle)
    .partiallyDecide[HotelCommand, HotelEvent]:
      case (HotelState.Idle, HotelCommand.Reserve)        => if failsOnReserve then List(HotelEvent.Failed) else List(HotelEvent.Reserved)
      case (HotelState.Reserved, HotelCommand.Compensate) => List(HotelEvent.Compensated)
    .evolveList:
      case (HotelState.Idle, HotelEvent.Reserved)        => HotelState.Reserved
      case (HotelState.Idle, HotelEvent.Failed)          => HotelState.Failed
      case (HotelState.Reserved, HotelEvent.Compensated) => HotelState.Compensated
      case (s, _)                                        => s

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


val behavior = new SagaBehavior[BookingCommand] {
  override def startCommand: BookingCommand = BookingCommand.Start
  override def steps: NonEmptySet[String] = NonEmptySet.of("Flight", "Hotel", "Car")
  override def compensationHandler: PartialFunction[BookingCommand, (String, SagaStepResult)] = {
    case BookingCommand.MarkFlightCompensationComplete => ("Flight", SagaStepResult.Completed)
    case BookingCommand.MarkFlightCompensationFailed   => ("Flight", SagaStepResult.Failed)
    case BookingCommand.MarkHotelCompensationComplete  => ("Hotel",  SagaStepResult.Completed)
    case BookingCommand.MarkHotelCompensationFailed    => ("Hotel",  SagaStepResult.Failed)
    case BookingCommand.MarkCarCompensationComplete    => ("Car",    SagaStepResult.Completed)
    case BookingCommand.MarkCarCompensationFailed      => ("Car",    SagaStepResult.Failed)
  }
  override def stepHandler: PartialFunction[BookingCommand, (String, SagaStepResult)] = {
    case BookingCommand.MarkFlightComplete => ("Flight", SagaStepResult.Completed)
    case BookingCommand.MarkFlightFailed   => ("Flight", SagaStepResult.Failed)
    case BookingCommand.MarkHotelComplete  => ("Hotel",  SagaStepResult.Completed)
    case BookingCommand.MarkHotelFailed    => ("Hotel",  SagaStepResult.Failed)
    case BookingCommand.MarkCarComplete    => ("Car",    SagaStepResult.Completed)
    case BookingCommand.MarkCarFailed      => ("Car",    SagaStepResult.Failed)
  }
}

val bookingDecider: FSM[Id, BookingCommand, List[SagaEvent]] = FSM.Basic(behavior.decider.toBaseMachine)

// ── Services wiring ───────────────────────────────────────────────────────────
//
// Bidirectional feedback loop:
//   bookingDecider emits List[SagaEvent]
//     → each SagaEvent routed to the matching sub-decider via lmapOrEmpty
//       (unmatched events yield List.empty without advancing state)
//     → sub-decider emits domain events (e.g. FlightEvent.Reserved)
//     → rmap translates domain events back to BookingCommand
//     → BookingCommand fed back into bookingDecider
//   Three services combined with merge: both run per event, outputs concatenated.
//   Only the matching one produces non-empty output.

def makeServices(
  flight: Decider[FlightState, FlightCommand, List[FlightEvent]] = flightDecider(),
  car:    Decider[CarState,    CarCommand,    List[CarEvent]]    = carDecider(),
  hotel:  Decider[HotelState,  HotelCommand,  List[HotelEvent]]  = hotelDecider()
): FSM[Id, SagaEvent, List[BookingCommand]] =

  val flightFSM: FSM[Id, SagaEvent, List[BookingCommand]] =
    FSM.Basic(flight.toBaseMachine[Id])
      .lmapOrEmpty[SagaEvent] {
        case SagaEvent.Booted("Flight", _)                | SagaEvent.StepStarted("Flight")         => FlightCommand.Reserve
        case SagaEvent.CompensationTriggered("Flight", _) | SagaEvent.CompensationStarted("Flight") => FlightCommand.Compensate
      }
      .rmap(_.collect {
        case FlightEvent.Reserved    => BookingCommand.MarkFlightComplete
        case FlightEvent.Failed      => BookingCommand.MarkFlightFailed
        case FlightEvent.Compensated => BookingCommand.MarkFlightCompensationComplete
      })

  val carFSM: FSM[Id, SagaEvent, List[BookingCommand]] =
    FSM.Basic(car.toBaseMachine[Id])
      .lmapOrEmpty[SagaEvent] {
        case SagaEvent.Booted("Car", _)                | SagaEvent.StepStarted("Car")            => CarCommand.Reserve
        case SagaEvent.CompensationTriggered("Car", _) | SagaEvent.CompensationStarted("Car")    => CarCommand.Compensate
      }
      .rmap(_.collect {
        case CarEvent.Reserved    => BookingCommand.MarkCarComplete
        case CarEvent.Failed      => BookingCommand.MarkCarFailed
        case CarEvent.Compensated => BookingCommand.MarkCarCompensationComplete
      })

  val hotelFSM: FSM[Id, SagaEvent, List[BookingCommand]] =
    FSM.Basic(hotel.toBaseMachine[Id])
      .lmapOrEmpty[SagaEvent] {
        case SagaEvent.Booted("Hotel", _)                | SagaEvent.StepStarted("Hotel")          => HotelCommand.Reserve
        case SagaEvent.CompensationTriggered("Hotel", _) | SagaEvent.CompensationStarted("Hotel")  => HotelCommand.Compensate
      }
      .rmap(_.collect {
        case HotelEvent.Reserved    => BookingCommand.MarkHotelComplete
        case HotelEvent.Failed      => BookingCommand.MarkHotelFailed
        case HotelEvent.Compensated => BookingCommand.MarkHotelCompensationComplete
      })

  flightFSM merge carFSM merge hotelFSM

def saga(
  flight: Decider[FlightState, FlightCommand, List[FlightEvent]] = flightDecider(),
  car:    Decider[CarState,    CarCommand,    List[CarEvent]]    = carDecider(),
  hotel:  Decider[HotelState,  HotelCommand,  List[HotelEvent]]  = hotelDecider()
): FSM[Id, BookingCommand, List[SagaEvent]] =
  bookingDecider <-> makeServices(flight, car, hotel)

// ── Tests ─────────────────────────────────────────────────────────────────────
//
// Steps sorted alphabetically: Car → Flight → Hotel
// (NonEmptySet.of("Flight","Hotel","Car").head == "Car")

class BookingSagaSpec extends munit.FunSuite:

  test("happy path: all steps complete in order"):
    assertEquals(
      FSM.runA(saga(), BookingCommand.Start),
      List(
        SagaEvent.Booted("Car", Set("Flight", "Hotel")),
        SagaEvent.StepProgressed("Car", SagaStepResult.Completed),
        SagaEvent.StepStarted("Flight"),
        SagaEvent.StepProgressed("Flight", SagaStepResult.Completed),
        SagaEvent.StepStarted("Hotel"),
        SagaEvent.StepProgressed("Hotel", SagaStepResult.Completed)
      )
    )

  test("hotel fails: compensation triggered for completed steps"):
    val events = FSM.runA(saga(hotel = hotelDecider(failsOnReserve = true)), BookingCommand.Start)
    assert(events.contains(SagaEvent.StepProgressed("Hotel", SagaStepResult.Failed)))
    assert(events.exists { case SagaEvent.CompensationTriggered(_, _) => true; case _ => false })
    assert(events.exists { case SagaEvent.CompensationProgressed("Car",    SagaStepResult.Completed) => true; case _ => false })
    assert(events.exists { case SagaEvent.CompensationProgressed("Flight", SagaStepResult.Completed) => true; case _ => false })

  test("flight fails: car compensated, flight not"):
    val events = FSM.runA(saga(flight = flightDecider(failsOnReserve = true)), BookingCommand.Start)
    assert(events.contains(SagaEvent.StepProgressed("Flight", SagaStepResult.Failed)))
    assert(events.exists { case SagaEvent.CompensationProgressed("Car", SagaStepResult.Completed) => true; case _ => false })
    assert(!events.exists { case SagaEvent.CompensationProgressed("Flight", _) => true; case _ => false })

  test("car fails: no compensation needed"):
    val events = FSM.runA(saga(car = carDecider(failsOnReserve = true)), BookingCommand.Start)
    assert(events.contains(SagaEvent.StepProgressed("Car", SagaStepResult.Failed)))
    assert(!events.exists { case SagaEvent.CompensationStarted(_) => true; case _ => false })
