package apparatus.core

import cats.*
import cats.data.NonEmptySet
import cats.implicits.*
import cats.derived.*

import scala.collection.immutable.SortedSet

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

val bookingDecider: FSM[Id, BookingCommand, List[SagaEvent[BookingStep]]] = FSM.Basic(behavior.decider.toBaseMachine)

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
): FSM[Id, SagaEvent[BookingStep], List[BookingCommand]] =

  val flightFSM: FSM[Id, SagaEvent[BookingStep], List[BookingCommand]] =
    FSM.Basic(flight.toBaseMachine[Id])
      .lmapOrEmpty[SagaEvent[BookingStep]] {
        case SagaEvent.Booted(BookingStep.Flight, _)                | SagaEvent.StepStarted(BookingStep.Flight)         => FlightCommand.Reserve
        case SagaEvent.CompensationTriggered(BookingStep.Flight, _) | SagaEvent.CompensationStarted(BookingStep.Flight) => FlightCommand.Compensate
      }
      .rmap(_.collect {
        case FlightEvent.Reserved    => BookingCommand.MarkFlightComplete
        case FlightEvent.Failed      => BookingCommand.MarkFlightFailed
        case FlightEvent.Compensated => BookingCommand.MarkFlightCompensationComplete
      })

  val carFSM: FSM[Id, SagaEvent[BookingStep], List[BookingCommand]] =
    FSM.Basic(car.toBaseMachine[Id])
      .lmapOrEmpty[SagaEvent[BookingStep]] {
        case SagaEvent.Booted(BookingStep.Car, _)                | SagaEvent.StepStarted(BookingStep.Car)            => CarCommand.Reserve
        case SagaEvent.CompensationTriggered(BookingStep.Car, _) | SagaEvent.CompensationStarted(BookingStep.Car)    => CarCommand.Compensate
      }
      .rmap(_.collect {
        case CarEvent.Reserved    => BookingCommand.MarkCarComplete
        case CarEvent.Failed      => BookingCommand.MarkCarFailed
        case CarEvent.Compensated => BookingCommand.MarkCarCompensationComplete
      })

  val hotelFSM: FSM[Id, SagaEvent[BookingStep], List[BookingCommand]] =
    FSM.Basic(hotel.toBaseMachine[Id])
      .lmapOrEmpty[SagaEvent[BookingStep]] {
        case SagaEvent.Booted(BookingStep.Hotel, _)                | SagaEvent.StepStarted(BookingStep.Hotel)          => HotelCommand.Reserve
        case SagaEvent.CompensationTriggered(BookingStep.Hotel, _) | SagaEvent.CompensationStarted(BookingStep.Hotel)  => HotelCommand.Compensate
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
): FSM[Id, BookingCommand, List[SagaEvent[BookingStep]]] =
  bookingDecider <-> makeServices(flight, car, hotel)

class BookingSagaSpec extends munit.FunSuite:

  test("happy path: all steps complete in order"):
    assertEquals(
      FSM.runA(saga(), BookingCommand.Start),
      List(
        SagaEvent.Booted(BookingStep.Hotel, SortedSet(BookingStep.Car, BookingStep.Flight)),
        SagaEvent.StepProgressed(BookingStep.Hotel, SagaStepResult.Completed),
        SagaEvent.StepStarted(BookingStep.Car),
        SagaEvent.StepProgressed(BookingStep.Car, SagaStepResult.Completed),
        SagaEvent.StepStarted(BookingStep.Flight),
        SagaEvent.StepProgressed(BookingStep.Flight, SagaStepResult.Completed)
      )
    )

  test("flight fails: compensation triggered for completed steps"):
    val events = FSM.runA(saga(flight = flightDecider(failsOnReserve = true)), BookingCommand.Start)
    assert(events.contains(SagaEvent.StepProgressed(BookingStep.Flight, SagaStepResult.Failed)))
    assert(events.exists { case SagaEvent.CompensationTriggered(_, _) => true; case _ => false })
    assert(events.exists { case SagaEvent.CompensationProgressed(BookingStep.Car,    SagaStepResult.Completed) => true; case _ => false })
    assert(events.exists { case SagaEvent.CompensationProgressed(BookingStep.Hotel, SagaStepResult.Completed) => true; case _ => false })

  test("flight fails: car compensated, flight not"):
    val events = FSM.runA(saga(flight = flightDecider(failsOnReserve = true)), BookingCommand.Start)
    assert(events.contains(SagaEvent.StepProgressed(BookingStep.Flight, SagaStepResult.Failed)))
    assert(events.exists { case SagaEvent.CompensationProgressed(BookingStep.Car, SagaStepResult.Completed) => true; case _ => false })
    assert(!events.exists { case SagaEvent.CompensationProgressed(BookingStep.Flight, _) => true; case _ => false })

  test("car fails: no compensation needed"):
    val events = FSM.runA(saga(car = carDecider(failsOnReserve = true)), BookingCommand.Start)
    assert(events.contains(SagaEvent.StepProgressed(BookingStep.Car, SagaStepResult.Failed)))
    assert(!events.exists { case SagaEvent.CompensationStarted(_) => true; case _ => false })
