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

def hotelDecider(failsOnReserve: Boolean = false, initialState: HotelState = HotelState.Idle): Decider[HotelState, HotelCommand, List[HotelEvent]] =
  DeciderBuilder.seed(initialState)
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

val bookingDecider: Apparatus[Id, BookingCommand, List[SagaEvent[BookingStep]]] = Apparatus.Basic(behavior.decider.toBaseMachine)

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

def flightServiceFSM(
  flight: Decider[FlightState, FlightCommand, List[FlightEvent]] = flightDecider()
): Apparatus[Id, SagaEvent[BookingStep], List[BookingCommand]] =
  Apparatus.Basic(flight.toBaseMachine[Id])
    .lmapOrEmpty[SagaEvent[BookingStep]] {
      case SagaEvent.Booted(BookingStep.Flight, _)                | SagaEvent.StepStarted(BookingStep.Flight)         => FlightCommand.Reserve
      case SagaEvent.CompensationTriggered(BookingStep.Flight, _) | SagaEvent.CompensationStarted(BookingStep.Flight) => FlightCommand.Compensate
    }
    .rmap(_.collect {
      case FlightEvent.Reserved    => BookingCommand.MarkFlightComplete
      case FlightEvent.Failed      => BookingCommand.MarkFlightFailed
      case FlightEvent.Compensated => BookingCommand.MarkFlightCompensationComplete
    })

def carServiceFSM(
  car: Decider[CarState, CarCommand, List[CarEvent]] = carDecider()
): Apparatus[Id, SagaEvent[BookingStep], List[BookingCommand]] =
  Apparatus.Basic(car.toBaseMachine[Id])
    .lmapOrEmpty[SagaEvent[BookingStep]] {
      case SagaEvent.Booted(BookingStep.Car, _)                | SagaEvent.StepStarted(BookingStep.Car)            => CarCommand.Reserve
      case SagaEvent.CompensationTriggered(BookingStep.Car, _) | SagaEvent.CompensationStarted(BookingStep.Car)    => CarCommand.Compensate
    }
    .rmap(_.collect {
      case CarEvent.Reserved    => BookingCommand.MarkCarComplete
      case CarEvent.Failed      => BookingCommand.MarkCarFailed
      case CarEvent.Compensated => BookingCommand.MarkCarCompensationComplete
    })

def hotelServiceFSM(
  hotel: Decider[HotelState, HotelCommand, List[HotelEvent]] = hotelDecider()
): Apparatus[Id, SagaEvent[BookingStep], List[BookingCommand]] =
  Apparatus.Basic(hotel.toBaseMachine[Id])
    .lmapOrEmpty[SagaEvent[BookingStep]] {
      case SagaEvent.Booted(BookingStep.Hotel, _)                | SagaEvent.StepStarted(BookingStep.Hotel)          => HotelCommand.Reserve
      case SagaEvent.CompensationTriggered(BookingStep.Hotel, _) | SagaEvent.CompensationStarted(BookingStep.Hotel)  => HotelCommand.Compensate
    }
    .rmap(_.collect {
      case HotelEvent.Reserved    => BookingCommand.MarkHotelComplete
      case HotelEvent.Failed      => BookingCommand.MarkHotelFailed
      case HotelEvent.Compensated => BookingCommand.MarkHotelCompensationComplete
    })

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
  bookingDecider <-> makeServices(flight, car, hotel)

// ── Rerooted at carDecider ────────────────────────────────────────────────────
//
// The carDecider becomes the entry point; the rest of the network reacts.
//
//   CarCommand
//     → carDecider                           (emits List[CarEvent])
//     → rmap                                 (List[CarEvent] → List[BookingCommand])
//     >>> FeedbackMany(bookingDecider, flightFSM merge hotelFSM)
//                                            (List[BookingCommand] → List[SagaEvent])
//
// The bookingDecider must be seeded with the state that corresponds to car being
// the active step: Running(Car, remaining={Flight}, compensation={Hotel}).
// Without that pre-seeding the first MarkCarComplete hits Waiting and is ignored.

def sagaRerootedAtCar(
  flight:        Decider[FlightState, FlightCommand, List[FlightEvent]] = flightDecider(),
  car:           Decider[CarState,    CarCommand,    List[CarEvent]]    = carDecider(),
  hotel:         Decider[HotelState,  HotelCommand,  List[HotelEvent]]  = hotelDecider(),
  bookingAtCar:  SagaState[BookingStep]                                  =
    SagaState.Running(BookingStep.Car, SortedSet(BookingStep.Flight), SortedSet(BookingStep.Hotel))
): Apparatus[Id, CarCommand, List[SagaEvent[BookingStep]]] =

  val carCore: Apparatus[Id, CarCommand, List[BookingCommand]] =
    Apparatus.Basic(car.toBaseMachine[Id])
      .rmap(_.collect {
        case CarEvent.Reserved    => BookingCommand.MarkCarComplete
        case CarEvent.Failed      => BookingCommand.MarkCarFailed
        case CarEvent.Compensated => BookingCommand.MarkCarCompensationComplete
      })

  val bookingDeciderAtCar: Apparatus[Id, BookingCommand, List[SagaEvent[BookingStep]]] =
    Apparatus.Basic(
      DeciderBuilder.seed(bookingAtCar)
        .decide[BookingCommand, List[SagaEvent[BookingStep]]]((s, i) => behavior.decide(s, i))
        .evolveList((s, e) => behavior.evolve(s, e))
        .toBaseMachine[Id]
    )

  carCore >>> bookingDeciderAtCar.feedbackMany(flightServiceFSM(flight) merge hotelServiceFSM(hotel))

def labeledSaga(
  flight: Decider[FlightState, FlightCommand, List[FlightEvent]] = flightDecider(),
  car:    Decider[CarState,    CarCommand,    List[CarEvent]]    = carDecider(),
  hotel:  Decider[HotelState,  HotelCommand,  List[HotelEvent]]  = hotelDecider()
): Apparatus[Id, BookingCommand, List[SagaEvent[BookingStep]]] =

  val labeledFlight: Apparatus[Id, SagaEvent[BookingStep], List[BookingCommand]] =
    (Apparatus.Basic(flight.toBaseMachine[Id]).label("Flight Decider")
      .lmapOrEmpty[SagaEvent[BookingStep]] {
        case SagaEvent.Booted(BookingStep.Flight, _)                | SagaEvent.StepStarted(BookingStep.Flight)         => FlightCommand.Reserve
        case SagaEvent.CompensationTriggered(BookingStep.Flight, _) | SagaEvent.CompensationStarted(BookingStep.Flight) => FlightCommand.Compensate
      }.label("Flight Event Router")
      .rmap(_.collect {
        case FlightEvent.Reserved    => BookingCommand.MarkFlightComplete
        case FlightEvent.Failed      => BookingCommand.MarkFlightFailed
        case FlightEvent.Compensated => BookingCommand.MarkFlightCompensationComplete
      })
    ).label("Flight Service")

  val labeledCar: Apparatus[Id, SagaEvent[BookingStep], List[BookingCommand]] =
    (Apparatus.Basic(car.toBaseMachine[Id]).label("Car Decider")
      .lmapOrEmpty[SagaEvent[BookingStep]] {
        case SagaEvent.Booted(BookingStep.Car, _)                | SagaEvent.StepStarted(BookingStep.Car)         => CarCommand.Reserve
        case SagaEvent.CompensationTriggered(BookingStep.Car, _) | SagaEvent.CompensationStarted(BookingStep.Car) => CarCommand.Compensate
      }.label("Car Event Router")
      .rmap(_.collect {
        case CarEvent.Reserved    => BookingCommand.MarkCarComplete
        case CarEvent.Failed      => BookingCommand.MarkCarFailed
        case CarEvent.Compensated => BookingCommand.MarkCarCompensationComplete
      })
    ).label("Car Service")

  val labeledHotel: Apparatus[Id, SagaEvent[BookingStep], List[BookingCommand]] =
    (Apparatus.Basic(hotel.toBaseMachine[Id]).label("Hotel Decider")
      .lmapOrEmpty[SagaEvent[BookingStep]] {
        case SagaEvent.Booted(BookingStep.Hotel, _)                | SagaEvent.StepStarted(BookingStep.Hotel)         => HotelCommand.Reserve
        case SagaEvent.CompensationTriggered(BookingStep.Hotel, _) | SagaEvent.CompensationStarted(BookingStep.Hotel) => HotelCommand.Compensate
      }.label("Hotel Event Router")
      .rmap(_.collect {
        case HotelEvent.Reserved    => BookingCommand.MarkHotelComplete
        case HotelEvent.Failed      => BookingCommand.MarkHotelFailed
        case HotelEvent.Compensated => BookingCommand.MarkHotelCompensationComplete
      })
    ).label("Hotel Service")

  bookingDecider.label("Booking Saga") <-> (labeledFlight merge labeledCar merge labeledHotel)

class BookingSagaSpec extends munit.FunSuite:

  test("happy path: all steps complete in order"):
    assertEquals(
      Apparatus.runA(saga(), BookingCommand.Start),
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
    val events = Apparatus.runA(saga(flight = flightDecider(failsOnReserve = true)), BookingCommand.Start)
    assert(events.contains(SagaEvent.StepProgressed(BookingStep.Flight, SagaStepResult.Failed)))
    assert(events.exists { case SagaEvent.CompensationTriggered(_, _) => true; case _ => false })
    assert(events.exists { case SagaEvent.CompensationProgressed(BookingStep.Car,    SagaStepResult.Completed) => true; case _ => false })
    assert(events.exists { case SagaEvent.CompensationProgressed(BookingStep.Hotel, SagaStepResult.Completed) => true; case _ => false })

  test("flight fails: car compensated, flight not"):
    val events = Apparatus.runA(saga(flight = flightDecider(failsOnReserve = true)), BookingCommand.Start)
    assert(events.contains(SagaEvent.StepProgressed(BookingStep.Flight, SagaStepResult.Failed)))
    assert(events.exists { case SagaEvent.CompensationProgressed(BookingStep.Car, SagaStepResult.Completed) => true; case _ => false })
    assert(!events.exists { case SagaEvent.CompensationProgressed(BookingStep.Flight, _) => true; case _ => false })

  test("car fails: no compensation needed"):
    val events = Apparatus.runA(saga(car = carDecider(failsOnReserve = true)), BookingCommand.Start)
    assert(events.contains(SagaEvent.StepProgressed(BookingStep.Car, SagaStepResult.Failed)))
    assert(!events.exists { case SagaEvent.CompensationStarted(_) => true; case _ => false })

  // ── feedbackMany (Feedback-level reroot) ─────────────────────────────────────

  test("feedbackMany: accepts List[BookingCommand] as entry point"):
    // bookingDecider.feedbackMany(makeServices) is the Feedback node rerooted:
    // instead of a single BookingCommand the caller supplies a List.
    assertEquals(
      Apparatus.runA(bookingDecider.feedbackMany(makeServices()), List(BookingCommand.Start)),
      List(
        SagaEvent.Booted(BookingStep.Hotel, SortedSet(BookingStep.Car, BookingStep.Flight)),
        SagaEvent.StepProgressed(BookingStep.Hotel, SagaStepResult.Completed),
        SagaEvent.StepStarted(BookingStep.Car),
        SagaEvent.StepProgressed(BookingStep.Car, SagaStepResult.Completed),
        SagaEvent.StepStarted(BookingStep.Flight),
        SagaEvent.StepProgressed(BookingStep.Flight, SagaStepResult.Completed)
      )
    )

  // ── sagaRerootedAtCar ─────────────────────────────────────────────────────────

  test("rerooted at car: happy path — car reserved, flight follows"):
    // carDecider is the entry point; bookingDecider pre-seeded at Running(Car).
    // Reserve succeeds → saga continues with Flight and completes.
    assertEquals(
      Apparatus.runA(sagaRerootedAtCar(), CarCommand.Reserve),
      List(
        SagaEvent.StepProgressed(BookingStep.Car,    SagaStepResult.Completed),
        SagaEvent.StepStarted(BookingStep.Flight),
        SagaEvent.StepProgressed(BookingStep.Flight, SagaStepResult.Completed)
      )
    )

  test("rerooted at car: car fails — hotel compensated, flight skipped"):
    // Hotel must be pre-seeded at Reserved: the default bookingAtCar state has Hotel in the
    // compensation set, meaning hotel was already successfully booked before car started.
    val events = Apparatus.runA(
      sagaRerootedAtCar(
        car   = carDecider(failsOnReserve = true),
        hotel = hotelDecider(initialState = HotelState.Reserved)
      ),
      CarCommand.Reserve
    )
    assert(events.contains(SagaEvent.StepProgressed(BookingStep.Car, SagaStepResult.Failed)))
    // Hotel was in the compensation set; it must be compensated.
    assert(events.exists { case SagaEvent.CompensationProgressed(BookingStep.Hotel, SagaStepResult.Completed) => true; case _ => false })
    // Flight was never started, so it must NOT be compensated.
    assert(!events.exists { case SagaEvent.CompensationProgressed(BookingStep.Flight, _) => true; case _ => false })
