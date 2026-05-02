package apparatus.core

import cats.Id
import cats.arrow.Profunctor
import cats.implicits.*

// --- Service-level types ---

enum SvcState:
  case Idle, Reserved, Compensated, Failed

enum SvcCmd:
  case Reserve, Compensate

enum SvcEvt:
  case Reserved, Compensated, Failed

// --- Saga-level types ---

enum BookingInput:
  case Start
  case FlightEvt(e: SvcEvt)
  case CarEvt(e: SvcEvt)
  case HotelEvt(e: SvcEvt)

enum BookingOutput:
  case ToFlight(c: SvcCmd)
  case ToCar(c: SvcCmd)
  case ToHotel(c: SvcCmd)

enum SagaPhase:
  case Waiting
  case ReservingFlight, ReservingCar, ReservingHotel
  case CompensatingCar, CompensatingFlight
  case Succeeded, Failed

import SvcCmd.*, SvcEvt.*, BookingInput.*, BookingOutput.*, SagaPhase.*

// --- Service FSM (Decider-based, stateful) ---
//
// Rejects commands that are invalid for the current state, e.g.
// Compensate on an Idle service emits nothing.

def serviceDecider(failsOnReserve: Boolean = false): FSM[Id, SvcCmd, List[SvcEvt]] =
  FSM.Basic(
    Decider[SvcState, SvcCmd, List[SvcEvt]](
      SvcState.Idle,
      (cmd, state) => (state, cmd) match
        case (SvcState.Idle,     Reserve)    => if failsOnReserve then List(SvcEvt.Failed) else List(Reserved)
        case (SvcState.Reserved, Compensate) => List(Compensated)
        case _                               => Nil
      ,
      (evts, state) => evts.foldLeft(state):
        case (SvcState.Idle,     Reserved)    => SvcState.Reserved
        case (SvcState.Idle,     SvcEvt.Failed) => SvcState.Failed
        case (SvcState.Reserved, Compensated) => SvcState.Compensated
        case (s, _)                           => s
    ).toBaseMachine[Id]
  )

// --- Service router: Alternative routes one BookingOutput at a time ---
//
// dimap adapts the Either-typed Alternative into BookingOutput / List[BookingInput].
// Sequential per feedback step; each service carries independent state.

def makeServices(
  flight: FSM[Id, SvcCmd, List[SvcEvt]],
  car:    FSM[Id, SvcCmd, List[SvcEvt]],
  hotel:  FSM[Id, SvcCmd, List[SvcEvt]]
): FSM[Id, BookingOutput, List[BookingInput]] =
  val alt = FSM.Alternative(flight, FSM.Alternative(car, hotel))
  Profunctor[[I, O] =>> FSM[Id, I, O]].dimap(alt)(
    (out: BookingOutput) => out match
      case ToFlight(c) => Left(c)
      case ToCar(c)    => Right(Left(c))
      case ToHotel(c)  => Right(Right(c))
  ) {
    case Left(evts)         => evts.map(FlightEvt(_))
    case Right(Left(evts))  => evts.map(CarEvt(_))
    case Right(Right(evts)) => evts.map(HotelEvt(_))
  }

// --- Orchestrator ---

val orchestrator: FSM[Id, BookingInput, List[BookingOutput]] =
  FSM.Basic(BaseMachineT[Id, SagaPhase, BookingInput, List[BookingOutput]](
    Waiting,
    (phase, input) => (phase, input) match
      case (Waiting,            Start)                             => (List(ToFlight(Reserve)),   ReservingFlight)
      case (ReservingFlight,    FlightEvt(SvcEvt.Reserved))        => (List(ToCar(Reserve)),       ReservingCar)
      case (ReservingCar,       CarEvt(SvcEvt.Reserved))           => (List(ToHotel(Reserve)),     ReservingHotel)
      case (ReservingHotel,     HotelEvt(SvcEvt.Reserved))         => (Nil,                        Succeeded)
      case (ReservingFlight,    FlightEvt(SvcEvt.Failed))          => (Nil,                        SagaPhase.Failed)
      case (ReservingCar,       CarEvt(SvcEvt.Failed))             => (List(ToFlight(Compensate)), CompensatingFlight)
      case (ReservingHotel,     HotelEvt(SvcEvt.Failed))           => (List(ToCar(Compensate)),    CompensatingCar)
      case (CompensatingCar,    CarEvt(SvcEvt.Compensated))        => (List(ToFlight(Compensate)), CompensatingFlight)
      case (CompensatingFlight, FlightEvt(SvcEvt.Compensated))     => (Nil,                        SagaPhase.Failed)
      case _                                                        => (Nil,                        phase)
  ))

// --- Tests ---

class BookingSagaSpec extends munit.FunSuite:

  def saga(
    flight: FSM[Id, SvcCmd, List[SvcEvt]] = serviceDecider(),
    car:    FSM[Id, SvcCmd, List[SvcEvt]] = serviceDecider(),
    hotel:  FSM[Id, SvcCmd, List[SvcEvt]] = serviceDecider()
  ): FSM[Id, BookingInput, List[BookingOutput]] =
    FSM.feedback(orchestrator, makeServices(flight, car, hotel))

  test("happy path: full reservation command trail"):
    val (cmds, _) = FSM.run(saga(), Start)
    assertEquals(cmds, List(ToFlight(Reserve), ToCar(Reserve), ToHotel(Reserve)))

  test("hotel fails: car and flight compensated in reverse"):
    val (cmds, _) = FSM.run(saga(hotel = serviceDecider(failsOnReserve = true)), Start)
    assertEquals(cmds, List(
      ToFlight(Reserve), ToCar(Reserve), ToHotel(Reserve),
      ToCar(Compensate), ToFlight(Compensate)
    ))

  test("car fails: only flight compensated"):
    val (cmds, _) = FSM.run(saga(car = serviceDecider(failsOnReserve = true)), Start)
    assertEquals(cmds, List(ToFlight(Reserve), ToCar(Reserve), ToFlight(Compensate)))

  test("flight fails: nothing to compensate"):
    val (cmds, _) = FSM.run(saga(flight = serviceDecider(failsOnReserve = true)), Start)
    assertEquals(cmds, List(ToFlight(Reserve)))

  test("service state is respected: Compensate on Idle emits nothing"):
    // hotel fails → ToCar(Compensate) issued, but car is already Idle if it also failed
    // (edge case: car fails AND hotel fails — only flight needs compensating)
    val (cmds, _) = FSM.run(
      saga(
        car   = serviceDecider(failsOnReserve = true),
        hotel = serviceDecider(failsOnReserve = true)
      ),
      Start
    )
    // Car fails → compensate flight only (hotel never reached)
    assertEquals(cmds, List(ToFlight(Reserve), ToCar(Reserve), ToFlight(Compensate)))
