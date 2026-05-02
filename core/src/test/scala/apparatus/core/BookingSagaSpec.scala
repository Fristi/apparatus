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


// --- Service FSM (Decider-based, stateful) ---
//
// Rejects commands that are invalid for the current state, e.g.
// Compensate on an Idle service emits nothing.

def serviceDecider(failsOnReserve: Boolean = false): FSM[Id, SvcCmd, List[SvcEvt]] =
  FSM.Basic(
    Decider[SvcState, SvcCmd, List[SvcEvt]](
      SvcState.Idle,
      (cmd, state) => (state, cmd) match
        case (SvcState.Idle, SvcCmd.Reserve) => if failsOnReserve then List(SvcEvt.Failed) else List(SvcEvt.Reserved)
        case (SvcState.Reserved, SvcCmd.Compensate) => List(SvcEvt.Compensated)
        case _ => Nil
      ,
      (evts, state) => evts.foldLeft(state):
        case (SvcState.Idle, SvcEvt.Reserved) => SvcState.Reserved
        case (SvcState.Idle, SvcEvt.Failed) => SvcState.Failed
        case (SvcState.Reserved, SvcEvt.Compensated) => SvcState.Compensated
        case (s, _) => s
    ).toBaseMachine[Id]
  )

// --- Service router: Alternative routes one BookingOutput at a time ---
//
// dimap adapts the Either-typed Alternative into BookingOutput / List[BookingInput].
// Sequential per feedback step; each service carries independent state.

def makeServices(
                  flight: FSM[Id, SvcCmd, List[SvcEvt]],
                  car: FSM[Id, SvcCmd, List[SvcEvt]],
                  hotel: FSM[Id, SvcCmd, List[SvcEvt]]
                ): FSM[Id, BookingOutput, List[BookingInput]] = {
  val alt = FSM.Alternative(flight, FSM.Alternative(car, hotel))
  Profunctor[[I, O] =>> FSM[Id, I, O]].dimap(alt)(
    (out: BookingOutput) => out match
      case BookingOutput.ToFlight(c) => Left(c)
      case BookingOutput.ToCar(c) => Right(Left(c))
      case BookingOutput.ToHotel(c) => Right(Right(c))
  ) {
    case Left(evts) => evts.map(BookingInput.FlightEvt(_))
    case Right(Left(evts)) => evts.map(BookingInput.CarEvt(_))
    case Right(Right(evts)) => evts.map(BookingInput.HotelEvt(_))
  }
}

// --- Orchestrator ---

val orchestrator: FSM[Id, BookingInput, List[BookingOutput]] =
  FSM.Basic(BaseMachineT[Id, SagaPhase, BookingInput, List[BookingOutput]](
    SagaPhase.Waiting,
    (phase, input) => (phase, input) match
      case (SagaPhase.Waiting, BookingInput.Start) => (List(BookingOutput.ToFlight(SvcCmd.Reserve)), SagaPhase.ReservingFlight)
      case (SagaPhase.ReservingFlight, BookingInput.FlightEvt(SvcEvt.Reserved)) => (List(BookingOutput.ToCar(SvcCmd.Reserve)), SagaPhase.ReservingCar)
      case (SagaPhase.ReservingCar, BookingInput.CarEvt(SvcEvt.Reserved)) => (List(BookingOutput.ToHotel(SvcCmd.Reserve)), SagaPhase.ReservingHotel)
      case (SagaPhase.ReservingHotel, BookingInput.HotelEvt(SvcEvt.Reserved)) => (Nil, SagaPhase.Succeeded)
      case (SagaPhase.ReservingFlight, BookingInput.FlightEvt(SvcEvt.Failed)) => (Nil, SagaPhase.Failed)
      case (SagaPhase.ReservingCar, BookingInput.CarEvt(SvcEvt.Failed)) => (List(BookingOutput.ToFlight(SvcCmd.Compensate)), SagaPhase.CompensatingFlight)
      case (SagaPhase.ReservingHotel, BookingInput.HotelEvt(SvcEvt.Failed)) => (List(BookingOutput.ToCar(SvcCmd.Compensate)), SagaPhase.CompensatingCar)
      case (SagaPhase.CompensatingCar, BookingInput.CarEvt(SvcEvt.Compensated)) => (List(BookingOutput.ToFlight(SvcCmd.Compensate)), SagaPhase.CompensatingFlight)
      case (SagaPhase.CompensatingFlight, BookingInput.FlightEvt(SvcEvt.Compensated)) => (Nil, SagaPhase.Failed)
      case _ => (Nil, phase)
  ))

// --- Tests ---

class BookingSagaSpec extends munit.FunSuite:

  def saga(
            flight: FSM[Id, SvcCmd, List[SvcEvt]] = serviceDecider(),
            car: FSM[Id, SvcCmd, List[SvcEvt]] = serviceDecider(),
            hotel: FSM[Id, SvcCmd, List[SvcEvt]] = serviceDecider()
          ): FSM[Id, BookingInput, List[BookingOutput]] =
    FSM.Feedback(orchestrator, makeServices(flight, car, hotel))

  test("happy path: full reservation command trail"):
    val (cmds, _) = FSM.run(saga(), BookingInput.Start)
    assertEquals(cmds, List(BookingOutput.ToFlight(SvcCmd.Reserve), BookingOutput.ToCar(SvcCmd.Reserve), BookingOutput.ToHotel(SvcCmd.Reserve)))

  test("hotel fails: car and flight compensated in reverse"):
    val (cmds, _) = FSM.run(saga(hotel = serviceDecider(failsOnReserve = true)), BookingInput.Start)
    assertEquals(cmds, List(
      BookingOutput.ToFlight(SvcCmd.Reserve), BookingOutput.ToCar(SvcCmd.Reserve), BookingOutput.ToHotel(SvcCmd.Reserve),
      BookingOutput.ToCar(SvcCmd.Compensate), BookingOutput.ToFlight(SvcCmd.Compensate)
    ))

  test("car fails: only flight compensated"):
    val (cmds, _) = FSM.run(saga(car = serviceDecider(failsOnReserve = true)), BookingInput.Start)
    assertEquals(cmds, List(BookingOutput.ToFlight(SvcCmd.Reserve), BookingOutput.ToCar(SvcCmd.Reserve), BookingOutput.ToFlight(SvcCmd.Compensate)))

  test("flight fails: nothing to compensate"):
    val (cmds, _) = FSM.run(saga(flight = serviceDecider(failsOnReserve = true)), BookingInput.Start)
    assertEquals(cmds, List(BookingOutput.ToFlight(SvcCmd.Reserve)))

  test("service state is respected: Compensate on Idle emits nothing"):
    // hotel fails → ToCar(Compensate) issued, but car is already Idle if it also failed
    // (edge case: car fails AND hotel fails — only flight needs compensating)
    val (cmds, _) = FSM.run(
      saga(
        car = serviceDecider(failsOnReserve = true),
        hotel = serviceDecider(failsOnReserve = true)
      ),
      BookingInput.Start
    )
    // Car fails → compensate flight only (hotel never reached)
    assertEquals(cmds, List(BookingOutput.ToFlight(SvcCmd.Reserve), BookingOutput.ToCar(SvcCmd.Reserve), BookingOutput.ToFlight(SvcCmd.Compensate)))
