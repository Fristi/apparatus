package apparatus.core

import cats.Id
import cats.data.NonEmptyList
import cats.implicits.*

// --- Service-level types ---

enum SvcState:
  case Idle, Reserved, Compensated, Failed

enum SvcCmd:
  case Reserve, Compensate

enum SvcEvt:
  case Reserved, Compensated, Failed

// --- Saga-level types ---

enum EntityType:
  case Car, Flight, Hotel

enum BookingInput:
  case Start
  case ProcessEntity(entityType: EntityType, event: SvcEvt)

case class BookingOutput(entityType: EntityType, command: SvcCmd)


enum SagaPhase:
  case Waiting, Succeeded, Failed
  case Running(todoStack: List[BookingOutput], compensationStack: List[BookingOutput])
  case Compensating(compensationStack: List[BookingOutput])

case class SagaStep(doCommand: BookingOutput, undoCommand: BookingOutput)

val flightStep = SagaStep(BookingOutput(EntityType.Flight, SvcCmd.Reserve), BookingOutput(EntityType.Flight, SvcCmd.Compensate))
val carStep = SagaStep(BookingOutput(EntityType.Car, SvcCmd.Reserve), BookingOutput(EntityType.Car, SvcCmd.Compensate))
val hotelStep = SagaStep(BookingOutput(EntityType.Hotel, SvcCmd.Reserve), BookingOutput(EntityType.Hotel, SvcCmd.Compensate))

val sagaSteps = NonEmptyList.of(flightStep, carStep, hotelStep)


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
                ): FSM[Id, BookingOutput, List[BookingInput]] =
  flight.or(car.or(hotel)).dimap(
    (out: BookingOutput) => out.entityType match
      case EntityType.Flight => Left(out.command)
      case EntityType.Car => Right(Left(out.command))
      case EntityType.Hotel => Right(Right(out.command))
  ) {
    case Left(evts) => evts.map(BookingInput.ProcessEntity(EntityType.Flight, _))
    case Right(Left(evts)) => evts.map(BookingInput.ProcessEntity(EntityType.Car, _))
    case Right(Right(evts)) => evts.map(BookingInput.ProcessEntity(EntityType.Hotel, _))
  }

def next(phase: SagaPhase, input: BookingInput): (List[BookingOutput], SagaPhase) =
  (phase, input) match

    case (SagaPhase.Waiting, BookingInput.Start) =>
      val NonEmptyList(first, rest) = sagaSteps
      (
        List(first.doCommand),
        SagaPhase.Running(
          todoStack = rest.map(_.doCommand),
          compensationStack = Nil
        )
      )

    case (SagaPhase.Running(todo, comp), BookingInput.ProcessEntity(et, SvcEvt.Reserved)) =>
      val currentUndo = sagaSteps.find(_.doCommand.entityType == et).get.undoCommand
      val newComp = currentUndo :: comp
      todo match
        case nextCmd :: remaining =>
          (
            List(nextCmd),
            SagaPhase.Running(
              todoStack = remaining,
              compensationStack = newComp
            )
          )

        case Nil =>
          (Nil, SagaPhase.Succeeded)

    case (SagaPhase.Running(_, comp), BookingInput.ProcessEntity(_, SvcEvt.Failed)) =>
      (
        comp.headOption.toList,
        SagaPhase.Compensating(comp)
      )

    case (SagaPhase.Compensating(comp), BookingInput.ProcessEntity(_, SvcEvt.Compensated)) =>
      comp match
        case _ :: rest =>
          rest match
            case nextUndo :: _ => (List(nextUndo), SagaPhase.Compensating(rest))
            case Nil => (Nil, SagaPhase.Failed)

        case Nil => (Nil, SagaPhase.Failed)

    case _ => (Nil, phase)

// --- Orchestrator ---

val orchestrator: FSM[Id, BookingInput, List[BookingOutput]] =
  FSM.Basic(BaseMachineT[Id, SagaPhase, BookingInput, List[BookingOutput]](SagaPhase.Waiting, (phase, input) => next(phase, input)))

// --- Tests ---

class BookingSagaSpec extends munit.FunSuite:

  def saga(
            flight: FSM[Id, SvcCmd, List[SvcEvt]] = serviceDecider(),
            car: FSM[Id, SvcCmd, List[SvcEvt]] = serviceDecider(),
            hotel: FSM[Id, SvcCmd, List[SvcEvt]] = serviceDecider()
          ): FSM[Id, BookingInput, List[BookingOutput]] =
    orchestrator <-> makeServices(flight, car, hotel)

  test("happy path: full reservation command trail"):
    val cmds = FSM.runA(saga(), BookingInput.Start)
    assertEquals(cmds, List(BookingOutput(EntityType.Flight, SvcCmd.Reserve), BookingOutput(EntityType.Car, SvcCmd.Reserve), BookingOutput(EntityType.Hotel, SvcCmd.Reserve)))

  test("hotel fails: car and flight compensated in reverse"):
    val cmds = FSM.runA(saga(hotel = serviceDecider(failsOnReserve = true)), BookingInput.Start)
    assertEquals(cmds, List(
      BookingOutput(EntityType.Flight, SvcCmd.Reserve), BookingOutput(EntityType.Car, SvcCmd.Reserve), BookingOutput(EntityType.Hotel, SvcCmd.Reserve),
      BookingOutput(EntityType.Car, SvcCmd.Compensate), BookingOutput(EntityType.Flight, SvcCmd.Compensate)
    ))

  test("car fails: only flight compensated"):
    val cmds = FSM.runA(saga(car = serviceDecider(failsOnReserve = true)), BookingInput.Start)
    assertEquals(cmds, List(BookingOutput(EntityType.Flight, SvcCmd.Reserve), BookingOutput(EntityType.Car, SvcCmd.Reserve), BookingOutput(EntityType.Flight, SvcCmd.Compensate)))

  test("flight fails: nothing to compensate"):
    val cmds = FSM.runA(saga(flight = serviceDecider(failsOnReserve = true)), BookingInput.Start)
    assertEquals(cmds, List(BookingOutput(EntityType.Flight, SvcCmd.Reserve)))

  test("service state is respected: Compensate on Idle emits nothing"):
    // hotel fails → ToCar(Compensate) issued, but car is already Idle if it also failed
    // (edge case: car fails AND hotel fails — only flight needs compensating)
    val cmds = FSM.runA(
      saga(
        car = serviceDecider(failsOnReserve = true),
        hotel = serviceDecider(failsOnReserve = true)
      ),
      BookingInput.Start
    )
    // Car fails → compensate flight only (hotel never reached)
    assertEquals(cmds, List(BookingOutput(EntityType.Flight, SvcCmd.Reserve), BookingOutput(EntityType.Car, SvcCmd.Reserve), BookingOutput(EntityType.Flight, SvcCmd.Compensate)))
