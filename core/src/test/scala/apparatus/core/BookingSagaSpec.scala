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
    case BookingCommand.MarkFlightCompensationFailed => ("Flight", SagaStepResult.Failed)
    case BookingCommand.MarkHotelCompensationComplete => ("Hotel", SagaStepResult.Completed)
    case BookingCommand.MarkHotelCompensationFailed => ("Hotel", SagaStepResult.Failed)
    case BookingCommand.MarkCarCompensationComplete => ("Car", SagaStepResult.Completed)
    case BookingCommand.MarkCarCompensationFailed => ("Car", SagaStepResult.Failed)

  }
  override def stepHandler: PartialFunction[BookingCommand, (String, SagaStepResult)] = {
    case BookingCommand.MarkFlightComplete => ("Flight", SagaStepResult.Completed)
    case BookingCommand.MarkFlightFailed => ("Flight", SagaStepResult.Failed)
    case BookingCommand.MarkHotelComplete => ("Hotel", SagaStepResult.Completed)
    case BookingCommand.MarkHotelFailed => ("Hotel", SagaStepResult.Failed)
    case BookingCommand.MarkCarComplete => ("Car", SagaStepResult.Completed)
    case BookingCommand.MarkCarFailed => ("Car", SagaStepResult.Failed)
  }
}

val bookingDecider: FSM[Id, BookingCommand, List[SagaEvent]] = FSM.Basic(behavior.decider.toBaseMachine)


//def rehydrate(cmds: List[SagaCommand]): SagaState[BookingEvent] =
//  cmds.foldLeft(SagaState.Waiting: SagaState[BookingEvent])((s, c) => sagaNext(s, c)._2)
//
//def orchestratorFrom(state: SagaState[BookingEvent]): FSM[Id, SagaCommand, List[BookingEvent]] =
//  FSM.Basic(BaseMachineT[Id, SagaState[BookingEvent], SagaCommand, List[BookingEvent]](state, sagaNext))
//
//val orchestrator: FSM[Id, SagaCommand, List[BookingEvent]] =
//  orchestratorFrom(SagaState.Waiting)
//
//def makeServices(
//                  flight: FSM[Id, FlightCommand, List[FlightEvent]],
//                  car:    FSM[Id, CarCommand,    List[CarEvent]],
//                  hotel:  FSM[Id, HotelCommand,  List[HotelEvent]]
//                ): FSM[Id, BookingEvent, List[SagaCommand]] =
//  flight.or(car.or(hotel)).dimap(
//    (evt: BookingEvent) => evt match
//      case BookingEvent.ToFlight(cmd) => Left(cmd)
//      case BookingEvent.ToCar(cmd)    => Right(Left(cmd))
//      case BookingEvent.ToHotel(cmd)  => Right(Right(cmd))
//  ) {
//    case Left(evts)         => evts.map(SagaCommand.FlightResponse(_))
//    case Right(Left(evts))  => evts.map(SagaCommand.CarResponse(_))
//    case Right(Right(evts)) => evts.map(SagaCommand.HotelResponse(_))
//  }
//
//// ── Tests ─────────────────────────────────────────────────────────────────────
//
//class BookingSagaSpec extends munit.FunSuite:
//
//  def saga(
//            flight: Decider[FlightState, FlightCommand, List[FlightEvent]] = flightDecider(),
//            car:    Decider[CarState,    CarCommand,    List[CarEvent]]    = carDecider(),
//            hotel:  Decider[HotelState,  HotelCommand,  List[HotelEvent]]  = hotelDecider()
//          ): FSM[Id, SagaCommand, List[BookingEvent]] =
//    orchestrator <-> makeServices(
//      FSM.Basic(flight.toBaseMachine[Id]),
//      FSM.Basic(car.toBaseMachine[Id]),
//      FSM.Basic(hotel.toBaseMachine[Id])
//    )
//
//  test("happy path: full reservation command trail"):
//    assertEquals(
//      FSM.runA(saga(), SagaCommand.Start),
//      List(
//        BookingEvent.ToFlight(FlightCommand.Reserve),
//        BookingEvent.ToCar(CarCommand.Reserve),
//        BookingEvent.ToHotel(HotelCommand.Reserve)
//      )
//    )
//
//  test("hotel fails: car and flight compensated in reverse"):
//    assertEquals(
//      FSM.runA(saga(hotel = hotelDecider(failsOnReserve = true)), SagaCommand.Start),
//      List(
//        BookingEvent.ToFlight(FlightCommand.Reserve),
//        BookingEvent.ToCar(CarCommand.Reserve),
//        BookingEvent.ToHotel(HotelCommand.Reserve),
//        BookingEvent.ToCar(CarCommand.Compensate),
//        BookingEvent.ToFlight(FlightCommand.Compensate)
//      )
//    )
//
//  test("car fails: only flight compensated"):
//    assertEquals(
//      FSM.runA(saga(car = carDecider(failsOnReserve = true)), SagaCommand.Start),
//      List(
//        BookingEvent.ToFlight(FlightCommand.Reserve),
//        BookingEvent.ToCar(CarCommand.Reserve),
//        BookingEvent.ToFlight(FlightCommand.Compensate)
//      )
//    )
//
//  test("flight fails: nothing to compensate"):
//    assertEquals(
//      FSM.runA(saga(flight = flightDecider(failsOnReserve = true)), SagaCommand.Start),
//      List(BookingEvent.ToFlight(FlightCommand.Reserve))
//    )
//
//  test("service state respected: Compensate on Idle emits nothing"):
//    assertEquals(
//      FSM.runA(saga(car = carDecider(failsOnReserve = true), hotel = hotelDecider(failsOnReserve = true)), SagaCommand.Start),
//      List(
//        BookingEvent.ToFlight(FlightCommand.Reserve),
//        BookingEvent.ToCar(CarCommand.Reserve),
//        BookingEvent.ToFlight(FlightCommand.Compensate)
//      )
//    )
//
//  test("rehydration: saga continues after async pause at hotel step"):
//    // Simulate: Start + flight reserved + car reserved, then crash before hotel response
//    val pastSagaCmds = List(
//      SagaCommand.Start,
//      SagaCommand.FlightResponse(FlightEvent.Reserved),
//      SagaCommand.CarResponse(CarEvent.Reserved)
//    )
//    // Reconstruct FSM network from persisted saga state + replayed entity event streams
//    val network = orchestratorFrom(rehydrate(pastSagaCmds)) <-> makeServices(
//      FSM.Basic(flightDecider().evolveFrom(List(FlightEvent.Reserved)).toBaseMachine[Id]),
//      FSM.Basic(carDecider().evolveFrom(List(CarEvent.Reserved)).toBaseMachine[Id]),
//      FSM.Basic(hotelDecider().evolveFrom(List(HotelEvent.Reserved)).toBaseMachine[Id])
//    )
//    // Hotel Kafka confirmation finally arrives
//    assertEquals(
//      FSM.runA(network, SagaCommand.HotelResponse(HotelEvent.Reserved)),
//      List.empty[BookingEvent]  // all reserved, saga succeeded, no more commands
//    )
