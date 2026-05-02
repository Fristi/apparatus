package apparatus.core

import cats.Id
import cats.implicits.*
import cats.kernel.Monoid

// --- Domain model ---

enum DoorState:
  case Closed(knocked: Int)
  case Open

enum DoorCommand:
  case Knock
  case Close

enum DoorEvent:
  case Knocked
  case Opened
  case Closed

case class DoorStats(opened: Int, closed: Int):
  def incrOpened: DoorStats = copy(opened = opened + 1)
  def incrClosed: DoorStats = copy(closed = closed + 1)

object DoorStats:
  given Monoid[DoorStats]:
    override def empty: DoorStats = DoorStats(0,0)
    override def combine(x: DoorStats, y: DoorStats): DoorStats = DoorStats(x.opened.max(y.opened), x.closed.max(y.closed))


// --- Fixtures ---

val door: Decider[DoorState, DoorCommand, List[DoorEvent]] =
  Decider[DoorState, DoorCommand, List[DoorEvent]](
    DoorState.Closed(0),
    (c, s) => s match
      case DoorState.Closed(knocked) => c match
        case DoorCommand.Knock  => List(if knocked + 1 == 3 then DoorEvent.Opened else DoorEvent.Knocked)
        case DoorCommand.Close  => Nil
      case DoorState.Open => c match
        case DoorCommand.Knock  => Nil
        case DoorCommand.Close  => List(DoorEvent.Closed)
    ,
    (evs, s) => evs.foldLeft(s)((state, ev) => state match
      case DoorState.Closed(knocked) => ev match
        case DoorEvent.Opened => DoorState.Open
        case DoorEvent.Knocked => DoorState.Closed(knocked + 1)
        case _                => state
      case DoorState.Open => ev match
        case DoorEvent.Closed => DoorState.Closed(0)
        case _                => state
    )
  )

val doorProject: BaseMachineT[Id, List[DoorEvent], DoorStats] =
  BaseMachineT.apply[Id, DoorStats, List[DoorEvent], DoorStats](
    DoorStats(0, 0),
    (s, i) =>
      val res = i.foldLeft(s)((s, ev) => ev match
        case DoorEvent.Opened => s.incrOpened
        case DoorEvent.Closed => s.incrClosed
        case _                => s
      )
      (res, res)
  )

def freshNetwork: FSM[Id, DoorCommand, DoorStats] =
  FSM.Sequential(FSM.Basic(door.toBaseMachine[Id]), FSM.Basic(doorProject))

def runAll[O : Monoid](fsm: FSM[Id, DoorCommand, O], cmds: DoorCommand*): (O, FSM[Id, DoorCommand, O]) =
  FSM.runMultiple(fsm, cmds)

// --- Tests ---

class DoorSpec extends munit.FunSuite:

  test("single knock emits Knocked, stays closed"):
    val evts = door.decide(DoorCommand.Knock, DoorState.Closed(0))
    assertEquals(evts, List(DoorEvent.Knocked))

  test("third knock emits Opened"):
    val evts = door.decide(DoorCommand.Knock, DoorState.Closed(2))
    assertEquals(evts, List(DoorEvent.Opened))

  test("knock when open emits nothing"):
    val evts = door.decide(DoorCommand.Knock, DoorState.Open)
    assertEquals(evts, Nil)

  test("close when open emits Closed"):
    val evts = door.decide(DoorCommand.Close, DoorState.Open)
    assertEquals(evts, List(DoorEvent.Closed))

  test("close when closed emits nothing"):
    val evts = door.decide(DoorCommand.Close, DoorState.Closed(1))
    assertEquals(evts, Nil)

  test("three knocks open the door via FSM network"):
    val (stats, _) = runAll(freshNetwork, DoorCommand.Knock, DoorCommand.Knock, DoorCommand.Knock)
    assertEquals(stats.opened, 1)
    assertEquals(stats.closed, 0)

  test("open then close via FSM network"):
    val (stats, _) = runAll(
      freshNetwork,
      DoorCommand.Knock, DoorCommand.Knock, DoorCommand.Knock,
      DoorCommand.Close
    )
    assertEquals(stats.opened, 1)
    assertEquals(stats.closed, 1)

  test("stats accumulate across multiple open/close cycles"):
    val (stats, _) = runAll(
      freshNetwork,
      DoorCommand.Knock, DoorCommand.Knock, DoorCommand.Knock, // open
      DoorCommand.Close,                                        // close
      DoorCommand.Knock, DoorCommand.Knock, DoorCommand.Knock, // open again
      DoorCommand.Close                                         // close again
    )
    assertEquals(stats.opened, 2)
    assertEquals(stats.closed, 2)
