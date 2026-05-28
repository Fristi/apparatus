package apparatus.tests

import apparatus.core.*
import apparatus.core.machines.*
import cats.effect.SyncIO
import cats.implicits.*
import cats.kernel.Monoid
import zio.blocks.schema.Schema
import java.util.UUID

// --- Domain model ---

enum DoorState:
  case Closed(knocked: Int)
  case Open

  def decide(cmd: DoorCommand): List[DoorEvent] = this match {
    case DoorState.Closed(knocked) => cmd match
      case c: DoorCommand.Knock => List(if knocked + 1 == 3 then DoorEvent.Opened(c.id) else DoorEvent.Knocked(c.id))
      case _: DoorCommand.Close => Nil
    case DoorState.Open => cmd match
      case _: DoorCommand.Knock => Nil
      case c: DoorCommand.Close => List(DoorEvent.Closed(c.id))
  }

  def evolve(event: DoorEvent): DoorState = this match {
    case DoorState.Closed(knocked) => event match
      case DoorEvent.Opened(_)  => DoorState.Open
      case DoorEvent.Knocked(_) => DoorState.Closed(knocked + 1)
      case _                    => this
    case DoorState.Open => event match
      case DoorEvent.Closed(_) => DoorState.Closed(0)
      case _                   => this
  }

sealed trait DoorCommand:
  val id: UUID

object DoorCommand:
  case class Knock(id: UUID) extends DoorCommand
  case class Close(id: UUID) extends DoorCommand

enum DoorEvent derives Schema:
  case Knocked(id: UUID)
  case Opened(id: UUID)
  case Closed(id: UUID)

case class DoorStats(opened: Int, closed: Int):
  def incrOpened: DoorStats = copy(opened = opened + 1)
  def incrClosed: DoorStats = copy(closed = closed + 1)

object DoorStats:
  given Monoid[DoorStats]:
    override def empty: DoorStats = DoorStats(0,0)
    override def combine(x: DoorStats, y: DoorStats): DoorStats = DoorStats(x.opened.max(y.opened), x.closed.max(y.closed))


// --- Fixtures ---

val door: Decider[DoorState, DoorCommand, List[DoorEvent]] =
  DeciderBuilder
    .seed[DoorState](DoorState.Closed(0))
    .decide[DoorCommand, List[DoorEvent]](_.decide(_))
    .evolveList(_.evolve(_))

val doorProject: OpenMealy[SyncIO, List[DoorEvent], DoorStats] =
  OpenMealy.apply[SyncIO, DoorStats, List[DoorEvent], DoorStats](
    DoorStats(0, 0),
    (s, i) => SyncIO.pure {
      val res = i.foldLeft(s)((s, ev) => ev match
        case DoorEvent.Opened(_) => s.incrOpened
        case DoorEvent.Closed(_) => s.incrClosed
        case _                   => s
      )
      (res, res)
    }
  )

private val doorId = java.util.UUID.fromString("00000000-0000-0000-0000-000000000001")

def freshNetwork: Apparatus[SyncIO, DoorCommand, DoorStats] =
  Apparatus.aggregateMachine("door", door, _.id) >>> Apparatus.openMealy(doorProject)

def runAll[O: Monoid](fsm: Apparatus[SyncIO, DoorCommand, O], cmds: DoorCommand*): O =
  Apparatus.runMultiple(fsm, cmds, DeciderMaterializer.syncIO).unsafeRunSync()

// --- Tests ---

class DoorSpec extends munit.FunSuite:

  test("single knock emits Knocked, stays closed"):
    val evts = door.decide(DoorCommand.Knock(doorId), DoorState.Closed(0))
    assertEquals(evts, List(DoorEvent.Knocked(doorId)))

  test("third knock emits Opened"):
    val evts = door.decide(DoorCommand.Knock(doorId), DoorState.Closed(2))
    assertEquals(evts, List(DoorEvent.Opened(doorId)))

  test("knock when open emits nothing"):
    val evts = door.decide(DoorCommand.Knock(doorId), DoorState.Open)
    assertEquals(evts, Nil)

  test("close when open emits Closed"):
    val evts = door.decide(DoorCommand.Close(doorId), DoorState.Open)
    assertEquals(evts, List(DoorEvent.Closed(doorId)))

  test("close when closed emits nothing"):
    val evts = door.decide(DoorCommand.Close(doorId), DoorState.Closed(1))
    assertEquals(evts, Nil)

  test("three knocks open the door via Apparatus network"):
    val stats = runAll(freshNetwork, DoorCommand.Knock(doorId), DoorCommand.Knock(doorId), DoorCommand.Knock(doorId))
    assertEquals(stats.opened, 1)
    assertEquals(stats.closed, 0)

  test("open then close via Apparatus network"):
    val stats = runAll(
      freshNetwork,
      DoorCommand.Knock(doorId), DoorCommand.Knock(doorId), DoorCommand.Knock(doorId),
      DoorCommand.Close(doorId)
    )
    assertEquals(stats.opened, 1)
    assertEquals(stats.closed, 1)

  test("stats accumulate across multiple open/close cycles"):
    val stats = runAll(
      freshNetwork,
      DoorCommand.Knock(doorId), DoorCommand.Knock(doorId), DoorCommand.Knock(doorId), // open
      DoorCommand.Close(doorId),                                                         // close
      DoorCommand.Knock(doorId), DoorCommand.Knock(doorId), DoorCommand.Knock(doorId), // open again
      DoorCommand.Close(doorId)                                                          // close again
    )
    assertEquals(stats.opened, 2)
    assertEquals(stats.closed, 2)
