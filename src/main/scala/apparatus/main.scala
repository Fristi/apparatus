package apparatus

import cats.arrow.Profunctor
import cats.implicits.*
import cats.{Applicative, Functor, Id, Monad}

sealed trait FSM[F[_], I, O]

object FSM:
  case class Basic[F[_], I, O](baseMachineT: BaseMachineT[F, I, O]) extends FSM[F, I, O]
  case class Sequential[F[_], A, B, C](left: FSM[F, A, B], right: FSM[F, B, C]) extends FSM[F, A, C]
  case class Parallel[F[_], A, B, C, D](left: FSM[F, A, B], right: FSM[F, C, D]) extends FSM[F, (A, C), (B, D)]
  case class Alternative[F[_], A, B, C, D](left: FSM[F, A, B], right: FSM[F, C, D]) extends FSM[F, Either[A, C], Either[B, D]]

  def run[F[_] : Monad, I, O](fsm: FSM[F, I, O], input: I): F[(O, FSM[F, I, O])] = fsm match {
    case Basic(baseMachineT) => baseMachineT.step(input).map((o, s) => (o, FSM.Basic(BaseMachineT(s, (s, i) => baseMachineT.action(s, i)))))
    case Sequential(left, right) =>
      for {
        (o1, fsm1) <- FSM.run(left, input)
        (o2, fsm2) <- FSM.run(right, o1)
      } yield (o2, FSM.Sequential(fsm1, fsm2))
    case Parallel(left, right) =>
      for {
        (o1, fsm1) <- FSM.run(left, input._1)
        (o2, fsm2) <- FSM.run(right, input._2)
      } yield ((o1, o2), FSM.Parallel(fsm1, fsm2))
    case Alternative(left, right) =>
      input match {
        case Left(l) => ???
        case Right(r) => ???
      }
  }

  def statelessBasic[F[_] : Applicative, I, O](f: I => O): FSM[F, I, O] =
    Basic(BaseMachineT.stateless[F, I, O](i => f(i).pure))

  def statelessBasicT[F[_] : Applicative, I, O](f: I => F[O]): FSM[F, I, O] =
    Basic(BaseMachineT.stateless[F, I, O](f))

  implicit def profunctor[F[_] : Applicative]: Profunctor[[A, B] =>> FSM[F, A, B]] = new Profunctor[[A, B] =>> FSM[F, A, B]] {
    override def dimap[A, B, C, D](fab: FSM[F, A, B])(f: C => A)(g: B => D): FSM[F, C, D] =
      fab match {
        case Basic(baseMachineT) => ???
        case Sequential(left, right) => ???
        case machine => ???
      }
  }


trait BaseMachineT[F[_], I, O] {
  type State
  def initialState: State
  def action(state: State, input: I): F[(O, State)]

  def step(input: I): F[(O, State)] = action(initialState, input)
}

object BaseMachineT {

  def apply[F[_], S, I, O](seed: S, f: (S, I) => F[(O, S)]): BaseMachineT[F, I, O] =
    new BaseMachineT[F, I, O] {
      type State = S
      override def initialState: S = seed
      override def action(state: S, input: I): F[(O, S)] = f(state, input)
    }

  def stateless[F[_] : Functor, I, O](f: I => F[O]): BaseMachineT[F, I, O] =
    apply[F, Tuple, I, O](Tuple(), (_, i) => f(i).map(o => (o, Tuple())))

  implicit def profunctor[F[_] : Functor]: Profunctor[[A, B] =>> BaseMachineT[F, A, B]] = new Profunctor[[A, B] =>> BaseMachineT[F, A, B]] {
    override def dimap[A, B, C, D](fab: BaseMachineT[F, A, B])(f: C => A)(g: B => D): BaseMachineT[F, C, D] =
      new BaseMachineT[F, C, D]:
        type State = fab.State
        def initialState = fab.initialState
        def action(state: State, input: C): F[(D, State)] =
          fab.action(state, f(input)).map { case (b, s) => (g(b), s) }
  }
}

case class Decider[S, I, O](
  initialState: S,
  decide: (I, S) => O,
  evolve: (O, S) => S
) { self =>

  def toBaseMachine[F[_] : Applicative]: BaseMachineT[F, I, O] = new BaseMachineT[F, I, O] {
    override type State = S
    override def initialState = self.initialState
    override def action(state: State, input: I): F[(O, State)] = {
      val o = self.decide(input, state)
      val ns = self.evolve(o, state)

      (o, ns).pure
    }
  }
}


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



val door: Decider[DoorState, DoorCommand, List[DoorEvent]] = Decider[DoorState, DoorCommand, List[DoorEvent]](
  DoorState.Closed(0),
  (c, s) => s match {
    case DoorState.Closed(knocked) => c match {
      case DoorCommand.Knock => List(if (knocked + 1 == 3) DoorEvent.Opened else DoorEvent.Knocked)
      case DoorCommand.Close => Nil
    }
    case DoorState.Open => c match {
      case DoorCommand.Knock => Nil
      case DoorCommand.Close => List(DoorEvent.Closed)
    }
  },
  (evs, s) => evs.foldLeft(s)((state : DoorState, ev : DoorEvent) => state match {
    case DoorState.Closed(knocked) => ev match {
      case DoorEvent.Opened => DoorState.Open
      case DoorEvent.Knocked => DoorState.Closed(knocked + 1)
      case _ => s
    }
    case DoorState.Open => ev match {
      case DoorEvent.Closed => DoorState.Closed(0)
      case _ => s
    }
  })
)

case class DoorStats(opened: Int, closed: Int) {
  def incrOpended = copy(opened = opened + 1)
  def incrClosed = copy(closed = closed + 1)
}

val doorProject: BaseMachineT[Id, List[DoorEvent], DoorStats] =
  BaseMachineT.apply[Id, DoorStats, List[DoorEvent], DoorStats](DoorStats(0, 0), (s, i) => {
    val res = i.foldLeft(s)((s: DoorStats, i: DoorEvent) => i match {
      case DoorEvent.Opened => s.incrOpended
      case DoorEvent.Closed => s.incrClosed
      case _ => s
    })

    (res, res).pure
  })

val doorFSM: FSM[Id, DoorCommand, List[DoorEvent]] = FSM.Basic(door.toBaseMachine)
val projection: FSM[Id, List[DoorEvent], DoorStats] = FSM.Basic(doorProject)

val network = FSM.Sequential(doorFSM, projection)


@main def hello() = {
  val (_, fsm1) = FSM.run(network, DoorCommand.Knock)
  val (_, fsm2) = FSM.run(fsm1, DoorCommand.Knock)
  val (_, fsm3) = FSM.run(fsm2, DoorCommand.Knock)
  val (o, fsm4) = FSM.run(fsm3, DoorCommand.Close)

  println(o)
}