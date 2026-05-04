package apparatus.core

import cats.{Applicative, Foldable, Functor, Monad, Monoid, ~>}
import cats.arrow.{Category, Choice, Profunctor, Strong}
import cats.implicits.*

/** Composable, persistent finite-state machine GADT running in effect `F`.
  *
  * Every combinator returns the updated `FSM` alongside its output, making the
  * whole structure immutable and replayable. Build machines by wrapping a
  * [[BaseMachineT]] in [[FSM.Basic]] and composing with:
  *
  *   - [[FSM.Sequential]] — pipe output of `left` into input of `right`
  *   - [[FSM.Parallel]]   — run two independent machines on a pair of inputs
  *   - [[FSM.Alternative]] — route `Either`-typed input to left or right machine
  *   - [[FSM.Feedback]]   — close a loop: left produces commands for right,
  *                          right produces new inputs for left
  *
  * @tparam F effect type
  * @tparam I input type
  * @tparam O output type
  */
sealed trait FSM[F[_], I, O]:
  /** Advance the machine by one input, returning output and the updated machine. */
  def runWith(input: I)(using Monad[F]): F[(O, FSM[F, I, O])]

extension [F[_], A, B](left: FSM[F, A, B]) {

  def imap[A2, B2](using isoIn: Iso[A, A2], isoOut: Iso[B, B2], A: Applicative[F]): FSM[F, A2, B2] =
    left.lmap(isoIn.from).rmap(isoOut.to)

  def andThen[C](right: FSM[F, B, C]): FSM[F, A, C] = FSM.Sequential(left, right)
  def >>>[C](right: FSM[F, B, C]): FSM[F, A, C] = andThen(right)

  def par[C, D](right: FSM[F, C, D]): FSM[F, (A, C), (B, D)] = FSM.Parallel(left, right)
  def ***[C, D](right: FSM[F, C, D]): FSM[F, (A, C), (B, D)] = par(right)

  def or[C, D](right: FSM[F, C, D]): FSM[F, Either[A, C], Either[B, D]] = FSM.Alternative(left, right)
  def |||[C, D](right: FSM[F, C, D]): FSM[F, Either[A, C], Either[B, D]] = or(right)

  def lmap[C](f: C => A)(using A: Applicative[F]): FSM[F, C, B] =
    left match
      case FSM.Basic(m) => FSM.Basic(m.lmap(f))
      case FSM.Sequential(l, r) => FSM.Sequential(l.lmap(f), r)
      case machine => FSM.Basic(BaseMachineT.stateless[F, C, A](c => f(c).pure[F])) >>> machine

  def rmap[C](f: B => C)(using A: Applicative[F]): FSM[F, A, C] =
    left match
      case FSM.Basic(m) => FSM.Basic(m.rmap(f))
      case FSM.Sequential(l, r) => FSM.Sequential(l, r.rmap(f))
      case machine => machine >>> FSM.Basic(BaseMachineT.stateless[F, B, C](b => f(b).pure[F]))

  def dimap[C, D](f: C => A)(g: B => D)(using A: Applicative[F]): FSM[F, C, D] =
    left.lmap(f).rmap(g)

  def first[C](using A: Applicative[F]): FSM[F, (A, C), (B, C)] =
    FSM.Parallel(left, FSM.identity)

  def second[C](using A: Applicative[F]): FSM[F, (C, A), (C, B)] =
    FSM.Parallel(FSM.identity, left)

  def left[C, D](using A: Applicative[F]): FSM[F, Either[A, C], Either[B, C]] =
    FSM.Alternative(left, FSM.identity)

  def right[C, D](using A: Applicative[F]): FSM[F, Either[C, A], Either[C, B]] =
    FSM.Alternative(FSM.identity, left)
}

object FSM:
  /** Wraps a single [[BaseMachineT]], threading its internal state across steps. */
  case class Basic[F[_], I, O](machine: BaseMachineT[F, I, O]) extends FSM[F, I, O]:
    def runWith(input: I)(using Monad[F]): F[(O, FSM[F, I, O])] =
      machine.step(input).map((o, s) => (o, Basic(BaseMachineT(s, (s, i) => machine.action(s, i)))))

  /** Pipes `left`'s output directly into `right`'s input each step. */
  case class Sequential[F[_], A, B, C](left: FSM[F, A, B], right: FSM[F, B, C]) extends FSM[F, A, C]:
    def runWith(input: A)(using Monad[F]): F[(C, FSM[F, A, C])] =
      for
        (o1, l2) <- run(left, input)
        (o2, r2) <- run(right, o1)
      yield (o2, Sequential(l2, r2))

  /** Runs `left` and `right` independently on the two halves of a pair. */
  case class Parallel[F[_], A, B, C, D](left: FSM[F, A, B], right: FSM[F, C, D]) extends FSM[F, (A, C), (B, D)]:
    def runWith(input: (A, C))(using Monad[F]): F[((B, D), FSM[F, (A, C), (B, D)])] =
      for
        (o1, l2) <- run(left, input._1)
        (o2, r2) <- run(right, input._2)
      yield ((o1, o2), Parallel(l2, r2))

  /** Routes `Left` inputs to `left` and `Right` inputs to `right`; the unmatched
    * machine keeps its state untouched.
    */
  case class Alternative[F[_], A, B, C, D](left: FSM[F, A, B], right: FSM[F, C, D]) extends FSM[F, Either[A, C], Either[B, D]]:
    def runWith(input: Either[A, C])(using Monad[F]): F[(Either[B, D], FSM[F, Either[A, C], Either[B, D]])] =
      input match
        case Left(l)  => run(left, l).map  { case (o, l2) => (Left(o),  Alternative(l2, right)) }
        case Right(r) => run(right, r).map { case (o, r2) => (Right(o), Alternative(left, r2)) }

  /** Closed feedback loop between two machines.
    *
    * Given initial input `a`:
    *   1. `left` consumes `a` and emits `N[B]`.
    *   2. Each `B` is fed into `right`, which emits `N[A]`.
    *   3. Those new `A` values are queued and processed recursively until
    *      no further `A`s are produced.
    *
    * Both machines carry independent state across iterations. The accumulated
    * `N[B]` from all iterations is returned as the final output.
    *
    * @tparam N container with `Foldable` and `Monoid` (e.g. `List`)
    */
  case class Feedback[F[_], A, B, N[_]](left: FSM[F, A, N[B]], right: FSM[F, B, N[A]])(
    using foldN: Foldable[N], monoidNB: Monoid[N[B]], monoidNA: Monoid[N[A]]
  ) extends FSM[F, A, N[B]]:
    def runWith(a: A)(using Monad[F]): F[(N[B], FSM[F, A, N[B]])] =
      def loop(lf: FSM[F, A, N[B]], rf: FSM[F, B, N[A]], pending: List[A], acc: N[B]): F[(N[B], FSM[F, A, N[B]])] =
        pending match
          case Nil => (acc, Feedback(lf, rf)).pure[F]
          case head :: tail =>
            for
              (nb, lf2) <- run(lf, head)
              (na, rf2) <- foldN.toList(nb).foldLeftM((monoidNA.empty, rf)):
                             case ((naAcc, rf0), b) =>
                               run(rf0, b).map { case (na2, rf1) => (monoidNA.combine(naAcc, na2), rf1) }
              result    <- loop(lf2, rf2, foldN.toList(na) ++ tail, monoidNB.combine(acc, nb))
            yield result
      loop(left, right, List(a), monoidNB.empty)

  def identity[F[_] : Applicative, A]: FSM[F, A, A] =
    FSM.Basic(BaseMachineT.stateless[F, A, A](_.pure))

  /** Run a single step, returning output and the updated machine. */
  def run[F[_]: Monad, I, O](fsm: FSM[F, I, O], input: I): F[(O, FSM[F, I, O])] =
    fsm.runWith(input)

  /** Fold over a collection of inputs, combining outputs via `Monoid[O]`. */
  def runMultiple[F[_]: Monad, M[_]: Foldable, I, O: Monoid](fsm: FSM[F, I, O], entries: M[I]): F[(O, FSM[F, I, O])] =
    entries.foldM((Monoid[O].empty, fsm)) { case ((acc, m), i) =>
      run(m, i).map((o, nm) => (acc |+| o, nm))
    }

  implicit def category[F[_] : Applicative]: Category[[I, O] =>> FSM[F, I, O]] =
    new Category[[I, O] =>> FSM[F, I, O]] {
      override def id[A]: FSM[F, A, A] = identity
      override def compose[A, B, C](f: FSM[F, B, C], g: FSM[F, A, B]): FSM[F, A, C] =
        g.andThen(f)
    }

  /** `Profunctor` instance: adapt inputs (`lmap`) and outputs (`rmap`) of any `FSM`. */
  implicit def profunctor[F[_]: Monad]: Profunctor[[I, O] =>> FSM[F, I, O]] =
    new Profunctor[[I, O] =>> FSM[F, I, O]]:
      override def dimap[A, B, C, D](fab: FSM[F, A, B])(f: C => A)(g: B => D): FSM[F, C, D] =
        fab.dimap(f)(g)

  implicit def strong[F[_]: Applicative]: Strong[[I, O] =>> FSM[F, I, O]] =
    new Strong[[I, O] =>> FSM[F, I, O]] {
      override def first[A, B, C](fa: FSM[F, A, B]): FSM[F, (A, C), (B, C)] = fa.first
      override def second[A, B, C](fa: FSM[F, A, B]): FSM[F, (C, A), (C, B)] = fa.second
      override def dimap[A, B, C, D](fab: FSM[F, A, B])(f: C => A)(g: B => D): FSM[F, C, D] = fab.dimap(f)(g)
    }

  implicit def choice[F[_] : Applicative]: Choice[[I, O] =>> FSM[F, I, O]] =
    new Choice[[I, O] =>> FSM[F, I, O]] {
      override def choice[A, B, C](f: FSM[F, A, C], g: FSM[F, B, C]): FSM[F, Either[A, B], C] =
        f.or(g).rmap {
          case Left(l) => l
          case Right(r) => r
        }

      override def id[A]: FSM[F, A, A] = identity
      override def compose[A, B, C](f: FSM[F, B, C], g: FSM[F, A, B]): FSM[F, A, C] = g.andThen(f)
    }
