package apparatus.core

import cats.*
import cats.arrow.{Category, Choice, Profunctor, Strong}
import cats.implicits.*

/** Composable, immutable finite-state machine running in effect `F`.
  *
  * Every step returns the output **and** an updated `Apparatus` carrying the new
  * internal state, making the whole structure purely functional and replayable.
  *
  * === Building machines ===
  *
  * Wrap any [[BaseMachineT]] (or a [[Decider]] via `toBaseMachine`) in [[Apparatus.Basic]]:
  * {{{
  *   val door: Apparatus[Id, DoorCmd, List[DoorEvt]] =
  *     Apparatus.Basic(myDecider.toBaseMachine[Id])
  * }}}
  *
  * === Combinators ===
  *
  *   - [[Apparatus.Sequential]] / `>>>` — pipe `left` output into `right` input each step
  *   - [[Apparatus.Parallel]]   / `***` — run two independent machines on a pair of inputs
  *   - [[Apparatus.Alternative]]/ `|||` — route `Either`-typed input to left or right machine
  *   - [[Apparatus.Feedback]]   / `<->` — close a bidirectional loop; `left` drives `right`,
  *                                         `right` feeds new inputs back into `left`
  *   - [[Apparatus.FeedbackMany]]       — like `Feedback` but accepts `N[A]` as initial input
  *   - [[Apparatus.Merged]]     / `merge` — fan the same input to both machines, combine outputs
  *   - [[Apparatus.LmapOrEmpty]]         — partial contramap; silent when undefined
  *   - [[Apparatus.Labeled]]    / `.label` — attach a display name for [[ApparatusMermaid]]
  *
  * === Running ===
  *
  * {{{
  *   val (output, next) = Apparatus.run(machine, input)          // one step
  *   val (allOut, _)    = Apparatus.runMultiple(machine, inputs)  // fold over many inputs
  * }}}
  *
  * @tparam F effect type (e.g. `Id`, `IO`, `Either[E, *]`)
  * @tparam I input type
  * @tparam O output type
  */
sealed trait Apparatus[F[_], I, O] { outer =>

  /** Advance the machine by one input, returning output and the updated machine. */
  def runWith(input: I)(using Monad[F]): F[(O, Apparatus[F, I, O])]

  /** Transform the effect type via a natural transformation. */
  def mapK[G[_]](f: F ~> G): Apparatus[G, I, O]
}

extension [F[_], A, B](left: Apparatus[F, A, B]) {

  def label(name: String): Apparatus[F, A, B] = Apparatus.Labeled(left, name)

  def tap[C](right: Apparatus[F, B, C]): Apparatus[F, A, B] =
    new Apparatus[F, A, B]:
      def runWith(input: A)(using Monad[F]): F[(B, Apparatus[F, A, B])] =
        for
          (b, l2) <- left.runWith(input)
          (_, r2) <- right.runWith(b)
        yield (b, l2.tap(r2))

      def mapK[G[_]](f: F ~> G): Apparatus[G, A, B] =
        left.mapK(f).tap(right.mapK(f))

  /** Bidirectional mapping via [[Iso]]: adapt input with `isoIn.from`, output with `isoOut.to`. */
  def imap[A2, B2](using isoIn: Iso[A, A2], isoOut: Iso[B, B2], A: Applicative[F]): Apparatus[F, A2, B2] =
    left.lmap(isoIn.from).rmap(isoOut.to)

  /** Pipe this machine's output into `right`'s input each step. */
  def andThen[C](right: Apparatus[F, B, C]): Apparatus[F, A, C] = Apparatus.Sequential(left, right)
  /** Alias for [[andThen]]. */
  def >>>[C](right: Apparatus[F, B, C]): Apparatus[F, A, C] = andThen(right)

  /** Run this machine and `right` in parallel on the two halves of a pair. */
  def par[C, D](right: Apparatus[F, C, D]): Apparatus[F, (A, C), (B, D)] = Apparatus.Parallel(left, right)
  /** Alias for [[par]]. */
  def ***[C, D](right: Apparatus[F, C, D]): Apparatus[F, (A, C), (B, D)] = par(right)

  /** Route `Either`-typed input: `Left` to this machine, `Right` to `right`. */
  def or[C, D](right: Apparatus[F, C, D]): Apparatus[F, Either[A, C], Either[B, D]] = Apparatus.Alternative(left, right)
  /** Alias for [[or]]. */
  def |||[C, D](right: Apparatus[F, C, D]): Apparatus[F, Either[A, C], Either[B, D]] = or(right)

  /** Contramap the input: transform `C` to `A` before feeding each step. */
  def lmap[C](f: C => A)(using A: Applicative[F]): Apparatus[F, C, B] =
    left match
      case Apparatus.Basic(m) => Apparatus.Basic(m.lmap(f))
      case Apparatus.Sequential(l, r) => Apparatus.Sequential(l.lmap(f), r)
      case machine => Apparatus.Basic(BaseMachineT.stateless[F, C, A](c => f(c).pure[F])) >>> machine

  /** Map the output: transform `B` to `C` after each step. */
  def rmap[C](f: B => C)(using A: Applicative[F]): Apparatus[F, A, C] =
    left match
      case Apparatus.Basic(m) => Apparatus.Basic(m.rmap(f))
      case Apparatus.Sequential(l, r) => Apparatus.Sequential(l, r.rmap(f))
      case machine => machine >>> Apparatus.Basic(BaseMachineT.stateless[F, B, C](b => f(b).pure[F]))

  /** Adapt both input and output in a single pass. */
  def dimap[C, D](f: C => A)(g: B => D)(using A: Applicative[F]): Apparatus[F, C, D] =
    left.lmap(f).rmap(g)

  /** Pass a paired input through: run this machine on `_1`, forward `_2` unchanged. */
  def first[C](using A: Applicative[F]): Apparatus[F, (A, C), (B, C)] =
    Apparatus.Parallel(left, Apparatus.identity)

  /** Pass a paired input through: forward `_1` unchanged, run this machine on `_2`. */
  def second[C](using A: Applicative[F]): Apparatus[F, (C, A), (C, B)] =
    Apparatus.Parallel(Apparatus.identity, left)

  /** Route `Left` to this machine; forward `Right` unchanged. */
  def left[C, D](using A: Applicative[F]): Apparatus[F, Either[A, C], Either[B, C]] =
    Apparatus.Alternative(left, Apparatus.identity)

  /** Route `Right` to this machine; forward `Left` unchanged. */
  def right[C, D](using A: Applicative[F]): Apparatus[F, Either[C, A], Either[C, B]] =
    Apparatus.Alternative(Apparatus.identity, left)

  /** Map input via a partial function; returns [[Monoid.empty]] for the output (without
   * advancing state) when the function is undefined for the given input.
   */
  def lmapOrEmpty[C](pf: PartialFunction[C, A])(using m: Monoid[B]): Apparatus[F, C, B] =
    Apparatus.LmapOrEmpty(left, pf, m)

  /** Run both machines on the same input and combine their outputs via [[Monoid]].
   * Both machines advance their state independently on every step.
   */
  def merge(right: Apparatus[F, A, B])(using m: Monoid[B]): Apparatus[F, A, B] =
    Apparatus.Merged(left, right, m)
}

extension [F[_], M[_], A, B](left: Apparatus[F, A, M[B]]) {
  def feedback(right: Apparatus[F, B, M[A]])(using F: Foldable[M], MB: Monoid[M[B]], MA: Monoid[M[A]]): Apparatus[F, A, M[B]] =
    Apparatus.Feedback(left, right)

  def <->(right: Apparatus[F, B, M[A]])(using F: Foldable[M], MB: Monoid[M[B]], MA: Monoid[M[A]]): Apparatus[F, A, M[B]] =
    feedback(right)

  /** Like [[feedback]] but the entry point accepts `M[A]` (a collection) rather than a single `A`.
    *
    * Use when composing a sub-machine (whose output is already `M[A]`) with a feedback reactor,
    * or after rerooting a [[Apparatus.Feedback]] node.
    */
  def feedbackMany(right: Apparatus[F, B, M[A]])(using F: Foldable[M], MB: Monoid[M[B]], MA: Monoid[M[A]]): Apparatus[F, M[A], M[B]] =
    Apparatus.FeedbackMany(left, right)
}

object Apparatus:
  /** Wraps a single [[BaseMachineT]], threading its internal state across steps. */
  case class Basic[F[_], I, O](machine: BaseMachineT[F, I, O]) extends Apparatus[F, I, O]:
    def runWith(input: I)(using Monad[F]): F[(O, Apparatus[F, I, O])] =
      machine.step(input).map((o, s) => (o, Basic(BaseMachineT(s, (s, i) => machine.action(s, i)))))
    def mapK[G[_]](f: F ~> G): Apparatus[G, I, O] = Basic(machine.mapK(f))

  /** Pipes `left`'s output directly into `right`'s input each step. */
  case class Sequential[F[_], A, B, C](left: Apparatus[F, A, B], right: Apparatus[F, B, C]) extends Apparatus[F, A, C]:
    def runWith(input: A)(using Monad[F]): F[(C, Apparatus[F, A, C])] =
      for
        (o1, l2) <- run(left, input)
        (o2, r2) <- run(right, o1)
      yield (o2, Sequential(l2, r2))
    def mapK[G[_]](f: F ~> G): Apparatus[G, A, C] = Sequential(left.mapK(f), right.mapK(f))

  /** Runs `left` and `right` independently on the two halves of a pair. */
  case class Parallel[F[_], A, B, C, D](left: Apparatus[F, A, B], right: Apparatus[F, C, D]) extends Apparatus[F, (A, C), (B, D)]:
    def runWith(input: (A, C))(using Monad[F]): F[((B, D), Apparatus[F, (A, C), (B, D)])] =
      for
        (o1, l2) <- run(left, input._1)
        (o2, r2) <- run(right, input._2)
      yield ((o1, o2), Parallel(l2, r2))
    def mapK[G[_]](f: F ~> G): Apparatus[G, (A, C), (B, D)] = Parallel(left.mapK(f), right.mapK(f))

  /** Routes `Left` inputs to `left` and `Right` inputs to `right`; the unmatched
    * machine keeps its state untouched.
    */
  case class Alternative[F[_], A, B, C, D](left: Apparatus[F, A, B], right: Apparatus[F, C, D]) extends Apparatus[F, Either[A, C], Either[B, D]]:
    def runWith(input: Either[A, C])(using Monad[F]): F[(Either[B, D], Apparatus[F, Either[A, C], Either[B, D]])] =
      input match
        case Left(l)  => run(left, l).map  { case (o, l2) => (Left(o),  Alternative(l2, right)) }
        case Right(r) => run(right, r).map { case (o, r2) => (Right(o), Alternative(left, r2)) }
    def mapK[G[_]](f: F ~> G): Apparatus[G, Either[A, C], Either[B, D]] = Alternative(left.mapK(f), right.mapK(f))

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
  case class Feedback[F[_], A, B, N[_]](left: Apparatus[F, A, N[B]], right: Apparatus[F, B, N[A]])(
    using foldN: Foldable[N], monoidNB: Monoid[N[B]], monoidNA: Monoid[N[A]]
  ) extends Apparatus[F, A, N[B]]:
    def runWith(a: A)(using Monad[F]): F[(N[B], Apparatus[F, A, N[B]])] =
      def loop(lf: Apparatus[F, A, N[B]], rf: Apparatus[F, B, N[A]], pending: List[A], acc: N[B]): F[(N[B], Apparatus[F, A, N[B]])] =
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
    def mapK[G[_]](f: F ~> G): Apparatus[G, A, N[B]] = Feedback(left.mapK(f), right.mapK(f))

  /** Like [[Feedback]] but accepts `N[A]` (a collection) as the initial input instead of
    * a single `A`. All elements are processed through the same feedback loop in order.
    *
    * Typical use: after rerooting a [[Feedback]] node, or when composing a sub-machine
    * whose output is already `N[A]` (e.g. via `>>>`) with a feedback reactor.
    *
    * @tparam N container with `Foldable` and `Monoid` (e.g. `List`)
    */
  case class FeedbackMany[F[_], A, B, N[_]](left: Apparatus[F, A, N[B]], right: Apparatus[F, B, N[A]])(
    using foldN: Foldable[N], monoidNB: Monoid[N[B]], monoidNA: Monoid[N[A]]
  ) extends Apparatus[F, N[A], N[B]]:
    def runWith(nas: N[A])(using Monad[F]): F[(N[B], Apparatus[F, N[A], N[B]])] =
      def loop(lf: Apparatus[F, A, N[B]], rf: Apparatus[F, B, N[A]], pending: List[A], acc: N[B]): F[(N[B], Apparatus[F, N[A], N[B]])] =
        pending match
          case Nil => (acc, FeedbackMany(lf, rf)).pure[F]
          case head :: tail =>
            for
              (nb, lf2) <- run(lf, head)
              (na, rf2) <- foldN.toList(nb).foldLeftM((monoidNA.empty, rf)):
                             case ((naAcc, rf0), b) =>
                               run(rf0, b).map { case (na2, rf1) => (monoidNA.combine(naAcc, na2), rf1) }
              result    <- loop(lf2, rf2, foldN.toList(na) ++ tail, monoidNB.combine(acc, nb))
            yield result
      loop(left, right, foldN.toList(nas), monoidNB.empty)
    def mapK[G[_]](f: F ~> G): Apparatus[G, N[A], N[B]] = FeedbackMany(left.mapK(f), right.mapK(f))

  /** Routes input via a partial function; returns [[Monoid.empty]] without advancing state
    * when the function is undefined for the given input.
    */
  case class LmapOrEmpty[F[_], A, B, C](inner: Apparatus[F, A, B], pf: PartialFunction[C, A], mb: Monoid[B]) extends Apparatus[F, C, B]:
    def runWith(input: C)(using Monad[F]): F[(B, Apparatus[F, C, B])] =
      pf.lift(input) match
        case Some(a) => inner.runWith(a).map((b, m2) => (b, LmapOrEmpty(m2, pf, mb)))
        case None    => (mb.empty, this).pure[F]
    def mapK[G[_]](f: F ~> G): Apparatus[G, C, B] = LmapOrEmpty(inner.mapK(f), pf, mb)

  /** Runs both machines on the same input; outputs are combined via [[Monoid]].
    * Both machines advance state independently on every step.
    */
  case class Merged[F[_], A, B](left: Apparatus[F, A, B], right: Apparatus[F, A, B], mb: Monoid[B]) extends Apparatus[F, A, B]:
    def runWith(input: A)(using Monad[F]): F[(B, Apparatus[F, A, B])] =
      for
        (b1, l2) <- run(left, input)
        (b2, r2) <- run(right, input)
      yield (mb.combine(b1, b2), Merged(l2, r2, mb))
    def mapK[G[_]](f: F ~> G): Apparatus[G, A, B] = Merged(left.mapK(f), right.mapK(f), mb)

  /** Attaches a human-readable label used by [[apparatus.core.ApparatusMermaid]].
    * On a [[Basic]] it renames the node; on composites it wraps in a subgraph.
    */
  case class Labeled[F[_], I, O](inner: Apparatus[F, I, O], name: String) extends Apparatus[F, I, O]:
    def runWith(input: I)(using Monad[F]): F[(O, Apparatus[F, I, O])] =
      inner.runWith(input).map((o, m2) => (o, Labeled(m2, name)))
    def mapK[G[_]](f: F ~> G): Apparatus[G, I, O] = Labeled(inner.mapK(f), name)

  /** Stateless identity machine: passes every input through unchanged. */
  def identity[F[_] : Applicative, A]: Apparatus[F, A, A] =
    Apparatus.Basic(BaseMachineT.stateless[F, A, A](_.pure))

  /** Run a single step, returning output and the updated machine. */
  def run[F[_]: Monad, I, O](fsm: Apparatus[F, I, O], input: I): F[(O, Apparatus[F, I, O])] =
    fsm.runWith(input)

  def runA[F[_] : Monad, I, O](fsm: Apparatus[F, I, O], input: I): F[O] =
    run(fsm, input).map((x, _) => x)

  /** Fold over a collection of inputs, combining outputs via `Monoid[O]`. */
  def runMultiple[F[_]: Monad, M[_]: Foldable, I, O: Monoid](fsm: Apparatus[F, I, O], entries: M[I]): F[(O, Apparatus[F, I, O])] =
    entries.foldM((Monoid[O].empty, fsm)) { case ((acc, m), i) =>
      run(m, i).map((o, nm) => (acc |+| o, nm))
    }

  def runMultipleA[F[_]: Monad, M[_]: Foldable, I, O: Monoid](fsm: Apparatus[F, I, O], entries: M[I]): F[O] =
    runMultiple(fsm, entries).map((x, _) => x)

  /** `Category` instance: `id` is [[identity]], `compose` is [[andThen]] (reversed). */
  implicit def category[F[_] : Applicative]: Category[[I, O] =>> Apparatus[F, I, O]] =
    new Category[[I, O] =>> Apparatus[F, I, O]] {
      override def id[A]: Apparatus[F, A, A] = identity
      override def compose[A, B, C](f: Apparatus[F, B, C], g: Apparatus[F, A, B]): Apparatus[F, A, C] =
        g.andThen(f)
    }

  /** `Profunctor` instance: adapt inputs (`lmap`) and outputs (`rmap`) of any `Apparatus`. */
  implicit def profunctor[F[_]: Monad]: Profunctor[[I, O] =>> Apparatus[F, I, O]] =
    new Profunctor[[I, O] =>> Apparatus[F, I, O]]:
      override def dimap[A, B, C, D](fab: Apparatus[F, A, B])(f: C => A)(g: B => D): Apparatus[F, C, D] =
        fab.dimap(f)(g)

  /** `Strong` instance: [[first]] and [[second]] thread one half of a pair through unchanged. */
  implicit def strong[F[_]: Applicative]: Strong[[I, O] =>> Apparatus[F, I, O]] =
    new Strong[[I, O] =>> Apparatus[F, I, O]] {
      override def first[A, B, C](fa: Apparatus[F, A, B]): Apparatus[F, (A, C), (B, C)] = fa.first
      override def second[A, B, C](fa: Apparatus[F, A, B]): Apparatus[F, (C, A), (C, B)] = fa.second
      override def dimap[A, B, C, D](fab: Apparatus[F, A, B])(f: C => A)(g: B => D): Apparatus[F, C, D] = fab.dimap(f)(g)
    }

  /** `Choice` instance: merge two `Either`-routed machines into one that collapses output to `C`. */
  implicit def choice[F[_] : Applicative]: Choice[[I, O] =>> Apparatus[F, I, O]] =
    new Choice[[I, O] =>> Apparatus[F, I, O]] {
      override def choice[A, B, C](f: Apparatus[F, A, C], g: Apparatus[F, B, C]): Apparatus[F, Either[A, B], C] =
        f.or(g).rmap {
          case Left(l) => l
          case Right(r) => r
        }

      override def id[A]: Apparatus[F, A, A] = identity
      override def compose[A, B, C](f: Apparatus[F, B, C], g: Apparatus[F, A, B]): Apparatus[F, A, C] = g.andThen(f)
    }
