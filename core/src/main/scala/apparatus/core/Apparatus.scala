package apparatus.core

import apparatus.core
import apparatus.core.fix.alg.Mermaid
import apparatus.core.fix.{ApparatusF, HFix2, alg}
import apparatus.core.machines.{AggregateEntry, ClosedMealy, Decider, DeciderMaterializer, OpenMealy}
import cats.MonadThrow
import java.util.UUID
import apparatus.core.Iso
import cats.arrow.{Category, Choice, Profunctor, Strong}
import cats.effect.kernel.Ref
import cats.implicits.*
import cats.{Applicative, Foldable, Monad, Monoid}
import zio.blocks.schema.Schema

// ─── The fixed point type ────────────────────────────────────────────────────

/** Fixed-point of [[apparatus.core.fix.ApparatusF]] — the primary type for composing stateful,
  * effectful networks of machines.
  *
  * An `Apparatus[Eff, I, O]` is a dataflow network that accepts inputs of type `I` and
  * produces outputs of type `O`, with side effects tracked in `Eff`.  Networks are built
  * from leaf machines ([[openMealy]] / [[closedMealy]] / [[aggregateMachine]]) and
  * combinator nodes ([[sequential]], [[parallel]], [[alternative]], [[feedback]], etc.).
  *
  * The network is described as a recursive data structure (via `HFix2`) and is compiled
  * to a runnable form by [[alg.compile]] before execution.
  *
  * @tparam Eff the effect type (e.g. `cats.effect.IO`, `cats.effect.SyncIO`, `cats.Id`)
  * @tparam I   the input type
  * @tparam O   the output type
  */
type Apparatus[Eff[_], I, O] = HFix2[[F[_, _], I, O] =>> ApparatusF[F, Eff, I, O], I, O]


// ─── Smart constructors ──────────────────────────────────────────────────────

object Apparatus:

  /** Creates an aggregate machine that routes inputs to per-UUID [[machines.Decider]] instances.
    *
    * On first encounter of an aggregate ID the machine is materialised via `mat`; subsequent
    * inputs for the same ID reuse the cached [[machines.ClosedMealy]].  State is stored in a
    * `cats.effect.kernel.Ref` per aggregate.
    *
    * @param d         the decider describing aggregate behaviour
    * @param extractId function that extracts the routing UUID from an input
    * @tparam Eff the effect type
    * @tparam I   command / input type
    * @tparam E   event type emitted by the decider
    */
  def aggregateMachine[Eff[_], I, E](
    d:         Decider[?, I, List[E]],
    extractId: I => UUID
  )(using S: Schema[E]): Apparatus[Eff, I, List[E]] = {
    val entry = new AggregateEntry[Eff]:
      def compileRouter(m: DeciderMaterializer[Eff])(using Ref.Make[Eff], Monad[Eff]): Eff[ClosedMealy[Eff, ?, ?]] =
        Ref[Eff].of(Map.empty[UUID, ClosedMealy[Eff, Any, Any]]).map { registry =>
          new ClosedMealy[Eff, Any, Any]:
            def action(rawInput: Any): Eff[Any] =
              val i  = rawInput.asInstanceOf[I]
              val id = extractId(i)
              registry.get.flatMap { map =>
                map.get(id) match
                  case Some(cm) => cm.action(i)
                  case None     =>
                    m.materialize(d, id)(using S)
                      .asInstanceOf[Eff[ClosedMealy[Eff, Any, Any]]]
                      .flatMap { cm => registry.update(_ + (id -> cm)) *> cm.action(i) }
              }
        }.asInstanceOf[Eff[ClosedMealy[Eff, ?, ?]]]
    HFix2(ApparatusF.AggregateMachine(d.name, entry))
  }

  /** Per-UUID routing for an Either-typed Decider. On Left(err): raises into F via MonadThrow —
   *  downstream nodes (projections) never run. On Right(events): standard state evolution.
   *  Uses per-UUID Ref[S] (same granularity as [[aggregateMachine]]). Bypasses DeciderMaterializer.
   */
  def aggregateMachineE[Eff[_], S, I, Err <: Throwable, E](
    d:         Decider[S, I, Either[Err, List[E]]],
    extractId: I => UUID
  )(using mt: MonadThrow[Eff]): Apparatus[Eff, I, List[E]] = {
    val entry = new AggregateEntry[Eff]:
      def compileRouter(m: DeciderMaterializer[Eff])(using make: Ref.Make[Eff], monad: Monad[Eff]): Eff[ClosedMealy[Eff, ?, ?]] =
        Ref[Eff].of(Map.empty[UUID, ClosedMealy[Eff, Any, Any]]).map { registry =>
          new ClosedMealy[Eff, Any, Any]:
            def action(rawInput: Any): Eff[Any] =
              val i  = rawInput.asInstanceOf[I]
              val id = extractId(i)
              registry.get.flatMap { map =>
                map.get(id) match
                  case Some(cm) => cm.action(i)
                  case None     =>
                    Ref[Eff].of(d.state).map { stateRef =>
                      new ClosedMealy[Eff, Any, Any]:
                        def action(rawInput: Any): Eff[Any] =
                          val inp = rawInput.asInstanceOf[I]
                          stateRef.get.flatMap { s =>
                            d.decide(inp, s) match
                              case Left(err)     => mt.raiseError(err)
                              case Right(events) =>
                                val ns = d.evolve(Right(events), s)
                                stateRef.set(ns).as(events.asInstanceOf[Any])
                          }
                    }.flatMap { cm => registry.update(_ + (id -> cm)) *> cm.action(i) }
              }
        }.asInstanceOf[Eff[ClosedMealy[Eff, ?, ?]]]
    HFix2(ApparatusF.AggregateMachine(d.name, entry))
  }

  /** Connects `left` and `right` in sequence: output of `left` becomes input to `right`. */
  def sequential[Eff[_], A, B, C](left: Apparatus[Eff, A, B], right: Apparatus[Eff, B, C]): Apparatus[Eff, A, C] =
    HFix2(ApparatusF.Sequential(left, right))

  /** Runs `left` and `right` in parallel, pairing their inputs and outputs as tuples. */
  def parallel[Eff[_], A, B, C, D](left: Apparatus[Eff, A, B], right: Apparatus[Eff, C, D]): Apparatus[Eff, (A, C), (B, D)] =
    HFix2(ApparatusF.Parallel(left, right))

  /** Routes `Left` inputs to `left` and `Right` inputs to `right`, preserving the `Either` wrapper. */
  def alternative[Eff[_], A, B, C, D](left: Apparatus[Eff, A, B], right: Apparatus[Eff, C, D]): Apparatus[Eff, Either[A, C], Either[B, D]] =
    HFix2(ApparatusF.Alternative(left, right))

  /** Creates a feedback loop with a single-value entry point.
    *
    * `left` emits `N[B]`; each element is fed into `right` which produces `N[A]`, which is
    * re-queued into `left` until both sides produce empty collections.
    *
    * @param foldN     fold instance used to iterate the collection type `N`
    * @param monoidNB  identity and combine for `N[B]` (used to accumulate `left` outputs)
    * @param monoidNA  identity and combine for `N[A]` (used to accumulate `right` outputs)
    */
  def feedback[Eff[_], A, B, N[_]](
                                    left: Apparatus[Eff, A, N[B]], right: Apparatus[Eff, B, N[A]]
                                  )(using foldN: Foldable[N], monoidNB: Monoid[N[B]], monoidNA: Monoid[N[A]]): Apparatus[Eff, A, N[B]] =
    HFix2(ApparatusF.Feedback(left, right, foldN, monoidNB, monoidNA))

  /** Like [[feedback]] but the entry point accepts a collection `N[A]` instead of a single `A`. */
  def feedbackMany[Eff[_], A, B, N[_]](
                                        left: Apparatus[Eff, A, N[B]], right: Apparatus[Eff, B, N[A]]
                                      )(using foldN: Foldable[N], monoidNB: Monoid[N[B]], monoidNA: Monoid[N[A]]): Apparatus[Eff, N[A], N[B]] =
    HFix2(ApparatusF.FeedbackMany(left, right, foldN, monoidNB, monoidNA))

  /** Applies `pf` to the input before `inner`; returns `Monoid[B].empty` when `pf` is undefined. */
  def lmapOrEmpty[Eff[_], A, B, C](inner: Apparatus[Eff, A, B], pf: PartialFunction[C, A])(using mb: Monoid[B]): Apparatus[Eff, C, B] =
    HFix2(ApparatusF.LmapOrEmpty(inner, pf, mb))

  /** Fan-out: sends the same input to both `left` and `right`, then combines their outputs via `Monoid[B]`. */
  def merged[Eff[_], A, B](left: Apparatus[Eff, A, B], right: Apparatus[Eff, A, B])(using mb: Monoid[B]): Apparatus[Eff, A, B] =
    HFix2(ApparatusF.Merged(left, right, mb))

  /** Lifts a raw [[machines.OpenMealy]] (stateful machine) into an `Apparatus` leaf node.
    *
    * State is managed externally by the caller (or by [[alg.Normalize]] via a `Ref` upon compilation).
    */
  def openMealy[Eff[_], I, O](machine: OpenMealy[Eff, I, O]): Apparatus[Eff, I, O] =
    HFix2(ApparatusF.OpenMachine(machine))

  /** Lifts a [[machines.ClosedMealy]] (self-contained stateful machine) into an `Apparatus` leaf node. */
  def closedMealy[Eff[_], I, O](machine: ClosedMealy[Eff, I, O]): Apparatus[Eff, I, O] =
    HFix2(ApparatusF.ClosedMachine(machine))

  /** Wraps `inner` in a named label, used by Mermaid rendering and diagnostics. */
  def labeled[Eff[_], I, O](name: String)(inner: Apparatus[Eff, I, O]): Apparatus[Eff, I, O] =
    HFix2(ApparatusF.Labeled(inner, name))

  /** Identity machine: passes inputs through unchanged, lifting the value into `F` via `pure`. */
  def identity[F[_] : Applicative, A]: Apparatus[F, A, A] =
    closedMealy(ClosedMealy.stateless[F, A, A](_.pure))

  // ── Run methods ─────────────────────────────────────────────────────────────

  /** Run a single step. */
  def runA[F[_]: {Monad, Ref.Make}, I, O](fsm: Apparatus[F, I, O], input: I, mat: DeciderMaterializer[F]): F[O] =
    alg.compile(mat)(fsm).flatMap(_.run(input))

  /** Alias for [[runA]]. */
  def run[F[_]: {Monad, Ref.Make}, I, O](fsm: Apparatus[F, I, O], input: I, mat: DeciderMaterializer[F]): F[O] =
    runA(fsm, input, mat)

  /** Fold over inputs, threading state via Refs allocated at compile time. */
  def runMultipleA[F[_]: {Monad, Ref.Make}, M[_]: Foldable, I, O: Monoid](
    fsm: Apparatus[F, I, O], entries: M[I], mat: DeciderMaterializer[F]
  ): F[O] =
    alg.compile(mat)(fsm).flatMap { network =>
      entries.foldM(Monoid[O].empty)((acc, i) => network.run(i).map(acc |+| _))
    }

  /** Run a sequence of inputs, returning all outputs in order. State is threaded via Refs. */
  def runSteps[F[_]: {Monad, Ref.Make}, I, O](fsm: Apparatus[F, I, O], inputs: List[I], mat: DeciderMaterializer[F]): F[List[O]] =
    alg.compile(mat)(fsm).flatMap { network =>
      inputs.traverse(network.run)
    }

  /** Alias for [[runMultipleA]]. */
  def runMultiple[F[_]: {Monad, Ref.Make}, M[_]: Foldable, I, O: Monoid](
    fsm: Apparatus[F, I, O], entries: M[I], mat: DeciderMaterializer[F]
  ): F[O] =
    runMultipleA(fsm, entries, mat)

  // ── Cats instances ───────────────────────────────────────────────────────────

  /** `Category` instance: `id` is the pass-through identity machine; `compose` is [[sequential]]. */
  given [F[_] : Applicative]: Category[[I, O] =>> Apparatus[F, I, O]] with
    def id[A]: Apparatus[F, A, A] = Apparatus.identity[F, A]
    def compose[A, B, C](f: Apparatus[F, B, C], g: Apparatus[F, A, B]): Apparatus[F, A, C] =
      Apparatus.sequential(g, f)

  /** `Profunctor` instance: `dimap` is `lmap` ∘ `rmap` via stateless closed machines. */
  given [F[_] : Applicative]: Profunctor[[I, O] =>> Apparatus[F, I, O]] with
    def dimap[A, B, C, D](fab: Apparatus[F, A, B])(f: C => A)(g: B => D): Apparatus[F, C, D] =
      fab.lmap(f).rmap(g)

  /** `Strong` instance: `first`/`second` thread an untouched component alongside via [[parallel]]. */
  given [F[_] : Applicative]: Strong[[I, O] =>> Apparatus[F, I, O]] with
    def first[A, B, C](fa: Apparatus[F, A, B]): Apparatus[F, (A, C), (B, C)] = fa.first[C]
    def second[A, B, C](fa: Apparatus[F, A, B]): Apparatus[F, (C, A), (C, B)] = fa.second[C]
    def dimap[A, B, C, D](fab: Apparatus[F, A, B])(f: C => A)(g: B => D): Apparatus[F, C, D] =
      fab.lmap(f).rmap(g)

  /** `Choice` instance: routes `Either` inputs via [[alternative]], then merges outputs with `.merge`. */
  given [F[_] : Applicative]: Choice[[I, O] =>> Apparatus[F, I, O]] with
    def choice[A, B, C](f: Apparatus[F, A, C], g: Apparatus[F, B, C]): Apparatus[F, Either[A, B], C] =
      Apparatus.alternative(f, g).rmap(_.merge)
    def id[A]: Apparatus[F, A, A] = Apparatus.identity[F, A]
    def compose[A, B, C](f: Apparatus[F, B, C], g: Apparatus[F, A, B]): Apparatus[F, A, C] =
      Apparatus.sequential(g, f)

// ─── Extension methods ────────────────────────────────────────────────────────

extension [F[_], I, O](left: Apparatus[F, I, O]) {

  // Structural combinators — no Applicative constraint needed

  /** Annotates this machine with `name` for Mermaid diagrams and diagnostics. */
  def label(name: String): Apparatus[F, I, O] =
    Apparatus.labeled(name)(left)

  /** Sequences `right` after `left`: `left`'s output feeds `right`'s input. */
  def andThen[C](right: Apparatus[F, O, C]): Apparatus[F, I, C] =
    Apparatus.sequential(left, right)

  /** Operator alias for [[andThen]]. */
  def >>>[C](right: Apparatus[F, O, C]): Apparatus[F, I, C] = andThen(right)

  /** Runs `left` and `right` side by side, pairing inputs and outputs. */
  def par[C, D](right: Apparatus[F, C, D]): Apparatus[F, (I, C), (O, D)] =
    Apparatus.parallel(left, right)

  /** Operator alias for [[par]]. */
  def ***[C, D](right: Apparatus[F, C, D]): Apparatus[F, (I, C), (O, D)] = par(right)

  /** Routes `Left` to `left` and `Right` to `right`, preserving the `Either` wrapper. */
  def or[C, D](right: Apparatus[F, C, D]): Apparatus[F, Either[I, C], Either[O, D]] =
    Apparatus.alternative(left, right)

  /** Operator alias for [[or]]. */
  def |||[C, D](right: Apparatus[F, C, D]): Apparatus[F, Either[I, C], Either[O, D]] = or(right)

  /** Applies `pf` to map `C` → `I` before `left`; yields `Monoid[O].empty` when `pf` is undefined. */
  def lmapOrEmpty[C](pf: PartialFunction[C, I])(using m: Monoid[O]): Apparatus[F, C, O] =
    Apparatus.lmapOrEmpty(left, pf)

  /** Fan-out: sends every input to both `left` and `right`, combining their outputs with `Monoid[O]`. */
  def merge(right: Apparatus[F, I, O])(using m: Monoid[O]): Apparatus[F, I, O] =
    Apparatus.merged(left, right)

  // Effect-dependent combinators — Applicative[F] from the type

  /** Prepends a stateless mapping stage `f: C → I` before `left`. */
  def lmap[C](f: C => I)(using Applicative[F]): Apparatus[F, C, O] =
    Apparatus.sequential(
      Apparatus.closedMealy(ClosedMealy.stateless[F, C, I](c => f(c).pure)),
      left
    )

  /** Appends a stateless mapping stage `f: O → C` after `left`. */
  def rmap[C](f: O => C)(using Applicative[F]): Apparatus[F, I, C] =
    Apparatus.sequential(
      left,
      Apparatus.closedMealy(ClosedMealy.stateless[F, O, C](o => f(o).pure))
    )

  /** Contramap on input and map on output in one step. */
  def dimap[C, D](f: C => I)(g: O => D)(using Applicative[F]): Apparatus[F, C, D] =
    left.lmap(f).rmap(g)

  /** Re-types input and output via isomorphisms, composing [[lmap]] and [[rmap]]. */
  def imap[I2, O2](using isoIn: Iso[I, I2], isoOut: Iso[O, O2], A: Applicative[F]): Apparatus[F, I2, O2] =
    left.lmap(isoIn.from).rmap(isoOut.to)

  /** Threads an extra component `C` untouched alongside via `parallel` with the identity machine. */
  def first[C](using Applicative[F]): Apparatus[F, (I, C), (O, C)] =
    Apparatus.parallel(left, Apparatus.identity[F, C])

  /** Like [[first]] but `C` comes first in the pair. */
  def second[C](using Applicative[F]): Apparatus[F, (C, I), (C, O)] =
    Apparatus.parallel(Apparatus.identity[F, C], left)

  /** Runs `right` as a side-effect branch while passing `left`'s output through unchanged.
    *
    * The output of `right` is discarded; only `left`'s output is returned.
    */
  def tap[C](right: Apparatus[F, O, C])(using Applicative[F]): Apparatus[F, I, O] =
    left.rmap[(O, O)](o => (o, o))
      .andThen(Apparatus.parallel(Apparatus.identity[F, O], right))
      .rmap[O](_._1)

  /** Renders this network as a Mermaid `graph TD` diagram string. */
  def mermaid(using Monad[F]): String = Mermaid.print(left)
}

extension [F[_], N[_], A, B](left: Apparatus[F, A, N[B]]) {

  /** Closes a feedback loop: elements emitted by `right` are re-fed into `left` until quiescent. */
  def feedback(right: Apparatus[F, B, N[A]])(using foldN: Foldable[N], monoidNB: Monoid[N[B]], monoidNA: Monoid[N[A]]): Apparatus[F, A, N[B]] =
    Apparatus.feedback(left, right)

  /** Like [[feedback]] but the entry point accepts a collection `N[A]` instead of a single `A`. */
  def feedbackMany(right: Apparatus[F, B, N[A]])(using foldN: Foldable[N], monoidNB: Monoid[N[B]], monoidNA: Monoid[N[A]]): Apparatus[F, N[A], N[B]] =
    Apparatus.feedbackMany(left, right)
}
