package apparatus.core.fix

import apparatus.core
import apparatus.core.machines.{AggregateEntry, ClosedMealy, OpenMealy}
import cats.{Foldable, Monoid}

/** The base functor of the `Apparatus` fixed-point tree.
  *
  * `ApparatusF[F, Eff, I, O]` describes one layer of an `Apparatus` network in terms of a
  * generic recursive position type `F[_, _]`.  When `F` is instantiated as the `Apparatus`
  * type itself (via `HFix2`) the full recursive tree is recovered.
  *
  * Each variant corresponds to a combinator in the [[apparatus.core.Apparatus]] smart-constructor
  * API.  The [[hfmap]] method lifts a natural transformation `F ~> G` over one layer, making
  * `ApparatusF` an [[HFunctor2]] (provided by the `given` instance at the bottom of this file).
  *
  * @tparam F   the recursive position type (e.g. `Apparatus[Eff, *, *]`)
  * @tparam Eff the effect type that leaf machines operate in
  * @tparam I   the input type of this node
  * @tparam O   the output type of this node
  */
sealed trait ApparatusF[F[_, _], Eff[_], I, O]:
  /** Apply a natural transformation `nt: F ~> G` to all recursive positions in this node. */
  def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, Eff, I, O]

object ApparatusF:

  /** A reference node used after normalisation to replace duplicate sub-networks.
    *
    * The `networkId` key maps to an entry in the `NormalizedRegistry` produced by
    * [[apparatus.core.fix.alg.normalize]].
    */
  final case class Ref[F[_, _], Eff[_], I, O](networkId: String) extends ApparatusF[F, Eff, I, O]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, Eff, I, O] = Ref(networkId)

  /** Leaf node wrapping a stateful [[OpenMealy]] whose state is managed externally (via a `Ref`). */
  final case class OpenMachine[F[_, _], Eff[_], I, O](machine: OpenMealy[Eff, I, O]) extends ApparatusF[F, Eff, I, O]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, Eff, I, O] = OpenMachine(machine)

  /** Leaf node wrapping a self-contained [[ClosedMealy]] that manages its own state internally. */
  final case class ClosedMachine[F[_, _], Eff[_], I, O](machine: ClosedMealy[Eff, I, O]) extends ApparatusF[F, Eff, I, O]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, Eff, I, O] = ClosedMachine(machine)

  /** Leaf node for a per-UUID aggregate machine that lazily materialises deciders.
    *
    * @param name  aggregate name used for routing and Mermaid labels
    * @param entry factory that compiles the aggregate router into a [[ClosedMealy]]
    */
  final case class AggregateMachine[F[_, _], Eff[_], I, O](
    name:  String,
    entry: AggregateEntry[Eff]
  ) extends ApparatusF[F, Eff, I, O]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, Eff, I, O] =
      AggregateMachine(name, entry)

  /** Sequential composition: output of `left` feeds input of `right`. */
  final case class Sequential[F[_, _], Eff[_], A, B, C](
    left:  F[A, B],
    right: F[B, C]
  ) extends ApparatusF[F, Eff, A, C]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, Eff, A, C] =
      Sequential(nt(left), nt(right))

  /** Parallel composition: `left` and `right` run side-by-side on paired inputs and outputs. */
  final case class Parallel[F[_, _], Eff[_], A, B, C, D](
    left:  F[A, B],
    right: F[C, D]
  ) extends ApparatusF[F, Eff, (A, C), (B, D)]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, Eff, (A, C), (B, D)] =
      Parallel(nt(left), nt(right))

  /** `Either`-based routing: `Left` inputs go to `left`, `Right` inputs go to `right`. */
  final case class Alternative[F[_, _], Eff[_], A, B, C, D](
    left:  F[A, B],
    right: F[C, D]
  ) extends ApparatusF[F, Eff, Either[A, C], Either[B, D]]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, Eff, Either[A, C], Either[B, D]] =
      Alternative(nt(left), nt(right))

  /** Feedback loop: left emits N[B], each B fed into right (B → N[A]), re-queued until quiescent. */
  final case class Feedback[F[_, _], Eff[_], A, B, N[_]](
    left:     F[A, N[B]],
    right:    F[B, N[A]],
    foldN:    Foldable[N],
    monoidNB: Monoid[N[B]],
    monoidNA: Monoid[N[A]]
  ) extends ApparatusF[F, Eff, A, N[B]]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, Eff, A, N[B]] =
      Feedback(nt(left), nt(right), foldN, monoidNB, monoidNA)

  /** Like Feedback but entry point accepts N[A] instead of a single A. */
  final case class FeedbackMany[F[_, _], Eff[_], A, B, N[_]](
    left:     F[A, N[B]],
    right:    F[B, N[A]],
    foldN:    Foldable[N],
    monoidNB: Monoid[N[B]],
    monoidNA: Monoid[N[A]]
  ) extends ApparatusF[F, Eff, N[A], N[B]]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, Eff, N[A], N[B]] =
      FeedbackMany(nt(left), nt(right), foldN, monoidNB, monoidNA)

  /** Partial-function input filter: applies `pf` to map `C → A`; returns `Monoid[B].empty` when undefined.
    *
    * @param inner the downstream machine that processes matched inputs
    * @param pf    partial function mapping the wider input type `C` to the machine's input type `A`
    * @param mb    monoid for `B` supplying the empty value for unmatched inputs
    */
  final case class LmapOrEmpty[F[_, _], Eff[_], A, B, C](
    inner: F[A, B],
    pf:    PartialFunction[C, A],
    mb:    Monoid[B]
  ) extends ApparatusF[F, Eff, C, B]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, Eff, C, B] =
      LmapOrEmpty(nt(inner), pf, mb)

  /** Fan-out combinator: broadcasts the same input to `left` and `right`, combining outputs with `Monoid[B]`. */
  final case class Merged[F[_, _], Eff[_], A, B](
    left:  F[A, B],
    right: F[A, B],
    mb:    Monoid[B]
  ) extends ApparatusF[F, Eff, A, B]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, Eff, A, B] =
      Merged(nt(left), nt(right), mb)

  /** Decorates `inner` with a human-readable `name` for Mermaid rendering and diagnostics. */
  final case class Labeled[F[_, _], Eff[_], I, O](
    inner: F[I, O],
    name:  String
  ) extends ApparatusF[F, Eff, I, O]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, Eff, I, O] =
      Labeled(nt(inner), name)

  /** `HFunctor2` instance: delegates to each variant's own [[hfmap]] implementation. */
  given [Eff[_]]: HFunctor2[[F[_, _], I, O] =>> ApparatusF[F, Eff, I, O]] with
    override def hfmap[F[_, _], G[_, _], I, O](nt: FunctionK2[F, G])(hfio: ApparatusF[F, Eff, I, O]): ApparatusF[G, Eff, I, O] =
      hfio.hfmap(nt)