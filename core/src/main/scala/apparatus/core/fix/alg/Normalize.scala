package apparatus.core.fix.alg

import apparatus.core.Apparatus
import apparatus.core.fix.{ApparatusF, HFix2}
import apparatus.core.machines.{AggregateEntry, OpenMealy}
import cats.{Foldable, Monad, Monoid}
import cats.data.State
import cats.effect.kernel.Ref
import cats.implicits.*

/** Maps aggregate names to their [[AggregateEntry]] factories after normalisation.
  *
  * Built by [[normalize]] and consumed by `alg.compile` to wire up per-UUID routing at
  * network compilation time.
  */
type NormalizedRegistry[F[_]] = Map[String, AggregateEntry[F]]

/** Internal accumulator threaded through the [[go]] state traversal.
  *
  * Tracks:
  *   - the `NormalizedRegistry` of deduplicated aggregate entries
  *   - the map of auto-generated IDs to [[OpenMealy]] machines (converted from `OpenMachine` nodes)
  *   - a monotone counter used to generate unique IDs for `OpenMachine` nodes
  */
private type NormalizeState[F[_]] = (NormalizedRegistry[F], Map[String, OpenMealy[F, ?, ?]], Int)

/** Traverses `apparatus`, deduplicates `AggregateMachine` nodes into a flat registry,
  * and assigns stable IDs to `OpenMachine` nodes so they can be backed by a shared `Ref`.
  *
  * Returns a triple of:
  *   - the aggregate registry (name → [[AggregateEntry]])
  *   - the open-machine map (auto-id → [[OpenMealy]])
  *   - the normalised tree with `AggregateMachine`/`OpenMachine` replaced by `Ref` nodes
  */
def normalize[F[_]: Monad, I, O](apparatus: Apparatus[F, I, O])
  : (NormalizedRegistry[F], Map[String, OpenMealy[F, ?, ?]], Apparatus[F, I, O]) =
  go(apparatus).run((Map.empty, Map.empty, 0)).value match
    case ((registry, openMachines, _), tree) => (registry, openMachines, tree)

/** Recursive worker for [[normalize]].  Processes one node at a time, threading the
  * accumulator via `State`.
  */
private def go[F[_]: Monad, I, O](apparatus: Apparatus[F, I, O]): State[NormalizeState[F], Apparatus[F, I, O]] =
  apparatus.unfix match {

    case ApparatusF.AggregateMachine(name, entry) =>
      State.get[NormalizeState[F]].flatMap { case (reg, om, n) =>
        if reg.contains(name) then
          State.pure(HFix2(ApparatusF.Ref(name)))
        else
          State.modify[NormalizeState[F]] { case (r, om, n) => (r + (name -> entry), om, n) }
            .as(HFix2(ApparatusF.Ref(name)))
      }

    case ApparatusF.ClosedMachine(machine) =>
      State.pure(HFix2(ApparatusF.ClosedMachine(machine)))

    case ApparatusF.OpenMachine(machine) =>
      for {
        s <- State.get[NormalizeState[F]]
        (_, _, counter) = s
        id = s"__open_$counter"
        _ <- State.modify[NormalizeState[F]] { case (reg, om, n) =>
          (reg, om + (id -> machine.asInstanceOf[OpenMealy[F, ?, ?]]), n + 1)
        }
      } yield HFix2(ApparatusF.Ref(id))

    case ApparatusF.Ref(networkId) =>
      State.pure(HFix2(ApparatusF.Ref(networkId)))

    case ApparatusF.Sequential(left, right) =>
      (go(left), go(right)).mapN((l, r) => HFix2(ApparatusF.Sequential(l, r)))

    case ApparatusF.Parallel(left, right) =>
      (go(left), go(right)).mapN((l, r) => HFix2(ApparatusF.Parallel(l, r)))

    case ApparatusF.Alternative(left, right) =>
      (go(left), go(right)).mapN((l, r) => HFix2(ApparatusF.Alternative(l, r)))

    case ApparatusF.Feedback(left, right, foldN, monoidNB, monoidNA) =>
      goFeedback(left, right, foldN, monoidNB, monoidNA)
        .asInstanceOf[State[NormalizeState[F], Apparatus[F, I, O]]]

    case ApparatusF.FeedbackMany(left, right, foldN, monoidNB, monoidNA) =>
      goFeedbackMany(left, right, foldN, monoidNB, monoidNA)
        .asInstanceOf[State[NormalizeState[F], Apparatus[F, I, O]]]

    case ApparatusF.LmapOrEmpty(inner, pf, mb) =>
      go(inner).map(i => HFix2(ApparatusF.LmapOrEmpty(i, pf, mb)))

    case ApparatusF.Merged(left, right, mb) =>
      (go(left), go(right)).mapN((l, r) => HFix2(ApparatusF.Merged(l, r, mb)))

    case ApparatusF.Labeled(inner, name) =>
      go(inner).map(i => HFix2(ApparatusF.Labeled(i, name)))
  }

/** Normalises the two branches of a [[ApparatusF.Feedback]] node. */
private def goFeedback[F[_]: Monad, A, B, N[_]](
  left:     Apparatus[F, A, N[B]],
  right:    Apparatus[F, B, N[A]],
  foldN:    Foldable[N],
  monoidNB: Monoid[N[B]],
  monoidNA: Monoid[N[A]]
): State[NormalizeState[F], Apparatus[F, A, N[B]]] =
  (go(left), go(right)).mapN((l, r) => HFix2(ApparatusF.Feedback(l, r, foldN, monoidNB, monoidNA)))

/** Normalises the two branches of a [[ApparatusF.FeedbackMany]] node. */
private def goFeedbackMany[F[_]: Monad, A, B, N[_]](
  left:     Apparatus[F, A, N[B]],
  right:    Apparatus[F, B, N[A]],
  foldN:    Foldable[N],
  monoidNB: Monoid[N[B]],
  monoidNA: Monoid[N[A]]
): State[NormalizeState[F], Apparatus[F, N[A], N[B]]] =
  (go(left), go(right)).mapN((l, r) => HFix2(ApparatusF.FeedbackMany(l, r, foldN, monoidNB, monoidNA)))
