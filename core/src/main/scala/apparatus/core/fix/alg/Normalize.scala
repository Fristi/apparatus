package apparatus.core.fix.alg

import apparatus.core.{BaseMachineT, Decider, DeciderMaterializer}
import apparatus.core.fix.{Apparatus, ApparatusF, HFix2}
import cats.{Foldable, Monad, Monoid}
import cats.data.{State, StateT}
import cats.implicits.*

type NormalizedRegistry = Map[String, DeciderEntry]

/**
 * Traverses `apparatus`, collects every `DeciderNode` into a `Registry`,
 * and returns a rewritten tree where every `DeciderNode` is replaced by a `DeciderRef`.
 *
 * The two passes (collect + rewrite) are fused into a single `State` traversal.
 */
def normalize[F[_] : Monad, I, O](apparatus: Apparatus[I, O]): (NormalizedRegistry, Apparatus[I, O]) =
  go(apparatus).run(Map.empty).value

private def go[F[_] : Monad, I, O](apparatus: Apparatus[I, O]): State[NormalizedRegistry, Apparatus[I, O]] =
  apparatus.unfix match {

    case ApparatusF.DeciderNode(networkId, decider, schema) =>
      val entry = new DeciderEntry {
        override def materialize[G[_] : Monad](m: DeciderMaterializer[G]): G[BaseMachineT[G, ?, ?]] =
          m.materialize(decider, networkId)(using schema).asInstanceOf[G[BaseMachineT[G, ?, ?]]]
      }
      
      State.modify[NormalizedRegistry](_ + (networkId -> entry))
        .as(HFix2(ApparatusF.DeciderRef(networkId)))

    case ApparatusF.DeciderRef(networkId) =>
      StateT.pure(HFix2(ApparatusF.DeciderRef(networkId)))

    case ApparatusF.Sequential(left, right) =>
      (go(left), go(right)).mapN((l, r) => HFix2(ApparatusF.Sequential(l, r)))

    case ApparatusF.Parallel(left, right) =>
      (go(left), go(right)).mapN((l, r) => HFix2(ApparatusF.Parallel(l, r)))

    case ApparatusF.Alternative(left, right) =>
      (go(left), go(right)).mapN((l, r) => HFix2(ApparatusF.Alternative(l, r)))

    case ApparatusF.Feedback(left, right, foldN, monoidNB, monoidNA) =>
      goFeedback(left, right, foldN, monoidNB, monoidNA)
        .asInstanceOf[State[NormalizedRegistry, Apparatus[I, O]]]

    case ApparatusF.FeedbackMany(left, right, foldN, monoidNB, monoidNA) =>
      goFeedbackMany(left, right, foldN, monoidNB, monoidNA)
        .asInstanceOf[State[NormalizedRegistry, Apparatus[I, O]]]

    case ApparatusF.LmapOrEmpty(inner, pf, mb) =>
      go(inner).map(i => HFix2(ApparatusF.LmapOrEmpty(i, pf, mb)))

    case ApparatusF.Merged(left, right, mb) =>
      (go(left), go(right)).mapN((l, r) => HFix2(ApparatusF.Merged(l, r, mb)))

    case ApparatusF.Labeled(inner, name) =>
      go(inner).map(i => HFix2(ApparatusF.Labeled(i, name)))
  }

private def goFeedback[F[_] : Monad, A, B, N[_]](
  left:     Apparatus[A, N[B]],
  right:    Apparatus[N[B], A],
  foldN:    Foldable[N],
  monoidNB: Monoid[N[B]],
  monoidNA: Monoid[N[A]]
): State[NormalizedRegistry, Apparatus[A, N[B]]] =
  (go(left), go(right)).mapN((l, r) => HFix2(ApparatusF.Feedback(l, r, foldN, monoidNB, monoidNA)))

private def goFeedbackMany[F[_] : Monad, A, B, N[_]](
  left:     Apparatus[N[A], N[B]],
  right:    Apparatus[N[B], N[A]],
  foldN:    Foldable[N],
  monoidNB: Monoid[N[B]],
  monoidNA: Monoid[N[A]]
): State[NormalizedRegistry, Apparatus[N[A], N[B]]] =
  (go(left), go(right)).mapN((l, r) => HFix2(ApparatusF.FeedbackMany(l, r, foldN, monoidNB, monoidNA)))
