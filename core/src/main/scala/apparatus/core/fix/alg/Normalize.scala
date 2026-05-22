package apparatus.core.fix.alg

import apparatus.core.{BaseMachineT, DeciderMaterializer}
import apparatus.core.fix.{Apparatus, ApparatusF, HFix2}
import cats.{Foldable, Monad, Monoid}
import cats.data.{State, StateT}
import cats.implicits.*

type NormalizedRegistry[F[_]] = Map[String, DeciderEntry[F]]

/**
 * Traverses `apparatus`, collects every `DeciderMachine`/`BaseMachine` node into a registry,
 * and returns a rewritten tree where each is replaced by a `Ref`.
 */
def normalize[F[_] : Monad, I, O](apparatus: Apparatus[F, I, O]): (NormalizedRegistry[F], Apparatus[F, I, O]) =
  go(apparatus).run(Map.empty).value

private def go[F[_] : Monad, I, O](apparatus: Apparatus[F, I, O]): State[NormalizedRegistry[F], Apparatus[F, I, O]] =
  apparatus.unfix match {

    case ApparatusF.DeciderMachine(networkId, decider, schema) =>
      val entry = new DeciderEntry[F]:
        def materialize(m: DeciderMaterializer[F]): F[BaseMachineT[F, ?, ?]] =
          m.materialize(decider, networkId)(using schema).asInstanceOf[F[BaseMachineT[F, ?, ?]]]
      State.modify[NormalizedRegistry[F]](_ + (networkId -> entry))
        .as(HFix2(ApparatusF.Ref(networkId)))

    case ApparatusF.BaseMachine(machine) => // machine: BaseMachineT[F, I, O] — effect type known, no cast
      val id = java.util.UUID.randomUUID().toString
      val entry = new DeciderEntry[F]:
        def materialize(m: DeciderMaterializer[F]): F[BaseMachineT[F, ?, ?]] =
          machine.pure[F].asInstanceOf[F[BaseMachineT[F, ?, ?]]]
      State.modify[NormalizedRegistry[F]](_ + (id -> entry))
        .as(HFix2(ApparatusF.Ref(id)))

    case ApparatusF.Ref(networkId) =>
      StateT.pure(HFix2(ApparatusF.Ref(networkId)))

    case ApparatusF.Sequential(left, right) =>
      (go(left), go(right)).mapN((l, r) => HFix2(ApparatusF.Sequential(l, r)))

    case ApparatusF.Parallel(left, right) =>
      (go(left), go(right)).mapN((l, r) => HFix2(ApparatusF.Parallel(l, r)))

    case ApparatusF.Alternative(left, right) =>
      (go(left), go(right)).mapN((l, r) => HFix2(ApparatusF.Alternative(l, r)))

    case ApparatusF.Feedback(left, right, foldN, monoidNB, monoidNA) =>
      goFeedback(left, right, foldN, monoidNB, monoidNA)
        .asInstanceOf[State[NormalizedRegistry[F], Apparatus[F, I, O]]]

    case ApparatusF.FeedbackMany(left, right, foldN, monoidNB, monoidNA) =>
      goFeedbackMany(left, right, foldN, monoidNB, monoidNA)
        .asInstanceOf[State[NormalizedRegistry[F], Apparatus[F, I, O]]]

    case ApparatusF.LmapOrEmpty(inner, pf, mb) =>
      go(inner).map(i => HFix2(ApparatusF.LmapOrEmpty(i, pf, mb)))

    case ApparatusF.Merged(left, right, mb) =>
      (go(left), go(right)).mapN((l, r) => HFix2(ApparatusF.Merged(l, r, mb)))

    case ApparatusF.Labeled(inner, name) =>
      go(inner).map(i => HFix2(ApparatusF.Labeled(i, name)))
  }

private def goFeedback[F[_] : Monad, A, B, N[_]](
  left:     Apparatus[F, A, N[B]],
  right:    Apparatus[F, B, N[A]],
  foldN:    Foldable[N],
  monoidNB: Monoid[N[B]],
  monoidNA: Monoid[N[A]]
): State[NormalizedRegistry[F], Apparatus[F, A, N[B]]] =
  (go(left), go(right)).mapN((l, r) => HFix2(ApparatusF.Feedback(l, r, foldN, monoidNB, monoidNA)))

private def goFeedbackMany[F[_] : Monad, A, B, N[_]](
  left:     Apparatus[F, A, N[B]],
  right:    Apparatus[F, B, N[A]],
  foldN:    Foldable[N],
  monoidNB: Monoid[N[B]],
  monoidNA: Monoid[N[A]]
): State[NormalizedRegistry[F], Apparatus[F, N[A], N[B]]] =
  (go(left), go(right)).mapN((l, r) => HFix2(ApparatusF.FeedbackMany(l, r, foldN, monoidNB, monoidNA)))
