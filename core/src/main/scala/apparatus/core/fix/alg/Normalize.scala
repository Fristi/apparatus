package apparatus.core.fix.alg

import apparatus.core.Apparatus
import apparatus.core.fix.{ApparatusF, HFix2}
import apparatus.core.machines.{DeciderMaterializer, MealyMachine, OpenMealy}
import cats.{Foldable, Monad, Monoid}
import cats.data.State
import cats.implicits.*

type NormalizedRegistry[F[_]] = Map[String, DeciderEntry[F]]

private type NormalizeState[F[_]] = (NormalizedRegistry[F], Map[String, OpenMealy[F, ?, ?]], Int)

def normalize[F[_]: Monad, I, O](apparatus: Apparatus[F, I, O])
  : (NormalizedRegistry[F], Map[String, OpenMealy[F, ?, ?]], Apparatus[F, I, O]) =
  go(apparatus).run((Map.empty, Map.empty, 0)).value match
    case ((registry, openMachines, _), tree) => (registry, openMachines, tree)

private def go[F[_]: Monad, I, O](apparatus: Apparatus[F, I, O]): State[NormalizeState[F], Apparatus[F, I, O]] =
  apparatus.unfix match {

    case ApparatusF.DeciderMachine(networkId, decider, schema) =>
      val entry = new DeciderEntry[F]:
        def materialize(m: DeciderMaterializer[F]): F[MealyMachine[F, ?, ?]] =
          m.materialize(decider, networkId)(using schema).asInstanceOf[F[MealyMachine[F, ?, ?]]]
      State.modify[NormalizeState[F]] { case (reg, om, n) => (reg + (networkId -> entry), om, n) }
        .as(HFix2(ApparatusF.Ref(networkId)))

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

private def goFeedback[F[_]: Monad, A, B, N[_]](
  left:     Apparatus[F, A, N[B]],
  right:    Apparatus[F, B, N[A]],
  foldN:    Foldable[N],
  monoidNB: Monoid[N[B]],
  monoidNA: Monoid[N[A]]
): State[NormalizeState[F], Apparatus[F, A, N[B]]] =
  (go(left), go(right)).mapN((l, r) => HFix2(ApparatusF.Feedback(l, r, foldN, monoidNB, monoidNA)))

private def goFeedbackMany[F[_]: Monad, A, B, N[_]](
  left:     Apparatus[F, A, N[B]],
  right:    Apparatus[F, B, N[A]],
  foldN:    Foldable[N],
  monoidNB: Monoid[N[B]],
  monoidNA: Monoid[N[A]]
): State[NormalizeState[F], Apparatus[F, N[A], N[B]]] =
  (go(left), go(right)).mapN((l, r) => HFix2(ApparatusF.FeedbackMany(l, r, foldN, monoidNB, monoidNA)))
