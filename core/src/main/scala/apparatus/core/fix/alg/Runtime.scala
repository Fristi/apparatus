package apparatus.core.fix.alg

import apparatus.core.{BaseMachineT, Decider, DeciderMaterializer}
import apparatus.core.fix.{cata2, Apparatus, ApparatusF, HAlgebra2, given}
import cats.{Foldable, Monad, Monoid}
import cats.implicits.*
import cats.data.{Kleisli, StateT}

type MaterializedRegistry[F[_]] = Map[String, BaseMachineT[F, ?, ?]]
type CompiledNetwork[F[_], I, O] = Kleisli[[z] =>> StateT[F, MaterializedRegistry[F], z], I, O]

private def evalAlg[F[_] : Monad]: HAlgebra2[ApparatusF, [I, O] =>> CompiledNetwork[F, I, O]] =
  [I, O] => (node: ApparatusF[[x, y] =>> CompiledNetwork[F, x, y], I, O]) => node match {
    case ApparatusF.DeciderNode(networkId, _, _) =>
      Kleisli(x => StateT.liftF(???.asInstanceOf[F[O]]))

    case ApparatusF.Sequential(left, right) =>
      left.andThen(right)

    case ApparatusF.Parallel(left, right) =>
      Kleisli { case (a, c) => (left.run(a), right.run(c)).mapN((b, d) => (b, d)) }

    case ApparatusF.Alternative(left, right) =>
      Kleisli {
        case Left(a)  => left.run(a).map(Left(_))
        case Right(c) => right.run(c).map(Right(_))
      }

    case ApparatusF.Feedback(left, right, foldN, monoidNB, monoidNA) =>
      feedbackLoop(left, right, foldN, monoidNB)

    case ApparatusF.FeedbackMany(left, right, foldN, monoidNB, monoidNA) =>
      feedbackManyLoop(left, right, foldN, monoidNB)

    case ApparatusF.Labeled(inner, _) =>
      inner

    case ApparatusF.DeciderRef(networkId) =>
      Kleisli { input =>
        StateT { registry =>
          val machine = registry(networkId).asInstanceOf[BaseMachineT[F, I, O]]
          machine.advance(input).map { case (o, updated) =>
            (registry + (networkId -> updated.asInstanceOf[BaseMachineT[F, ?, ?]]), o)
          }
        }
      }
  }

def compile[F[_] : Monad, I, O](materializer: DeciderMaterializer[F])(apparatus: Apparatus[I, O]): F[CompiledNetwork[F, I, O]] = {
  val (normalizedRegistry, normalized) = normalize(apparatus)

  materializeNormalizedRegistryMap(normalizedRegistry, materializer)
    .map { initialRegistry =>
      val rewritten: CompiledNetwork[F, I, O] = cata2(evalAlg[F])(normalized)
      Kleisli { (i: I) =>
        StateT { registry =>
          rewritten.run(i).run(registry)
        }
      }
    }
}

private def materializeNormalizedRegistryMap[F[_] : Monad](entries: NormalizedRegistry, m: DeciderMaterializer[F]): F[MaterializedRegistry[F]] =
  entries
    .toList
    .traverse { case (id, entry) => entry.materialize(m).map(id -> _) }
    .map(_.toMap)

private def feedbackLoop[F[_] : Monad, A, B, N[_]](
                                                    left:     CompiledNetwork[F, A, N[B]],
                                                    right:    CompiledNetwork[F, N[B], A],
                                                    foldN:    Foldable[N],
                                                    monoidNB: Monoid[N[B]]
): CompiledNetwork[F, A, N[B]] =
  Kleisli { a =>
    def loop(current: A, acc: N[B]): StateT[F, MaterializedRegistry[F], N[B]] =
      left.run(current).flatMap { nb =>
        if foldN.isEmpty(nb) then acc.pure
        else right.run(nb).flatMap(a2 => loop(a2, monoidNB.combine(acc, nb)))
      }
    loop(a, monoidNB.empty)
  }

private def feedbackManyLoop[F[_] : Monad, A, B, N[_]](
                                                        left:     CompiledNetwork[F, N[A], N[B]],
                                                        right:    CompiledNetwork[F, N[B], N[A]],
                                                        foldN:    Foldable[N],
                                                        monoidNB: Monoid[N[B]]
): CompiledNetwork[F, N[A], N[B]] =
  Kleisli { nas =>
    def loop(pending: N[A], acc: N[B]): StateT[F, MaterializedRegistry[F], N[B]] =
      if foldN.isEmpty(pending) then acc.pure
      else
        left.run(pending).flatMap { nb =>
          right.run(nb).flatMap { na =>
            loop(na, monoidNB.combine(acc, nb))
          }
        }
    loop(nas, monoidNB.empty)
  }
