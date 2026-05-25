package apparatus.core.fix.alg

import apparatus.core.Apparatus
import apparatus.core.fix.alg.NormalizedRegistry
import apparatus.core.fix.{ApparatusF, HAlgebra2, cata2}
import apparatus.core.machines.{DeciderMaterializer, OpenMealy}
import cats.data.{Kleisli, StateT}
import cats.implicits.*
import cats.{Foldable, Monad, Monoid}

final case class NetworkState[F[_]](
                                     deciders: Map[String, OpenMealy[F, ?, ?]],
                                     states: Map[String, Any]
                                   ) {
  def withUpdatedState(id: String, state: Any): NetworkState[F] =
    copy(states = states + (id -> state))
}

type CompiledNetwork[F[_], I, O] = Kleisli[F, I, (O, Apparatus[F, I, O])]

private def evalAlg[F[_] : Monad]: HAlgebra2[[G[_, _], I, O] =>> ApparatusF[G, F, I, O], [I, O] =>> CompiledNetwork[F, I, O]] =
  [I, O] => (node: ApparatusF[[x, y] =>> CompiledNetwork[F, x, y], F, I, O]) => node match {
    case ApparatusF.DeciderMachine(_, _, _) =>
      sys.error("DeciderMachine reached evalAlg — normalize must be called first")

    case ApparatusF.OpenMachine(machine) =>
      sys.error("BaseMachine reached evalAlg — normalize must be called first")

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
      feedbackLoop(left, right, foldN, monoidNB, monoidNA)

    case ApparatusF.FeedbackMany(left, right, foldN, monoidNB, monoidNA) =>
      feedbackManyLoop(left, right, foldN, monoidNB, monoidNA)

    case ApparatusF.LmapOrEmpty(inner, pf, mb) =>
      Kleisli { c =>
        pf.lift(c) match
          case Some(a) => inner.run(a)
          case None    => mb.empty.pure[[z] =>> StateT[F, NetworkState[F], z]]
      }

    case ApparatusF.Merged(left, right, mb) =>
      Kleisli { a => (left.run(a), right.run(a)).mapN(mb.combine) }

    case ApparatusF.Labeled(inner, _) =>
      inner

    case ApparatusF.Ref(networkId) =>
      Kleisli { input =>
        for {
          registry <- StateT.get[F, NetworkState[F]]
          machine:OpenMealy[F, I, O] = registry.deciders(networkId).asInstanceOf[OpenMealy[F, I, O]]
          state = registry.states.getOrElse(networkId, machine.initialState).asInstanceOf[machine.State]
          (o, newState) <- StateT.liftF(machine.action(state, input))
          _ <- StateT.modify[F, NetworkState[F]](_.withUpdatedState(networkId, newState))
        } yield o
      }
  }

def compile[F[_] : Monad, I, O](materializer: DeciderMaterializer[F])(apparatus: Apparatus[F, I, O]): F[(CompiledNetwork[F, I, O], NetworkState[F])] = {
  val (normalizedRegistry, normalized) = normalize(apparatus)
  materializeNormalizedRegistryMap(normalizedRegistry, materializer)
    .map { initialRegistry =>
      val rewritten: CompiledNetwork[F, I, O] = cata2(evalAlg[F])(normalized)
      val network: CompiledNetwork[F, I, O] = Kleisli { (i: I) =>
        StateT[F, NetworkState[F], O] { registry =>
          rewritten.run(i).run(registry)
        }
      }
      (network, initialRegistry)
    }
}

private[alg] def materializeNormalizedRegistryMap[F[_] : Monad](entries: NormalizedRegistry[F], m: DeciderMaterializer[F]): F[NetworkState[F]] =
  entries
    .toList
    .traverse { case (id, entry) => entry.materialize(m).map(id -> _) }
    .map(x => NetworkState(x.toMap, Map.empty))

/** Each B in N[B] fed individually to right (B → N[A]), resulting A values re-queued until quiescent. */
private def feedbackLoop[F[_] : Monad, A, B, N[_]](
                                                    left:     CompiledNetwork[F, A, N[B]],
                                                    right:    CompiledNetwork[F, B, N[A]],
                                                    foldN:    Foldable[N],
                                                    monoidNB: Monoid[N[B]],
                                                    monoidNA: Monoid[N[A]]
                                                  ): CompiledNetwork[F, A, N[B]] =
  Kleisli { a =>
    def loop(pending: List[A], acc: N[B]) =
      pending match
        case Nil => acc.pure
        case head :: tail =>
          left.run(head).flatMap { nb =>
            foldN.toList(nb).foldLeftM(monoidNA.empty) { (naAcc, b) =>
              right.run(b).map(na => monoidNA.combine(naAcc, na))
            }.flatMap { na =>
              loop(foldN.toList(na) ++ tail, monoidNB.combine(acc, nb))
            }
          }
    loop(List(a), monoidNB.empty)
  }

/** Like feedbackLoop but starts with N[A] instead of a single A. */
private def feedbackManyLoop[F[_] : Monad, A, B, N[_]](
                                                        left:     CompiledNetwork[F, A, N[B]],
                                                        right:    CompiledNetwork[F, B, N[A]],
                                                        foldN:    Foldable[N],
                                                        monoidNB: Monoid[N[B]],
                                                        monoidNA: Monoid[N[A]]
                                                      ): CompiledNetwork[F, N[A], N[B]] =
  Kleisli { nas =>
    def loop(pending: List[A], acc: N[B]) =
      pending match
        case Nil => acc.pure
        case head :: tail =>
          left.run(head).flatMap { nb =>
            foldN.toList(nb).foldLeftM(monoidNA.empty) { (naAcc, b) =>
              right.run(b).map(na => monoidNA.combine(naAcc, na))
            }.flatMap { na =>
              loop(foldN.toList(na) ++ tail, monoidNB.combine(acc, nb))
            }
          }
    loop(foldN.toList(nas), monoidNB.empty)
  }
