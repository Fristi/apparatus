package apparatus.core.fix.alg

import apparatus.core.Apparatus
import apparatus.core.fix.{ApparatusF, HAlgebra2, cata2}
import apparatus.core.machines.{ClosedMealy, DeciderMaterializer, MealyMachine, OpenMealy}
import cats.data.{Kleisli, StateT}
import cats.implicits.*
import cats.{Foldable, Monad, Monoid}

/** Runtime state threaded through a compiled network.
 *  - `deciders` — materialized machines looked up by Ref nodes
 *  - `states`   — current state for Open machines, keyed by network ID
 */
final case class CompiledNetworkState[F[_]](
  deciders: Map[String, MealyMachine[F, ?, ?]],
  states:   Map[String, Any]
):
  def withState(id: String, state: Any): CompiledNetworkState[F] =
    copy(states = states + (id -> state))

/** A compiled apparatus: a Kleisli arrow in the StateT monad over CompiledNetworkState.
 *  StateT threads Ref-node state across all steps without re-compiling. */
type CompiledNetwork[F[_], I, O] = Kleisli[[x] =>> StateT[F, CompiledNetworkState[F], x], I, O]

private def evalAlg[F[_]: Monad]: HAlgebra2[[G[_, _], I, O] =>> ApparatusF[G, F, I, O], [I, O] =>> CompiledNetwork[F, I, O]] =
  [I, O] => (node: ApparatusF[[x, y] =>> CompiledNetwork[F, x, y], F, I, O]) => node match {

    case ApparatusF.DeciderMachine(_, _, _) =>
      sys.error("DeciderMachine reached evalAlg — normalize must be called first")

    case ApparatusF.OpenMachine(machine) =>
      // OpenMachine here is always stateless (lmap/rmap/identity use ClosedMealy now).
      // State is not threaded — always run from initialState.
      Kleisli(i => StateT.liftF(machine.action(machine.initialState, i).map(_._1)))

    case ApparatusF.ClosedMachine(machine) =>
      Kleisli(i => StateT.liftF(machine.action(i)))

    case ApparatusF.Ref(networkId) =>
      Kleisli { input =>
        for {
          s      <- StateT.get[F, CompiledNetworkState[F]]
          output <- s.deciders(networkId) match {
            case MealyMachine.Open(m) =>
              val typed = m.asInstanceOf[OpenMealy[F, I, O]]
              val state = s.states.getOrElse(networkId, typed.initialState).asInstanceOf[typed.State]
              StateT.liftF(typed.action(state, input)).flatMap { case (o, ns) =>
                StateT.modify[F, CompiledNetworkState[F]](_.withState(networkId, ns)).as(o)
              }
            case MealyMachine.Closed(m) =>
              StateT.liftF(m.asInstanceOf[ClosedMealy[F, I, O]].action(input))
          }
        } yield output
      }

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
          case None    => StateT.pure(mb.empty)
      }

    case ApparatusF.Merged(left, right, mb) =>
      Kleisli { a => (left.run(a), right.run(a)).mapN(mb.combine) }

    case ApparatusF.Labeled(inner, _) =>
      inner
  }

def compile[F[_]: Monad, I, O](materializer: DeciderMaterializer[F])(apparatus: Apparatus[F, I, O]): F[(CompiledNetwork[F, I, O], CompiledNetworkState[F])] = {
  val (normalizedRegistry, normalized) = normalize(apparatus)
  materializeRegistry(normalizedRegistry, materializer).map { deciders =>
    val compiled     = cata2(evalAlg[F])(normalized)
    val initialState = CompiledNetworkState(deciders, Map.empty)
    (compiled, initialState)
  }
}

private[alg] def materializeRegistry[F[_]: Monad](entries: NormalizedRegistry[F], m: DeciderMaterializer[F]): F[Map[String, MealyMachine[F, ?, ?]]] =
  entries
    .toList
    .traverse { case (id, entry) => entry.materialize(m).map(id -> _) }
    .map(_.toMap)

/** Each B in N[B] fed individually to right (B → N[A]), resulting A values re-queued until quiescent. */
private def feedbackLoop[F[_]: Monad, A, B, N[_]](
  left:     CompiledNetwork[F, A, N[B]],
  right:    CompiledNetwork[F, B, N[A]],
  foldN:    Foldable[N],
  monoidNB: Monoid[N[B]],
  monoidNA: Monoid[N[A]]
): CompiledNetwork[F, A, N[B]] =
  Kleisli { a =>
    def loop(pending: List[A], acc: N[B]): StateT[F, CompiledNetworkState[F], N[B]] =
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
private def feedbackManyLoop[F[_]: Monad, A, B, N[_]](
  left:     CompiledNetwork[F, A, N[B]],
  right:    CompiledNetwork[F, B, N[A]],
  foldN:    Foldable[N],
  monoidNB: Monoid[N[B]],
  monoidNA: Monoid[N[A]]
): CompiledNetwork[F, N[A], N[B]] =
  Kleisli { nas =>
    def loop(pending: List[A], acc: N[B]): StateT[F, CompiledNetworkState[F], N[B]] =
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
