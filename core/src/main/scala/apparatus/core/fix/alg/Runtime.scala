package apparatus.core.fix.alg

import apparatus.core.Apparatus
import apparatus.core.fix.{ApparatusF, HAlgebra2, cata2}
import apparatus.core.machines.{ClosedMealy, DeciderMaterializer, OpenMealy}
import cats.effect.kernel.Ref
import cats.implicits.*
import cats.{Foldable, Monad, Monoid, ~>}

/** A fully compiled network: each input produces an output in effect F.
 *  All state (open mealy and decider) lives in Ref cells allocated at compile time. */
final case class CompiledNetwork[F[_], I, O](run: I => F[O]):
  def mapK[G[_]](nt: F ~> G): CompiledNetwork[G, I, O] = CompiledNetwork(i => nt(run(i)))

private type ApparatusK[F[_]] = [G[_, _], I, O] =>> ApparatusF[G, F, I, O]
private type NetworkK[F[_]] = [I, O] =>> CompiledNetwork[F, I, O]

/** Seal an OpenMealy into a ClosedMealy by allocating a Ref for its state. */
private def sealWithRef[F[_]: Monad, I, O](m: OpenMealy[F, I, O])(using Ref.Make[F]): F[ClosedMealy[F, I, O]] =
  Ref[F].of(m.initialState).map { ref =>
    new ClosedMealy[F, I, O]:
      def action(input: I): F[O] =
        ref.get.flatMap { s =>
          m.action(s, input).flatMap { case (o, ns) =>
            ref.set(ns).as(o)
          }
        }
  }

/** Pure algebra: all machines are pre-sealed ClosedMealy, no state threading needed. */
private def evalAlg[F[_]: Monad](machines: Map[String, ClosedMealy[F, ?, ?]]): HAlgebra2[ApparatusK[F], NetworkK[F]] =
  [I, O] => (node: ApparatusF[[x, y] =>> CompiledNetwork[F, x, y], F, I, O]) => node match {
    case ApparatusF.AggregateMachine(_, _) => sys.error("AggregateMachine reached evalAlg — normalize must be called first")
    case ApparatusF.OpenMachine(_)               => sys.error("OpenMachine reached evalAlg — normalize must be called first")
    case ApparatusF.ClosedMachine(machine)      => CompiledNetwork(machine.action)
    case ApparatusF.Ref(networkId)              => CompiledNetwork(machines(networkId).asInstanceOf[ClosedMealy[F, I, O]].action)
    case ApparatusF.Sequential(left, right)     => CompiledNetwork(i => left.run(i).flatMap(right.run))
    case ApparatusF.Parallel(left, right)       => CompiledNetwork(i => (left.run(i._1), right.run(i._2)).mapN((_, _)))
    case ApparatusF.Alternative(left, right)    => CompiledNetwork {
                                                     case Left(a)  => left.run(a).map(Left(_))
                                                     case Right(c) => right.run(c).map(Right(_))
                                                   }
    case ApparatusF.LmapOrEmpty(inner, pf, mb)  => CompiledNetwork(i => pf.lift(i).fold(mb.empty.pure[F])(inner.run))
    case ApparatusF.Merged(left, right, mb)     => CompiledNetwork(i => (left.run(i), right.run(i)).mapN(mb.combine))
    case ApparatusF.Labeled(inner, _)           => inner
    case ApparatusF.Feedback(left, right, foldN, monoidNB, monoidNA) =>
      feedbackLoop(left, right, foldN, monoidNB, monoidNA)
    case ApparatusF.FeedbackMany(left, right, foldN, monoidNB, monoidNA) =>
      feedbackManyLoop(left, right, foldN, monoidNB, monoidNA)
  }

/** Two-phase compile:
 *  Phase 1 (pure): normalize — extract AggregateMachine and OpenMachine nodes into registries.
 *  Phase 2 (effectful): compile aggregate routers + seal all Open machines with Ref cells.
 *  Phase 3 (pure): fold normalized tree with sealed machine map. */
def compile[F[_]: Monad, I, O](materializer: DeciderMaterializer[F])(apparatus: Apparatus[F, I, O])(using Ref.Make[F]): F[CompiledNetwork[F, I, O]] = {
  val (aggregateRegistry, openMachineRegistry, normalized) = normalize(apparatus)

  val routingMachines: F[Map[String, ClosedMealy[F, ?, ?]]] =
    compileRouters(aggregateRegistry, materializer)

  val sealedOpenMachines: F[Map[String, ClosedMealy[F, ?, ?]]] =
    openMachineRegistry.toList.traverse { case (id, m) =>
      sealWithRef(m.asInstanceOf[OpenMealy[F, ?, ?]]).map(id -> _)
    }.map(_.toMap)

  (routingMachines, sealedOpenMachines).mapN { (routers, openMachines) =>
    cata2(evalAlg[F](routers ++ openMachines))(normalized)
  }
}

private[alg] def compileRouters[F[_]: Monad](entries: NormalizedRegistry[F], m: DeciderMaterializer[F])(using Ref.Make[F]): F[Map[String, ClosedMealy[F, ?, ?]]] =
  entries
    .toList
    .traverse { case (key, entry) => entry.compileRouter(m).map(key -> _) }
    .map(_.toMap)

private def feedbackLoop[F[_]: Monad, A, B, N[_]](
  left:     CompiledNetwork[F, A, N[B]],
  right:    CompiledNetwork[F, B, N[A]],
  foldN:    Foldable[N],
  monoidNB: Monoid[N[B]],
  monoidNA: Monoid[N[A]]
): CompiledNetwork[F, A, N[B]] =
  CompiledNetwork(a => {
    def loop(pending: List[A], acc: N[B]): F[N[B]] = pending match
      case Nil => acc.pure[F]
      case head :: tail =>
        left.run(head).flatMap { nb =>
          foldN.toList(nb).foldLeftM(monoidNA.empty) { (naAcc, b) =>
            right.run(b).map(na => monoidNA.combine(naAcc, na))
          }.flatMap { na =>
            loop(foldN.toList(na) ++ tail, monoidNB.combine(acc, nb))
          }
        }
    loop(List(a), monoidNB.empty)
  })

private def feedbackManyLoop[F[_]: Monad, A, B, N[_]](
  left:     CompiledNetwork[F, A, N[B]],
  right:    CompiledNetwork[F, B, N[A]],
  foldN:    Foldable[N],
  monoidNB: Monoid[N[B]],
  monoidNA: Monoid[N[A]]
): CompiledNetwork[F, N[A], N[B]] =
  CompiledNetwork(nas => {
    def loop(pending: List[A], acc: N[B]): F[N[B]] = pending match
      case Nil => acc.pure[F]
      case head :: tail =>
        left.run(head).flatMap { nb =>
          foldN.toList(nb).foldLeftM(monoidNA.empty) { (naAcc, b) =>
            right.run(b).map(na => monoidNA.combine(naAcc, na))
          }.flatMap { na =>
            loop(foldN.toList(na) ++ tail, monoidNB.combine(acc, nb))
          }
        }
    loop(foldN.toList(nas), monoidNB.empty)
  })
