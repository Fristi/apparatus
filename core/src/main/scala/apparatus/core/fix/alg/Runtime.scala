package apparatus.core.fix.alg

import apparatus.core.Apparatus
import apparatus.core.fix.{ApparatusF, HAlgebra2, HFix2, cata2}
import apparatus.core.machines.{ClosedMealy, DeciderMaterializer, MealyMachine, OpenMealy}
import cats.data.StateT
import cats.implicits.*
import cats.{Foldable, Monad, Monoid}

/** State threaded through a compiled network.
 *  - `deciders` — materialized machines looked up by Ref nodes
 *  - `states`   — current state for Open decider machines (Ref nodes only)
 */
final case class DeciderStates[F[_]](deciders: Map[String, MealyMachine[F, ?, ?]], states: Map[String, Any]) {
  def withState(id: String, state: Any): DeciderStates[F] =
    copy(states = states + (id -> state))
}

/** A fully stepped machine: carries the current Apparatus tree (with up-to-date
 *  OpenMachine states baked into initialState) plus a run function that produces
 *  output and the next SteppedMachine in one step. */
final case class CompiledNetwork[F[_], I, O](run: I => StateT[F, DeciderStates[F], (O, CompiledNetwork[F, I, O])])

private def evalAlg[F[_]: Monad]: HAlgebra2[[G[_, _], I, O] =>> ApparatusF[G, F, I, O], [I, O] =>> CompiledNetwork[F, I, O]] =
  [I, O] => (node: ApparatusF[[x, y] =>> CompiledNetwork[F, x, y], F, I, O]) => node match {
    case ApparatusF.DeciderMachine(_, _, _) => sys.error("DeciderMachine reached evalAlg — normalize must be called first")
    case ApparatusF.OpenMachine(machine) => openMachineStep(machine)
    case ApparatusF.ClosedMachine(machine) => closedMachineStep(machine)
    case ApparatusF.Ref(networkId) => refStep(networkId)
    case ApparatusF.Sequential(left, right) => seqStep(left, right)
    case ApparatusF.Parallel(left, right) => parStep(left, right)
    case ApparatusF.Alternative(left, right) => altStep(left, right)
    case ApparatusF.Feedback(left, right, foldN, monoidNB, monoidNA) => feedbackLoop(left, right, foldN, monoidNB, monoidNA)
    case ApparatusF.FeedbackMany(left, right, foldN, monoidNB, monoidNA) => feedbackManyLoop(left, right, foldN, monoidNB, monoidNA)
    case ApparatusF.LmapOrEmpty(inner, pf, mb) => lmapStep(inner, pf, mb)
    case ApparatusF.Merged(left, right, mb) => mergeStep(left, right, mb)
    case ApparatusF.Labeled(inner, _) => labeledStep(inner)
  }

def compile[F[_]: Monad, I, O](materializer: DeciderMaterializer[F])(apparatus: Apparatus[F, I, O]): F[(CompiledNetwork[F, I, O], DeciderStates[F])] = {
  val (normalizedRegistry, normalized) = normalize(apparatus)
  materializeRegistry(normalizedRegistry, materializer).map { deciders =>
    val initialStates = DeciderStates(deciders, Map.empty)
    val stepped       = cata2(evalAlg[F])(normalized)
    (stepped, initialStates)
  }
}

private def labeledStep[F[_] : Monad, I, O](inner: CompiledNetwork[F, I, O]) = {
  def go(m: CompiledNetwork[F, I, O]): CompiledNetwork[F, I, O] =
    CompiledNetwork(run = input => m.run(input).map { case (o, nextM) => (o, go(nextM)) })

  go(inner)
}

private def mergeStep[F[_] : Monad, I, O](left: CompiledNetwork[F, I, O], right: CompiledNetwork[F, I, O], mb: Monoid[O]) = {
  def go(l: CompiledNetwork[F, I, O], r: CompiledNetwork[F, I, O]): CompiledNetwork[F, I, O] =
    CompiledNetwork(run = input =>
      for {
        (b1, nextL) <- l.run(input)
        (b2, nextR) <- r.run(input)
      } yield (mb.combine(b1, b2), go(nextL, nextR)))

  go(left, right)
}

private def refStep[F[_] : Monad, I, O](networkId: String) = {
  def go: CompiledNetwork[F, I, O] =
    CompiledNetwork(run = input =>
      for {
        s <- StateT.get[F, DeciderStates[F]]
        output <- s.deciders(networkId) match {
          case MealyMachine.Open(m) =>
            val typed = m.asInstanceOf[OpenMealy[F, I, O]]
            val state = s.states.getOrElse(networkId, typed.initialState).asInstanceOf[typed.State]
            StateT.liftF(typed.action(state, input)).flatMap { case (o, ns) =>
              StateT.modify[F, DeciderStates[F]](_.withState(networkId, ns)).as(o)
            }
          case MealyMachine.Closed(m) =>
            StateT.liftF(m.asInstanceOf[ClosedMealy[F, I, O]].action(input))
        }
      } yield (output, go))

  go
}

private def closedMachineStep[F[_] : Monad , I, O](machine: ClosedMealy[F, I, O]) = {
  def go: CompiledNetwork[F, I, O] =
    CompiledNetwork(run = input => StateT.liftF(machine.action(input)).map(o => (o, go)))

  go
}
private def openMachineStep[F[_] : Monad, I, O](machine: OpenMealy[F, I, O]) = {
  def go(m: OpenMealy[F, I, O]): CompiledNetwork[F, I, O] =
    CompiledNetwork(run = input =>
      StateT.liftF(m.action(m.initialState, input)).map { case (o, ns) =>
        (o, go(m.atState(ns)))
      })

  go(machine)
}

private def seqStep[F[_]: Monad, A, B, C](
                                           l: CompiledNetwork[F, A, B],
                                           r: CompiledNetwork[F, B, C]
                                         ): CompiledNetwork[F, A, C] =
  CompiledNetwork(run = input =>
    for {
      (b, nextL) <- l.run(input)
      (c, nextR) <- r.run(b)
    } yield (c, seqStep(nextL, nextR)))

private def parStep[F[_]: Monad, A, B, C, D](
                                              l: CompiledNetwork[F, A, B],
                                              r: CompiledNetwork[F, C, D]
                                            ): CompiledNetwork[F, (A, C), (B, D)] =
  CompiledNetwork(run = input =>
    for {
      (b, nextL) <- l.run(input._1)
      (d, nextR) <- r.run(input._2)
    } yield ((b, d), parStep(nextL, nextR)))

private def altStep[F[_]: Monad, A, B, C, D](
                                              l: CompiledNetwork[F, A, B],
                                              r: CompiledNetwork[F, C, D]
                                            ): CompiledNetwork[F, Either[A, C], Either[B, D]] =
  CompiledNetwork(run = {
    case Left(a) => l.run(a).map { case (b, nextL) => (Left(b), altStep(nextL, r)) }
    case Right(c) => r.run(c).map { case (d, nextR) => (Right(d), altStep(l, nextR)) }
  })

private def lmapStep[F[_]: Monad, A, B, C](
                                            m:  CompiledNetwork[F, A, B],
                                            pf: PartialFunction[C, A],
                                            mb: Monoid[B]
                                          ): CompiledNetwork[F, C, B] =
  CompiledNetwork(run = input =>
    pf.lift(input) match
      case Some(a) => m.run(a).map { case (b, nextM) => (b, lmapStep(nextM, pf, mb)) }
      case None => StateT.pure((mb.empty, lmapStep(m, pf, mb))))

private[alg] def materializeRegistry[F[_]: Monad](entries: NormalizedRegistry[F], m: DeciderMaterializer[F]): F[Map[String, MealyMachine[F, ?, ?]]] =
  entries
    .toList
    .traverse { case (id, entry) => entry.materialize(m).map(id -> _) }
    .map(_.toMap)

private def feedbackLoop[F[_]: Monad, A, B, N[_]](
                                                   left:     CompiledNetwork[F, A, N[B]],
                                                   right:    CompiledNetwork[F, B, N[A]],
                                                   foldN:    Foldable[N],
                                                   monoidNB: Monoid[N[B]],
                                                   monoidNA: Monoid[N[A]]
): CompiledNetwork[F, A, N[B]] = {
  def go(l: CompiledNetwork[F, A, N[B]], r: CompiledNetwork[F, B, N[A]]): CompiledNetwork[F, A, N[B]] =
    CompiledNetwork(run = a => loop(List(a), monoidNB.empty, l, r))

  def loop(
            pending: List[A],
            acc: N[B],
            l: CompiledNetwork[F, A, N[B]],
            r: CompiledNetwork[F, B, N[A]]
          ): StateT[F, DeciderStates[F], (N[B], CompiledNetwork[F, A, N[B]])] =
    pending match
      case Nil => StateT.pure((acc, go(l, r)))
      case head :: tail =>
        l.run(head).flatMap { case (nb, nextL) =>
          foldN.toList(nb).foldLeftM[[x] =>> StateT[F, DeciderStates[F], x], (N[A], CompiledNetwork[F, B, N[A]])](
            (monoidNA.empty, r)
          ) { case ((naAcc, ri), b) =>
            ri.run(b).map { case (na, nextR) => (monoidNA.combine(naAcc, na), nextR) }
          }.flatMap { case (na, nextR) =>
            loop(foldN.toList(na) ++ tail, monoidNB.combine(acc, nb), nextL, nextR)
          }
        }

  go(left, right)
}

private def feedbackManyLoop[F[_]: Monad, A, B, N[_]](
                                                       left:     CompiledNetwork[F, A, N[B]],
                                                       right:    CompiledNetwork[F, B, N[A]],
                                                       foldN:    Foldable[N],
                                                       monoidNB: Monoid[N[B]],
                                                       monoidNA: Monoid[N[A]]
): CompiledNetwork[F, N[A], N[B]] = {

  def go(l: CompiledNetwork[F, A, N[B]], r: CompiledNetwork[F, B, N[A]]): CompiledNetwork[F, N[A], N[B]] =
    CompiledNetwork(run = nas => loop(foldN.toList(nas), monoidNB.empty, l, r))

  def loop(
            pending: List[A],
            acc: N[B],
            l: CompiledNetwork[F, A, N[B]],
            r: CompiledNetwork[F, B, N[A]]
          ): StateT[F, DeciderStates[F], (N[B], CompiledNetwork[F, N[A], N[B]])] =
    pending match
      case Nil => StateT.pure((acc, go(l, r)))
      case head :: tail =>
        l.run(head).flatMap { case (nb, nextL) =>
          foldN.toList(nb).foldLeftM[[x] =>> StateT[F, DeciderStates[F], x], (N[A], CompiledNetwork[F, B, N[A]])](
            (monoidNA.empty, r)
          ) { case ((naAcc, ri), b) =>
            ri.run(b).map { case (na, nextR) => (monoidNA.combine(naAcc, na), nextR) }
          }.flatMap { case (na, nextR) =>
            loop(foldN.toList(na) ++ tail, monoidNB.combine(acc, nb), nextL, nextR)
          }
        }

  go(left, right)
}