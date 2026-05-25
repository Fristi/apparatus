package apparatus.core.fix.alg

import apparatus.core.Apparatus
import apparatus.core.fix.{ApparatusF, HAlgebra2, cata2}
import apparatus.core.machines.{ClosedMealy, DeciderMaterializer, MealyMachine, OpenMealy}
import cats.implicits.*
import cats.{Foldable, Monad, Monoid}

/** A compiled Mealy machine: each step returns output and the next network (state baked in via closure). */
final class CompiledNetwork[F[_], I, O](val step: I => F[(O, CompiledNetwork[F, I, O])]):
  def andThen[P](right: CompiledNetwork[F, O, P])(using M: Monad[F]): CompiledNetwork[F, I, P] =
    new CompiledNetwork(i =>
      step(i).flatMap { case (o, l2) =>
        right.step(o).map { case (p, r2) => (p, l2.andThen(r2)) }
      }
    )

private def evalAlg[F[_]: Monad](registry: Map[String, MealyMachine[F, ?, ?]]): HAlgebra2[[G[_, _], I, O] =>> ApparatusF[G, F, I, O], [I, O] =>> CompiledNetwork[F, I, O]] =
  [I, O] => (node: ApparatusF[[x, y] =>> CompiledNetwork[F, x, y], F, I, O]) => node match {

    case ApparatusF.DeciderMachine(_, _, _) =>
      sys.error("DeciderMachine reached evalAlg — normalize must be called first")

    case ApparatusF.OpenMachine(machine) =>
      def go(state: machine.State): CompiledNetwork[F, I, O] =
        new CompiledNetwork(i => machine.action(state, i).map { case (o, ns) => (o, go(ns)) })
      go(machine.initialState)

    case ApparatusF.ClosedMachine(machine) =>
      lazy val self: CompiledNetwork[F, I, O] =
        new CompiledNetwork(i => machine.action(i).map(o => (o, self)))
      self

    case ApparatusF.Ref(networkId) =>
      registry(networkId) match {
        case MealyMachine.Open(m) =>
          val typed = m.asInstanceOf[OpenMealy[F, I, O]]
          def go(state: typed.State): CompiledNetwork[F, I, O] =
            new CompiledNetwork(i => typed.action(state, i).map { case (o, ns) => (o, go(ns)) })
          go(typed.initialState)
        case MealyMachine.Closed(m) =>
          val typed = m.asInstanceOf[ClosedMealy[F, I, O]]
          lazy val self: CompiledNetwork[F, I, O] =
            new CompiledNetwork(i => typed.action(i).map(o => (o, self)))
          self
      }

    case ApparatusF.Sequential(left, right) =>
      left.andThen(right)

    case ApparatusF.Parallel(left, right) =>
      parNetwork(left, right)

    case ApparatusF.Alternative(left, right) =>
      altNetwork(left, right)

    case ApparatusF.Feedback(left, right, foldN, monoidNB, monoidNA) =>
      feedbackLoop(left, right, foldN, monoidNB, monoidNA)

    case ApparatusF.FeedbackMany(left, right, foldN, monoidNB, monoidNA) =>
      feedbackManyLoop(left, right, foldN, monoidNB, monoidNA)

    case ApparatusF.LmapOrEmpty(inner, pf, mb) =>
      lmapOrEmptyNetwork(inner, pf, mb)

    case ApparatusF.Merged(left, right, mb) =>
      mergedNetwork(left, right, mb)

    case ApparatusF.Labeled(inner, _) =>
      inner
  }

def compile[F[_]: Monad, I, O](materializer: DeciderMaterializer[F])(apparatus: Apparatus[F, I, O]): F[CompiledNetwork[F, I, O]] = {
  val (normalizedRegistry, normalized) = normalize(apparatus)
  materializeRegistry(normalizedRegistry, materializer).map { registry =>
    cata2(evalAlg[F](registry))(normalized)
  }
}

private[alg] def materializeRegistry[F[_]: Monad](entries: NormalizedRegistry[F], m: DeciderMaterializer[F]): F[Map[String, MealyMachine[F, ?, ?]]] =
  entries
    .toList
    .traverse { case (id, entry) => entry.materialize(m).map(id -> _) }
    .map(_.toMap)

private def parNetwork[F[_]: Monad, A, B, C, D](l: CompiledNetwork[F, A, B], r: CompiledNetwork[F, C, D]): CompiledNetwork[F, (A, C), (B, D)] =
  new CompiledNetwork(ac =>
    (l.step(ac._1), r.step(ac._2)).mapN { case ((b, l2), (d, r2)) => ((b, d), parNetwork(l2, r2)) }
  )

private def altNetwork[F[_]: Monad, A, B, C, D](l: CompiledNetwork[F, A, B], r: CompiledNetwork[F, C, D]): CompiledNetwork[F, Either[A, C], Either[B, D]] =
  new CompiledNetwork({
    case Left(a)  => l.step(a).map { case (b, l2) => (Left(b),  altNetwork(l2, r)) }
    case Right(c) => r.step(c).map { case (d, r2) => (Right(d), altNetwork(l, r2)) }
  })

private def lmapOrEmptyNetwork[F[_]: Monad, A, B, C](
  inner: CompiledNetwork[F, A, B],
  pf:    PartialFunction[C, A],
  mb:    Monoid[B]
): CompiledNetwork[F, C, B] =
  new CompiledNetwork(c =>
    pf.lift(c) match
      case Some(a) => inner.step(a).map { case (b, next) => (b, lmapOrEmptyNetwork(next, pf, mb)) }
      case None    => (mb.empty, lmapOrEmptyNetwork(inner, pf, mb)).pure
  )

private def mergedNetwork[F[_]: Monad, A, B](
  left:  CompiledNetwork[F, A, B],
  right: CompiledNetwork[F, A, B],
  mb:    Monoid[B]
): CompiledNetwork[F, A, B] =
  new CompiledNetwork(a =>
    (left.step(a), right.step(a)).mapN { case ((b1, l2), (b2, r2)) =>
      (mb.combine(b1, b2), mergedNetwork(l2, r2, mb))
    }
  )

/** Each B in N[B] fed individually to right (B → N[A]), resulting A values re-queued until quiescent. */
private def feedbackLoop[F[_]: Monad, A, B, N[_]](
  left:     CompiledNetwork[F, A, N[B]],
  right:    CompiledNetwork[F, B, N[A]],
  foldN:    Foldable[N],
  monoidNB: Monoid[N[B]],
  monoidNA: Monoid[N[A]]
): CompiledNetwork[F, A, N[B]] =
  new CompiledNetwork(a => {
    def loop(
      pending: List[A],
      acc:     N[B],
      l:       CompiledNetwork[F, A, N[B]],
      r:       CompiledNetwork[F, B, N[A]]
    ): F[(N[B], CompiledNetwork[F, A, N[B]])] =
      pending match
        case Nil => (acc, feedbackLoop(l, r, foldN, monoidNB, monoidNA)).pure
        case head :: tail =>
          l.step(head).flatMap { case (nb, l2) =>
            foldN.toList(nb).foldLeftM((monoidNA.empty, r)) { case ((naAcc, r1), b) =>
              r1.step(b).map { case (na, r2) => (monoidNA.combine(naAcc, na), r2) }
            }.flatMap { case (na, r2) =>
              loop(foldN.toList(na) ++ tail, monoidNB.combine(acc, nb), l2, r2)
            }
          }
    loop(List(a), monoidNB.empty, left, right)
  })

/** Like feedbackLoop but starts with N[A] instead of a single A. */
private def feedbackManyLoop[F[_]: Monad, A, B, N[_]](
  left:     CompiledNetwork[F, A, N[B]],
  right:    CompiledNetwork[F, B, N[A]],
  foldN:    Foldable[N],
  monoidNB: Monoid[N[B]],
  monoidNA: Monoid[N[A]]
): CompiledNetwork[F, N[A], N[B]] =
  new CompiledNetwork(nas => {
    def loop(
      pending: List[A],
      acc:     N[B],
      l:       CompiledNetwork[F, A, N[B]],
      r:       CompiledNetwork[F, B, N[A]]
    ): F[(N[B], CompiledNetwork[F, N[A], N[B]])] =
      pending match
        case Nil => (acc, feedbackManyLoop(l, r, foldN, monoidNB, monoidNA)).pure
        case head :: tail =>
          l.step(head).flatMap { case (nb, l2) =>
            foldN.toList(nb).foldLeftM((monoidNA.empty, r)) { case ((naAcc, r1), b) =>
              r1.step(b).map { case (na, r2) => (monoidNA.combine(naAcc, na), r2) }
            }.flatMap { case (na, r2) =>
              loop(foldN.toList(na) ++ tail, monoidNB.combine(acc, nb), l2, r2)
            }
          }
    loop(foldN.toList(nas), monoidNB.empty, left, right)
  })
