package apparatus.core.fix

import apparatus.core.{Apparatus, Decider}
import cats.{Foldable, Functor, Monoid}

sealed trait ApparatusF[F[_], A, B, R] {
  def map[S](f: R => S): ApparatusF[F, A, B, S]
}

object ApparatusF {

  final case class DeciderNode[F[_], I, O](
                                            networkId: String,
                                            decider: Decider[?, I, O]
                                          ) extends ApparatusF[F, I, O, Nothing] {
    override def map[S](f: Nothing => S): ApparatusF[F, I, O, S] = this.asInstanceOf[ApparatusF[F, I, O, S]]
  }

  final case class Sequential[F[_], A, B, C, R](
                                                 left: R,
                                                 right: R
                                               ) extends ApparatusF[F, A, C, R] {
    override def map[S](f: R => S): ApparatusF[F, A, C, S] = ApparatusF.Sequential(f(left), f(right))
  }

  final case class Parallel[F[_], A, B, C, D, R](
                                                  left: R,
                                                  right: R
                                                ) extends ApparatusF[F, (A, C), (B, D), R] {
    override def map[S](f: R => S): ApparatusF[F, (A, C), (B, D), S] = ApparatusF.Parallel(f(left), f(right))
  }

  final case class Alternative[F[_], A, B, C, D, R](
                                                     left: R,
                                                     right: R
                                                   ) extends ApparatusF[F, Either[A, C], Either[B, D], R] {
    override def map[S](f: R => S): ApparatusF[F, Either[A, C], Either[B, D], S] = ApparatusF.Alternative(f(left), f(right))
  }

  final case class Feedback[F[_], A, B, N[_], R](
                                                  left: R,
                                                  right: R,
                                                  foldN: Foldable[N],
                                                  monoidNB: Monoid[N[B]],
                                                  monoidNA: Monoid[N[A]]
                                                ) extends ApparatusF[F, A, N[B], R] {
    override def map[S](f: R => S): ApparatusF[F, A, N[B], S] = ApparatusF.Feedback(f(left), f(right), foldN, monoidNB, monoidNA)
  }

  final case class FeedbackMany[F[_], A, B, N[_], R](
                                                      left: R,
                                                      right: R,
                                                      foldN: Foldable[N],
                                                      monoidNB: Monoid[N[B]],
                                                      monoidNA: Monoid[N[A]]
                                                    ) extends ApparatusF[F, N[A], N[B], R] {
    override def map[S](f: R => S): ApparatusF[F, N[A], N[B], S] = ApparatusF.FeedbackMany(f(left), f(right), foldN, monoidNB, monoidNA)
  }

  final case class Labeled[F[_], I, O, R](
                                           inner: R,
                                           name: String
                                         ) extends ApparatusF[F, I, O, R] {
    override def map[S](f: R => S): ApparatusF[F, I, O, S] = ApparatusF.Labeled(f(inner), name)
  }

}

given functor: [F[_] : Functor, I, O] => Functor[[R] =>> ApparatusF[F, I, O, R]]:
  def map[A, B](fa: ApparatusF[F, I, O, A])(f: A => B): ApparatusF[F, I, O, B] =
    fa.map(f)

opaque type Apparatus[F[_], I, O] = Fix[[R] =>> ApparatusF[F, I, O, R]]

//extension [F[_], M[_], A, B](left: Apparatus[F, A, M[B]]) {
//  def feedback(right: Apparatus[F, B, M[A]])(using F: Foldable[M], MB: Monoid[M[B]], MA: Monoid[M[A]]): Apparatus[F, A, M[B]] =
//    Fix(ApparatusF.Feedback(left, right, F, MB, MA))
//  def feedbackMany(right: Apparatus[F, B, M[A]])(using F: Foldable[M], MB: Monoid[M[B]], MA: Monoid[M[A]]): Apparatus[F, M[A], M[B]] =
//    Fix(ApparatusF.FeedbackMany(left, right.unfix, F, MB, MA))
//}
