package apparatus.core.fix

import apparatus.core.Decider
import apparatus.core.fix.HFix2
import cats.{Foldable, Monoid}
import zio.blocks.schema.Schema

sealed trait ApparatusF[F[_, _], I, O]:
  def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, I, O]

object ApparatusF:

  final case class DeciderRef[F[_, _], I, O](networkId: String) extends ApparatusF[F, I, O]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, I, O] = DeciderRef(networkId)
  
  final case class DeciderNode[F[_, _], I, O](
                                               networkId: String,
                                               decider:   Decider[?, I, List[O]],
                                               schema: Schema[O]
                                             ) extends ApparatusF[F, I, O]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, I, O] =
      DeciderNode(networkId, decider, schema)

  final case class Sequential[F[_, _], A, B, C](
                                                 left:  F[A, B],
                                                 right: F[B, C]
                                               ) extends ApparatusF[F, A, C]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, A, C] =
      Sequential(nt(left), nt(right))

  final case class Parallel[F[_, _], A, B, C, D](
                                                  left:  F[A, B],
                                                  right: F[C, D]
                                                ) extends ApparatusF[F, (A, C), (B, D)]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, (A, C), (B, D)] =
      Parallel(nt(left), nt(right))

  final case class Alternative[F[_, _], A, B, C, D](
                                                     left:  F[A, B],
                                                     right: F[C, D]
                                                   ) extends ApparatusF[F, Either[A, C], Either[B, D]]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, Either[A, C], Either[B, D]] =
      Alternative(nt(left), nt(right))

  final case class Feedback[F[_, _], A, B, N[_]](
                                                  left:     F[A, N[B]],
                                                  right:    F[N[B], A],
                                                  foldN:    Foldable[N],
                                                  monoidNB: Monoid[N[B]],
                                                  monoidNA: Monoid[N[A]]
                                                ) extends ApparatusF[F, A, N[B]]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, A, N[B]] =
      Feedback(nt(left), nt(right), foldN, monoidNB, monoidNA)

  final case class FeedbackMany[F[_, _], A, B, N[_]](
                                                      left:     F[N[A], N[B]],
                                                      right:    F[N[B], N[A]],
                                                      foldN:    Foldable[N],
                                                      monoidNB: Monoid[N[B]],
                                                      monoidNA: Monoid[N[A]]
                                                    ) extends ApparatusF[F, N[A], N[B]]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, N[A], N[B]] =
      FeedbackMany(nt(left), nt(right), foldN, monoidNB, monoidNA)

  final case class LmapOrEmpty[F[_, _], A, B, C](
                                                   inner: F[A, B],
                                                   pf:    PartialFunction[C, A],
                                                   mb:    Monoid[B]
                                                 ) extends ApparatusF[F, C, B]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, C, B] =
      LmapOrEmpty(nt(inner), pf, mb)

  final case class Merged[F[_, _], A, B](
                                          left:  F[A, B],
                                          right: F[A, B],
                                          mb:    Monoid[B]
                                        ) extends ApparatusF[F, A, B]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, A, B] =
      Merged(nt(left), nt(right), mb)

  final case class Labeled[F[_, _], I, O](
                                           inner: F[I, O],
                                           name:  String
                                         ) extends ApparatusF[F, I, O]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, I, O] =
      Labeled(nt(inner), name)


// ─── HFunctor2 instance ──────────────────────────────────────────────────────

given HFunctor2[ApparatusF] with
  override def hfmap[F[_, _], G[_, _], I, O](nt: FunctionK2[F, G])(hfio: ApparatusF[F, I, O]): ApparatusF[G, I, O] =
    hfio.hfmap(nt)


// ─── The fixed point type ────────────────────────────────────────────────────

type Apparatus[I, O] = HFix2[ApparatusF, I, O]


// ─── Smart constructors ──────────────────────────────────────────────────────

object Apparatus {
  def decider[I, O](networkId: String, d: Decider[?, I, List[O]])(using S: Schema[O]): Apparatus[I, O] =
    HFix2(ApparatusF.DeciderNode(networkId, d, S))

  def sequential[A, B, C](left: Apparatus[A, B], right: Apparatus[B, C]): Apparatus[A, C] =
    HFix2(ApparatusF.Sequential(left, right))

  def parallel[A, B, C, D](left: Apparatus[A, B], right: Apparatus[C, D]): Apparatus[(A, C), (B, D)] =
    HFix2(ApparatusF.Parallel(left, right))

  def alternative[A, B, C, D](left: Apparatus[A, B], right: Apparatus[C, D]): Apparatus[Either[A, C], Either[B, D]] =
    HFix2(ApparatusF.Alternative(left, right))

  def feedback[A, B, N[_]](
                            left: Apparatus[A, N[B]], right: Apparatus[N[B], A]
                          )(using foldN: Foldable[N], monoidNB: Monoid[N[B]], monoidNA: Monoid[N[A]]): Apparatus[A, N[B]] =
    HFix2(ApparatusF.Feedback(left, right, foldN, monoidNB, monoidNA))

  def feedbackMany[A, B, N[_]](
                                left: Apparatus[N[A], N[B]], right: Apparatus[N[B], N[A]]
                              )(using foldN: Foldable[N], monoidNB: Monoid[N[B]], monoidNA: Monoid[N[A]]): Apparatus[N[A], N[B]] =
    HFix2(ApparatusF.FeedbackMany(left, right, foldN, monoidNB, monoidNA))

  def lmapOrEmpty[A, B, C](inner: Apparatus[A, B], pf: PartialFunction[C, A])(using mb: Monoid[B]): Apparatus[C, B] =
    HFix2(ApparatusF.LmapOrEmpty(inner, pf, mb))

  def merged[A, B](left: Apparatus[A, B], right: Apparatus[A, B])(using mb: Monoid[B]): Apparatus[A, B] =
    HFix2(ApparatusF.Merged(left, right, mb))

  def labeled[I, O](name: String)(inner: Apparatus[I, O]): Apparatus[I, O] =
    HFix2(ApparatusF.Labeled(inner, name))
}