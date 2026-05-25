package apparatus.core.fix

import apparatus.core
import apparatus.core.machines.{ClosedMealy, Decider, OpenMealy}
import cats.{Foldable, Monoid}
import zio.blocks.schema.Schema

sealed trait ApparatusF[F[_, _], Eff[_], I, O]:
  def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, Eff, I, O]

object ApparatusF:

  final case class Ref[F[_, _], Eff[_], I, O](networkId: String) extends ApparatusF[F, Eff, I, O]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, Eff, I, O] = Ref(networkId)
  
  final case class OpenMachine[F[_, _], Eff[_], I, O](machine: OpenMealy[Eff, I, O]) extends ApparatusF[F, Eff, I, O]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, Eff, I, O] = OpenMachine(machine)

  final case class ClosedMachine[F[_, _], Eff[_], I, O](machine: ClosedMealy[Eff, I, O]) extends ApparatusF[F, Eff, I, O]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, Eff, I, O] = ClosedMachine(machine)

  final case class DeciderMachine[F[_, _], Eff[_], I, E](
    networkId: String,
    decider:   Decider[?, I, List[E]],
    schema:    Schema[E]
  ) extends ApparatusF[F, Eff, I, List[E]]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, Eff, I, List[E]] =
      DeciderMachine(networkId, decider, schema)

  final case class Sequential[F[_, _], Eff[_], A, B, C](
    left:  F[A, B],
    right: F[B, C]
  ) extends ApparatusF[F, Eff, A, C]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, Eff, A, C] =
      Sequential(nt(left), nt(right))

  final case class Parallel[F[_, _], Eff[_], A, B, C, D](
    left:  F[A, B],
    right: F[C, D]
  ) extends ApparatusF[F, Eff, (A, C), (B, D)]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, Eff, (A, C), (B, D)] =
      Parallel(nt(left), nt(right))

  final case class Alternative[F[_, _], Eff[_], A, B, C, D](
    left:  F[A, B],
    right: F[C, D]
  ) extends ApparatusF[F, Eff, Either[A, C], Either[B, D]]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, Eff, Either[A, C], Either[B, D]] =
      Alternative(nt(left), nt(right))

  /** Feedback loop: left emits N[B], each B fed into right (B → N[A]), re-queued until quiescent. */
  final case class Feedback[F[_, _], Eff[_], A, B, N[_]](
    left:     F[A, N[B]],
    right:    F[B, N[A]],
    foldN:    Foldable[N],
    monoidNB: Monoid[N[B]],
    monoidNA: Monoid[N[A]]
  ) extends ApparatusF[F, Eff, A, N[B]]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, Eff, A, N[B]] =
      Feedback(nt(left), nt(right), foldN, monoidNB, monoidNA)

  /** Like Feedback but entry point accepts N[A] instead of a single A. */
  final case class FeedbackMany[F[_, _], Eff[_], A, B, N[_]](
    left:     F[A, N[B]],
    right:    F[B, N[A]],
    foldN:    Foldable[N],
    monoidNB: Monoid[N[B]],
    monoidNA: Monoid[N[A]]
  ) extends ApparatusF[F, Eff, N[A], N[B]]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, Eff, N[A], N[B]] =
      FeedbackMany(nt(left), nt(right), foldN, monoidNB, monoidNA)

  final case class LmapOrEmpty[F[_, _], Eff[_], A, B, C](
    inner: F[A, B],
    pf:    PartialFunction[C, A],
    mb:    Monoid[B]
  ) extends ApparatusF[F, Eff, C, B]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, Eff, C, B] =
      LmapOrEmpty(nt(inner), pf, mb)

  final case class Merged[F[_, _], Eff[_], A, B](
    left:  F[A, B],
    right: F[A, B],
    mb:    Monoid[B]
  ) extends ApparatusF[F, Eff, A, B]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, Eff, A, B] =
      Merged(nt(left), nt(right), mb)

  final case class Labeled[F[_, _], Eff[_], I, O](
    inner: F[I, O],
    name:  String
  ) extends ApparatusF[F, Eff, I, O]:
    def hfmap[G[_, _]](nt: FunctionK2[F, G]): ApparatusF[G, Eff, I, O] =
      Labeled(nt(inner), name)

  given [Eff[_]]: HFunctor2[[F[_, _], I, O] =>> ApparatusF[F, Eff, I, O]] with
    override def hfmap[F[_, _], G[_, _], I, O](nt: FunctionK2[F, G])(hfio: ApparatusF[F, Eff, I, O]): ApparatusF[G, Eff, I, O] =
      hfio.hfmap(nt)