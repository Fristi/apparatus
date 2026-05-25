package apparatus.core

import apparatus.core
import apparatus.core.fix.alg.Mermaid
import apparatus.core.fix.{ApparatusF, HFix2, alg}
import apparatus.core.machines.{ClosedMealy, Decider, DeciderMaterializer, OpenMealy}
import apparatus.core.Iso
import cats.arrow.{Category, Choice, Profunctor, Strong}
import cats.implicits.*
import cats.{Applicative, Foldable, Monad, Monoid}
import zio.blocks.schema.Schema

// ─── The fixed point type ────────────────────────────────────────────────────

type Apparatus[Eff[_], I, O] = HFix2[[F[_, _], I, O] =>> ApparatusF[F, Eff, I, O], I, O]


// ─── Smart constructors ──────────────────────────────────────────────────────

object Apparatus:

  def deciderMachine[Eff[_], I, E](networkId: String, d: Decider[?, I, List[E]])(using S: Schema[E]): Apparatus[Eff, I, List[E]] =
    HFix2(ApparatusF.DeciderMachine(networkId, d, S))

  def sequential[Eff[_], A, B, C](left: Apparatus[Eff, A, B], right: Apparatus[Eff, B, C]): Apparatus[Eff, A, C] =
    HFix2(ApparatusF.Sequential(left, right))

  def parallel[Eff[_], A, B, C, D](left: Apparatus[Eff, A, B], right: Apparatus[Eff, C, D]): Apparatus[Eff, (A, C), (B, D)] =
    HFix2(ApparatusF.Parallel(left, right))

  def alternative[Eff[_], A, B, C, D](left: Apparatus[Eff, A, B], right: Apparatus[Eff, C, D]): Apparatus[Eff, Either[A, C], Either[B, D]] =
    HFix2(ApparatusF.Alternative(left, right))

  def feedback[Eff[_], A, B, N[_]](
                                    left: Apparatus[Eff, A, N[B]], right: Apparatus[Eff, B, N[A]]
                                  )(using foldN: Foldable[N], monoidNB: Monoid[N[B]], monoidNA: Monoid[N[A]]): Apparatus[Eff, A, N[B]] =
    HFix2(ApparatusF.Feedback(left, right, foldN, monoidNB, monoidNA))

  def feedbackMany[Eff[_], A, B, N[_]](
                                        left: Apparatus[Eff, A, N[B]], right: Apparatus[Eff, B, N[A]]
                                      )(using foldN: Foldable[N], monoidNB: Monoid[N[B]], monoidNA: Monoid[N[A]]): Apparatus[Eff, N[A], N[B]] =
    HFix2(ApparatusF.FeedbackMany(left, right, foldN, monoidNB, monoidNA))

  def lmapOrEmpty[Eff[_], A, B, C](inner: Apparatus[Eff, A, B], pf: PartialFunction[C, A])(using mb: Monoid[B]): Apparatus[Eff, C, B] =
    HFix2(ApparatusF.LmapOrEmpty(inner, pf, mb))

  def merged[Eff[_], A, B](left: Apparatus[Eff, A, B], right: Apparatus[Eff, A, B])(using mb: Monoid[B]): Apparatus[Eff, A, B] =
    HFix2(ApparatusF.Merged(left, right, mb))

  def openMealy[Eff[_], I, O](machine: OpenMealy[Eff, I, O]): Apparatus[Eff, I, O] =
    HFix2(ApparatusF.OpenMachine(machine))

  def closedMealy[Eff[_], I, O](machine: ClosedMealy[Eff, I, O]): Apparatus[Eff, I, O] =
    HFix2(ApparatusF.ClosedMachine(machine))

  def labeled[Eff[_], I, O](name: String)(inner: Apparatus[Eff, I, O]): Apparatus[Eff, I, O] =
    HFix2(ApparatusF.Labeled(inner, name))

  def identity[F[_] : Applicative, A]: Apparatus[F, A, A] =
    closedMealy(ClosedMealy.stateless[F, A, A](_.pure))

  // ── Run methods ─────────────────────────────────────────────────────────────

  /** Run a single step. */
  def runA[F[_]: Monad, I, O](fsm: Apparatus[F, I, O], input: I, mat: DeciderMaterializer[F]): F[O] =
    alg.compile(mat)(fsm).flatMap { case (network, s0) =>
      network.run(input).runA(s0)
    }

  /** Alias for [[runA]]. */
  def run[F[_]: Monad, I, O](fsm: Apparatus[F, I, O], input: I, mat: DeciderMaterializer[F]): F[O] =
    runA(fsm, input, mat)

  /** Fold over inputs, threading registry state across all steps. */
  def runMultipleA[F[_]: Monad, M[_]: Foldable, I, O: Monoid](
                                                               fsm: Apparatus[F, I, O], entries: M[I], mat: DeciderMaterializer[F]
                                                             ): F[O] =
    alg.compile(mat)(fsm).flatMap { case (network, s0) =>
      entries.foldM((Monoid[O].empty, s0)) { case ((acc, s), i) =>
        network.run(i).run(s).map { case (s2, o) => (acc |+| o, s2) }
      }.map(_._1)
    }

  /** Run a sequence of inputs, threading registry state, returning all outputs in order. */
  def runSteps[F[_]: Monad, I, O](fsm: Apparatus[F, I, O], inputs: List[I], mat: DeciderMaterializer[F]): F[List[O]] =
    alg.compile(mat)(fsm).flatMap { case (network, s0) =>
      inputs.foldM((List.empty[O], s0)) { case ((acc, s), i) =>
        network.run(i).run(s).map { case (s2, o) => (acc :+ o, s2) }
      }.map(_._1)
    }

  /** Alias for [[runMultipleA]]. */
  def runMultiple[F[_]: Monad, M[_]: Foldable, I, O: Monoid](
                                                              fsm: Apparatus[F, I, O], entries: M[I], mat: DeciderMaterializer[F]
                                                            ): F[O] =
    runMultipleA(fsm, entries, mat)

  // ── Cats instances ───────────────────────────────────────────────────────────

  given [F[_] : Applicative]: Category[[I, O] =>> Apparatus[F, I, O]] with
    def id[A]: Apparatus[F, A, A] = Apparatus.identity[F, A]
    def compose[A, B, C](f: Apparatus[F, B, C], g: Apparatus[F, A, B]): Apparatus[F, A, C] =
      Apparatus.sequential(g, f)

  given [F[_] : Applicative]: Profunctor[[I, O] =>> Apparatus[F, I, O]] with
    def dimap[A, B, C, D](fab: Apparatus[F, A, B])(f: C => A)(g: B => D): Apparatus[F, C, D] =
      fab.lmap(f).rmap(g)

  given [F[_] : Applicative]: Strong[[I, O] =>> Apparatus[F, I, O]] with
    def first[A, B, C](fa: Apparatus[F, A, B]): Apparatus[F, (A, C), (B, C)] = fa.first[C]
    def second[A, B, C](fa: Apparatus[F, A, B]): Apparatus[F, (C, A), (C, B)] = fa.second[C]
    def dimap[A, B, C, D](fab: Apparatus[F, A, B])(f: C => A)(g: B => D): Apparatus[F, C, D] =
      fab.lmap(f).rmap(g)

  given [F[_] : Applicative]: Choice[[I, O] =>> Apparatus[F, I, O]] with
    def choice[A, B, C](f: Apparatus[F, A, C], g: Apparatus[F, B, C]): Apparatus[F, Either[A, B], C] =
      Apparatus.alternative(f, g).rmap(_.merge)
    def id[A]: Apparatus[F, A, A] = Apparatus.identity[F, A]
    def compose[A, B, C](f: Apparatus[F, B, C], g: Apparatus[F, A, B]): Apparatus[F, A, C] =
      Apparatus.sequential(g, f)

// ─── Extension methods ────────────────────────────────────────────────────────

extension [F[_], I, O](left: Apparatus[F, I, O]) {

  // Structural combinators — no Applicative constraint needed

  def label(name: String): Apparatus[F, I, O] =
    Apparatus.labeled(name)(left)

  def andThen[C](right: Apparatus[F, O, C]): Apparatus[F, I, C] =
    Apparatus.sequential(left, right)

  def >>>[C](right: Apparatus[F, O, C]): Apparatus[F, I, C] = andThen(right)

  def par[C, D](right: Apparatus[F, C, D]): Apparatus[F, (I, C), (O, D)] =
    Apparatus.parallel(left, right)

  def ***[C, D](right: Apparatus[F, C, D]): Apparatus[F, (I, C), (O, D)] = par(right)

  def or[C, D](right: Apparatus[F, C, D]): Apparatus[F, Either[I, C], Either[O, D]] =
    Apparatus.alternative(left, right)

  def |||[C, D](right: Apparatus[F, C, D]): Apparatus[F, Either[I, C], Either[O, D]] = or(right)

  def lmapOrEmpty[C](pf: PartialFunction[C, I])(using m: Monoid[O]): Apparatus[F, C, O] =
    Apparatus.lmapOrEmpty(left, pf)

  def merge(right: Apparatus[F, I, O])(using m: Monoid[O]): Apparatus[F, I, O] =
    Apparatus.merged(left, right)

  // Effect-dependent combinators — Applicative[F] from the type

  def lmap[C](f: C => I)(using Applicative[F]): Apparatus[F, C, O] =
    Apparatus.sequential(
      Apparatus.closedMealy(ClosedMealy.stateless[F, C, I](c => f(c).pure)),
      left
    )

  def rmap[C](f: O => C)(using Applicative[F]): Apparatus[F, I, C] =
    Apparatus.sequential(
      left,
      Apparatus.closedMealy(ClosedMealy.stateless[F, O, C](o => f(o).pure))
    )

  def dimap[C, D](f: C => I)(g: O => D)(using Applicative[F]): Apparatus[F, C, D] =
    left.lmap(f).rmap(g)

  def imap[I2, O2](using isoIn: Iso[I, I2], isoOut: Iso[O, O2], A: Applicative[F]): Apparatus[F, I2, O2] =
    left.lmap(isoIn.from).rmap(isoOut.to)

  def first[C](using Applicative[F]): Apparatus[F, (I, C), (O, C)] =
    Apparatus.parallel(left, Apparatus.identity[F, C])

  def second[C](using Applicative[F]): Apparatus[F, (C, I), (C, O)] =
    Apparatus.parallel(Apparatus.identity[F, C], left)

  def tap[C](right: Apparatus[F, O, C])(using Applicative[F]): Apparatus[F, I, O] =
    left.rmap[(O, O)](o => (o, o))
      .andThen(Apparatus.parallel(Apparatus.identity[F, O], right))
      .rmap[O](_._1)

  def mermaid(using Monad[F]): String = Mermaid.print(left)
}

extension [F[_], N[_], A, B](left: Apparatus[F, A, N[B]]) {
  def feedback(right: Apparatus[F, B, N[A]])(using foldN: Foldable[N], monoidNB: Monoid[N[B]], monoidNA: Monoid[N[A]]): Apparatus[F, A, N[B]] =
    Apparatus.feedback(left, right)

  def feedbackMany(right: Apparatus[F, B, N[A]])(using foldN: Foldable[N], monoidNB: Monoid[N[B]], monoidNA: Monoid[N[A]]): Apparatus[F, N[A], N[B]] =
    Apparatus.feedbackMany(left, right)
}
