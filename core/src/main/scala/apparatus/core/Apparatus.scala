package apparatus.core

import apparatus.core.fix as fixPkg
import apparatus.core.fix.alg.{Mermaid, compile as fixCompile}
import cats.*
import cats.arrow.{Category, Choice, Profunctor, Strong}
import cats.implicits.*
import zio.blocks.schema.Schema

// ─── The type ─────────────────────────────────────────────────────────────────

type Apparatus[F[_], I, O] = fixPkg.Apparatus[F, I, O]

// ─── Companion ────────────────────────────────────────────────────────────────

object Apparatus:

  // ── Smart constructors ──────────────────────────────────────────────────────

  object Fresh:
    def apply[F[_], I, O](machine: BaseMachineT[F, I, O]): Apparatus[F, I, O] =
      fixPkg.Apparatus.fresh(machine)

  object Stable:
    /** Wraps machine and tags it with `id` for Mermaid display. */
    def apply[F[_], I, O](id: String, machine: BaseMachineT[F, I, O]): Apparatus[F, I, O] =
      fixPkg.Apparatus.labeled(id)(fixPkg.Apparatus.fresh(machine))

  object DeciderMachine:
    def apply[F[_], I, E](id: String, decider: Decider[?, I, List[E]])(using Schema[E]): Apparatus[F, I, List[E]] =
      fixPkg.Apparatus.decider[F, I, E](id, decider)

  def identity[F[_] : Applicative, A]: Apparatus[F, A, A] =
    fixPkg.Apparatus.fresh(BaseMachineT.stateless[F, A, A](_.pure))

  // ── Run methods ─────────────────────────────────────────────────────────────

  /** Run a single step. */
  def runA[F[_]: Monad, I, O](fsm: Apparatus[F, I, O], input: I, mat: DeciderMaterializer[F]): F[O] =
    fixCompile(mat)(fsm).flatMap { case (network, registry) =>
      network.run(input).runA(registry)
    }

  /** Alias for [[runA]]. */
  def run[F[_]: Monad, I, O](fsm: Apparatus[F, I, O], input: I, mat: DeciderMaterializer[F]): F[O] =
    runA(fsm, input, mat)

  /** Fold over inputs, threading registry state across all steps. */
  def runMultipleA[F[_]: Monad, M[_]: Foldable, I, O: Monoid](
    fsm: Apparatus[F, I, O], entries: M[I], mat: DeciderMaterializer[F]
  ): F[O] =
    fixCompile(mat)(fsm).flatMap { case (network, initialRegistry) =>
      entries.foldM((Monoid[O].empty, initialRegistry)) { case ((acc, registry), i) =>
        network.run(i).run(registry).map { case (newRegistry, o) => (acc |+| o, newRegistry) }
      }.map(_._1)
    }

  /** Run a sequence of inputs, threading registry state, returning all outputs in order. */
  def runSteps[F[_]: Monad, I, O](fsm: Apparatus[F, I, O], inputs: List[I], mat: DeciderMaterializer[F]): F[List[O]] =
    fixCompile(mat)(fsm).flatMap { case (network, initialRegistry) =>
      inputs.foldM((List.empty[O], initialRegistry)) { case ((acc, registry), i) =>
        network.run(i).run(registry).map { case (newRegistry, o) => (acc :+ o, newRegistry) }
      }.map(_._1)
    }

  /** Alias for [[runMultipleA]]. */
  def runMultiple[F[_]: Monad, M[_]: Foldable, I, O: Monoid](
    fsm: Apparatus[F, I, O], entries: M[I], mat: DeciderMaterializer[F]
  ): F[O] =
    runMultipleA(fsm, entries, mat)

  // ── Cats instances ───────────────────────────────────────────────────────────

  given [F[_] : Applicative]: Category[[I, O] =>> Apparatus[F, I, O]] with
    def id[A]: Apparatus[F, A, A] = identity[F, A]
    def compose[A, B, C](f: Apparatus[F, B, C], g: Apparatus[F, A, B]): Apparatus[F, A, C] =
      fixPkg.Apparatus.sequential(g, f)

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
      fixPkg.Apparatus.alternative(f, g).rmap(_.merge)
    def id[A]: Apparatus[F, A, A] = identity[F, A]
    def compose[A, B, C](f: Apparatus[F, B, C], g: Apparatus[F, A, B]): Apparatus[F, A, C] =
      fixPkg.Apparatus.sequential(g, f)


// ─── Extension methods ────────────────────────────────────────────────────────

extension [F[_], I, O](left: Apparatus[F, I, O]) {

  // Structural combinators — no Applicative constraint needed

  def label(name: String): Apparatus[F, I, O] =
    fixPkg.Apparatus.labeled(name)(left)

  def andThen[C](right: Apparatus[F, O, C]): Apparatus[F, I, C] =
    fixPkg.Apparatus.sequential(left, right)

  def >>>[C](right: Apparatus[F, O, C]): Apparatus[F, I, C] = andThen(right)

  def par[C, D](right: Apparatus[F, C, D]): Apparatus[F, (I, C), (O, D)] =
    fixPkg.Apparatus.parallel(left, right)

  def ***[C, D](right: Apparatus[F, C, D]): Apparatus[F, (I, C), (O, D)] = par(right)

  def or[C, D](right: Apparatus[F, C, D]): Apparatus[F, Either[I, C], Either[O, D]] =
    fixPkg.Apparatus.alternative(left, right)

  def |||[C, D](right: Apparatus[F, C, D]): Apparatus[F, Either[I, C], Either[O, D]] = or(right)

  def lmapOrEmpty[C](pf: PartialFunction[C, I])(using m: Monoid[O]): Apparatus[F, C, O] =
    fixPkg.Apparatus.lmapOrEmpty(left, pf)

  def merge(right: Apparatus[F, I, O])(using m: Monoid[O]): Apparatus[F, I, O] =
    fixPkg.Apparatus.merged(left, right)

  // Effect-dependent combinators — Applicative[F] from the type

  def lmap[C](f: C => I)(using Applicative[F]): Apparatus[F, C, O] =
    fixPkg.Apparatus.sequential(
      fixPkg.Apparatus.fresh(BaseMachineT.stateless[F, C, I](c => f(c).pure)),
      left
    )

  def rmap[C](f: O => C)(using Applicative[F]): Apparatus[F, I, C] =
    fixPkg.Apparatus.sequential(
      left,
      fixPkg.Apparatus.fresh(BaseMachineT.stateless[F, O, C](o => f(o).pure))
    )

  def dimap[C, D](f: C => I)(g: O => D)(using Applicative[F]): Apparatus[F, C, D] =
    left.lmap(f).rmap(g)

  def imap[I2, O2](using isoIn: Iso[I, I2], isoOut: Iso[O, O2], A: Applicative[F]): Apparatus[F, I2, O2] =
    left.lmap(isoIn.from).rmap(isoOut.to)

  def first[C](using Applicative[F]): Apparatus[F, (I, C), (O, C)] =
    fixPkg.Apparatus.parallel(left, Apparatus.identity[F, C])

  def second[C](using Applicative[F]): Apparatus[F, (C, I), (C, O)] =
    fixPkg.Apparatus.parallel(Apparatus.identity[F, C], left)

  def tap[C](right: Apparatus[F, O, C])(using Applicative[F]): Apparatus[F, I, O] =
    left.rmap[(O, O)](o => (o, o))
      .andThen(fixPkg.Apparatus.parallel(Apparatus.identity[F, O], right))
      .rmap[O](_._1)
      
  def mermaid(using Monad[F]): String = Mermaid.print(left)
}

extension [F[_], N[_], A, B](left: Apparatus[F, A, N[B]]) {
  def feedback(right: Apparatus[F, B, N[A]])(using foldN: Foldable[N], monoidNB: Monoid[N[B]], monoidNA: Monoid[N[A]]): Apparatus[F, A, N[B]] =
    fixPkg.Apparatus.feedback(left, right)

  def feedbackMany(right: Apparatus[F, B, N[A]])(using foldN: Foldable[N], monoidNB: Monoid[N[B]], monoidNA: Monoid[N[A]]): Apparatus[F, N[A], N[B]] =
    fixPkg.Apparatus.feedbackMany(left, right)
}
