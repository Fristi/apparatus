package apparatus.core.machines

import apparatus.core.patterns
import cats.arrow.Profunctor
import cats.implicits.*
import cats.{Functor, ~>}

trait ClosedMealy[F[_], I, O] { self =>

  def action(input: I): F[O]

  final def mapK[G[_]](f: F ~> G): ClosedMealy[G, I, O] =
    new ClosedMealy[G, I, O] {
      override def action(input: I): G[O] =
        f(self.action(input))
    }

  /** Contramap the input: adapt `H` to `I` before each step. */
  final def lmap[H](f: H => I)(using F: Functor[F]): ClosedMealy[F, H, O] = dimap[H, O](f)(identity)

  /** Map the output: transform `O` to `P` after each step. */
  final def rmap[P](f: O => P)(using F: Functor[F]): ClosedMealy[F, I, P] = dimap[I, P](identity)(f)

  /** Adapt both input and output in a single pass. */
  final def dimap[H, P](f: H => I)(g: O => P)(using F: Functor[F]): ClosedMealy[F, H, P] =
    new ClosedMealy[F, H, P] {
      override def action(input: H): F[P] =
        self.action(f(input)).map(b => g(b))
    }
}

object ClosedMealy:

  def stateless[F[_]: Functor, I, O](f: I => F[O]): ClosedMealy[F, I, O] =
    new ClosedMealy[F, I, O] {
      override def action(input: I): F[O] = f(input)
    }
  
  /** `Profunctor` instance: `lmap` adapts the input, `rmap` adapts the output. */
  implicit def profunctor[F[_]: Functor]: Profunctor[[A, B] =>> ClosedMealy[F, A, B]] =
    new Profunctor[[A, B] =>> ClosedMealy[F, A, B]]:
      override def dimap[A, B, C, D](fab: ClosedMealy[F, A, B])(f: C => A)(g: B => D): ClosedMealy[F, C, D] =
        fab.dimap(f)(g)