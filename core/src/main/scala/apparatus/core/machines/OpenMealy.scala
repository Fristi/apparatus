package apparatus.core.machines

import apparatus.core.patterns
import cats.arrow.Profunctor
import cats.implicits.*
import cats.{Functor, ~>}

/** Core abstraction for a stateful machine running in effect `F`.
  *
  * Each step receives the current `State` and an input `I`, and produces an
  * effectful pair of output `O` and the next `State`. The `State` type is
  * abstract, keeping it hidden from callers and enabling heterogeneous
  * composition.
  *
  * @tparam F effect type (e.g. `Id`, `IO`, `Either[E, *]`)
  * @tparam I input type
  * @tparam O output type
  */
trait OpenMealy[F[_], I, O] {
  self =>
  type State

  /** The state the machine starts from. */
  def initialState: State

  /** Transition function: given current state and input, return output + next state. */
  def action(state: State, input: I): F[(O, State)]

  final def mapK[G[_]](f: F ~> G): OpenMealy[G, I, O] =
    new OpenMealy[G, I, O] {
      override type State = self.State
      override def initialState: State = self.initialState
      override def action(state: State, input: I): G[(O, State)] =
        f(self.action(state, input))
    }

  /** Contramap the input: adapt `H` to `I` before each step. */
  final def lmap[H](f: H => I)(using F: Functor[F]): OpenMealy[F, H, O] = dimap[H, O](f)(identity)

  /** Map the output: transform `O` to `P` after each step. */
  final def rmap[P](f: O => P)(using F: Functor[F]): OpenMealy[F, I, P] = dimap[I, P](identity)(f)

  /** Adapt both input and output in a single pass. */
  final def dimap[H, P](f: H => I)(g: O => P)(using F: Functor[F]): OpenMealy[F, H, P] =
    new OpenMealy[F, H, P] {
      override type State = self.State
      override def initialState: State = self.initialState
      override def action(state: State, input: H): F[(P, State)] =
        self.action(state, f(input)).map { case (b, s) => (g(b), s) }
    }
}

object OpenMealy:

  /** Construct a machine from an explicit seed state and transition function. */
  def apply[F[_], S, I, O](seed: S, f: (S, I) => F[(O, S)]): OpenMealy[F, I, O] =
    new OpenMealy[F, I, O]:
      type State = S
      def initialState: S = seed
      def action(state: S, input: I): F[(O, S)] = f(state, input)

  /** Construct a machine with no meaningful state (unit state `Tuple`). */
  def stateless[F[_]: Functor, I, O](f: I => F[O]): OpenMealy[F, I, O] =
    apply[F, Tuple, I, O](Tuple(), (_, i) => f(i).map(o => (o, Tuple())))

  /** `Profunctor` instance: `lmap` adapts the input, `rmap` adapts the output. */
  implicit def profunctor[F[_]: Functor]: Profunctor[[A, B] =>> OpenMealy[F, A, B]] =
    new Profunctor[[A, B] =>> OpenMealy[F, A, B]]:
      override def dimap[A, B, C, D](fab: OpenMealy[F, A, B])(f: C => A)(g: B => D): OpenMealy[F, C, D] =
        fab.dimap(f)(g)