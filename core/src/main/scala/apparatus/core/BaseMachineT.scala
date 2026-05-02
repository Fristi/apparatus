package apparatus.core

import cats.arrow.Profunctor
import cats.{Functor, Applicative}
import cats.implicits.*

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
trait BaseMachineT[F[_], I, O]:
  type State

  /** The state the machine starts from. */
  def initialState: State

  /** Transition function: given current state and input, return output + next state. */
  def action(state: State, input: I): F[(O, State)]

  /** Convenience: run one step from `initialState`. */
  def step(input: I): F[(O, State)] = action(initialState, input)

object BaseMachineT:

  /** Construct a machine from an explicit seed state and transition function. */
  def apply[F[_], S, I, O](seed: S, f: (S, I) => F[(O, S)]): BaseMachineT[F, I, O] =
    new BaseMachineT[F, I, O]:
      type State = S
      def initialState: S = seed
      def action(state: S, input: I): F[(O, S)] = f(state, input)

  /** Construct a machine with no meaningful state (unit state `Tuple`). */
  def stateless[F[_]: Functor, I, O](f: I => F[O]): BaseMachineT[F, I, O] =
    apply[F, Tuple, I, O](Tuple(), (_, i) => f(i).map(o => (o, Tuple())))

  /** `Profunctor` instance: `lmap` adapts the input, `rmap` adapts the output. */
  implicit def profunctor[F[_]: Functor]: Profunctor[[A, B] =>> BaseMachineT[F, A, B]] =
    new Profunctor[[A, B] =>> BaseMachineT[F, A, B]]:
      override def dimap[A, B, C, D](fab: BaseMachineT[F, A, B])(f: C => A)(g: B => D): BaseMachineT[F, C, D] =
        new BaseMachineT[F, C, D]:
          type State = fab.State
          def initialState = fab.initialState
          def action(state: State, input: C): F[(D, State)] =
            fab.action(state, f(input)).map { case (b, s) => (g(b), s) }
