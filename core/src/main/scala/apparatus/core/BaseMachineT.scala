package apparatus.core

import cats.arrow.Profunctor
import cats.{Functor, Applicative}
import cats.implicits.*

trait BaseMachineT[F[_], I, O]:
  type State
  def initialState: State
  def action(state: State, input: I): F[(O, State)]

  def step(input: I): F[(O, State)] = action(initialState, input)

object BaseMachineT:

  def apply[F[_], S, I, O](seed: S, f: (S, I) => F[(O, S)]): BaseMachineT[F, I, O] =
    new BaseMachineT[F, I, O]:
      type State = S
      def initialState: S = seed
      def action(state: S, input: I): F[(O, S)] = f(state, input)

  def stateless[F[_]: Functor, I, O](f: I => F[O]): BaseMachineT[F, I, O] =
    apply[F, Tuple, I, O](Tuple(), (_, i) => f(i).map(o => (o, Tuple())))

  implicit def profunctor[F[_]: Functor]: Profunctor[[A, B] =>> BaseMachineT[F, A, B]] =
    new Profunctor[[A, B] =>> BaseMachineT[F, A, B]]:
      override def dimap[A, B, C, D](fab: BaseMachineT[F, A, B])(f: C => A)(g: B => D): BaseMachineT[F, C, D] =
        new BaseMachineT[F, C, D]:
          type State = fab.State
          def initialState = fab.initialState
          def action(state: State, input: C): F[(D, State)] =
            fab.action(state, f(input)).map { case (b, s) => (g(b), s) }
