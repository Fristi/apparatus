package apparatus.core

import cats.Applicative
import cats.implicits.*

case class Decider[S, I, O](
  initialState: S,
  decide: (I, S) => O,
  evolve: (O, S) => S
) { self =>

  def toBaseMachine[F[_]: Applicative]: BaseMachineT[F, I, O] =
    new BaseMachineT[F, I, O]:
      override type State = S
      override def initialState: S = self.initialState
      override def action(state: State, input: I): F[(O, State)] =
        val o  = self.decide(input, state)
        val ns = self.evolve(o, state)
        (o, ns).pure
}
