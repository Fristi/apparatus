package apparatus.core

import cats.Applicative
import cats.implicits.*

/** Pure, effect-free state machine following the Decider pattern.
  *
  * Separates decision logic from state evolution:
  *   - `decide` maps an input and the current state to an output (typically a
  *     list of events).
  *   - `evolve` folds that output back into a new state.
  *
  * This separation makes individual functions easy to test in isolation and
  * enables the same `Decider` to be reused across different effect stacks via
  * [[toBaseMachine]].
  *
  * @tparam S state type
  * @tparam I input / command type
  * @tparam O output / event type
  * @param initialState starting state of the machine
  * @param decide       pure function `(input, state) => output`
  * @param evolve       pure function `(output, state) => newState`
  */
case class Decider[S, I, O](
  initialState: S,
  decide: (I, S) => O,
  evolve: (O, S) => S
) { self =>

  /** Lift into [[BaseMachineT]] under effect `F`, running `decide` then `evolve` per step. */
  def toBaseMachine[F[_]: Applicative]: BaseMachineT[F, I, O] =
    new BaseMachineT[F, I, O]:
      override type State = S
      override def initialState: S = self.initialState
      override def action(state: State, input: I): F[(O, State)] =
        val o  = self.decide(input, state)
        val ns = self.evolve(o, state)
        (o, ns).pure
}
