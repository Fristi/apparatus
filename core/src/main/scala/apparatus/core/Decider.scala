package apparatus.core

import cats.{Applicative, MonadError}
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
 * [[toAppartus]].
 *
 * @tparam S state type
 * @tparam I input / command type
 * @tparam O output / event type
 * @param state  starting state of the machine
 * @param decide pure function `(input, state) => output`
 * @param evolve pure function `(output, state) => newState`
 */
final case class Decider[S, I, O](state: S, decide: (I, S) => O, evolve: (O, S) => S) { self =>

  def toApparatus[F[_] : Applicative](id: String): Apparatus[F, I, O] = {
    val baseMachine =
      new BaseMachineT[F, I, O]:
        override type State = S

        override def initialState: S = self.state

        override def action(state: State, input: I): F[(O, State)] =
          val o = self.decide(input, state)
          val ns = self.evolve(o, state)
          (o, ns).pure
          
    Apparatus.Stable(id, baseMachine)
  }
  
  /** Lift into [[BaseMachineT]] under effect `F`, running `decide` then `evolve` per step. */
  def toBaseMachine[F[_] : Applicative]: BaseMachineT[F, I, O] =
    new BaseMachineT[F, I, O]:
      override type State = S

      override def initialState: S = self.state

      override def action(state: State, input: I): F[(O, State)] =
        val o = self.decide(input, state)
        val ns = self.evolve(o, state)
        (o, ns).pure
}

final class SeedDeciderBuilder[S] private[core] (val initialState: S) {
  def withError[E](invalidCommand: E): FallibleDeciderBuilder[S, E] =
    new FallibleDeciderBuilder(initialState, invalidCommand)
  
  def decide[I, O](decide: (S, I) => O): WithDecideDeciderBuilder[S, I, O] =
    new WithDecideDeciderBuilder(initialState, decide)

  def partiallyDecide[I, O](decide: PartialFunction[(S, I), List[O]]): WithDecideDeciderBuilder[S, I, List[O]] =
    new WithDecideDeciderBuilder(initialState, (s, i) => decide.applyOrElse((s, i), _ => Nil))
}

final class FallibleDeciderBuilder[S, E] private[core] (initialState: S, invalidCommand: E) {
  def partiallyDecide[I, O](decide: PartialFunction[(I, S), Either[E, O]]): WithDecideDeciderBuilder[S, I, Either[E, O]] =
    new WithDecideDeciderBuilder(initialState, (i, s) => decide.applyOrElse((s, i), _ => Left(invalidCommand)))
}

final class WithDecideDeciderBuilder[S, I, O] private [core] (val initialState: S, val decide: (S, I) => O)

extension [S, I, O](builder: WithDecideDeciderBuilder[S, I, O]) {
  def evolveSingle(f: (S, O) => S): Decider[S, I, O] =
    Decider(builder.initialState, (i, s) => builder.decide(s, i), (o, s) => f(s, o))
}

extension [S, I, O](builder: WithDecideDeciderBuilder[S, I, List[O]]) {
  def evolveList(f: (S, O) => S): Decider[S, I, List[O]] =
    Decider(builder.initialState, (i, s) => builder.decide(s, i), (o, s) => o.foldLeft(s)(f))
}

extension [S, E, I, O](builder: WithDecideDeciderBuilder[S, I, Either[E, List[O]]]) {
  def evolveErrorList(f: (S, O) => S): Decider[S, I, Either[E, List[O]]] =
    Decider(builder.initialState, (i, s) => builder.decide(s, i), (o, s) => o match {
      case Left(value) => s
      case Right(value) => value.foldLeft(s)(f)
    })
}

object DeciderBuilder {
  def seed[S](initialState: S): SeedDeciderBuilder[S] =
    new SeedDeciderBuilder(initialState)
}

extension [S, I, O](decider: Decider[S, I, List[O]]) {
  /** Replay `stream` through `evolve` to advance the decider's initial state. */
  def evolveFrom(stream: List[O]): Decider[S, I, List[O]] =
    Decider(
      state = decider.evolve(stream, decider.state),
      decide = (i, s) => decider.decide(i, s),
      evolve = (o, s) => decider.evolve(o, s)
    )
}

extension [S, E, I, O](decider: Decider[S, I, Either[E, List[O]]]) {
  def absorbEitherToBaseMachine[F[_], EE](using M: MonadError[F, EE], ev: E =:= EE): BaseMachineT[F, I, List[O]] =
    absorbEitherToBaseMachine_(ev)

  def absorbEitherToBaseMachine_[F[_], EE](liftError: E => EE)(using M: MonadError[F, EE]): BaseMachineT[F, I, List[O]] =
    new BaseMachineT[F, I, List[O]] {
      override type State = S
      override def initialState: S = decider.state
      override def action(state: State, input: I): F[(List[O], State)] =
        for {
          o <- M.fromEither(decider.decide(input, state).left.map(liftError))
          ns = decider.evolve(Right(o), state)
        } yield (o, ns)
    }
}
