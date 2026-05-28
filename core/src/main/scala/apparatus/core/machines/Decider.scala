package apparatus.core.machines

import cats.Id
import cats.effect.SyncIO
import cats.effect.kernel.Ref
import zio.blocks.schema.Schema

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
  def toOpenMealy: OpenMealy[Id, I, O] {type State = S} =
    new OpenMealy[Id, I, O]:
      override type State = S
      override def initialState: S = self.state
      override def action(state: State, input: I): Id[(O, State)] =
        val o = self.decide(input, state)
        val ns = self.evolve(o, state)
        (o, ns)
  
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



trait DeciderMaterializer[F[_]]:
  def materialize[S, I, O: Schema](
    decider:     Decider[S, I, List[O]],
    aggregateId: java.util.UUID
  ): F[ClosedMealy[F, I, List[O]]]

object DeciderMaterializer:
  val syncIO: DeciderMaterializer[SyncIO] = new DeciderMaterializer[SyncIO]:
    override def materialize[S, I, O: Schema](decider: Decider[S, I, List[O]], aggregateId: java.util.UUID): SyncIO[ClosedMealy[SyncIO, I, List[O]]] =
      Ref[SyncIO].of(decider.state).map { ref =>
        new ClosedMealy[SyncIO, I, List[O]]:
          def action(input: I): SyncIO[List[O]] =
            ref.get.flatMap { s =>
              val o  = decider.decide(input, s)
              val ns = decider.evolve(o, s)
              ref.set(ns).as(o)
            }
      }