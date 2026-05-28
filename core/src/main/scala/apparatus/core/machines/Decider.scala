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
 * @param name   aggregate name (used for routing and Mermaid diagrams)
 * @param state  starting state of the machine
 * @param decide pure function `(input, state) => output`
 * @param evolve pure function `(output, state) => newState`
 */
final case class Decider[S, I, O](name: String, state: S, decide: (I, S) => O, evolve: (O, S) => S) { self =>

  /** Converts this `Decider` into a pure [[OpenMealy]] running in `cats.Id`.
    *
    * The resulting machine's `State` type is refined to `S` and its `initialState` matches
    * [[state]].  Use this to embed the decider in an `Apparatus` via `Apparatus.openMealy`.
    */
  def toOpenMealy: OpenMealy[Id, I, O] {type State = S} =
    new OpenMealy[Id, I, O]:
      override type State = S
      override def initialState: S = self.state
      override def action(state: State, input: I): Id[(O, State)] =
        val o = self.decide(input, state)
        val ns = self.evolve(o, state)
        (o, ns)
}

/** First stage of the [[DeciderBuilder]] fluent DSL.
  *
  * Holds the aggregate `name` and `initialState`; provides three ways to add the `decide`
  * function: total (`decide`), partial-list (`partiallyDecide`), or fallible (`withError`).
  */
final class SeedDeciderBuilder[S] private[core] (val name: String, val initialState: S) {

  /** Switches to the fallible builder path where an invalid command yields `Left(invalidCommand)`. */
  def withError[E](invalidCommand: E): FallibleDeciderBuilder[S, E] =
    new FallibleDeciderBuilder(name, initialState, invalidCommand)

  /** Supplies a total `decide` function `(state, input) => output`. */
  def decide[I, O](decide: (S, I) => O): WithDecideDeciderBuilder[S, I, O] =
    new WithDecideDeciderBuilder(name, initialState, decide)

  /** Supplies a partial `decide` function; unmatched `(state, input)` pairs yield `Nil`. */
  def partiallyDecide[I, O](decide: PartialFunction[(S, I), List[O]]): WithDecideDeciderBuilder[S, I, List[O]] =
    new WithDecideDeciderBuilder(name, initialState, (s, i) => decide.applyOrElse((s, i), _ => Nil))
}

/** Builder stage that adds an `invalidCommand` fallback to a partial `decide` function.
  *
  * Unmatched commands yield `Left(invalidCommand)` so the aggregate can signal rejection.
  */
final class FallibleDeciderBuilder[S, E] private[core] (name: String, initialState: S, invalidCommand: E) {

  /** Supplies a partial `decide` function; unmatched inputs yield `Left(invalidCommand)`. */
  def partiallyDecide[I, O](decide: PartialFunction[(I, S), Either[E, O]]): WithDecideDeciderBuilder[S, I, Either[E, O]] =
    new WithDecideDeciderBuilder(name, initialState, (i, s) => decide.applyOrElse((s, i), _ => Left(invalidCommand)))
}

/** Intermediate builder that holds the `decide` function and awaits an `evolve` function
  * to produce the final [[Decider]].
  */
final class WithDecideDeciderBuilder[S, I, O] private [core] (val name: String, val initialState: S, val decide: (S, I) => O)

/** Finalises a builder whose output is a single value `O` (not a list). */
extension [S, I, O](builder: WithDecideDeciderBuilder[S, I, O]) {
  /** Builds a `Decider` using `f` to fold a single output into the state. */
  def evolveSingle(f: (S, O) => S): Decider[S, I, O] =
    Decider(builder.name, builder.initialState, (i, s) => builder.decide(s, i), (o, s) => f(s, o))
}

/** Finalises a builder whose output is `List[O]`. */
extension [S, I, O](builder: WithDecideDeciderBuilder[S, I, List[O]]) {
  /** Builds a `Decider` by left-folding each emitted event through `f`. */
  def evolveList(f: (S, O) => S): Decider[S, I, List[O]] =
    Decider(builder.name, builder.initialState, (i, s) => builder.decide(s, i), (o, s) => o.foldLeft(s)(f))
}

/** Finalises a builder whose output is `Either[E, List[O]]`. */
extension [S, E, I, O](builder: WithDecideDeciderBuilder[S, I, Either[E, List[O]]]) {
  /** Builds a `Decider`: on `Right(events)` folds each event through `f`; on `Left` leaves state unchanged. */
  def evolveErrorList(f: (S, O) => S): Decider[S, I, Either[E, List[O]]] =
    Decider(builder.name, builder.initialState, (i, s) => builder.decide(s, i), (o, s) => o match {
      case Left(value) => s
      case Right(value) => value.foldLeft(s)(f)
    })
}

/** Entry point for the [[Decider]] fluent builder DSL. */
object DeciderBuilder {
  /** Creates a new builder seeded with `name` and `initialState`. */
  def seed[S](name: String, initialState: S): SeedDeciderBuilder[S] =
    new SeedDeciderBuilder(name, initialState)
}

extension [S, I, O](decider: Decider[S, I, List[O]]) {
  /** Replay `stream` through `evolve` to advance the decider's initial state. */
  def evolveFrom(stream: List[O]): Decider[S, I, List[O]] =
    Decider(
      name   = decider.name,
      state  = decider.evolve(stream, decider.state),
      decide = (i, s) => decider.decide(i, s),
      evolve = (o, s) => decider.evolve(o, s)
    )
}



/** Compiles a [[Decider]] into a stateful [[ClosedMealy]] for a specific aggregate instance.
  *
  * The materializer is responsible for allocating the mutable state carrier (typically a
  * `cats.effect.kernel.Ref`) and wiring it to `decide` and `evolve`.  Different
  * implementations can target different effect stacks or persistence backends.
  *
  * @tparam F the effect type the compiled machine runs in
  */
trait DeciderMaterializer[F[_]]:
  /** Compiles `decider` into a [[ClosedMealy]] for the given `aggregateId`.
    *
    * The resulting machine holds its own `Ref[F, S]` and applies `decide` then `evolve` on
    * each call to `action`, making it safe to call concurrently if `F` is concurrent-safe.
    *
    * @param decider     the pure decider to compile
    * @param aggregateId the UUID of the aggregate instance (used for loading persisted state)
    * @tparam S state type
    * @tparam I input / command type
    * @tparam O event type; must have a [[zio.blocks.schema.Schema]] for serialisation
    */
  def materialize[S, I, O: Schema](
    decider:     Decider[S, I, List[O]],
    aggregateId: java.util.UUID
  ): F[ClosedMealy[F, I, List[O]]]

object DeciderMaterializer:
  /** A `DeciderMaterializer` that runs in `cats.effect.SyncIO`.
    *
    * Allocates an in-memory `Ref[SyncIO, S]` per aggregate instance; state is not persisted
    * across process restarts.  Suitable for tests and in-memory use cases.
    */
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