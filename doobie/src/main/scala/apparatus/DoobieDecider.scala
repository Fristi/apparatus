package apparatus

import apparatus.core.*
import doobie.free.connection
import doobie.{Read, Write}
import doobie.free.connection.ConnectionIO

import java.util.UUID

/** An event envelope pairing a monotonically increasing `sequenceNr` with its `body`. */
final case class EventEntry[O](sequenceNr: Int, body: O)

/** Persistence interface for an append-only aggregate event stream. */
trait EventStore[F[_], O]:
  def create(): F[Int]
  /** Acquire an advisory lock for `id`; returns `false` if already held. */
  def lockAggregate(id: UUID): F[Boolean]
  /** Load all stored events for `id` in sequence-number order. */
  def loadAggregateStream(id: UUID): F[List[EventEntry[O]]]
  /** Append `events` to the stream for `id`; returns the row count inserted. */
  def appendAggregateStream(id: UUID, events: List[EventEntry[O]]): F[Int]


extension [S, I, O : {Read, Write}](decider: Decider[S, I, List[O]]) {
  /** Build a transactional [[Apparatus]] for aggregate `id`.
    *
    * Within a single `ConnectionIO` transaction this will:
    *   1. Acquire an advisory lock on `id` (raises on failure).
    *   2. Load and replay the existing event stream to restore state.
    *   3. Return a [[Apparatus.Fresh]] whose `action` decides, appends, and evolves atomically.
    */
  def transactionalDecider(id: UUID): ConnectionIO[Apparatus[ConnectionIO, I, List[O]]] =
    for {
      store = PostgresEventStore[O]()
      acquired <- store.lockAggregate(id)
      _ <- if(!acquired) connection.raiseError(new Throwable("Cannot acquire lock")) else connection.unit
      events <- store.loadAggregateStream(id)
      evolvedDecider = decider.evolveFrom(events.map(_.body))
    } yield {
      val nextSequenceNr = events.maxByOption(_.sequenceNr).map(_.sequenceNr + 1).getOrElse(0)
      val baseMachineT = new BaseMachineT[ConnectionIO, I, List[O]] {
        override type State = S
        override def initialState: S = evolvedDecider.state
        override def action(state: State, input: I): ConnectionIO[(List[O], State)] =
          val o = evolvedDecider.decide(input, state)
          val eventStreamToAppend = o.zipWithIndex.map((o, idx) => EventEntry(nextSequenceNr + idx, o))
          val ns = evolvedDecider.evolve(o, state)

          store.appendAggregateStream(id, eventStreamToAppend).map(_ => (o, ns))
      }

      Apparatus.Fresh(baseMachineT)
    }
}