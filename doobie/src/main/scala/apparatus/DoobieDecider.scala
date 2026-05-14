package apparatus

import apparatus.core.*
import doobie.free.connection
import doobie.free.connection.ConnectionIO
import zio.blocks.schema.*

import java.util.UUID

/** An event envelope pairing a monotonically increasing `sequenceNr` with its `body`. */
final case class EventEntry[O](sequenceNr: Int, body: O)

/** Persistence interface for an append-only aggregate event stream. */
trait EventStore[F[_]]:
  def create(): F[Int]
  /** Acquire an advisory lock for `id`; returns `false` if already held. */
  def lockAggregate(networkId: String, aggregateId: UUID): F[Boolean]
  /** Load all stored events for `id` in sequence-number order. */
  def loadAggregateStream[O: Schema](networkId: String, aggregateId: UUID): F[List[EventEntry[O]]]
  /** Append `events` to the stream for `id`; returns the row count inserted. */
  def appendAggregateStream[O: Schema](networkId: String, aggregateId: UUID, events: List[EventEntry[O]]): F[Int]

object EventStore {
  def deciderMaterializer(eventStore: EventStore[ConnectionIO], aggregateId: UUID): DeciderMaterializer[ConnectionIO] =
    new DeciderMaterializer[ConnectionIO] {
      override def materialize[S, I, O: Schema](apparatus: Decider[S, I, List[O]], networkId: String): ConnectionIO[BaseMachineT[ConnectionIO, I, List[O]]] =
        for {
          acquired <- eventStore.lockAggregate(networkId, aggregateId)
          _ <- if (!acquired) connection.raiseError(new Throwable("Cannot acquire lock")) else connection.unit
          events <- eventStore.loadAggregateStream(networkId, aggregateId)
          evolvedDecider = apparatus.evolveFrom(events.map(_.body))
        } yield {
          val nextSequenceNr = events.maxByOption(_.sequenceNr).map(_.sequenceNr + 1).getOrElse(0)

          new BaseMachineT[ConnectionIO, I, List[O]] {
            override type State = S
            override def initialState: S = evolvedDecider.state
            override def action(state: State, input: I): ConnectionIO[(List[O], State)] =
              val o = evolvedDecider.decide(input, state)
              val eventStreamToAppend = o.zipWithIndex.map((o, idx) => EventEntry(nextSequenceNr + idx, o))
              val ns = evolvedDecider.evolve(o, state)

              eventStore.appendAggregateStream(networkId, aggregateId, eventStreamToAppend).map(_ => (o, ns))
          }
        }
    }
}
