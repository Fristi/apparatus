package apparatus

import apparatus.core.*
import apparatus.core.machines.{ClosedMealy, Decider, DeciderMaterializer}
import doobie.free.connection
import doobie.free.connection.ConnectionIO
import zio.blocks.schema.*

import java.util.UUID

/** An event envelope pairing a monotonically increasing `sequenceNr` with its `body`. */
final case class EventEntry[O](sequenceNr: Int, body: O)

/** Persistence interface for an append-only aggregate event stream. */
trait EventStore[F[_]]:
  def create(): F[Int]
  /** Acquire an advisory lock for `aggregateId`; returns `false` if already held. */
  def lockAggregate(aggregateId: UUID): F[Boolean]
  /** Load all stored events for `aggregateId` in sequence-number order. */
  def loadAggregateStream[O: Schema](aggregateId: UUID): F[List[EventEntry[O]]]
  /** Append `events` to the stream for `aggregateId`; returns the row count inserted. */
  def appendAggregateStream[O: Schema](aggregateId: UUID, events: List[EventEntry[O]]): F[Int]

object EventStore:
  def deciderMaterializer(eventStore: EventStore[ConnectionIO]): DeciderMaterializer[ConnectionIO] =
    new DeciderMaterializer[ConnectionIO]:
      override def materialize[S, I, O: Schema](decider: Decider[S, I, List[O]], aggregateId: UUID): ConnectionIO[ClosedMealy[ConnectionIO, I, List[O]]] =
        eventStore.lockAggregate(aggregateId).flatMap { acquired =>
          if !acquired then connection.raiseError(new Throwable(s"Cannot acquire lock for $aggregateId"))
          else connection.pure {
            new ClosedMealy[ConnectionIO, I, List[O]]:
              override def action(input: I): ConnectionIO[List[O]] =
                for
                  events   <- eventStore.loadAggregateStream[O](aggregateId)
                  ns        = decider.evolve(events.sortBy(_.sequenceNr).map(_.body), decider.state)
                  o         = decider.decide(input, ns)
                  nextSeqNr = events.maxByOption(_.sequenceNr).map(_.sequenceNr + 1).getOrElse(0)
                  toAppend  = o.zipWithIndex.map((ev, idx) => EventEntry(nextSeqNr + idx, ev))
                  _        <- eventStore.appendAggregateStream[O](aggregateId, toAppend)
                yield o
          }
        }
