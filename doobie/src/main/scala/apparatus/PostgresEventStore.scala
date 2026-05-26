package apparatus

import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import zio.blocks.schema.*

import java.util.UUID
import scala.util.NotGiven

/** Postgres-backed [[EventStore]] using `pg_try_advisory_lock` for optimistic aggregate locking. */
object PostgresEventStore extends EventStore[ConnectionIO] {

  /** Uses `pg_try_advisory_lock` keyed on `hashtext(id)` — non-blocking, session-scoped. */
  override def lockAggregate(networkId: String, aggregateId: UUID): ConnectionIO[Boolean] = {
    val compositeId = s"${networkId}-${aggregateId.toString}"
    sql"SELECT pg_try_advisory_lock(hashtext($compositeId))"
      .query[Boolean]
      .unique
  }

  /** Reads all events for `id` ordered by `sequence_nr` ascending. */
  override def loadAggregateStream[O : Schema](networkId: String, aggregateId: UUID): ConnectionIO[List[EventEntry[O]]] =
    sql"SELECT sequence_nr, body FROM eventstreams WHERE network_id = $networkId AND aggregate_id = $aggregateId ORDER BY sequence_nr ASC"
      .query[EventEntry[O]]
      .to[List]

  /** Bulk-inserts `events` into `eventstreams`; returns the total number of rows inserted. */
  override def appendAggregateStream[O : Schema](networkId: String, aggregateId: UUID, events: List[EventEntry[O]]): ConnectionIO[Int] =
    Update[(String, UUID, EventEntry[O])]("INSERT INTO eventstreams (network_id, aggregate_id, sequence_nr, body) VALUES (?, ?, ?, ?)")
      .updateMany(events.map((networkId, aggregateId, _)))

  override def create(): ConnectionIO[Int] =
    sql"""
                 CREATE TABLE IF NOT EXISTS eventstreams (
                   network_id   TEXT NOT NULL,
                   aggregate_id UUID NOT NULL,
                   sequence_nr  INT  NOT NULL,
                   body         TEXT NOT NULL,
                   PRIMARY KEY (network_id, aggregate_id, sequence_nr)
                 )
               """.update.run


  given [A](using Schema[A], NotGiven[Meta[A]]): Meta[A] =
    Meta[String].tiemap[A](_.fromJson[A].left.map(_.message))(_.toJsonString)
}
