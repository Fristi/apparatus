package apparatus

import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import zio.blocks.schema.*

import java.util.UUID
import scala.util.NotGiven

/** Postgres-backed [[EventStore]] using `pg_try_advisory_lock` for optimistic aggregate locking. */
object PostgresEventStore extends EventStore[doobie.free.connection.ConnectionIO] {

  /** Uses `pg_try_advisory_lock` keyed on the aggregate UUID hash — non-blocking, session-scoped. */
  override def lockAggregate(aggregateId: UUID): doobie.free.connection.ConnectionIO[Boolean] =
    sql"SELECT pg_try_advisory_lock(hashtext(${aggregateId.toString}))"
      .query[Boolean]
      .unique

  /** Reads all events for `aggregateId` ordered by `sequence_nr` ascending. */
  override def loadAggregateStream[O: Schema](aggregateId: UUID): doobie.free.connection.ConnectionIO[List[EventEntry[O]]] =
    sql"SELECT sequence_nr, body FROM eventstreams WHERE aggregate_id = $aggregateId ORDER BY sequence_nr ASC"
      .query[EventEntry[O]]
      .to[List]

  /** Bulk-inserts `events` into `eventstreams`; returns the total number of rows inserted. */
  override def appendAggregateStream[O: Schema](aggregateId: UUID, events: List[EventEntry[O]]): doobie.free.connection.ConnectionIO[Int] =
    Update[(UUID, EventEntry[O])]("INSERT INTO eventstreams (aggregate_id, sequence_nr, body) VALUES (?, ?, ?)")
      .updateMany(events.map((aggregateId, _)))

  override def create(): doobie.free.connection.ConnectionIO[Int] =
    sql"""
      CREATE TABLE IF NOT EXISTS eventstreams (
        aggregate_id UUID NOT NULL,
        sequence_nr  INT  NOT NULL,
        body         TEXT NOT NULL,
        PRIMARY KEY (aggregate_id, sequence_nr)
      )
    """.update.run


  given [A](using Schema[A], NotGiven[Meta[A]]): Meta[A] =
    Meta[String].tiemap[A](_.fromJson[A].left.map(_.message))(_.toJsonString)
}
