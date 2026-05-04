package apparatus

import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*

import java.util.UUID

/** Postgres-backed [[EventStore]] using `pg_try_advisory_lock` for optimistic aggregate locking. */
case class PostgresEventStore[O : {Read, Write}]() extends EventStore[ConnectionIO, O] {

  /** Uses `pg_try_advisory_lock` keyed on `hashtext(id)` — non-blocking, session-scoped. */
  override def lockAggregate(id: UUID): ConnectionIO[Boolean] =
    sql"SELECT pg_try_advisory_lock(hashtext(CAST($id AS text)))"
      .query[Boolean]
      .unique

  /** Reads all events for `id` ordered by `sequence_nr` ascending. */
  override def loadAggregateStream(id: UUID): ConnectionIO[List[EventEntry[O]]] =
    sql"SELECT sequence_nr, body FROM eventstreams WHERE aggregate_id = $id ORDER BY sequence_nr ASC"
      .query[EventEntry[O]]
      .to[List]

  /** Bulk-inserts `events` into `eventstreams`; returns the total number of rows inserted. */
  override def appendAggregateStream(id: UUID, events: List[EventEntry[O]]): ConnectionIO[Int] =
    Update[(UUID, EventEntry[O])]("INSERT INTO eventstreams (aggregate_id, sequence_nr, body) VALUES (?, ?, ?)")
      .updateMany(events.map((id, _)))
}
