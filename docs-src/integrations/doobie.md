# Doobie Integration

The `apparatus-doobie` module provides a `DeciderMaterializer[ConnectionIO]` backed by a
Postgres event store. Using it, every `Apparatus.run` call becomes a single database
transaction: load state, decide, append events, update projections — all atomically.

```scala
libraryDependencies += "io.github.fristi" %% "apparatus-doobie" % "<VERSION>"
```

## The event store table

`PostgresEventStore.create()` creates the `eventstreams` table:

```sql
CREATE TABLE IF NOT EXISTS eventstreams (
  aggregate_id UUID NOT NULL,
  sequence_nr  INT  NOT NULL,
  body         TEXT NOT NULL,
  PRIMARY KEY (aggregate_id, sequence_nr)
)
```

Each row is one event. `body` stores the event as JSON (via `zio-blocks-schema`).
`sequence_nr` is a monotonically increasing per-aggregate counter used for ordered replay.

## Materializer strategy

`EventStore.deciderMaterializer(PostgresEventStore)` returns a
`DeciderMaterializer[ConnectionIO]` that follows this strategy per aggregate on every `action` call:

1. **Lock** — `pg_try_advisory_lock(hashtext(aggregateId))` acquires a session-scoped advisory
   lock. If already held (concurrent write), the call raises immediately.
2. **Load** — all events for `aggregateId` are loaded in `sequence_nr` order.
3. **Reconstruct** — state is rebuilt by folding events through `decider.evolve`.
4. **Decide** — `decider.decide(input, reconstructedState)` produces new events.
5. **Append** — new events are inserted with the next available `sequence_nr` values.
6. **Return** — the new events are returned to the caller.

Steps 1–6 run inside the same `ConnectionIO` transaction, giving you **strong consistency**:
no dirty reads, no lost updates.

```scala
import apparatus.*
import doobie.free.connection.ConnectionIO

val dbMat: DeciderMaterializer[ConnectionIO] =
  EventStore.deciderMaterializer(PostgresEventStore)
```

## Running a network in ConnectionIO

`Apparatus` is parameterised on `F[_]`, so an `Apparatus[ConnectionIO, I, O]` runs entirely
within a single `ConnectionIO` transaction. Build the network exactly as you would for any
other effect:

```scala
import apparatus.core.*
import apparatus.core.machines.*
import apparatus.examples.*
import doobie.free.connection.ConnectionIO
import java.util.UUID

// The bankAccount decider lives in apparatus.examples
val bankFsm: Apparatus[ConnectionIO, BankAccountCommand, Either[BankAccountError, List[BankAccountEvent]]] =
  Apparatus.aggregateMachineE(bankAccount, _.id)
```

Then run it through a doobie `Transactor`:

```scala
import cats.effect.IO
import doobie.util.transactor.Transactor

val xa: Transactor[IO] = ??? // your transactor

val result: IO[Either[BankAccountError, List[BankAccountEvent]]] =
  xa.trans.apply(
    Apparatus.run(bankFsm, BankAccountCommand.Open(UUID.randomUUID(), java.time.Instant.now()), dbMat)
  )
```

## Projections inside the same transaction

Chain a projection after the aggregate with `>>>`. Because both machines run in the same
`ConnectionIO` transaction, the projection update is committed atomically with the events:

```scala
import apparatus.examples.*
import doobie.free.connection.ConnectionIO

val withProjection: Apparatus[ConnectionIO, BankAccountCommand, Int] =
  Apparatus.aggregateMachineE[ConnectionIO, BankAccountState, BankAccountCommand, BankAccountError, BankAccountEvent](bankAccount, _.id)
    .rmap {
      case Right(evts) => evts
      case Left(_)     => Nil
    }
    >>> transactionsProjection(DoobieBankAccountTransactionRepository)
```

`transactionsProjection` inserts one row per `Deposited` or `Withdrawn` event. Everything —
aggregate state, new events, and projection rows — lands in a single `COMMIT`.

## mapK: mixing transactional and non-transactional parts

Sometimes only part of the network needs to be transactional. Use `cats.arrow.FunctionK`
(i.e. a natural transformation `ConnectionIO ~> IO`) to lift a `ConnectionIO` sub-network
into `IO`:

```scala
import cats.effect.IO
import cats.~>
import doobie.free.connection.ConnectionIO
import doobie.util.transactor.Transactor

val xa: Transactor[IO] = ???

// Natural transformation: run each ConnectionIO step as its own transaction
val toIO: ConnectionIO ~> IO = xa.trans

// If ClosedMealy exposed mapK you would use it here directly;
// for now wrap via Apparatus.closedMealy with a lifted machine:
val nonTransactionalPart: Apparatus[IO, BankAccountCommand, Either[BankAccountError, List[BankAccountEvent]]] =
  Apparatus.closedMealy(new ClosedMealy[IO, BankAccountCommand, Either[BankAccountError, List[BankAccountEvent]]]:
    private val inner = Apparatus.aggregateMachineE[ConnectionIO, BankAccountState, BankAccountCommand, BankAccountError, BankAccountEvent](bankAccount, _.id)
    private val compiled = toIO(Apparatus.runA(inner, ???, dbMat)) // illustrative
    def action(input: BankAccountCommand): IO[Either[BankAccountError, List[BankAccountEvent]]] =
      toIO(Apparatus.runA(inner, input, dbMat))
  )
```

In practice, keep the transactional sub-network in `ConnectionIO` and lift the result at the
boundary of your HTTP handler or stream consumer.

## Transactions table (supplementary)

The `BankAccount` example also creates a `transactions` table for the projection:

```sql
CREATE TABLE IF NOT EXISTS transactions (
  aggregate_id     UUID        NOT NULL,
  transaction_type TEXT        NOT NULL,
  amount           NUMERIC     NOT NULL,
  at               TIMESTAMPTZ NOT NULL
)
```

`DoobieBankAccountTransactionRepository` implements inserts and queries against this table.
