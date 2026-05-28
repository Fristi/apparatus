# Decider

`Decider` implements the **Decider pattern** — a pure model of aggregate behaviour commonly used
in event-sourced systems. It separates two concerns:

| Function | Signature | Role |
|----------|-----------|------|
| `decide` | `(I, S) => O` | Maps a command and the current state to an output (usually a list of events) |
| `evolve` | `(O, S) => S` | Folds the output back into a new state |

Because both functions are **pure**, they are trivially testable in isolation and composable with
any effect stack via `aggregateMachine` or `toOpenMealy`.

```scala mdoc
import apparatus.core.*
import apparatus.core.machines.*
import cats.implicits.*
```

## Relationship to event sourcing

In a typical event-sourced aggregate:

1. A **command** arrives.
2. `decide` validates it against the current state and emits zero or more **events**.
3. `evolve` folds each event into a new state — exactly the same function used during replay.

```
Command ──► decide(cmd, state) ──► [Event, …]
                                        │
                                        ▼
                              evolve(event, state) ──► newState
```

`decide` never mutates state. `evolve` never needs to know about commands. This strict
separation means you can reconstruct any aggregate by replaying its event log through `evolve`
alone, and you can validate any command without touching persistence.

## Building a Decider

Use `DeciderBuilder` for a fluent construction API:

```scala mdoc
enum LightState { case Off, On }
enum LightCmd   { case TurnOn, TurnOff }
enum LightEvt   { case TurnedOn, TurnedOff }

val light: Decider[LightState, LightCmd, List[LightEvt]] =
  DeciderBuilder
    .seed("light", LightState.Off)
    .decide[LightCmd, List[LightEvt]] { (state, cmd) =>
      (state, cmd) match
        case (LightState.Off, LightCmd.TurnOn)  => List(LightEvt.TurnedOn)
        case (LightState.On,  LightCmd.TurnOff) => List(LightEvt.TurnedOff)
        case _                                   => Nil
    }
    .evolveList { (state, evt) =>
      (state, evt) match
        case (LightState.Off, LightEvt.TurnedOn)  => LightState.On
        case (LightState.On,  LightEvt.TurnedOff) => LightState.Off
        case _                                     => state
    }
```

### Builder variants

| Method | Use when |
|--------|----------|
| `decide(f)` | Total function `(S, I) => O` |
| `partiallyDecide(pf)` | Partial function on `(S, I)`; returns `Nil` for unmatched cases |
| `withError(invalid).partiallyDecide(pf)` | Partial function returning `Either[E, O]`; returns `Left(invalid)` for unmatched cases |
| `evolveSingle(f)` | Finalise with `(S, O) => S` — output is a single value |
| `evolveList(f)` | Finalise with `(S, O) => S` — output is `List[O]`, folds each element |
| `evolveErrorList(f)` | Finalise with `(S, O) => S` — output is `Either[E, List[O]]`, folds `Right` events only |

## Testing decide and evolve in isolation

Because both functions are pure you can call them directly without any Apparatus machinery:

```scala mdoc
val evts1 = light.decide(LightCmd.TurnOn,  LightState.Off)
val evts2 = light.decide(LightCmd.TurnOn,  LightState.On)   // guard: already on
val evts3 = light.decide(LightCmd.TurnOff, LightState.On)

val s1 = light.evolve(List(LightEvt.TurnedOn),  LightState.Off)
val s2 = light.evolve(Nil,                       LightState.Off)
```

## Replaying history — evolveFrom

`evolveFrom` advances a `Decider`'s initial state by replaying a list of stored events. Use
this to reconstruct aggregate state before processing a new command, without a materialiser:

```scala mdoc:silent
val history: List[LightEvt] = List(LightEvt.TurnedOn, LightEvt.TurnedOff, LightEvt.TurnedOn)

// light at its natural initial state → fast-forwarded to after replay
val rehydrated = light.evolveFrom(history)
// rehydrated.state == LightState.On
```

## Converting to an OpenMealy

`toOpenMealy` lifts the `Decider` into a pure `OpenMealy[Id, I, O]`. This is useful for
running the machine in test code without any effect stack, or when you need to compose it
with other `OpenMealy` machines before wrapping in an `Apparatus`.

```scala mdoc:silent
import cats.Id

val lightMealy: OpenMealy[Id, LightCmd, List[LightEvt]] = light.toOpenMealy
```

## Lifting into an Apparatus

### In-memory: aggregateMachine

`Apparatus.aggregateMachine` routes inputs to per-UUID instances of the decider. On first
access for a given ID, the `DeciderMaterializer` is called to allocate state storage. On
subsequent calls the cached machine is reused.

```scala mdoc:silent
import apparatus.core.machines.*
import cats.effect.SyncIO
import zio.blocks.schema.Schema
import java.util.UUID

enum DoorEvt derives Schema { case Opened, Closed }
enum DoorState { case Closed, Open }

sealed trait DoorCmd { val id: UUID }
object DoorCmd:
  case class Open(id: UUID)  extends DoorCmd
  case class Close(id: UUID) extends DoorCmd

val door: Decider[DoorState, DoorCmd, List[DoorEvt]] =
  DeciderBuilder.seed("door", DoorState.Closed)
    .partiallyDecide[DoorCmd, DoorEvt]:
      case (DoorState.Closed, _: DoorCmd.Open)  => List(DoorEvt.Opened)
      case (DoorState.Open,   _: DoorCmd.Close) => List(DoorEvt.Closed)
    .evolveList:
      case (DoorState.Closed, DoorEvt.Opened) => DoorState.Open
      case (DoorState.Open,   DoorEvt.Closed) => DoorState.Closed
      case (s, _)                              => s

val doorFsm: Apparatus[SyncIO, DoorCmd, List[DoorEvt]] =
  Apparatus.aggregateMachine(door, _.id)
```

## How materialization works

`DeciderMaterializer` is the bridge between a pure `Decider` and a stateful `ClosedMealy`.

```scala
trait DeciderMaterializer[F[_]]:
  def materialize[S, I, O: Schema](
    decider:     Decider[S, I, List[O]],
    aggregateId: UUID
  ): F[ClosedMealy[F, I, List[O]]]
```

Different implementations give different durability guarantees.

### In-memory materializer

`DeciderMaterializer.syncIO` allocates a `Ref[SyncIO, S]` per aggregate, initialised from
`decider.state`. State is lost when the process restarts. Good for tests and demos.

```scala mdoc:silent
val mat    = DeciderMaterializer.syncIO
val doorId = UUID.fromString("00000000-0000-0000-0000-000000000001")

val events: List[DoorEvt] =
  Apparatus.run(doorFsm, DoorCmd.Open(doorId), mat).unsafeRunSync()
```

### Database-backed materializer (doobie)

The `apparatus-doobie` module provides `EventStore.deciderMaterializer`, which runs in
`ConnectionIO`:

1. **Lock** the aggregate row with `pg_try_advisory_lock` to prevent concurrent writes.
2. **Load** all stored events from the `eventstreams` table.
3. **Reconstruct** state by replaying events through `evolve`.
4. **Decide** on the new command.
5. **Append** new events to the table.
6. Return the new events.

This gives you **strong consistency**: state reconstruction and event append happen inside the
same database transaction. See [Doobie](../integrations/doobie.md) for the full setup.

```scala
// In the doobie module:
val dbMat: DeciderMaterializer[ConnectionIO] =
  EventStore.deciderMaterializer(PostgresEventStore)
```

The `Apparatus` topology is identical whether you use the in-memory or the database
materializer — only the `DeciderMaterializer` passed to `Apparatus.run` (or `runSteps`)
changes.
