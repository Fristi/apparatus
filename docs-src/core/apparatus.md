# Apparatus

`Apparatus[F[_], I, O]` is a composable network of state machines running in effect `F`.
Each step runs in `F[O]`, with state threaded automatically through cats-effect `Ref`s.

```scala mdoc
import apparatus.core.*
import apparatus.core.machines.*
import cats.effect.SyncIO
import cats.implicits.*
import zio.blocks.schema.Schema
```

## Building a machine

Wrap a `Decider` with `Apparatus.aggregateMachine` to get a stateful, effect-backed node.
Each command must carry an `id: UUID` so the network can route to the correct per-aggregate `Ref`.

```scala mdoc
import java.util.UUID

enum DoorState { case Closed, Open }
sealed trait DoorCmd { val id: UUID }
object DoorCmd:
  case class Open(id: UUID)  extends DoorCmd
  case class Close(id: UUID) extends DoorCmd
enum DoorEvt derives Schema { case Opened, Closed }

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

## Constructors

| Constructor | When to use |
|-------------|-------------|
| `Apparatus.aggregateMachine(decider, extractId)` | Decider pattern — pure decide/evolve, per-UUID Ref-backed state |
| `Apparatus.closedMealy(m)` | Stateless or self-contained effectful machine |
| `Apparatus.openMealy(m)` | Machine with explicit, externally-threaded state |

```scala mdoc:silent
val stringify: Apparatus[SyncIO, List[DoorEvt], String] =
  Apparatus.closedMealy(ClosedMealy.stateless[SyncIO, List[DoorEvt], String](evts => SyncIO.pure(evts.mkString(","))))
```

## How the tree becomes a graph: normalisation

An `Apparatus` value is a **tree** of `ApparatusF` nodes, built from the smart constructors.
Before execution, the tree is **normalised** into a flat graph:

1. Every `AggregateMachine` node is deduplicated by name into a `NormalizedRegistry`. If the
   same decider appears more than once in the tree, all references share a single compiled
   instance.
2. Every `OpenMachine` node is assigned a stable auto-generated ID and stored in an open-machine
   map. The node is replaced by a `Ref` placeholder in the tree.
3. The resulting tree contains only `Ref`, `ClosedMachine`, and structural combinator nodes.

This normalisation is run automatically inside `Apparatus.run`, `runSteps`, and `runMultiple`
— you never call it directly.

## How deciders are materialised

Once normalised, `alg.compile` walks the tree and allocates runtime state:

- Each `AggregateMachine` entry in the registry is materialised by the `DeciderMaterializer`
  you supply (see [Decider](./decider.md)). The materialiser allocates a `Ref` (or loads from
  a database) and returns a `ClosedMealy`.
- Each `OpenMachine` entry gets its state wrapped in a `Ref`, also becoming a `ClosedMealy`.
- `ClosedMachine` nodes are used directly.

After compilation the network is a tree of `ClosedMealy`s wired by the structural combinators,
run step-by-step via `network.run(input)`.

## Running a machine

`Apparatus.run` and `Apparatus.runA` execute a single step. `Apparatus.runMultiple` folds over a
collection combining outputs via `Monoid`. State is threaded via `Ref`s allocated at compile time
by the `DeciderMaterializer`.

```scala mdoc
val mat = DeciderMaterializer.syncIO
val doorId = UUID.fromString("00000000-0000-0000-0000-000000000001")

val evts: List[DoorEvt] = Apparatus.run(doorFsm, DoorCmd.Open(doorId), mat).unsafeRunSync()

val allEvts: List[DoorEvt] = Apparatus.runMultiple(doorFsm, List(DoorCmd.Open(doorId), DoorCmd.Close(doorId), DoorCmd.Open(doorId)), mat).unsafeRunSync()
```

`runSteps` returns each step's output in a `List` rather than combining them.

## Combinators

### Sequential — `>>>` / `andThen`

Pipe output of the left machine directly into the right machine's input each step.

```
A ──► [left: A→B] ──► B ──► [right: B→C] ──► C
```

```scala mdoc:silent
val pipeline: Apparatus[SyncIO, DoorCmd, String] =
  doorFsm >>> Apparatus.closedMealy(ClosedMealy.stateless[SyncIO, List[DoorEvt], String](evts => SyncIO.pure(evts.mkString(","))))
```

### Parallel — `***` / `par`

Run two independent machines on the two halves of a pair simultaneously.

```
(A, C) ──► [left: A→B] × [right: C→D] ──► (B, D)
```

```scala mdoc:silent
val paired: Apparatus[SyncIO, (DoorCmd, DoorCmd), (List[DoorEvt], List[DoorEvt])] =
  doorFsm *** doorFsm
```

### Alternative — `|||` / `or`

Route `Either`-typed input to the matching machine; the other machine keeps its state untouched.

```
Left(A)  ──► [left:  A→B] ──► Left(B)
Right(C) ──► [right: C→D] ──► Right(D)
```

```scala mdoc:silent
val router: Apparatus[SyncIO, Either[DoorCmd, DoorCmd], Either[List[DoorEvt], List[DoorEvt]]] =
  doorFsm ||| doorFsm
```

### Feedback — `feedback`

Closes a bidirectional loop between two machines.

```
       ┌──────────────────────────────────┐
A ──►  [left: A → N[B]]  ──► N[B] output │
       each B fed into ──►                │
       [right: B → N[A]] ──► new A queue ─┘
```

Given initial input `a`:
1. `left` consumes `a`, emits `N[B]`.
2. Each `B` is fed into `right`, which emits `N[A]`.
3. New `A` values are queued and processed recursively until no more `A`s are produced.
4. All `N[B]` accumulated across iterations is returned.

Both machines carry independent state across iterations. `N` must have `Foldable` and `Monoid`
instances (typically `List`).

```scala mdoc:silent
val echo: Apparatus[SyncIO, DoorEvt, List[DoorCmd]] =
  Apparatus.closedMealy(ClosedMealy.stateless[SyncIO, DoorEvt, List[DoorCmd]] {
    case DoorEvt.Opened => SyncIO.pure(List(DoorCmd.Close(doorId)))
    case DoorEvt.Closed => SyncIO.pure(Nil)
  })

val loop: Apparatus[SyncIO, DoorCmd, List[DoorEvt]] =
  doorFsm.feedback(echo)
```

### `lmapOrEmpty`

Contramap via a **partial function**. When the function is undefined for the given input the
machine's state is **not advanced** and `Monoid.empty` is returned for the output.

```scala mdoc:silent
val openOnly: Apparatus[SyncIO, Option[DoorCmd], List[DoorEvt]] =
  doorFsm.lmapOrEmpty { case Some(cmd: DoorCmd.Open) => cmd }
```

This combinator is the key primitive for fanout networks: each sub-machine filters only the
events it cares about (analogous to `SemigroupK` / `<+>` in http4s routing).

```scala mdoc:silent
enum ServiceEvt:
  case ForFlight(id: String)
  case ForHotel(id: String)
  case ForCar(id: String)

val flightMachine: Apparatus[SyncIO, ServiceEvt, List[String]] =
  Apparatus.closedMealy(ClosedMealy.stateless[SyncIO, String, List[String]](s => SyncIO.pure(List(s + "-ack"))))
    .lmapOrEmpty { case ServiceEvt.ForFlight(id) => id }
```

### `merge`

Run **two machines on the same input** and combine their outputs via `Monoid`. Both machines
advance their state independently on every step.

```
         ┌──► [left:  A→B] ──► B ─┐
A ───────┤                        ├──► combine(B, B)
         └──► [right: A→B] ──► B ─┘
```

`merge` is the natural companion to `lmapOrEmpty`: each sub-machine is silent for events it
doesn't own, and `merge` fans the same event to all of them, collecting the one non-empty result.

```scala mdoc:silent
val hotelMachine: Apparatus[SyncIO, ServiceEvt, List[String]] =
  Apparatus.closedMealy(ClosedMealy.stateless[SyncIO, String, List[String]](s => SyncIO.pure(List(s + "-ack"))))
    .lmapOrEmpty { case ServiceEvt.ForHotel(id) => id }

val carMachine: Apparatus[SyncIO, ServiceEvt, List[String]] =
  Apparatus.closedMealy(ClosedMealy.stateless[SyncIO, String, List[String]](s => SyncIO.pure(List(s + "-ack"))))
    .lmapOrEmpty { case ServiceEvt.ForCar(id) => id }

val services: Apparatus[SyncIO, ServiceEvt, List[String]] =
  flightMachine.merge(carMachine.merge(hotelMachine))
```

## Input / output adapters

| Combinator | What it does |
|------------|-------------|
| `lmap(f)` | Contramap input: `f: C => A` applied before every step |
| `rmap(f)` | Map output: `f: B => C` applied after every step |
| `dimap(f)(g)` | `lmap` and `rmap` in one call |
| `imap` | Bidirectional via `Iso[A, A2]` and `Iso[B, B2]` |
| `lmapOrEmpty(pf)` | Partial contramap; silent when undefined |
| `first` | Thread `_2` of a pair through unchanged; run machine on `_1` |
| `second` | Thread `_1` of a pair through unchanged; run machine on `_2` |
| `tap(side)` | Run `side` for effects; pass `left`'s output through unchanged |

## Patterns

### Projections

A **projection** is a read-model machine chained after an aggregate with `>>>`. Because the
projection runs inside the same compiled network, its update and the aggregate's event emission
happen in the same transaction (when using a database materialiser).

```
[aggregate] ──► List[Event] ──► [projection] ──► read-model update
```

```scala mdoc:silent
val projection: Apparatus[SyncIO, List[DoorEvt], List[String]] =
  Apparatus.closedMealy(ClosedMealy.stateless[SyncIO, List[DoorEvt], List[String]] { evts =>
    SyncIO.pure(evts.map(_.toString))
  })

val doorWithProjection: Apparatus[SyncIO, DoorCmd, List[String]] =
  doorFsm >>> projection
```

### Sagas

A **saga** uses the `feedback` combinator to create a policy loop: an orchestrator machine
emits events that drive sub-machines, which respond with commands that the orchestrator
processes. The loop runs to quiescence in one step.

See [Saga](./saga.md) for the full saga pattern, `SagaBehavior`, and the booking example.

## Type class instances

`Apparatus` ships instances for the standard Cats profunctor hierarchy:

| Instance | What it enables |
|----------|----------------|
| `Category` | `id` + `compose` (= `andThen`) |
| `Profunctor` | `lmap` / `rmap` / `dimap` |
| `Strong` | `first` / `second` |
| `Choice` | `left` / `right` |
