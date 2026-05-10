# Apparatus

`Apparatus[F[_], I, O]` is a composable, immutable finite-state machine running in effect `F`. Every
call to `runWith` returns the output **and** a new `Apparatus` carrying updated internal state, making
the whole structure purely functional and replayable.

```scala mdoc
import apparatus.core.*
import cats.Id
import cats.implicits.*
```

## Building a machine

The simplest way to get an `Apparatus` is to wrap a `Decider` (or any `BaseMachineT`):

```scala mdoc
enum DoorState { case Closed, Open }
enum DoorCmd   { case Open, Close }
enum DoorEvt   { case Opened, Closed }

val door: Decider[DoorState, DoorCmd, List[DoorEvt]] =
  DeciderBuilder.seed(DoorState.Closed)
    .partiallyDecide[DoorCmd, DoorEvt]:
      case (DoorState.Closed, DoorCmd.Open)  => List(DoorEvt.Opened)
      case (DoorState.Open,   DoorCmd.Close) => List(DoorEvt.Closed)
    .evolveList:
      case (DoorState.Closed, DoorEvt.Opened) => DoorState.Open
      case (DoorState.Open,   DoorEvt.Closed) => DoorState.Closed
      case (s, _)                              => s

val doorFsm: Apparatus[Id, DoorCmd, List[DoorEvt]] =
  Apparatus.Fresh(door.toBaseMachine[Id])
```

## Leaf nodes: `Fresh` and `Stable`

Two constructors wrap a `BaseMachineT` into a leaf node. The difference is how state is shared
when the same logical machine appears in multiple positions of a composed tree.

### `Apparatus.Fresh` — independent evolution

Each `Fresh` node owns its state exclusively. Even if you pass the same `BaseMachineT` value to
two `Fresh` calls, the resulting nodes advance independently.

```scala mdoc:silent
val a = Apparatus.Fresh(door.toBaseMachine[Id])
val b = Apparatus.Fresh(door.toBaseMachine[Id])
// a and b are independent: running a does not affect b
```

Use `Fresh` for the vast majority of nodes. It is the default choice.

### `Apparatus.Stable` — shared state via id

`Stable` wraps a `BaseMachineT` and tags it with a `String` id. When two `Stable` nodes carry
the same id in a `>>>` (Sequential) composition, the state advanced by the **earlier** node is
propagated into every **later** node before it runs — without any manual pre-seeding.

This mirrors ZIO ZLayer memoization: the same logical service is instantiated once and reused
wherever its id appears in the graph.

```scala mdoc:silent
val doorMachine = door.toBaseMachine[Id]  // reuse same BaseMachineT

// Both positions carry id "door".
// After the left node runs (e.g. transitions to Open state), Sequential propagates
// the updated machine into the right node before the right node executes.
val stableLeft  = Apparatus.Stable("door", doorMachine)
val stableRight = Apparatus.Stable("door", doorMachine)

val sharedPipeline: Apparatus[Id, DoorCmd, List[DoorEvt]] =
  stableLeft >>> Apparatus.Fresh(BaseMachineT.stateless[Id, List[DoorEvt], List[DoorEvt]](_.pure))
// stableRight, placed inside a feedback reactor, starts from the state
// that stableLeft produced — not from the initial state
```

The practical motivation is the **rerooted saga** pattern: a car-booking service appears both as
the entry point (`carCore`) and inside the feedback compensation reactor. With `Stable("car", m)`
in both positions, after `carCore` processes `Reserve` (car transitions to `Reserved`), the
compensation service automatically starts from `Reserved` — no `.copy(state = CarState.Reserved)`
required.

**Rule of thumb**

| When | Use |
|------|-----|
| Machine appears once, or each position should start fresh | `Fresh` |
| Same logical machine drives two different phases (e.g. forward + compensation) within one `>>>` chain | `Stable` |

## Running a machine

```scala mdoc
// Single step — returns (output, updated machine)
val (evts, next) = Apparatus.run(doorFsm, DoorCmd.Open)

// Multiple steps — folds over a collection, combining outputs via Monoid
val (allEvts, _) = Apparatus.runMultiple(doorFsm, List(DoorCmd.Open, DoorCmd.Close, DoorCmd.Open))
```

`runA` / `runMultipleA` discard the updated machine when you only need the output.

## Combinators

### Sequential — `>>>` / `andThen`

Pipe output of the left machine directly into the right machine's input each step.
Any [[Apparatus.Stable]] nodes updated by `left` are propagated into `right` before `right` runs,
so shared nodes (same id) carry forward the state advanced by `left`.

```
A ──► [left: A→B] ──► B ──► [right: B→C] ──► C
```

```scala mdoc:silent
val pipeline: Apparatus[Id, DoorCmd, String] =
  doorFsm >>> Apparatus.Fresh(BaseMachineT.stateless[Id, List[DoorEvt], String](evts => evts.mkString(",")))
```

### Parallel — `***` / `par`

Run two independent machines on the two halves of a pair simultaneously.

```
(A, C) ──► [left: A→B] × [right: C→D] ──► (B, D)
```

```scala mdoc:silent
val paired: Apparatus[Id, (DoorCmd, DoorCmd), (List[DoorEvt], List[DoorEvt])] =
  doorFsm *** doorFsm
```

### Alternative — `|||` / `or`

Route `Either`-typed input to the matching machine; the other machine keeps its state untouched.

```
Left(A)  ──► [left:  A→B] ──► Left(B)
Right(C) ──► [right: C→D] ──► Right(D)
```

```scala mdoc:silent
val router: Apparatus[Id, Either[DoorCmd, DoorCmd], Either[List[DoorEvt], List[DoorEvt]]] =
  doorFsm ||| doorFsm
```

### Feedback — `<->` / `feedback`

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
// Contrived example: bounce door commands back-and-forth
val echo: Apparatus[Id, DoorEvt, List[DoorCmd]] =
  Apparatus.Fresh(BaseMachineT.stateless[Id, DoorEvt, List[DoorCmd]] {
    case DoorEvt.Opened => List(DoorCmd.Close)
    case DoorEvt.Closed => Nil
  })

val loop: Apparatus[Id, DoorCmd, List[DoorEvt]] =
  doorFsm <-> echo
```

### `lmapOrEmpty`

Contramap via a **partial function**. When the function is undefined for the given input the
machine's state is **not advanced** and `Monoid.empty` is returned for the output.

```scala mdoc:silent
// Only feed Open commands to doorFsm; ignore everything else
val openOnly: Apparatus[Id, Option[DoorCmd], List[DoorEvt]] =
  doorFsm.lmapOrEmpty { case Some(DoorCmd.Open) => DoorCmd.Open }
```

This combinator is especially useful when multiple service machines share the same input stream
(e.g. `List[SagaEvent]`) but each only cares about a subset of events:

```scala mdoc:silent
// Each sub-machine is silent (List.empty) for unrecognised events
val flightMachine: Apparatus[Id, SagaEvent[String], List[String]] =
  Apparatus.Fresh(BaseMachineT.stateless[Id, String, List[String]](_ => List("flight-ack")))
    .lmapOrEmpty { case SagaEvent.StepStarted("flight") => "flight" }
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
doesn't own, and `merge` fans the same event out to all of them, collecting the one non-empty
result.

```scala mdoc:silent
val hotelMachine: Apparatus[Id, SagaEvent[String], List[String]] =
  Apparatus.Fresh(BaseMachineT.stateless[Id, String, List[String]](_ => List("hotel-ack")))
    .lmapOrEmpty { case SagaEvent.StepStarted("hotel") => "hotel" }

val carMachine: Apparatus[Id, SagaEvent[String], List[String]] =
  Apparatus.Fresh(BaseMachineT.stateless[Id, String, List[String]](_ => List("car-ack")))
    .lmapOrEmpty { case SagaEvent.StepStarted("car") => "car" }

val services: Apparatus[Id, SagaEvent[String], List[String]] =
  flightMachine.merge(carMachine.merge(hotelMachine))
```

## Input / Output adapters

| Combinator | What it does |
|------------|-------------|
| `lmap(f)` | Contramap input: `f: C => A` applied before every step |
| `rmap(f)` | Map output: `f: B => C` applied after every step |
| `dimap(f)(g)` | `lmap` and `rmap` in one call |
| `imap` | Bidirectional via `Iso[A, A2]` and `Iso[B, B2]` |
| `lmapOrEmpty(pf)` | Partial contramap; silent (empty output, no state change) when undefined |
| `first` | Thread `_2` of a pair through unchanged; run machine on `_1` |
| `second` | Thread `_1` of a pair through unchanged; run machine on `_2` |

## Type class instances

`Apparatus` ships instances for the standard Cats profunctor hierarchy:

| Instance | What it enables |
|----------|----------------|
| `Category` | `id` + `compose` (= `andThen`) |
| `Profunctor` | `lmap` / `rmap` / `dimap` |
| `Strong` | `first` / `second` |
| `Choice` | `left` / `right` |

## Effect transformation

`mapK` converts the effect type via a natural transformation `F ~> G`, letting you lift an
`Apparatus[Id, I, O]` into `Apparatus[IO, I, O]` for production use while keeping pure `Id`-based tests:

```scala mdoc:silent
import cats.~>
import cats.effect.IO

val liftToIO: Id ~> IO = new (Id ~> IO):
  def apply[A](a: A): IO[A] = IO.pure(a)

val doorIO: Apparatus[IO, DoorCmd, List[DoorEvt]] = doorFsm.mapK(liftToIO)
```