# FSM

`FSM[F[_], I, O]` is a composable, immutable finite-state machine running in effect `F`. Every
call to `runWith` returns the output **and** a new `FSM` carrying updated internal state, making
the whole structure purely functional and replayable.

```scala mdoc
import apparatus.core.*
import cats.Id
import cats.implicits.*
```

## Building a machine

The simplest way to get an `FSM` is to wrap a `Decider` (or any `BaseMachineT`):

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

val doorFsm: FSM[Id, DoorCmd, List[DoorEvt]] =
  FSM.Basic(door.toBaseMachine[Id])
```

## Running a machine

```scala mdoc
// Single step — returns (output, updated machine)
val (evts, next) = FSM.run(doorFsm, DoorCmd.Open)

// Multiple steps — folds over a collection, combining outputs via Monoid
val (allEvts, _) = FSM.runMultiple(doorFsm, List(DoorCmd.Open, DoorCmd.Close, DoorCmd.Open))
```

`runA` / `runMultipleA` discard the updated machine when you only need the output.

## Combinators

### Sequential — `>>>` / `andThen`

Pipe output of the left machine directly into the right machine's input each step.

```
A ──► [left: A→B] ──► B ──► [right: B→C] ──► C
```

```scala mdoc:silent
val pipeline: FSM[Id, DoorCmd, String] =
  doorFsm >>> FSM.Basic(BaseMachineT.stateless[Id, List[DoorEvt], String](evts => evts.mkString(",")))
```

### Parallel — `***` / `par`

Run two independent machines on the two halves of a pair simultaneously.

```
(A, C) ──► [left: A→B] × [right: C→D] ──► (B, D)
```

```scala mdoc:silent
val paired: FSM[Id, (DoorCmd, DoorCmd), (List[DoorEvt], List[DoorEvt])] =
  doorFsm *** doorFsm
```

### Alternative — `|||` / `or`

Route `Either`-typed input to the matching machine; the other machine keeps its state untouched.

```
Left(A)  ──► [left:  A→B] ──► Left(B)
Right(C) ──► [right: C→D] ──► Right(D)
```

```scala mdoc:silent
val router: FSM[Id, Either[DoorCmd, DoorCmd], Either[List[DoorEvt], List[DoorEvt]]] =
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
val echo: FSM[Id, DoorEvt, List[DoorCmd]] =
  FSM.Basic(BaseMachineT.stateless[Id, DoorEvt, List[DoorCmd]] {
    case DoorEvt.Opened => List(DoorCmd.Close)
    case DoorEvt.Closed => Nil
  })

val loop: FSM[Id, DoorCmd, List[DoorEvt]] =
  doorFsm <-> echo
```

### `lmapOrEmpty`

Contramap via a **partial function**. When the function is undefined for the given input the
machine's state is **not advanced** and `Monoid.empty` is returned for the output. 

```scala mdoc:silent
// Only feed Open commands to doorFsm; ignore everything else
val openOnly: FSM[Id, Option[DoorCmd], List[DoorEvt]] =
  doorFsm.lmapOrEmpty { case Some(DoorCmd.Open) => DoorCmd.Open }
```

This combinator is especially useful when multiple service machines share the same input stream
(e.g. `List[SagaEvent]`) but each only cares about a subset of events:

```scala mdoc:silent
// Each sub-machine is silent (List.empty) for unrecognised events
val flightMachine: FSM[Id, SagaEvent[String], List[String]] =
  FSM.Basic(BaseMachineT.stateless[Id, String, List[String]](_ => List("flight-ack")))
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
val flight: FSM[Id, SagaEvent[String], List[String]] = ??? // defined above
val car:    FSM[Id, SagaEvent[String], List[String]] = ???
val hotel:  FSM[Id, SagaEvent[String], List[String]] = ???

val services: FSM[Id, SagaEvent[String], List[String]] =
  flight merge car merge hotel
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

`FSM` ships instances for the standard Cats profunctor hierarchy:

| Instance | What it enables |
|----------|----------------|
| `Category` | `id` + `compose` (= `andThen`) |
| `Profunctor` | `lmap` / `rmap` / `dimap` |
| `Strong` | `first` / `second` |
| `Choice` | `left` / `right` |

## Effect transformation

`mapK` converts the effect type via a natural transformation `F ~> G`, letting you lift an
`FSM[Id, I, O]` into `FSM[IO, I, O]` for production use while keeping pure `Id`-based tests:

```scala mdoc:silent
import cats.~>
import cats.effect.IO

val liftToIO: Id ~> IO = new (Id ~> IO):
  def apply[A](a: A): IO[A] = IO.pure(a)

val doorIO: FSM[IO, DoorCmd, List[DoorEvt]] = doorFsm.mapK(liftToIO)
```
