# Saga

A **saga** is a pattern for coordinating a sequence of steps across multiple services or
aggregates, with automatic **compensation** (rollback) when any step fails. Instead of a
single distributed transaction, each step is an independent operation; if a later step fails,
previously completed steps are undone in reverse order.

Apparatus models sagas as a **feedback loop**: an orchestrator state machine emits events that
drive service sub-machines, which emit acknowledgement commands that the orchestrator processes.
The entire round-trip runs inside a single `Apparatus.run` call.

```scala mdoc:silent
import apparatus.core.*
import apparatus.core.machines.*
import apparatus.core.patterns.*
import apparatus.examples.*
import cats.Id
import cats.implicits.*
```

## The feedback topology

```
                ┌──────────────────────────────────────────┐
BookingCommand  │  [Booking Saga]  ──► List[SagaEvent]     │
────────────►   │                                           │
                │  each SagaEvent fan-out to:               │
                │  ┌────────────────────────────────────┐   │
                │  │ [Flight/Car/Hotel service machines] │   │
                │  └──────────────┬─────────────────────┘   │
                │                 │ List[BookingCommand]     │
                └─────────────────┴──────────────────────────┘
```

The orchestrator (`booking`) emits `SagaEvent`s. Each service machine receives the event
stream via `lmapOrEmpty` (filtering to only events relevant to that step), processes the
command, and emits acknowledgement `BookingCommand`s. These feed back into the orchestrator
via the `feedback` loop, which advances the saga state.

## SagaBehavior and SagaState

`SagaBehavior[Cmd, Step]` is a trait you implement (or build with `SagaBehaviorFactory`)
to describe the saga orchestration logic. The library provides fully implemented `decide` and
`evolve` functions; you supply:

- `name` — aggregate name for the decider
- `startCommand` — the command that boots the saga from `Waiting`
- `steps` — the ordered `NonEmptySet[Step]` to execute in sequence
- `stepHandler` — maps incoming commands to `(Step, SagaStepResult)` during the forward run
- `compensationHandler` — maps incoming commands to `(Step, SagaStepResult)` during compensation

### SagaState lifecycle

```
Waiting ──(Boot)──► Prepared ──(StepStarted)──► Running ──(all steps complete)──► Succeeded
                                                    │
                                        (step fails)──► CompensationPrepared ──► Compensating ──► Failed
```

`Running` tracks `current` (the executing step), `todo` (remaining steps), and `compensation`
(completed steps that must be rolled back if a later step fails).

### Building with SagaBehaviorFactory

`SagaBehaviorFactory` derives `stepHandler` and `compensationHandler` from a `SagaAdvancePrism`:

```scala mdoc:silent
val behavior: SagaBehavior[BookingCommand, BookingStep] = SagaBehaviorFactory(
  name         = "booking",
  startCommand = BookingCommand.Start(bookingId),
  prism        = BookingCommand.advancePrism,
  steps        = cats.data.NonEmptySet.of(BookingStep.Hotel, BookingStep.Car, BookingStep.Flight)
)
```

## SagaAdvancePrism

A `SagaAdvancePrism[Cmd, Stp]` is a bidirectional optic between the saga's command type and
the triple `(step, phase, result)`:

```scala mdoc:silent
val advancePrism: SagaAdvancePrism[BookingCommand, BookingStep] =
  new Prism[BookingCommand, (BookingStep, SagaPhase, SagaStepResult)] {
    def getOption(cmd: BookingCommand) = cmd match {
      case BookingCommand.Advance(_, step, phase, result) => Some((step, phase, result))
      case _                                              => None
    }
    def reverseGet(t: (BookingStep, SagaPhase, SagaStepResult)) =
      BookingCommand.Advance(bookingId, t._1, t._2, t._3)
  }
```

The prism is used both by `SagaBehaviorFactory` (to decode inbound acknowledgements) and by
`SagaStepAdapter.rmap` (to encode service events as saga commands).

## SagaStepAdapter — wiring service machines

Each service participating in the saga needs two wiring operations:

1. **Input side** (`lmapOrEmpty`): filter the orchestrator's `SagaEvent` stream to only the
   events relevant to this step, and map them to the service's command type.
2. **Output side** (`rmap`): translate the service's domain events into saga acknowledgement
   commands using `classify` and the prism.

`SagaStepAdapter` encapsulates both:

```scala
trait SagaStepAdapter[Cmd, Evt, Stp]:
  def step:       Stp                                           // which saga step this adapter represents
  def start:      Cmd                                           // command sent on StepStarted
  def compensate: Cmd                                           // command sent on CompensationStarted
  def classify(event: Evt): Option[(SagaPhase, SagaStepResult)] // interpret domain events as saga signals
```

### lmapOrEmpty — input side

`SagaStepAdapter.lmapOrEmpty(apparatus)` wraps a service machine so it fires only when the
orchestrator emits a `SagaEvent` targeting this step:

```
SagaEvent.StepStarted(Flight)        ──► FlightCommand.Reserve
SagaEvent.CompensationStarted(Flight) ──► FlightCommand.Compensate
all other SagaEvents                 ──► Monoid.empty (silent)
```

This is exactly what `lmapOrEmpty` does on `Apparatus`: the partial function is undefined for
unrelated steps, so those events pass through silently without advancing the service machine.

Analogy: this is equivalent to `<+>` / `SemigroupK` in http4s routing — each route handles
its own path and falls through otherwise.

### rmap — output side

`SagaStepAdapter.rmap(apparatus, prism)` wraps the service machine's `List[Evt]` output,
calling `classify` on each event and encoding matching ones via the prism:

```
FlightEvent.Reserved(_)           → Some(Forward, Completed)  → BookingCommand.Advance(_, Flight, Forward, Completed)
FlightEvent.Failed(_)             → Some(Forward, Failed)     → BookingCommand.Advance(_, Flight, Forward, Failed)
FlightEvent.Compensated(_)        → Some(Compensation, Completed)
FlightEvent.CompensationFailed(_) → Some(Compensation, Failed)
other events                      → None                       → dropped
```

### Full service adapter example

```scala mdoc:silent
val flightStep: SagaStepAdapter[FlightCommand, FlightEvent, BookingStep] =
  new SagaStepAdapter[FlightCommand, FlightEvent, BookingStep] {
    def step       = BookingStep.Flight
    def start      = FlightCommand.Reserve(flightId)
    def compensate = FlightCommand.Compensate(flightId)
    def classify(event: FlightEvent) = event match {
      case FlightEvent.Reserved(_)           => Some(SagaPhase.Forward      -> SagaStepResult.Completed)
      case FlightEvent.Failed(_)             => Some(SagaPhase.Forward      -> SagaStepResult.Failed)
      case FlightEvent.Compensated(_)        => Some(SagaPhase.Compensation -> SagaStepResult.Completed)
      case FlightEvent.CompensationFailed(_) => Some(SagaPhase.Compensation -> SagaStepResult.Failed)
    }
  }

def flightServiceFSM[F[_]: cats.Applicative](flight: FlightDecider): Apparatus[F, SagaEvent[BookingStep], List[BookingCommand]] =
  flightStep.rmap(
    flightStep.lmapOrEmpty(Apparatus.aggregateMachine[F, FlightCommand, FlightEvent](flight, _.id)),
    BookingCommand.advancePrism
  ).label("Flight service")
```

## Assembling the saga

The orchestrator and all service machines combine with `merge` (for the fan-out side) and
`feedback` (for the loop).  The `apparatus.examples` package exposes the fully assembled
`saga[F]()` function:

```scala mdoc:silent
// saga[F]() from apparatus.examples — uses flightDecider/carDecider/hotelDecider defaults
val wiredSaga: Apparatus[cats.effect.SyncIO, BookingCommand, List[SagaEvent[BookingStep]]] =
  saga[cats.effect.SyncIO]()
```

Conceptually the assembly is:

```scala
// Each service: lmapOrEmpty (filter SagaEvents for this step) + rmap (classify domain events)
val flightFSM = flightStep.rmap(flightStep.lmapOrEmpty(aggregateMachine[F, FlightCommand, FlightEvent](flightDecider(), _.id)), BookingCommand.advancePrism)
val carFSM    = carStep.rmap(carStep.lmapOrEmpty(aggregateMachine[F, CarCommand, CarEvent](carDecider(), _.id)), BookingCommand.advancePrism)
val hotelFSM  = hotelStep.rmap(hotelStep.lmapOrEmpty(aggregateMachine[F, HotelCommand, HotelEvent](hotelDecider(), _.id)), BookingCommand.advancePrism)

// All services merged (fan-out: same SagaEvent hits all three, only the matching one fires)
val services = flightFSM.merge(carFSM.merge(hotelFSM))

// Orchestrator → feedback loop
val fullSaga = aggregateMachine[F, BookingCommand, SagaEvent[BookingStep]](behavior.decider, _.id)
  .feedback(services)
```

## Running the saga

```scala mdoc
val mat       = DeciderMaterializer.syncIO
val bookingId2 = java.util.UUID.fromString("00000000-0000-0000-0000-000000000004")

val events = Apparatus.runSteps(
  saga[cats.effect.SyncIO](),
  List(BookingCommand.Start(bookingId2)),
  mat
).unsafeRunSync()
```

A single `Start` command drives the entire feedback loop to completion in one step: the
orchestrator emits `Booted` + `StepStarted(Hotel)`, the hotel service machine processes
`Reserve`, emits `Reserved`, which becomes `Advance(Hotel, Forward, Completed)`, feeding
back into the orchestrator, which emits `StepStarted(Car)`, and so on until all three steps
complete and the orchestrator reaches `Succeeded`.

## Compensation example

Pass a failing decider to see the compensation path:

```scala mdoc:silent
val failingFlight: FlightDecider = flightDecider(failsOnReserve = true)

val compensatingEvents = Apparatus.runSteps(
  saga[cats.effect.SyncIO](flight = failingFlight),
  List(BookingCommand.Start(bookingId2)),
  mat
).unsafeRunSync()
// Saga executes Hotel, Car, then Flight fails → compensates Car, Hotel
```

## See also

- [Apparatus](./apparatus.md) — `feedback`, `lmapOrEmpty`, `merge` combinators
- [Decider](./decider.md) — building the orchestrator decider
- [BookingSaga source](https://github.com/Fristi/apparatus/blob/main/examples/src/main/scala/apparatus/examples/BookingSaga.scala) — full example code
