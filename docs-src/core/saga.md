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
import java.time.LocalDate
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
stream via `SagaStepAdapter.adapt` (filtering to only events relevant to that step), processes
the command, and emits acknowledgement `BookingCommand`s. These feed back into the orchestrator
via the `feedback` loop, which advances the saga state.

## SagaBehavior and SagaState

`SagaBehavior[Cmd, Step, SagaData]` is a trait you implement (or build with `SagaBehaviorFactory`)
to describe the saga orchestration logic. The library provides fully implemented `decide` and
`evolve` functions; you supply:

- `name` — aggregate name for the decider
- `startCommand` — partial function extracting `(correlationId, sagaData)` from the boot command
- `compensateCommand` — partial function extracting `correlationId` from a manual rollback command
- `commandCorrelationId` — extracts the saga id from any command (including advance acks)
- `steps` — the ordered `NonEmptySet[Step]` to execute in sequence
- `advanceHandler` — maps incoming commands to `(Step, SagaPhase, SagaStepResult)`

### SagaState lifecycle

```
Waiting ──(Boot)──► Prepared ──(StepStarted)──► Running ──(all steps complete)──► Succeeded
                                                    │
                                        (step fails)──► CompensationPrepared ──► Compensating ──► Failed
```

`Running` tracks `current` (the executing step), `todo` (remaining steps), and `completed`
(dispatches for steps that finished successfully and must be compensated if a later step fails).

### Building with SagaBehaviorFactory

`SagaBehaviorFactory` derives `advanceHandler` from a `SagaAdvanceCodec`:

```scala mdoc:silent
val bookingId = java.util.UUID.fromString("00000000-0000-0000-0000-000000000004")
val sagaState = BookingSagaState("Amsterdam", "London", LocalDate.of(2026, 11, 1), LocalDate.of(2026, 11, 8))

val behavior: SagaBehavior[BookingCommand, BookingStep, BookingSagaState] = SagaBehaviorFactory(
  name                = "booking",
  startCommand        = { case BookingCommand.Start(id, state) => (id, state) },
  compensateCommand   = { case BookingCommand.Compensate(id) => id },
  commandCorrelationId = _.id,
  codec               = BookingCommand.advanceCodec,
  steps               = cats.data.NonEmptySet.of(BookingStep.Hotel, BookingStep.Car, BookingStep.Flight)
)
```

## SagaAdvanceCodec

A `SagaAdvanceCodec[Cmd, Stp]` is a bidirectional mapping between the saga's command type and
the quadruple `(correlationId, step, phase, result)`:

```scala mdoc:silent
val advanceCodec: SagaAdvanceCodec[BookingCommand, BookingStep] =
  SagaAdvanceCodec(
    {
      case BookingCommand.Advance(id, step, phase, result) => Some((id, step, phase, result))
      case _ => None
    },
    (id, step, phase, result) => BookingCommand.Advance(id, step, phase, result)
  )
```

The codec is used both by `SagaBehaviorFactory` (to decode inbound acknowledgements) and by
`SagaStepAdapter.adapt` (to encode service events as saga commands).

## SagaStepAdapter — wiring service machines

Each service participating in the saga needs three domain-specific pieces:

1. **`start`** — command sent when the orchestrator emits `StepStarted` (receives sub-aggregate id, boot state, and saga correlation id)
2. **`compensate`** — command sent on `CompensationStarted`
3. **`classify`** — interprets domain events as `(SagaPhase, SagaStepResult)`

`SagaStepAdapter.adapt` combines input filtering (`lmapOrEmpty`) and output encoding (`rmap`)
into a single call:

```scala
trait SagaStepAdapter[Cmd, Evt <: SagaCorrelated, Stp, SagaData]:
  def step: SagaStep
  def start(id: UUID, sagaState: SagaData, correlationId: UUID): Cmd
  def compensate(id: UUID): Cmd
  def classify(event: Evt): Option[(SagaPhase, SagaStepResult)]
  def adapt[F[_]: Applicative, SagaCmd](machine: Apparatus[F, Cmd, List[Evt]], codec: SagaAdvanceCodec[SagaCmd, Stp]): Apparatus[F, SagaEvent[Stp, SagaData], List[SagaCmd]]
```

For search-reserve-cancel service shapes, use the shared helper:

```scala mdoc:silent
val classifyFlight = SagaStepAdapter.classifyReserveSearch[FlightEvent](
  { case FlightEvent.Reserved(_, _)    => () },
  { case FlightEvent.Failed(_, _)      => () },
  { case FlightEvent.Compensated(_, _) => () }
)
```

### adapt — full service wiring

```
SagaEvent.StepStarted(Hotel, ...)     ──► HotelCommand.InitSearch(...)
SagaEvent.CompensationStarted(Hotel)  ──► HotelCommand.Cancel(...)
HotelEvent.Reserved                   ──► Advance(bookingId, Hotel, Forward, Completed)
all other events                      ──► silent / dropped
```

### Full service adapter example

The booking example wires each service with `adapt` inside `serviceFSM` helpers — see
[`BookingSaga.scala`](https://github.com/Fristi/apparatus/blob/main/examples/src/main/scala/apparatus/examples/BookingSaga.scala).

## Assembling the saga

The orchestrator and all service machines combine with `merge` (for the fan-out side) and
`feedback` (for the loop). The `apparatus.examples` package exposes the fully assembled
`saga[F]()` function:

```scala mdoc:silent
val wiredSaga: Apparatus[cats.effect.SyncIO, BookingCommand, List[SagaEvent[BookingStep, BookingSagaState]]] =
  saga[cats.effect.SyncIO](BookingServices.default[cats.effect.SyncIO])
```

Conceptually the assembly is:

```scala
// Each service: adapt (filter SagaEvents for this step + classify domain events)
val flightFSM = flightStep.adapt(aggregateMachine(...).feedback(connector), BookingCommand.advanceCodec)
val carFSM    = carStep.adapt(aggregateMachine(...).feedback(connector), BookingCommand.advanceCodec)
val hotelFSM  = hotelStep.adapt(aggregateMachine(...).feedback(connector), BookingCommand.advanceCodec)

// All services merged (fan-out: same SagaEvent hits all three, only the matching one fires)
val services = flightFSM.merge(carFSM.merge(hotelFSM))

// Orchestrator → feedback loop
val fullSaga = aggregateMachine[F, BookingCommand, SagaEvent[BookingStep, BookingSagaState]](behavior.decider, _.id)
  .feedback(services)
```

## Running the saga

```scala mdoc
val mat = DeciderMaterializer.syncIO
val bookingId2 = java.util.UUID.fromString("00000000-0000-0000-0000-000000000004")
val bootState = BookingSagaState("Amsterdam", "London", LocalDate.of(2026, 11, 1), LocalDate.of(2026, 11, 8))

val events = Apparatus.runSteps(
  saga[cats.effect.SyncIO](BookingServices.default[cats.effect.SyncIO]),
  List(BookingCommand.Start(bookingId2, bootState)),
  mat
).unsafeRunSync()
```

A single `Start` command drives the entire feedback loop to completion in one step: the
orchestrator emits `Booted` + `StepStarted(Hotel)`, the hotel service machine processes
`InitSearch`, emits `Reserved`, which becomes `Advance(Hotel, Forward, Completed)`, feeding
back into the orchestrator, which emits `StepStarted(Car)`, and so on until all three steps
complete and the orchestrator reaches `Succeeded`.

## Compensation example

Pass mock services that fail a search to trigger compensation — see
`BookingSagaMocks.failingFlightSearch` in the test suite. The saga executes Hotel and Car,
then Flight fails and compensates the completed steps.

## See also

- [Apparatus](./apparatus.md) — `feedback`, `lmapOrEmpty`, `merge` combinators
- [Decider](./decider.md) — building the orchestrator decider
- [BookingSaga source](https://github.com/Fristi/apparatus/blob/main/examples/src/main/scala/apparatus/examples/BookingSaga.scala) — full example code
