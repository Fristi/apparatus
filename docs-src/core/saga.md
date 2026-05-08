# Saga

A **saga** coordinates a sequence of steps across independent services (or sub-state-machines)
and automatically runs compensation (rollback) when any step fails.

To implement this with Apparatus, you need these things:

- `SagaBehavior` — a trait you implement to describe your saga's steps and how incoming commands map
  to step results. The step results are used to have the saga `decide` what _events_ to emit, and based on th events how to `evolve` the internal state of orchestrator. This is a opinionated `Decider` which can be made persistent.
- `Apparatus.Feedback` (`<->`) — the combinator that closes the loop between the saga orchestrator and
  the service machines it drives.

```scala
import apparatus.core.*
import cats.Id
import cats.data.NonEmptySet
import cats.implicits.*
import cats.derived.*
import scala.collection.immutable.SortedSet
```

## Lifecycle

A saga always moves through the same states/phases:

```
Waiting ──(Boot)──► Running ──(all steps done)──► Succeeded
                       │
                       └──(a step fails)──► Compensating ──(all compensations done)──► Failed
```

`SagaState` encodes this as a sealed type:

| State | Meaning |
|---|---|
| `Waiting` | Not yet started |
| `Running(current, todo, compensation)` | Executing forward steps; `compensation` accumulates completed steps that must be rolled back on failure |
| `Compensating(current, todo)` | Rolling back; `todo` is the remaining compensation steps |
| `Succeeded` | All forward steps completed |
| `Failed` | Compensation finished (regardless of individual compensation outcomes) |

## Defining a saga

### 1. Model the steps

Steps are an ordered set. Use an enum with a numeric `position` field and `derives Order`:

```scala
enum BookingStep(val position: Int) derives Eq, Order, Show:
  case Hotel  extends BookingStep(1)
  case Car    extends BookingStep(2)
  case Flight extends BookingStep(3)
```

`Order` is required because steps are stored in a `SortedSet` — the saga executes them
lowest-to-highest.

### 2. Model the commands

The saga receives commands from external sources (service replies, timeouts, webhooks, Kafka
messages). Commands either boot the saga or report the result of a step:

```scala
enum BookingCommand:
  case Start
  // forward step results
  case MarkFlightComplete; case MarkFlightFailed
  case MarkHotelComplete;  case MarkHotelFailed
  case MarkCarComplete;    case MarkCarFailed
  // compensation step results
  case MarkFlightCompensationComplete; case MarkFlightCompensationFailed
  case MarkHotelCompensationComplete;  case MarkHotelCompensationFailed
  case MarkCarCompensationComplete;    case MarkCarCompensationFailed
```

### 3. Implement SagaBehavior

```scala
val behavior = new SagaBehavior[BookingCommand, BookingStep]:
  def startCommand = BookingCommand.Start

  def steps = NonEmptySet.of(BookingStep.Hotel, BookingStep.Car, BookingStep.Flight)

  def stepHandler: PartialFunction[BookingCommand, (BookingStep, SagaStepResult)] = {
    case BookingCommand.MarkFlightComplete => (BookingStep.Flight, SagaStepResult.Completed)
    case BookingCommand.MarkFlightFailed   => (BookingStep.Flight, SagaStepResult.Failed)
    case BookingCommand.MarkHotelComplete  => (BookingStep.Hotel,  SagaStepResult.Completed)
    case BookingCommand.MarkHotelFailed    => (BookingStep.Hotel,  SagaStepResult.Failed)
    case BookingCommand.MarkCarComplete    => (BookingStep.Car,    SagaStepResult.Completed)
    case BookingCommand.MarkCarFailed      => (BookingStep.Car,    SagaStepResult.Failed)
  }

  def compensationHandler: PartialFunction[BookingCommand, (BookingStep, SagaStepResult)] = {
    case BookingCommand.MarkFlightCompensationComplete => (BookingStep.Flight, SagaStepResult.Completed)
    case BookingCommand.MarkFlightCompensationFailed   => (BookingStep.Flight, SagaStepResult.Failed)
    case BookingCommand.MarkHotelCompensationComplete  => (BookingStep.Hotel,  SagaStepResult.Completed)
    case BookingCommand.MarkHotelCompensationFailed    => (BookingStep.Hotel,  SagaStepResult.Failed)
    case BookingCommand.MarkCarCompensationComplete    => (BookingStep.Car,    SagaStepResult.Completed)
    case BookingCommand.MarkCarCompensationFailed      => (BookingStep.Car,    SagaStepResult.Failed)
  }
```

`behavior.decider` gives you a `Decider[SagaState[BookingStep], BookingCommand, List[SagaEvent[BookingStep]]]`
ready to lift into an Apparatus network.

## Wiring up service machines

Each service machine:

1. Listens to `SagaEvent[BookingStep]` via `lmapOrEmpty` — silent for events it doesn't own.
2. Does its work (in the simple case: a pure `Decider`; in practice: a database call or HTTP request).
3. Translates its output back to `BookingCommand` via `rmap`.

```scala
def flightServiceFSM(
  flight: Decider[FlightState, FlightCommand, List[FlightEvent]] = flightDecider()
): Apparatus[Id, SagaEvent[BookingStep], List[BookingCommand]] =
  Apparatus.Basic(flight.toBaseMachine[Id])
    .lmapOrEmpty[SagaEvent[BookingStep]] {
      case SagaEvent.Booted(BookingStep.Flight, _)                | SagaEvent.StepStarted(BookingStep.Flight)         => FlightCommand.Reserve
      case SagaEvent.CompensationTriggered(BookingStep.Flight, _) | SagaEvent.CompensationStarted(BookingStep.Flight) => FlightCommand.Compensate
    }
    .rmap(_.collect {
      case FlightEvent.Reserved    => BookingCommand.MarkFlightComplete
      case FlightEvent.Failed      => BookingCommand.MarkFlightFailed
      case FlightEvent.Compensated => BookingCommand.MarkFlightCompensationComplete
    })
```

Use `merge` to combine all service machines into one fan-out node:

```scala
val services: Apparatus[Id, SagaEvent[BookingStep], List[BookingCommand]] =
  flightServiceFSM() merge carServiceFSM() merge hotelServiceFSM()
```

Because each service machine uses `lmapOrEmpty`, it stays silent (returns `List.empty` and does
not advance state) for any event it doesn't handle. `merge` combines all outputs — so exactly one
non-empty list comes out per event.

## Closing the feedback loop

Connect the saga orchestrator to the services with `<->` (or `feedback`):

```
BookingCommand ──► [bookingDecider: emits List[SagaEvent]]
                          │
                          ▼
                   List[SagaEvent]  (output AND reactor input)
                          │
                    ┌─────┘
                    ▼
             [services: SagaEvent → List[BookingCommand]]
                    │
                    └─────► new BookingCommands ─► back into bookingDecider
```

```scala
val bookingDecider: Apparatus[Id, BookingCommand, List[SagaEvent[BookingStep]]] =
  Apparatus.Basic(behavior.decider.toBaseMachine)

def saga(...): Apparatus[Id, BookingCommand, List[SagaEvent[BookingStep]]] =
  bookingDecider <-> services
```

Send `BookingCommand.Start` — the saga boots, drives all three services, and returns every
`SagaEvent` that occurred during the full run:

```scala
val events = Apparatus.runA(saga(), BookingCommand.Start)
// List(
//   Booted(Hotel, {Car, Flight}),
//   StepProgressed(Hotel, Completed),
//   StepStarted(Car),
//   StepProgressed(Car, Completed),
//   StepStarted(Flight),
//   StepProgressed(Flight, Completed)
// )
```

When a step fails, compensation fires automatically:

```scala
val events = Apparatus.runA(saga(flight = flightDecider(failsOnReserve = true)), BookingCommand.Start)
// … StepProgressed(Flight, Failed)
// … CompensationTriggered(Car, {Hotel})
// … CompensationProgressed(Car, Completed)
// … CompensationStarted(Hotel)
// … CompensationProgressed(Hotel, Completed)
```

Only steps that had already completed are compensated. Flight failed before completing, so it is
not compensated.

## Rerooting the entry point

### The problem

In practice, service machines are not in-process pure functions. A hotel reservation goes out
over the network; the reply might arrive via a webhook, a Kafka message, or a timeout — seconds
or minutes later. In the meantime other commands may have arrived and advanced the service
machine's own state.

When the reply eventually arrives you cannot replay the full saga from `Waiting`. The saga
orchestrator state you reconstruct from the event log already has the earlier steps baked in. You
need to resume from the *current* saga state and drive *only the remaining* services.

### `feedbackMany`

`feedbackMany` is like `<->` but accepts `List[BookingCommand]` as input instead of a single
`BookingCommand`. Use it when the entry point is a sub-machine whose output is already `List[_]`:

```scala
val node: Apparatus[Id, List[BookingCommand], List[SagaEvent[BookingStep]]] =
  bookingDecider.feedbackMany(services)

val events = Apparatus.runA(node, List(BookingCommand.Start))
```

### Rerooting at a service machine

Sometimes the trigger is not a saga-level command but a reply to a specific service. For example:
car booking is the active step, and an async webhook delivers the result. The car service machine
may have gone through several internal states since the saga last touched it (pending → retrying →
confirmed). You want to feed the car result in directly without replaying from the beginning.

The pattern:

1. **Rehydrate the saga orchestrator** via events from the used `Decider`
2. **Use `>>>` to chain** the car service machine into the (pre-seeded) saga feedback node.

```scala
def sagaRerootedAtCar(
  flight:       Decider[FlightState, FlightCommand, List[FlightEvent]] = flightDecider(),
  car:          Decider[CarState,    CarCommand,    List[CarEvent]]    = carDecider(),
  hotel:        Decider[HotelState,  HotelCommand,  List[HotelEvent]]  = hotelDecider(),
  bookingEvents: List[SagaEvent[BookingStep]]
): Apparatus[Id, CarCommand, List[SagaEvent[BookingStep]]] =

  // Car service as the entry point: CarCommand → List[BookingCommand]
  val carCore: Apparatus[Id, CarCommand, List[BookingCommand]] =
    Apparatus.Basic(car.toBaseMachine[Id])
      .rmap(_.collect {
        case CarEvent.Reserved    => BookingCommand.MarkCarComplete
        case CarEvent.Failed      => BookingCommand.MarkCarFailed
        case CarEvent.Compensated => BookingCommand.MarkCarCompensationComplete
      })

  // Saga orchestrator rehydrated
  val bookingDeciderAtCar: Apparatus[Id, BookingCommand, List[SagaEvent[BookingStep]]] =
    Apparatus.Basic(
      DeciderBuilder.seed(SagaState.Waiting)
        .decide[BookingCommand, List[SagaEvent[BookingStep]]]((s, i) => behavior.decide(s, i))
        .evolveList((s, e) => behavior.evolve(s, e))
        .evolveFrom(bookingEvents)
        .toBaseMachine[Id]
    )

  // Chain: CarCommand → List[BookingCommand] → feedback(saga, flight+hotel services)
  carCore >>> bookingDeciderAtCar.feedbackMany(flightServiceFSM(flight) merge hotelServiceFSM(hotel))
```

The orchestrator rehydrates to the state `Running(Car, remaining={Flight}, compensation={Hotel})` tells the
orchestrator: hotel was already booked successfully (it's in `compensation`), car is the active
step, flight is still to come. Now a single `CarCommand.Reserve` drives the rest of the saga:

```scala
val events = Apparatus.runA(sagaRerootedAtCar(), CarCommand.Reserve)
// List(
//   StepProgressed(Car, Completed),
//   StepStarted(Flight),
//   StepProgressed(Flight, Completed)
// )
```

If car fails, compensation triggers for hotel only (flight was never started):

```scala
val events = Apparatus.runA(
  sagaRerootedAtCar(
    car   = carDecider(failsOnReserve = true),
    hotel = hotelDecider(initialState = HotelState.Reserved)
  ),
  CarCommand.Reserve
)
// StepProgressed(Car, Failed)
// CompensationTriggered(Hotel, {})
// CompensationProgressed(Hotel, Completed)
```

### When to reroot

Reroot whenever:

- **The service machine is stateful and async.** It has gone through multiple states (e.g.,
  pending, polling, confirmed) between saga steps. You receive the intermediate results to drive the sub state machine via webhooks or
  message queues and want to resume the saga from its current position without re-running earlier
  steps.
- **The entry trigger is a service reply, not a saga command.** The webhook payload translates
  naturally to a `CarCommand`, not a `BookingCommand`. Rerooting lets you keep that translation
  at the edge.
- **The service machine is itself a nested saga.** If `CarDecider` is replaced by a
  `CarSagaBehavior` with its own sub-steps (reserve → confirm → issue-ticket), the same pattern
  applies: compose the inner saga as a `BaseMachineT` and use `>>>` to feed its terminal output
  into the outer saga feedback node.

## Nested sagas

Because every `Apparatus` has the same interface, an inner saga can be plugged in wherever a simple
service machine would go. Replace the `carDecider` with a fully-fledged `Apparatus[Id, CarCommand,
List[CarEvent]]` built from its own `SagaBehavior`, and the outer saga sees only `CarEvent`
values — it has no knowledge of the inner saga's steps.

```
BookingCommand ──► bookingDecider ──► List[SagaEvent[BookingStep]]
                                              │
                                    ┌─────────┤
                                    ▼         │
                             carSaga          │  (inner SagaBehavior[CarCommand, CarStep])
                             flightService    │
                             hotelService     │
                                    │         │
                                    └── List[BookingCommand] ──► back into bookingDecider
```

The inner `carSaga` is opaque from the outside. It may succeed, fail, and compensate its own
steps — the outer orchestrator only sees whether the car step `Completed` or `Failed`.

This nesting is unlimited in principle, though in practice keeping sagas to one or two levels
keeps the state model comprehensible.

## Summary

| Concept | What it does |
|---|---|
| `SagaBehavior` | Describes your saga: start command, ordered steps, step handlers, compensation handlers |
| `SagaState` | Typed lifecycle: `Waiting → Running → Succeeded / Compensating → Failed` |
| `SagaEvent` | Persistent record of what happened; replay via `evolve` rebuilds state |
| `<->` / `feedback` | Closes the orchestrator ↔ services loop; runs to completion synchronously |
| `feedbackMany` | Like `feedback` but accepts `List[Cmd]` as input; used after rerooting |
| `lmapOrEmpty` + `merge` | Fan the same `SagaEvent` to all services; only the matching one reacts |
| Reroot | Pre-seed the orchestrator at the current saga state; chain via `>>>` to resume mid-flight |
| Nested sagas | Plug an inner `SagaBehavior` Apparatus in place of a simple service machine |
