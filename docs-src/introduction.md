# Apparatus

Apparatus is a Scala library for building and composing **finite state machines** (FSMs) in a
purely functional style — from simple aggregates up to networked sagas with compensation.

## What problem does it solve?

Business logic inside a service accumulates invisible complexity over time. A payment can be
retried, refunded, or disputed. A booking flows through pending, confirmed, and cancelled.
An onboarding process branches on user choices and external confirmations. This logic ends
up scattered across service classes, database flags, and ad-hoc conditional branches with
no single place that owns "what state am I in, and what is allowed here?"

Apparatus gives that logic a home.

- **Explicit state transitions.** Every state, command, and event is a typed value. Impossible
  transitions cannot be expressed. The type system enforces the domain model.
- **Pure functions.** `decide` and `evolve` are plain functions with no framework annotations,
  no reflection, and no runtime agents. Call them in a unit test with zero setup.
- **Composable by construction.** Small machines combine into larger ones with a small set of
  algebraic combinators (`>>>`, `feedback`, `merge`, `lmapOrEmpty`, …). The result is still a
  single `Apparatus` — same interface, composable further.
- **Effect-agnostic.** The network is parameterised on `F[_]`. Use `SyncIO` for tests, `IO`
  for production, `ConnectionIO` for database-transactional aggregates.

## Three building blocks

### Machines

An `Apparatus` leaf node is one of three machine types:

- **`ClosedMealy`** — a self-contained stateful machine. You hand it an input, it gives back
  `F[O]`. State is managed internally (e.g. via a `cats.effect.Ref`).
- **`OpenMealy`** — a machine with an explicit `State` type and `initialState`. State is
  threaded externally; the normalisation pass converts it to a `Ref`-backed `ClosedMealy` on
  compile.
- **`Decider`** — a pure, effect-free machine built from two functions: `decide(input, state)`
  and `evolve(output, state)`. The builder DSL produces a `Decider`; `aggregateMachine` lifts
  it into a per-UUID routed `Apparatus` node.

### The Decider pattern

Separates command handling into two pure functions:

```
Command ──► decide(cmd, state) ──► [Event, …]
                                        │
                                        ▼
                              evolve(event, state) ──► newState
```

`decide` is the guard: validate the command, emit events that *should* happen.
`evolve` is the projection: fold events into the next state, identical to event-log replay.
This separation makes both functions trivially testable and the aggregate fully reconstructable
from its event history.

### Networked Apparatus

Machines compose into networks using algebraic combinators:

- **Projections** (`>>>`): an aggregate pipes its events into a read-model machine that
  accumulates derived state — counters, summaries, audit trails — in the same transaction.
- **Sagas** (`feedback`): a feedback loop where an orchestrating machine emits events that
  drive sub-machines, which emit commands that feed back into the orchestrator. The loop runs
  to completion in one pass, with full state tracked across all participants.

## When is Apparatus useful?

| Use-case | Why it fits |
|---|---|
| Event-sourced aggregates inside a single service | `Decider` enforces the decide/evolve split; `evolveFrom` replays history |
| Transactional sagas with rollback | `SagaBehavior` gives typed saga orchestration with compensation |
| Strongly consistent read models | `>>>` chains emit events directly into projections in the same transaction |
| Complex lifecycle logic | Explicit states make every allowed transition visible |
| Testing business rules without infrastructure | Pure functions — call `decide` and `evolve` directly in unit tests |
