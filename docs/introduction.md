# Apparatus

Apparatus is a Scala library for building and composing **finite state machines** (FSMs) at three
levels of expressiveness — basic, eventful, and networked — in a purely functional style.

## What problem does it solve?

Business logic inside a microservice often has complex lifecycle: a booking goes through pending,
confirmed, and cancelled states; a payment can be retried, refunded, or disputed; an onboarding
flow has many conditional branches. This logic typically ends up scattered across service classes,
database flags, and ad-hoc retry loops with no single place that owns "what state am I in and
what is allowed here?"

Apparatus gives that logic a home.

- **State transitions are explicit.** Every state, command, and event is a typed value. Impossible
  transitions simply cannot be expressed.
- **Pure functions, no magic.** `decide` and `evolve` are plain functions. No framework
  annotations, no reflection, no runtime agents. You can call them in a unit test with zero setup.
- **Composable by construction.** Small machines combine into larger ones via a small set of
  algebraic combinators (`>>>`, `<->`, `merge`, …). The resulting network is still just an `FSM`
  — it has the same interface and can be composed further.

## Three building blocks

### Basic FSM

Receives input and evolves internal state, emitting output. The minimal unit of computation.
Useful for simple lifecycle objects, counters, or as building blocks for larger networks.

### Eventful FSM — the Decider pattern

Separates command handling into two pure functions:

```
Command ──► decide(cmd, state) ──► [Event, …]
                                        │
                                        ▼
                              evolve(event, state) ──► newState
```

`decide` is the guard: it validates the command and emits the events that *should* happen.
`evolve` is the projection: it folds events into the next state, exactly as it would during
event-log replay. This strict separation makes both functions trivially testable and makes the
aggregate fully reconstructable from its event history.

### Networked FSM

Basic and eventful machines compose into networks. Two important patterns:

- **Projections** (`>>>`): an eventful machine pipes its events into a read-model machine that
  accumulates derived state — counters, summaries, audit trails.
- **Sagas** (`<->`): a feedback loop where the orchestrating machine emits events that drive
  sub-machines, which in turn emit commands that feed back into the orchestrator. The loop runs
  to completion in one synchronous pass, with full state tracked across all participants.

## When is Apparatus useful?

| Use-case | Why it fits |
|---|---|
| Event-sourced aggregates inside a single service | `Decider` enforces the decide/evolve split; `evolveFrom` replays history |
| Transactional sagas with rollback | `SagaBehavior` gives a typed saga with compensation built in |
| Strongly consistent read models | `>>>` chains emit events directly into projection machines in the same transaction |
| Complex lifecycle logic (multi-step onboarding, approval flows) | Explicit states make every allowed transition visible |
| Testing business rules without infrastructure | Pure functions — call `decide` and `evolve` directly in unit tests |

## How does it compare?

### Akka / Apache Pekko

Akka and Pekko are distributed actor frameworks. They solve clustering, back-pressure, remote
messaging, and fault tolerance across many nodes. That power comes with significant operational
complexity: cluster membership, serialisation, supervision hierarchies, and stateful actors that
are hard to test without a running system.

Apparatus operates at a lower level and a smaller scope. It is a **library, not a framework** —
no actor system, no remote transport, no cluster. Everything runs in-process as plain Scala
values. You compose machines with functions; you test them by calling those functions.

If you need cross-service distributed state and resilient actor supervision, Akka/Pekko is the
right tool. If you need strongly typed, testable, composable state logic *within* a service,
Apparatus is lighter and more direct.

### Temporal

Temporal is a durable workflow execution platform. It persists workflow state to an external
server, replays function execution after failures, and manages retries and timeouts across
distributed workers. It is excellent for long-running workflows that must survive process crashes
and span multiple services.

Apparatus does not persist anything on its own. State is a plain Scala value; durability is your
responsibility (e.g., via event sourcing with doobie, Skunk, or Slick). What Apparatus provides
is the **pure decision and evolution logic** that you compose into your persistence layer.

The sweet spot for Apparatus is inside a single microservice where you want transactional
consistency (one database transaction = one FSM step) and you own the persistence. Temporal is
the better choice when workflows must survive process crashes, span multiple services, or require
Temporal's built-in retry/timeout infrastructure.

### Summary

| | Apparatus | Akka / Pekko | Temporal |
|---|---|---|---|
| Scope | In-process, single service | Distributed, multi-node | Distributed, multi-service |
| State durability | You own it | Actor mailboxes + plugins | Temporal server |
| Testing | Pure function calls | TestKit / probe actors | Test server or mocks |
| Composition | Algebraic combinators | Actor hierarchy | Workflow + Activity DSL |
| Learning curve | Low | High | Medium |

## What Apparatus is not

- Not a persistence library. Use doobie, Skunk, or Slick to persist events and replay state.
- Not a distributed system. All machines run in the same process.
- Not a streaming framework. Machines are step-driven; integrate with fs2, Kafka streams, or
  any other source to feed commands.
