# Machines

An `Apparatus` network is built from leaf nodes, each of which wraps one of three machine
types. Understanding the difference helps you choose the right abstraction for each part of
your domain.

```scala mdoc:silent
import apparatus.core.*
import apparatus.core.machines.*
import cats.effect.SyncIO
import cats.implicits.*
import zio.blocks.schema.Schema
import java.util.UUID
```

## ClosedMealy

`ClosedMealy[F, I, O]` is the simplest interface: given an input `I`, produce `F[O]`. State,
if any, is managed internally and not exposed to callers.

```scala
trait ClosedMealy[F[_], I, O]:
  def action(input: I): F[O]
```

Use `ClosedMealy.stateless` to build a machine with no state — a pure effectful function:

```scala mdoc:silent
val stringify: ClosedMealy[SyncIO, List[Int], String] =
  ClosedMealy.stateless[SyncIO, List[Int], String](ints => SyncIO.pure(ints.mkString(",")))
```

To build a stateful `ClosedMealy` you allocate a `Ref` yourself:

```scala mdoc:silent
val counter: SyncIO[ClosedMealy[SyncIO, Unit, Int]] =
  cats.effect.kernel.Ref[SyncIO].of(0).map { ref =>
    new ClosedMealy[SyncIO, Unit, Int]:
      def action(input: Unit): SyncIO[Int] =
        ref.updateAndGet(_ + 1)
  }
```

Lift a `ClosedMealy` into a network node with `Apparatus.closedMealy`.

## OpenMealy

`OpenMealy[F, I, O]` exposes its state type explicitly:

```scala
trait OpenMealy[F[_], I, O]:
  type State
  def initialState: State
  def action(state: State, input: I): F[(O, State)]
```

The state is threaded externally. When you pass an `OpenMealy` to `Apparatus.openMealy`, the
**normalisation** pass (run at compile time) automatically allocates a `Ref` for it, converting
it to a `ClosedMealy` backed by that `Ref`. You never manage the `Ref` manually.

`Decider.toOpenMealy` converts a pure `Decider` into an `OpenMealy[Id, I, O]` — useful when
you want to test the raw state machine without any effect stack.

## Decider

`Decider[S, I, O]` is a pure, effect-free machine defined by two functions:

| Function | Signature | Role |
|----------|-----------|------|
| `decide` | `(I, S) => O` | Given a command and current state, produce output (typically events) |
| `evolve` | `(O, S) => S` | Fold the output into a new state |

`Decider` has no `F[_]` — it is completely pure. It is the primary building block for
event-sourced aggregates. See [Decider](./decider.md) for the builder DSL and materialization.

## Comparison

| | `ClosedMealy` | `OpenMealy` | `Decider` |
|---|---|---|---|
| State | Internal / hidden | External, explicit type | Plain Scala value `S` |
| Effect | `F[_]` | `F[_]` | None (pure) |
| State allocation | Manual `Ref` | Auto-`Ref` on compile | Via `DeciderMaterializer` |
| Primary use | Projections, policies, glue | Rare; prefer Decider | Event-sourced aggregates |

## Wrapping machines in a network

| Constructor | Input |
|---|---|
| `Apparatus.closedMealy(m)` | `ClosedMealy[F, I, O]` |
| `Apparatus.openMealy(m)` | `OpenMealy[F, I, O]` |
| `Apparatus.aggregateMachine(d, extractId)` | `Decider[S, I, List[E]]` with per-UUID routing |

All three produce an `Apparatus[F, I, O]` that composes with the same combinators.

## Next: Decider

The [Decider](./decider.md) page covers the builder DSL in depth, explains how `DeciderMaterializer`
converts a pure decider into a stateful `ClosedMealy`, and shows both in-memory and
database-backed strategies.
