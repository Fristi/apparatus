# Decider

`Decider` implements the **Decider pattern** — a pure model of aggregate behaviour commonly used in
event-sourced systems. It separates two concerns:

| Function | Signature | Role |
|----------|-----------|------|
| `decide` | `(I, S) => O` | Maps a command and the current state to an output (usually a list of events) |
| `evolve` | `(O, S) => S` | Folds the output back into a new state |

Because both functions are **pure**, they are trivially testable in isolation and composable with
any effect stack via `toBaseMachine`.

```scala
import apparatus.core.*
import cats.Id
import cats.implicits.*
```

## Relationship to event sourcing

In a typical event-sourced aggregate:

1. A **command** arrives.
2. `decide` validates it against the current state and emits zero or more **events**.
3. `evolve` folds each event into a new state — exactly the same function used during replay.

`Decider` makes this separation explicit and enforces it structurally: `decide` never mutates state,
and `evolve` never has to know about commands.

```
Command ──► decide(cmd, state) ──► [Event, …]
                                        │
                                        ▼
                              evolve(event, state) ──► newState
```

## Building a Decider

Use `DeciderBuilder` for a fluent construction API:

```scala
// Domain
enum LightState { case Off, On }
enum LightCmd   { case TurnOn, TurnOff }
enum LightEvt   { case TurnedOn, TurnedOff }

val light: Decider[LightState, LightCmd, List[LightEvt]] =
  DeciderBuilder
    .seed(LightState.Off)
    .decide[LightCmd, List[LightEvt]] { (state, cmd) =>
      (state, cmd) match
        case (LightState.Off, LightCmd.TurnOn)   => List(LightEvt.TurnedOn)
        case (LightState.On,  LightCmd.TurnOff)  => List(LightEvt.TurnedOff)
        case _                                    => Nil
    }
    .evolveList { (state, evt) =>
      (state, evt) match
        case (LightState.Off, LightEvt.TurnedOn)  => LightState.On
        case (LightState.On,  LightEvt.TurnedOff) => LightState.Off
        case _                                     => state
    }
// light: Decider[LightState, LightCmd, List[LightEvt]] = Decider(
//   state = Off,
//   decide = apparatus.core.Decider$package$$$Lambda$12449/0x000000e002d59100@7dca6075,
//   evolve = apparatus.core.Decider$package$$$Lambda$12450/0x000000e002d596b0@58382787
// )
```

## Testing decide and evolve in isolation

Because both functions are pure you can call them directly without any FSM machinery:

```scala
// decide
val evts1 = light.decide(LightCmd.TurnOn,  LightState.Off)
// evts1: List[LightEvt] = List(TurnedOn)
val evts2 = light.decide(LightCmd.TurnOn,  LightState.On)   // idempotent guard
// evts2: List[LightEvt] = List()
val evts3 = light.decide(LightCmd.TurnOff, LightState.On)
// evts3: List[LightEvt] = List(TurnedOff)

// evolve
val s1 = light.evolve(List(LightEvt.TurnedOn), LightState.Off)
// s1: LightState = On
val s2 = light.evolve(Nil, LightState.Off)
// s2: LightState = Off
```

## Replaying an event stream

`evolveFrom` rebuilds the aggregate state from a persisted event log:

```scala
val history = List(LightEvt.TurnedOn, LightEvt.TurnedOff, LightEvt.TurnedOn)
// history: List[LightEvt] = List(TurnedOn, TurnedOff, TurnedOn)
val replayed = light.evolveFrom(history)
// replayed: Decider[LightState, LightCmd, List[LightEvt]] = Decider(
//   state = On,
//   decide = apparatus.core.Decider$package$$$Lambda$12451/0x000000e002d59f68@392eb4cd,
//   evolve = apparatus.core.Decider$package$$$Lambda$12452/0x000000e002d5a518@42692af4
// )
// replayed.state == LightState.On
```

## Lifting into an effect stack

`toBaseMachine[F]` converts the decider into a `BaseMachineT` running in any `Applicative` `F`:

```scala
val lightFsm: FSM[Id, LightCmd, List[LightEvt]] =
  FSM.Basic(light.toBaseMachine[Id])
// lightFsm: FSM[Id, LightCmd, List[LightEvt]] = Basic(
//   apparatus.core.Decider$$anon$1@537d925c
// )

val (fsmEvts1, next) = FSM.run(lightFsm, LightCmd.TurnOn)
// fsmEvts1: List[LightEvt] = List(TurnedOn)
// next: FSM[[A >: Nothing <: Any] =>> A, LightCmd, List[LightEvt]] = Basic(
//   apparatus.core.BaseMachineT$$anon$3@2770957e
// )
val (fsmEvts2, _)    = FSM.run(next,     LightCmd.TurnOff)
// fsmEvts2: List[LightEvt] = List(TurnedOff)
```

## Composing with a projection

Because `toBaseMachine` returns a `BaseMachineT`, you can pipe it into a projection machine with
`>>>`. The decider emits events; the projection folds them into a read model.

```scala
case class LightStats(on: Int, off: Int)

val projection: BaseMachineT[Id, List[LightEvt], LightStats] =
  BaseMachineT[Id, LightStats, List[LightEvt], LightStats](
    LightStats(0, 0),
    (stats, evts) =>
      val next = evts.foldLeft(stats) {
        case (s, LightEvt.TurnedOn)  => s.copy(on  = s.on  + 1)
        case (s, LightEvt.TurnedOff) => s.copy(off = s.off + 1)
      }
      (next, next)
  )
// projection: BaseMachineT[Id, List[LightEvt], LightStats] = apparatus.core.BaseMachineT$$anon$3@6410f2f7

val network: FSM[Id, LightCmd, LightStats] =
  FSM.Basic(light.toBaseMachine[Id]) >>> FSM.Basic(projection)
// network: FSM[Id, LightCmd, LightStats] = Sequential(
//   left = Basic(apparatus.core.Decider$$anon$1@5e2a11fb),
//   right = Basic(apparatus.core.BaseMachineT$$anon$3@6410f2f7)
// )

given cats.kernel.Monoid[LightStats] =
  cats.kernel.Monoid.instance(LightStats(0, 0), (a, b) => LightStats(a.on max b.on, a.off max b.off))

val (stats, _) = FSM.runMultiple(network, List(
  LightCmd.TurnOn, LightCmd.TurnOff, LightCmd.TurnOn
))
// stats: LightStats = LightStats(on = 2, off = 1)
```

## Fallible commands

Use `withError` / `partiallyDecide` / `evolveErrorList` when some commands are invalid and you
want to surface that as `Either[E, List[O]]`:

```scala
enum Error { case InvalidCommand }

val safeDoor =
  DeciderBuilder
    .seed(LightState.Off)
    .withError(Error.InvalidCommand)
    .partiallyDecide[LightCmd, List[LightEvt]] {
      case (LightCmd.TurnOn,  LightState.Off) => Right(List(LightEvt.TurnedOn))
      case (LightCmd.TurnOff, LightState.On)  => Right(List(LightEvt.TurnedOff))
    }
    .evolveErrorList { (state, evt) =>
      (state, evt) match
        case (LightState.Off, LightEvt.TurnedOn)  => LightState.On
        case (LightState.On,  LightEvt.TurnedOff) => LightState.Off
        case _                                     => state
    }
```
