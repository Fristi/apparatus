# Decider

`Decider` implements the **Decider pattern** — a pure model of aggregate behaviour commonly used in
event-sourced systems. It separates two concerns:

| Function | Signature | Role |
|----------|-----------|------|
| `decide` | `(I, S) => O` | Maps a command and the current state to an output (usually a list of events) |
| `evolve` | `(O, S) => S` | Folds the output back into a new state |

Because both functions are **pure**, they are trivially testable in isolation and composable with
any effect stack via `toOpenMealy`.

```scala mdoc
import apparatus.core.*
import apparatus.core.machines.*
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

```scala mdoc
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
```

## Testing decide and evolve in isolation

Because both functions are pure you can call them directly without any Apparatus machinery:

```scala mdoc
// decide
val evts1 = light.decide(LightCmd.TurnOn,  LightState.Off)
val evts2 = light.decide(LightCmd.TurnOn,  LightState.On)   // idempotent guard
val evts3 = light.decide(LightCmd.TurnOff, LightState.On)

// evolve
val s1 = light.evolve(List(LightEvt.TurnedOn), LightState.Off)
val s2 = light.evolve(Nil, LightState.Off)
```