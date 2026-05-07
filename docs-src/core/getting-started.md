# Getting started

This guide walks from zero to a working networked FSM. It uses a door that opens after three
knocks and can be closed again.

```scala mdoc
import apparatus.core.*
import cats.Id
import cats.implicits.*
```

## 1. Model your domain

Define state, commands, and events as sealed types.

```scala mdoc
enum DoorState:
  case Closed(knocked: Int)
  case Open

enum DoorCommand:
  case Knock
  case Close

enum DoorEvent:
  case Knocked
  case Opened
  case Closed
```

## 2. Build a Decider

`DeciderBuilder` constructs a `Decider[State, Command, List[Event]]` from two pure functions:

- `decide` — given the current state and a command, what events should happen?
- `evolve` — given the current state and one event, what is the next state?

```scala mdoc
val doorExplicit: Decider[DoorState, DoorCommand, List[DoorEvent]] =
  DeciderBuilder
    .seed[DoorState](DoorState.Closed(0))
    .decide[DoorCommand, List[DoorEvent]] { (state, cmd) =>
      (state, cmd) match
        case (DoorState.Closed(n), DoorCommand.Knock) =>
          List(if n + 1 == 3 then DoorEvent.Opened else DoorEvent.Knocked)
        case (DoorState.Open, DoorCommand.Close) =>
          List(DoorEvent.Closed)
        case _ => Nil
    }
    .evolveList { (state, event) =>
      (state, event) match
        case (DoorState.Closed(n), DoorEvent.Knocked) => DoorState.Closed(n + 1)
        case (DoorState.Closed(_), DoorEvent.Opened)  => DoorState.Open
        case (DoorState.Open,      DoorEvent.Closed)  => DoorState.Closed(0)
        case _                                         => state
    }
```

`partiallyDecide` / `evolveList` is a compact alternative when you only care about matched cases:

```scala mdoc
val door: Decider[DoorState, DoorCommand, List[DoorEvent]] =
  DeciderBuilder
    .seed[DoorState](DoorState.Closed(0))
    .partiallyDecide[DoorCommand, DoorEvent]:
      case (DoorState.Closed(n), DoorCommand.Knock) =>
        List(if n + 1 == 3 then DoorEvent.Opened else DoorEvent.Knocked)
      case (DoorState.Open, DoorCommand.Close) =>
        List(DoorEvent.Closed)
    .evolveList:
      case (DoorState.Closed(n), DoorEvent.Knocked) => DoorState.Closed(n + 1)
      case (DoorState.Closed(_), DoorEvent.Opened)  => DoorState.Open
      case (DoorState.Open,      DoorEvent.Closed)  => DoorState.Closed(0)
      case (s, _)                                    => s
```

## 3. Test in isolation

Because `decide` and `evolve` are plain functions you can call them directly — no FSM, no effect
stack, no test kit.

```scala mdoc
// decide
assert(door.decide(DoorCommand.Knock, DoorState.Closed(0)) == List(DoorEvent.Knocked))
assert(door.decide(DoorCommand.Knock, DoorState.Closed(2)) == List(DoorEvent.Opened))
assert(door.decide(DoorCommand.Knock, DoorState.Open)      == Nil)

// evolve
assert(door.evolve(List(DoorEvent.Knocked), DoorState.Closed(1)) == DoorState.Closed(2))
assert(door.evolve(List(DoorEvent.Opened),  DoorState.Closed(2)) == DoorState.Open)
```

## 4. Lift into an FSM

`toBaseMachine[F]` turns the `Decider` into a `BaseMachineT` running in effect `F`.
Wrap it in `FSM.Basic` to get a composable machine.

```scala mdoc
val doorFsm: FSM[Id, DoorCommand, List[DoorEvent]] =
  FSM.Basic(door.toBaseMachine[Id])

val (events, nextFsm) = FSM.run(doorFsm, DoorCommand.Knock)
```

Use `FSM.runMultiple` to feed multiple commands, accumulating outputs via `Monoid`:

```scala mdoc
val (allEvents, _) = FSM.runMultiple(doorFsm, List(
  DoorCommand.Knock,
  DoorCommand.Knock,
  DoorCommand.Knock,  // third knock opens the door
  DoorCommand.Close
))
```

## 5. Add a projection

A projection is a `BaseMachineT` that folds events into a read model. Chain it with `>>>` so
every event the decider emits flows straight into the projection.

```scala mdoc
case class DoorStats(opened: Int, closed: Int)

val projection: BaseMachineT[Id, List[DoorEvent], DoorStats] =
  BaseMachineT[Id, DoorStats, List[DoorEvent], DoorStats](
    DoorStats(0, 0),
    (stats, events) =>
      val next = events.foldLeft(stats) {
        case (s, DoorEvent.Opened) => s.copy(opened = s.opened + 1)
        case (s, DoorEvent.Closed) => s.copy(closed = s.closed + 1)
        case (s, _)                => s
      }
      (next, next)
  )

val network: FSM[Id, DoorCommand, DoorStats] =
  FSM.Basic(door.toBaseMachine[Id]) >>> FSM.Basic(projection)

given cats.kernel.Monoid[DoorStats] =
  cats.kernel.Monoid.instance(DoorStats(0, 0), (a, b) => DoorStats(a.opened max b.opened, a.closed max b.closed))

val (stats, _) = FSM.runMultiple(network, List(
  DoorCommand.Knock, DoorCommand.Knock, DoorCommand.Knock, // open
  DoorCommand.Close,                                        // close
  DoorCommand.Knock, DoorCommand.Knock, DoorCommand.Knock, // open again
  DoorCommand.Close                                         // close again
))
```

## 6. Change effect (Id → IO)

Machines built with `Id` are easy to test. Use `mapK` to lift them into any other effect for
production use:

```scala mdoc
import cats.effect.IO
import cats.~>

val liftToIO: Id ~> IO = new (Id ~> IO):
  def apply[A](a: A): IO[A] = IO.pure(a)

val networkIO: FSM[IO, DoorCommand, DoorStats] = network.mapK(liftToIO)
```

## Next steps

- [Decider](decider.md) — full reference for the Decider pattern and event sourcing
- [FSM](fsm.md) — all combinators: `>>>`, `<->`, `merge`, `lmapOrEmpty`, `par`, `|||`
- [Saga](saga.md) — orchestrating multi-step distributed transactions with rollback
