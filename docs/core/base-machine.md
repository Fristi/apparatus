# BaseMachineT

`BaseMachineT[F[_], I, O]` is the primitive building block for every stateful machine in apparatus.
It hides its internal `State` type behind a type member, which lets heterogeneous machines compose
without leaking their state types to callers.

```scala
import apparatus.core.*
import cats.Id
```

## Core shape

```
state: State         -- current snapshot
action(state, input) -- F[(output, nextState)]
```

A single call to `action` advances the machine by one step: it consumes the current state and an
input, then returns an effectful pair of the output and the **new** state.

## Constructing a machine

Use `BaseMachineT.apply` when you have explicit state:

```scala
// Counter: state is Int, input is Int (delta), output is Int (running total)
val counter: BaseMachineT[Id, Int, Int] =
  BaseMachineT[Id, Int, Int, Int](
    seed  = 0,
    f     = (state, delta) => (state + delta, state + delta)
  )
// counter: BaseMachineT[Id, Int, Int] = apparatus.core.BaseMachineT$$anon$3@55fdf324

// run three steps manually
val (out1, s1) = counter.action(counter.initialState, 10)
// out1: Int = 10
// s1: State = 10
val (out2, s2) = counter.action(s1, 5)
// out2: Int = 15
// s2: State = 15
val (out3, _)  = counter.action(s2, -3)
// out3: Int = 12
```

For machines with no meaningful state use `BaseMachineT.stateless`:

```scala
// Pure function: parse a String to Double
val parser: BaseMachineT[Id, String, Double] =
  BaseMachineT.stateless[Id, String, Double](s => s.toDouble)
// parser: BaseMachineT[Id, String, Double] = apparatus.core.BaseMachineT$$anon$3@7643d14f
```

## Input / output adapters

`BaseMachineT` is a `Profunctor`: you can adapt inputs with `lmap` and outputs with `rmap`.

```scala
// Adapt input: accept a (String, Int) pair, ignore the Int
val fromPair: BaseMachineT[Id, (String, Double), Double] =
  parser.lmap[(String, Double)](_._1)
// fromPair: BaseMachineT[Id, Tuple2[String, Double], Double] = apparatus.core.BaseMachineT$$anon$2@58a02c72

// Adapt output: convert the parsed Double to a String label
val labelled: BaseMachineT[Id, String, String] =
  parser.rmap(d => s"parsed: $d")
// labelled: BaseMachineT[Id, String, String] = apparatus.core.BaseMachineT$$anon$2@48716fab
```

## Effect polymorphism

The `F[_]` parameter is open. Use `cats.Id` for pure logic, `cats.effect.IO` for side effects, or
`Either[E, *]` for fallible transitions.

```scala
import cats.data.EitherT

type Fallible[A] = Either[String, A]

val safe: BaseMachineT[Fallible, String, Double] =
  BaseMachineT.stateless[Fallible, String, Double] { s =>
    s.toDoubleOption.toRight(s"not a number: $s")
  }
```

## Wrapping in FSM

`BaseMachineT` becomes composable once wrapped in `FSM.Basic`:

```scala
val fsmCounter: FSM[Id, Int, Int] = FSM.Basic(counter)
```

See [FSM](fsm.md) for how to compose machines with `>>>`, `***`, `|||`, and `<->`.
