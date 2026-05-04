# FSM

`FSM[F[_], I, O]` is a composable, immutable finite-state machine GADT. Each call to `runWith`
returns the output **and** an updated copy of the machine, making the whole graph replayable.

```scala mdoc
import apparatus.core.*
import cats.Id
import cats.implicits.*
import cats.kernel.Monoid
```

## Basic — wrapping a single machine

`FSM.Basic` lifts any `BaseMachineT` into the composable `FSM` layer:

```scala mdoc
val counter: FSM[Id, Int, Int] =
  FSM.Basic(BaseMachineT[Id, Int, Int, Int](0, (s, d) => (s + d, s + d)))

val (out1, next) = FSM.run(counter, 10)
val (out2, _)    = FSM.run(next,    5)
```

---

## Sequential — pipe output into input

`left >>> right` (or `left.andThen(right)`) wires the output of `left` directly into the input of
`right` at every step. Both machines carry independent state.

**Use when** you want a processing pipeline: e.g. a `Decider` emitting events piped into a
projection that folds those events into a read model.

```scala mdoc
// Machine A: doubles its input
val double: FSM[Id, Int, Int] =
  FSM.Basic(BaseMachineT.stateless[Id, Int, Int](n => n * 2))

// Machine B: keeps a running sum
val runningSum: FSM[Id, Int, Int] =
  FSM.Basic(BaseMachineT[Id, Int, Int, Int](0, (s, n) => (s + n, s + n)))

// Pipeline: input is doubled, then added to the running sum
val pipeline: FSM[Id, Int, Int] = double >>> runningSum

val (rs1, p1) = FSM.run(pipeline, 3)  // 3*2=6 → sum=6
val (rs2, p2) = FSM.run(p1,       4)  // 4*2=8 → sum=14
val (rs3, _)  = FSM.run(p2,       1)  // 1*2=2 → sum=16
```

---

## Parallel — independent machines on a pair

`left *** right` (or `left.par(right)`) runs two machines concurrently on the two halves of an
`(A, C)` pair, producing `(B, D)`.

**Use when** you need to process two independent streams simultaneously or fan out a single event
to multiple projections after splitting it.

```scala mdoc
// Two independent counters
val addCounter:  FSM[Id, Int, Int] =
  FSM.Basic(BaseMachineT[Id, Int, Int, Int](0, (s, n) => (s + n, s + n)))

val mulCounter:  FSM[Id, Int, Int] =
  FSM.Basic(BaseMachineT[Id, Int, Int, Int](1, (s, n) => (s * n, s * n)))

val parallel: FSM[Id, (Int, Int), (Int, Int)] = addCounter *** mulCounter

// Feed (3, 3): left gets 3 (sum=3), right gets 3 (product=3)
val ((a1, m1), par1) = FSM.run(parallel, (3, 3))
// Feed (4, 2): left gets 4 (sum=7), right gets 2 (product=6)
val ((a2, m2), _)    = FSM.run(par1,    (4, 2))
```

---

## Alternative — route Either input

`left ||| right` (or `left.or(right)`) routes `Left(a)` to `left` and `Right(c)` to `right`.
The machine that does **not** receive input keeps its state frozen.

**Use when** you handle a discriminated union of inputs — e.g. routing commands to different
aggregates or dispatching events by type.

```scala mdoc
enum Coin { case Quarter, Dime }
enum Note  { case One, Five }

val coinCounter: FSM[Id, Coin, String] =
  FSM.Basic(BaseMachineT[Id, Int, Coin, String](0, (s, c) =>
    val v = c match { case Coin.Quarter => 25; case Coin.Dime => 10 }
    (s"coins: ${s + v}¢", s + v)
  ))

val noteCounter: FSM[Id, Note, String] =
  FSM.Basic(BaseMachineT[Id, Int, Note, String](0, (s, n) =>
    val v = n match { case Note.One => 1; case Note.Five => 5 }
    (s"notes: $$${s + v}", s + v)
  ))

val router: FSM[Id, Either[Coin, Note], Either[String, String]] =
  coinCounter ||| noteCounter

val (Left(c1),  ra1) = FSM.run(router, Left(Coin.Quarter))
val (Right(n1), ra2) = FSM.run(ra1,     Right(Note.Five))
val (Left(c2),  _)  = FSM.run(ra2,     Left(Coin.Dime))
```

---

## Feedback — closed feedback loop

`left <-> right` closes a loop between two machines:

1. `left` consumes an `A` and emits `N[B]`.
2. Each `B` is fed into `right`, which emits `N[A]`.
3. Those new `A` values are processed recursively until quiescence.

`N[_]` must have both `Foldable` and `Monoid` (e.g. `List`).

**Use when** one machine drives another in a saga or workflow: the orchestrator emits commands, the
services execute them and return results, which in turn drive the orchestrator forward.

```scala mdoc:silent
// Tiny echo example: left echoes its input as a single-element list.
// right converts each B back to an A (here both are String).
// The loop terminates because right always returns Nil.

val echo: FSM[Id, String, List[String]] =
  FSM.Basic(BaseMachineT.stateless[Id, String, List[String]](s => List(s)))

val sink: FSM[Id, String, List[String]] =
  FSM.Basic(BaseMachineT.stateless[Id, String, List[String]](_ => Nil))

val loop: FSM[Id, String, List[String]] = echo <-> sink
```

A realistic example is the booking saga in the test suite (`BookingSagaSpec`): an orchestrator FSM
(`left`) emits `List[BookingOutput]` service commands; the service router (`right`) processes each
command and emits `List[BookingInput]` result events back to the orchestrator.

---

## Running a machine

```scala mdoc:silent
// Single step — returns (output, updated FSM)
val (out, nextFsm) = FSM.run(counter, 42)

// Single step — returns output only
val outOnly = FSM.runA(counter, 42)

// Fold over many inputs, combining outputs via Monoid
val (total, finalFsm) = FSM.runMultiple(counter, List(1, 2, 3, 4))
```

---

## Adapter combinators

| Method | Signature | Purpose |
|--------|-----------|---------|
| `lmap`   | `(C => A) => FSM[F, C, B]` | Adapt the input |
| `rmap`   | `(B => C) => FSM[F, A, C]` | Adapt the output |
| `dimap`  | `(C => A, B => D) => FSM[F, C, D]` | Adapt both |
| `first`  | `FSM[F, (A,C), (B,C)]` | Pass `_2` through unchanged |
| `second` | `FSM[F, (C,A), (C,B)]` | Pass `_1` through unchanged |
| `left`   | `FSM[F, Either[A,C], Either[B,C]]` | Route `Left`; forward `Right` |
| `right`  | `FSM[F, Either[C,A], Either[C,B]]` | Route `Right`; forward `Left` |
| `imap`   | requires `Iso` implicits | Adapt between tuple/Either and ADT |
| `tap`    | `FSM[F, B, C] => FSM[F, A, B]` | Run `right` for side effects, discard its output |
