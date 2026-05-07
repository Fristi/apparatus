# Iso

`Iso[A, B]` is a bidirectional, lossless isomorphism: it converts `A → B` (`to`) and `B → A`
(`from`) with no information loss.

apparatus derives `Iso` instances automatically for **products** (case classes) and **coproducts**
(sealed traits / enums) via `scala.deriving.Mirror`. This lets you write machines in terms of
primitive tuples and right-nested `Either`s, then lift them to your domain ADTs with a single
`.imap` call.

```scala
import apparatus.core.*
import cats.Id
```

---

## Products — case class ↔ tuple

`productIso` derives `Iso[ElemTypes, CaseClass]` where `ElemTypes` is the flat tuple of field types.

```scala
case class Point(x: Double, y: Double)

val isoPoint = summon[Iso[(Double, Double), Point]]
// isoPoint: productIso[Point, Tuple2[Double, Double]] = apparatus.core.Iso$productIso@4b197d70

isoPoint.to((1.0, 2.0))        // Point(1.0, 2.0)
// res0: Point = Point(x = 1.0, y = 2.0)
isoPoint.from(Point(3.0, 4.0)) // (3.0, 4.0)
// res1: Tuple2[Double, Double] = (3.0, 4.0)
```

The round-trips are total and lossless:

```scala
val p = Point(7.0, -1.5)
// p: Point = Point(x = 7.0, y = -1.5)
assert(isoPoint.to(isoPoint.from(p)) == p)

val t = (42.0, 0.0)
// t: Tuple2[Double, Double] = (42.0, 0.0)
assert(isoPoint.from(isoPoint.to(t)) == t)
```

---

## Coproducts — sealed trait ↔ right-nested Either

`sumIso` derives `Iso[NE, SealedTrait]` where `NE` is the **right-nested** `Either` built from the
subtypes in declaration order.

```
sealed trait Color
case object Red   extends Color   // → Left(Red)
case object Green extends Color   // → Right(Left(Green))
case object Blue  extends Color   // → Right(Right(Blue))

NE = Either[Red.type, Either[Green.type, Blue.type]]
```

```scala
sealed trait Op
case class Add(n: Int) extends Op
case class Mul(n: Int) extends Op
case object Reset      extends Op

val isoOp = summon[Iso[Either[Add, Either[Mul, Reset.type]], Op]]
// isoOp: sumIso[Op, *:[Add, *:[Mul, *:[Reset, EmptyTuple]]], Either[Add, Either[Mul, Reset]]] = apparatus.core.Iso$sumIso@346ba3a9

isoOp.from(Add(1))          // Left(Add(1))
// res4: Either[Add, Either[Mul, Reset]] = Left(Add(1))
isoOp.from(Mul(3))          // Right(Left(Mul(3)))
// res5: Either[Add, Either[Mul, Reset]] = Right(Left(Mul(3)))
isoOp.from(Reset)           // Right(Right(Reset))
// res6: Either[Add, Either[Mul, Reset]] = Right(Right(Reset))

isoOp.to(Right(Left(Mul(3))))  // Mul(3)
// res7: Op = Mul(3)
```

---

## FSM.imap — lifting machines to ADTs

`FSM.imap` uses `Iso` to adapt a machine's input and output simultaneously. It requires two `Iso`
instances to be in scope: one for the input type and one for the output type.

### Product example

```scala
case class In(x: Int, flag: Boolean)
case class Out(label: String, value: Double)

val tupleFsm: FSM[Id, (Int, Boolean), (String, Double)] =
  FSM.Basic(BaseMachineT.stateless[Id, (Int, Boolean), (String, Double)] {
    case (n, b) => (if b then "yes" else "no", n.toDouble)
  })
// tupleFsm: FSM[Id, Tuple2[Int, Boolean], Tuple2[String, Double]] = Basic(
//   apparatus.core.BaseMachineT$$anon$3@604fdc8a
// )

// Lift to case-class I/O with imap
val adtFsm: FSM[Id, In, Out] = tupleFsm.imap
// adtFsm: FSM[Id, In, Out] = Basic(
//   apparatus.core.BaseMachineT$$anon$2@43c964d1
// )

val (out, _) = FSM.run(adtFsm, In(42, true))
// out: Out = Out(label = "yes", value = 42.0)
```

### Coproduct example

`||| ` is left-associative; `sumIso` uses **right-nesting** — group explicitly with parentheses.

```scala
sealed trait Result
case class Doubled(n: Int)  extends Result
case class Negated(n: Int)  extends Result
case object Zero            extends Result

val doubleFsm: FSM[Id, Add, Doubled] =
  FSM.Basic(BaseMachineT.stateless[Id, Add, Doubled](c => Doubled(c.n * 2)))
// doubleFsm: FSM[Id, Add, Doubled] = Basic(
//   apparatus.core.BaseMachineT$$anon$3@7950bf00
// )

val negateFsm: FSM[Id, Mul, Negated] =
  FSM.Basic(BaseMachineT.stateless[Id, Mul, Negated](c => Negated(-c.n)))
// negateFsm: FSM[Id, Mul, Negated] = Basic(
//   apparatus.core.BaseMachineT$$anon$3@6a45ddd2
// )

val resetFsm2: FSM[Id, Reset.type, Zero.type] =
  FSM.Basic(BaseMachineT.stateless[Id, Reset.type, Zero.type](_ => Zero))
// resetFsm2: FSM[Id, Reset, Zero] = Basic(
//   apparatus.core.BaseMachineT$$anon$3@dc59f45
// )

// Right-group to match sumIso nesting
val eitherFsm = doubleFsm ||| (negateFsm ||| resetFsm2)
// eitherFsm: FSM[[A >: Nothing <: Any] =>> A, Either[Add, Either[Mul, Reset]], Either[Doubled, Either[Negated, Zero]]] = Alternative(
//   left = Basic(apparatus.core.BaseMachineT$$anon$3@7950bf00),
//   right = Alternative(
//     left = Basic(apparatus.core.BaseMachineT$$anon$3@6a45ddd2),
//     right = Basic(apparatus.core.BaseMachineT$$anon$3@dc59f45)
//   )
// )

val adtFsm2: FSM[Id, Op, Result] = eitherFsm.imap
// adtFsm2: FSM[Id, Op, Result] = Sequential(
//   left = Basic(apparatus.core.BaseMachineT$$anon$3@41ba569f),
//   right = Sequential(
//     left = Alternative(
//       left = Basic(apparatus.core.BaseMachineT$$anon$3@7950bf00),
//       right = Alternative(
//         left = Basic(apparatus.core.BaseMachineT$$anon$3@6a45ddd2),
//         right = Basic(apparatus.core.BaseMachineT$$anon$3@dc59f45)
//       )
//     ),
//     right = Basic(apparatus.core.BaseMachineT$$anon$3@7d22654e)
//   )
// )

val (r1, f1) = FSM.run(adtFsm2, Add(5))
// r1: Result = Doubled(10)
// f1: FSM[[A >: Nothing <: Any] =>> A, Op, Result] = Sequential(
//   left = Basic(apparatus.core.BaseMachineT$$anon$3@52ad812a),
//   right = Sequential(
//     left = Alternative(
//       left = Basic(apparatus.core.BaseMachineT$$anon$3@47fa8d40),
//       right = Alternative(
//         left = Basic(apparatus.core.BaseMachineT$$anon$3@6a45ddd2),
//         right = Basic(apparatus.core.BaseMachineT$$anon$3@dc59f45)
//       )
//     ),
//     right = Basic(apparatus.core.BaseMachineT$$anon$3@2cf6291a)
//   )
// )
val (r2, f2) = FSM.run(f1,      Mul(3))
// r2: Result = Negated(-3)
// f2: FSM[[A >: Nothing <: Any] =>> A, Op, Result] = Sequential(
//   left = Basic(apparatus.core.BaseMachineT$$anon$3@29b8f050),
//   right = Sequential(
//     left = Alternative(
//       left = Basic(apparatus.core.BaseMachineT$$anon$3@47fa8d40),
//       right = Alternative(
//         left = Basic(apparatus.core.BaseMachineT$$anon$3@26c36744),
//         right = Basic(apparatus.core.BaseMachineT$$anon$3@dc59f45)
//       )
//     ),
//     right = Basic(apparatus.core.BaseMachineT$$anon$3@4479f3ca)
//   )
// )
val (r3, _)  = FSM.run(f2,      Reset)
// r3: Result = Zero
```

---

## Why isomorphisms matter

Tuples and right-nested `Either`s are the **canonical structural types** that Scala's `Mirror`
exposes. They compose well generically but are verbose in domain code.

`Iso` bridges the two worlds:

- **Write machines** using structural types — easy to compose, test, and parameterise.
- **Expose machines** using domain ADTs — readable, type-safe API for callers.

No runtime overhead: `Iso.to` and `Iso.from` compile to direct field access or a single pattern
match, equivalent to what you would write by hand.
