# Iso

`Iso[A, B]` is a bidirectional, lossless isomorphism: it converts `A → B` (`to`) and `B → A`
(`from`) with no information loss.

apparatus derives `Iso` instances automatically for **products** (case classes) and **coproducts**
(sealed traits / enums) via `scala.deriving.Mirror`. This lets you write machines in terms of
primitive tuples and right-nested `Either`s, then lift them to your domain ADTs with a single
`.imap` call.

```scala mdoc
import apparatus.core.*
import cats.Id
```

---

## Products — case class ↔ tuple

`productIso` derives `Iso[ElemTypes, CaseClass]` where `ElemTypes` is the flat tuple of field types.

```scala mdoc
case class Point(x: Double, y: Double)

val isoPoint = summon[Iso[(Double, Double), Point]]

isoPoint.to((1.0, 2.0))        // Point(1.0, 2.0)
isoPoint.from(Point(3.0, 4.0)) // (3.0, 4.0)
```

The round-trips are total and lossless:

```scala mdoc
val p = Point(7.0, -1.5)
assert(isoPoint.to(isoPoint.from(p)) == p)

val t = (42.0, 0.0)
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

```scala mdoc
sealed trait Op
case class Add(n: Int) extends Op
case class Mul(n: Int) extends Op
case object Reset      extends Op

val isoOp = summon[Iso[Either[Add, Either[Mul, Reset.type]], Op]]

isoOp.from(Add(1))          // Left(Add(1))
isoOp.from(Mul(3))          // Right(Left(Mul(3)))
isoOp.from(Reset)           // Right(Right(Reset))

isoOp.to(Right(Left(Mul(3))))  // Mul(3)
```

---

## Apparatus.imap — lifting machines to ADTs

`Apparatus.imap` uses `Iso` to adapt a machine's input and output simultaneously. It requires two `Iso`
instances to be in scope: one for the input type and one for the output type.

### Product example

```scala mdoc
case class In(x: Int, flag: Boolean)
case class Out(label: String, value: Double)

val tupleFsm: Apparatus[Id, (Int, Boolean), (String, Double)] =
  Apparatus.Basic(BaseMachineT.stateless[Id, (Int, Boolean), (String, Double)] {
    case (n, b) => (if b then "yes" else "no", n.toDouble)
  })

// Lift to case-class I/O with imap
val adtFsm: Apparatus[Id, In, Out] = tupleFsm.imap

val (out, _) = Apparatus.run(adtFsm, In(42, true))
```

### Coproduct example

`||| ` is left-associative; `sumIso` uses **right-nesting** — group explicitly with parentheses.

```scala mdoc
sealed trait Result
case class Doubled(n: Int)  extends Result
case class Negated(n: Int)  extends Result
case object Zero            extends Result

val doubleFsm: Apparatus[Id, Add, Doubled] =
  Apparatus.Basic(BaseMachineT.stateless[Id, Add, Doubled](c => Doubled(c.n * 2)))

val negateFsm: Apparatus[Id, Mul, Negated] =
  Apparatus.Basic(BaseMachineT.stateless[Id, Mul, Negated](c => Negated(-c.n)))

val resetFsm2: Apparatus[Id, Reset.type, Zero.type] =
  Apparatus.Basic(BaseMachineT.stateless[Id, Reset.type, Zero.type](_ => Zero))

// Right-group to match sumIso nesting
val eitherFsm = doubleFsm ||| (negateFsm ||| resetFsm2)

val adtFsm2: Apparatus[Id, Op, Result] = eitherFsm.imap

val (r1, f1) = Apparatus.run(adtFsm2, Add(5))
val (r2, f2) = Apparatus.run(f1,      Mul(3))
val (r3, _)  = Apparatus.run(f2,      Reset)
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