package apparatus.core

import cats.Id
import cats.implicits.*

// --- domain: 3-subtype coproduct ---

sealed trait Cmd
case class Inc(n: Int) extends Cmd
case class Dec(n: Int) extends Cmd
case class Reset()     extends Cmd

sealed trait Evt
case class Incremented(n: Int) extends Evt
case class Decremented(n: Int) extends Evt
case class WasReset()          extends Evt

// --- domain: product ---

case class In(x: Int, y: String)
case class Out(a: Boolean, b: Double)

// --- fixtures ---

val incFsm:   FSM[Id, Inc,   Incremented] = FSM.Basic(BaseMachineT.stateless[Id, Inc,   Incremented](c => Incremented(c.n)))
val decFsm:   FSM[Id, Dec,   Decremented] = FSM.Basic(BaseMachineT.stateless[Id, Dec,   Decremented](c => Decremented(c.n)))
val resetFsm: FSM[Id, Reset, WasReset]    = FSM.Basic(BaseMachineT.stateless[Id, Reset, WasReset](_ => WasReset()))

val boolFsm: FSM[Id, Int,    Boolean] = FSM.Basic(BaseMachineT.stateless[Id, Int,    Boolean](n => n > 0))
val strFsm:  FSM[Id, String, Double]  = FSM.Basic(BaseMachineT.stateless[Id, String, Double](_.toDouble))

// --- tests ---

class IsoSpec extends munit.FunSuite:

  // ---- sumIso roundtrips ----

  test("sumIso: from then to is identity for all subtypes"):
    val iso = summon[Iso[Either[Inc, Either[Dec, Reset]], Cmd]]
    List[Cmd](Inc(1), Dec(2), Reset()).foreach(cmd =>
      assertEquals(iso.to(iso.from(cmd)), cmd)
    )

  test("sumIso: to then from is identity for all Either variants"):
    val iso = summon[Iso[Either[Inc, Either[Dec, Reset]], Cmd]]
    val variants: List[Either[Inc, Either[Dec, Reset]]] =
      List(Left(Inc(1)), Right(Left(Dec(2))), Right(Right(Reset())))
    variants.foreach(e => assertEquals(iso.from(iso.to(e)), e))

  test("sumIso: correct ordinal → right-nested Either encoding"):
    val iso = summon[Iso[Either[Inc, Either[Dec, Reset]], Cmd]]
    assertEquals(iso.from(Inc(5)),  Left(Inc(5)))
    assertEquals(iso.from(Dec(3)),  Right(Left(Dec(3))))
    assertEquals(iso.from(Reset()), Right(Right(Reset())))

  // ---- productIso roundtrips ----

  test("productIso: from then to is identity"):
    val iso = summon[Iso[(Int, String), In]]
    assertEquals(iso.to(iso.from(In(7, "hi"))), In(7, "hi"))

  test("productIso: to then from is identity"):
    val iso = summon[Iso[(Int, String), In]]
    assertEquals(iso.from(iso.to((42, "x"))), (42, "x"))

  // ---- FSM.imap: coproduct ----
  //
  // ||| is left-associative: a ||| b ||| c = (a ||| b) ||| c  (left-nested)
  // sumIso uses right-nesting: Either[A, Either[B, C]]
  // Use explicit right grouping:  a ||| (b ||| c)

  test("imap lifts right-nested Either FSM to sealed-trait Cmd/Evt"):
    val adtFsm: FSM[Id, Cmd, Evt] = (incFsm ||| (decFsm ||| resetFsm)).imap

    val (e1, f1) = FSM.run(adtFsm, Inc(10))
    assertEquals(e1, Incremented(10))
    val (e2, f2) = FSM.run(f1, Dec(3))
    assertEquals(e2, Decremented(3))
    val (e3, _)  = FSM.run(f2, Reset())
    assertEquals(e3, WasReset())

  // ---- FSM.imap: product ----

  test("imap lifts tuple FSM to case-class In/Out"):
    val adtFsm: FSM[Id, In, Out] = (boolFsm *** strFsm).imap

    val (out, _) = FSM.run(adtFsm, In(5, "3.14"))
    assertEquals(out, Out(true, 3.14))

  // ---- imap preserves internal state ----

  test("imap preserves state across steps"):
    val accumFsm: FSM[Id, Inc, Incremented] =
      FSM.Basic(BaseMachineT[Id, Int, Inc, Incremented](0, (s, c) => (Incremented(s + c.n), s + c.n)))

    val adtFsm: FSM[Id, Cmd, Evt] = (accumFsm ||| (decFsm ||| resetFsm)).imap

    val (e1, f1) = FSM.run(adtFsm, Inc(3))
    assertEquals(e1, Incremented(3))
    val (e2, f2) = FSM.run(f1, Inc(4))
    assertEquals(e2, Incremented(7))
    val (e3, _)  = FSM.run(f2, Reset())
    assertEquals(e3, WasReset())
