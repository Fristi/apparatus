package apparatus.tests

import apparatus.core.*
import apparatus.core.machines.*
import apparatus.core.patterns.*
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

val incFsm:   Apparatus[Id, Inc,   Incremented] = Apparatus.openMealy(OpenMealy.stateless[Id, Inc,   Incremented](c => Incremented(c.n)))
val decFsm:   Apparatus[Id, Dec,   Decremented] = Apparatus.openMealy(OpenMealy.stateless[Id, Dec,   Decremented](c => Decremented(c.n)))
val resetFsm: Apparatus[Id, Reset, WasReset]    = Apparatus.openMealy(OpenMealy.stateless[Id, Reset, WasReset](_ => WasReset()))

val boolFsm: Apparatus[Id, Int,    Boolean] = Apparatus.openMealy(OpenMealy.stateless[Id, Int,    Boolean](n => n > 0))
val strFsm:  Apparatus[Id, String, Double]  = Apparatus.openMealy(OpenMealy.stateless[Id, String, Double](_.toDouble))

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

  // ---- Apparatus.imap: coproduct ----

  test("imap lifts right-nested Either Apparatus to sealed-trait Cmd/Evt"):
    val adtFsm: Apparatus[Id, Cmd, Evt] = (incFsm ||| (decFsm ||| resetFsm)).imap
    val mat = DeciderMaterializer.id
    assertEquals(Apparatus.runA(adtFsm, Inc(10), mat), Incremented(10))
    assertEquals(Apparatus.runA(adtFsm, Dec(3),  mat), Decremented(3))
    assertEquals(Apparatus.runA(adtFsm, Reset(), mat), WasReset())

  // ---- Apparatus.imap: product ----

  test("imap lifts tuple Apparatus to case-class In/Out"):
    val adtFsm: Apparatus[Id, In, Out] = (boolFsm *** strFsm).imap
    assertEquals(Apparatus.runA(adtFsm, In(5, "3.14"), DeciderMaterializer.id), Out(true, 3.14))

  // ---- imap preserves internal state ----

  test("imap preserves state across steps"):
    val accumFsm: Apparatus[Id, Inc, Incremented] =
      Apparatus.openMealy(OpenMealy[Id, Int, Inc, Incremented](0, (s, c) => (Incremented(s + c.n), s + c.n)))

    val adtFsm: Apparatus[Id, Cmd, Evt] = (accumFsm ||| (decFsm ||| resetFsm)).imap

    val results = Apparatus.runSteps(adtFsm, List(Inc(3), Inc(4), Reset()), DeciderMaterializer.id)
    assertEquals(results(0), Incremented(3))
    assertEquals(results(1), Incremented(7))
    assertEquals(results(2), WasReset())
