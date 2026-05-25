package apparatus.core

import apparatus.core.patterns
import cats.Applicative

import scala.deriving.Mirror

/** Bidirectional, lossless isomorphism between types `A` and `B`.
  *
  * Instances are derived automatically via `scala.deriving.Mirror` for:
  *   - **Products** (case classes, named tuples): `Iso[MirroredElemTypes, T]`
  *   - **Coproducts** (sealed traits, enums): `Iso[NE, T]` where `NE` is the
  *     right-nested `Either` chain built from `MirroredElemTypes`
  *
  * Coproduct encoding — subtypes map to nesting depth by declaration order:
  * {{{
  *   sealed trait Color
  *   case object Red   extends Color
  *   case object Green extends Color
  *   case object Blue  extends Color
  *
  *   // NE = Either[Red.type, Either[Green.type, Blue.type]]
  *   // Red   <-> Left(Red)
  *   // Green <-> Right(Left(Green))
  *   // Blue  <-> Right(Right(Blue))
  * }}}
  */
trait Iso[A, B]:
  def to(a: A): B
  def from(b: B): A

object Iso:

  /** Right-nested `Either` encoding for a non-empty tuple of types (documentation alias).
    *
    * {{{
    * NestedEither[(A, B, C)] =:= Either[A, Either[B, C]]
    * NestedEither[(A, B)]    =:= Either[A, B]
    * NestedEither[(A)]       =:= A
    * }}}
    */
  type NestedEither[T <: Tuple] = T match
    case h *: EmptyTuple => h
    case h *: t          => Either[h, NestedEither[t]]

  // ---------------------------------------------------------------------------
  // ToNestedEither: typeclass that maps a Tuple type to its right-nested Either.
  //
  // Unlike the NestedEither match type alias, this typeclass lets Scala's
  // implicit search resolve the Either type *forward* (Tuple → NE) rather than
  // requiring backward match-type inference (NE → Tuple), which Scala 3 cannot do.
  // ---------------------------------------------------------------------------

  /** Typeclass mapping a `Tuple` type to its [[NestedEither]] representation. */
  sealed trait ToNestedEither[T <: Tuple]:
    type Out

  object ToNestedEither:
    /** Helper alias pinning the associated `Out` type. */
    type Aux[T <: Tuple, O] = ToNestedEither[T] { type Out = O }

    /** Base case: a single-element tuple maps directly to its head type. */
    given [H]: Aux[H *: EmptyTuple, H] =
      new ToNestedEither[H *: EmptyTuple] { type Out = H }

    /** Inductive case: prepend `H` as the `Left` branch of the tail's `Either`. */
    given [H, T <: Tuple, TO](using rest: Aux[T, TO]): Aux[H *: T, Either[H, TO]] =
      new ToNestedEither[H *: T] { type Out = Either[H, TO] }

  // ---------------------------------------------------------------------------
  // Derived instances
  // ---------------------------------------------------------------------------

  /** Derive `Iso` between product type `T` and its flat tuple of field types.
    *
    * {{{
    * case class Point(x: Double, y: Double)
    * val iso = summon[Iso[(Double, Double), Point]]
    * iso.to((1.0, 2.0))        // Point(1.0, 2.0)
    * iso.from(Point(1.0, 2.0)) // (1.0, 2.0)
    * }}}
    */
  given productIso[T <: Product, Elems <: Tuple](
    using m: Mirror.ProductOf[T] { type MirroredElemTypes = Elems }
  ): Iso[Elems, T] with
    def to(a: Elems): T   = m.fromProduct(a)
    def from(b: T): Elems = Tuple.fromProduct(b).asInstanceOf[Elems]

  /** Derive `Iso` between a coproduct `T` and its right-nested `Either` representation.
    *
    * `NE` is the concrete `Either` type computed by [[ToNestedEither]] from
    * `Mirror.SumOf[T]#MirroredElemTypes`, so Scala can match it directly without
    * needing to invert a match type.
    *
    * {{{
    * sealed trait Op
    * case class Add(n: Int) extends Op
    * case class Mul(n: Int) extends Op
    * case object Reset      extends Op
    *
    * val iso = summon[Iso[Either[Add, Either[Mul, Reset.type]], Op]]
    * iso.from(Mul(3))            // Right(Left(Mul(3)))
    * iso.to(Right(Left(Mul(3)))) // Mul(3)
    * }}}
    */
  given sumIso[T, Elems <: Tuple, NE](
    using m:  Mirror.SumOf[T] { type MirroredElemTypes = Elems },
          ne: ToNestedEither.Aux[Elems, NE],
          ts: TupleSize[Elems]
  ): Iso[NE, T] with

    private val sz: Int = ts.value

    def to(e: NE): T =
      def loop(v: Any, remaining: Int): (Int, Any) =
        if remaining == 1 then (0, v)
        else v match
          case Left(a)  => (0, a)
          case Right(r) =>
            val (i, x) = loop(r, remaining - 1)
            (i + 1, x)
      loop(e, sz)._2.asInstanceOf[T]

    def from(t: T): NE =
      val ord = m.ordinal(t)
      def build(depth: Int, remaining: Int, v: Any): Any =
        if depth == 0 then (if remaining == 1 then v else Left(v))
        else Right(build(depth - 1, remaining - 1, v))
      build(ord, sz, t).asInstanceOf[NE]

  /** Witness for the runtime element count of a `Tuple` type. */
  sealed trait TupleSize[T <: Tuple]:
    def value: Int

  object TupleSize:
    /** Base case: empty tuple has size 0. */
    given TupleSize[EmptyTuple] with
      def value = 0
    /** Inductive case: size of `H *: T` is one more than the size of `T`. */
    given [H, T <: Tuple](using ts: TupleSize[T]): TupleSize[H *: T] with
      def value = 1 + ts.value
