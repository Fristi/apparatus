# HFix2 — Advanced

This page explains the type-theoretic foundations of how `Apparatus` is represented as a
recursive data structure. It is not required reading for day-to-day use, but useful if you
want to understand how the library's algebra works internally or extend it.

## Fixed-point types in one dimension

A standard fixed-point type `Fix[F[_]]` is defined as:

```scala
case class Fix[F[_]](unfix: F[Fix[F]])
```

`F` is the **base functor**: it describes one layer of the recursive structure.
`Fix[F]` is the self-referential knot that ties the layers together.

For example, a list can be expressed as `Fix[ListF]` where:

```scala
sealed trait ListF[+A]
case class ConsF[A](head: Int, tail: A) extends ListF[A]
case object NilF                        extends ListF[Nothing]
```

With `Fix`, the `A` position is the recursive position, filled in by `Fix[ListF]` itself.
An algebra `F[A] => A` (a `Functor` mapping) then interprets the structure — this is
the catamorphism (fold) pattern.

## Two-dimensional fixed points: HFix2

`Apparatus` nodes are **indexed by two type parameters**: `I` (input) and `O` (output).
A standard `Fix[F[_]]` cannot express this because `F` takes only one type argument.

`HFix2` generalises the fixed-point to a binary-indexed functor `F[_, _]`:

```scala
case class HFix2[F[_[_, _], _, _], I, O](unfix: F[[I2, O2] =>> HFix2[F, I2, O2], I, O])
```

- `F[_[_, _], _, _]` — the base functor, parameterised on a recursive position `R[_, _]`, an
  input index `I`, and an output index `O`.
- `HFix2[F, I, O]` — the fixed point: `unfix` gives back one layer `F[HFix2[F, ?, ?], I, O]`.

For `Apparatus`:

```scala
type Apparatus[Eff[_], I, O] =
  HFix2[[F[_, _], I, O] =>> ApparatusF[F, Eff, I, O], I, O]
```

`ApparatusF[F, Eff, I, O]` is the base functor. It describes **one layer** of a network:
`Sequential`, `Parallel`, `Feedback`, leaf nodes, etc. The recursive positions (sub-networks)
are filled with the type parameter `F[_, _]`. When `F` is instantiated as `Apparatus[Eff, ?, ?]`,
the full recursive tree is recovered.

## HFunctor2

A standard `Functor[F[_]]` provides `map: (F[A], A => B) => F[B]` — it lifts a function
over the values inside `F`.

For two-indexed structures, the equivalent is `HFunctor2`:

```scala
trait HFunctor2[H[_[_, _], _, _]]:
  def hfmap[F[_, _], G[_, _], I, O](
    nt: FunctionK2[F, G]
  )(hfio: H[F, I, O]): H[G, I, O]
```

`FunctionK2[F, G]` is a **natural transformation** between binary functors: for all `I`, `O`,
it converts `F[I, O]` to `G[I, O]`. It is the indexed analogue of `cats.~>[F, G]`.

`ApparatusF` implements `HFunctor2` by threading `nt` over the recursive `F[_, _]` positions
in each case class variant via the per-variant `hfmap` method.

## How algorithms use this

Algorithms like `normalize` and `compile` are written as **natural transformation chains**
that walk the `HFix2` tree:

1. **`normalize`** uses `State` to traverse the tree, replacing `AggregateMachine` and
   `OpenMachine` nodes with `Ref` nodes, accumulating a registry. It operates as a function
   `Apparatus[F, I, O] => State[NormalizeState[F], Apparatus[F, I, O]]` (i.e. `HFix2` → `HFix2`).

2. **`compile`** transforms `Apparatus[F, I, O]` (an `HFix2` tree) into a `ClosedMealy[F, I, O]`
   (the runtime representation). The algebra is:

   ```
   ApparatusF[ClosedMealy[F, ?, ?], F, I, O] => F[ClosedMealy[F, I, O]]
   ```

   This is a monadic catamorphism (histomorphism) — it folds the `HFix2` tree bottom-up,
   accumulating effectful steps in `F`.

## Extending the algebra

To add a new node type to `Apparatus`:

1. Add a new case to `ApparatusF` with the appropriate `F[_, _]` positions.
2. Implement `hfmap` for the new case.
3. Add a smart constructor to `Apparatus` (and/or an extension method).
4. Handle the new case in `normalize` (in `Normalize.scala`).
5. Handle the new case in the runtime interpreter (in `alg/compile` or equivalent).
6. Optionally add rendering in `Mermaid.render`.

The type system enforces that every algorithm visits every node: `HFunctor2` and the sealed
`ApparatusF` hierarchy make exhaustiveness checking work at compile time.

## Why not Free?

An alternative encoding is a `Free[F, A]` monad. Free monads give you a flat sequence of
operations whereas `HFix2` gives you a **tree**. Trees are preferable here because:

- The topology is *structural*, not sequential — parallelism, feedback loops, and routing are
  first-class nodes, not encoded as sequences.
- Tree algorithms (normalisation, Mermaid rendering, compilation) are natural catamorphisms
  over the base functor.
- The binary indexing `[I, O]` carries the dataflow types statically through the entire
  structure, giving compile-time safety for network composition.
