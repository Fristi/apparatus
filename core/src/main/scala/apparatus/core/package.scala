package apparatus

import cats.Order
import cats.implicits.*
import cats.data.NonEmptySet
import zio.blocks.schema.{Schema, SchemaError}

import scala.collection.immutable.SortedSet

package object core {
  given [A: {Schema, Order}]: Schema[NonEmptySet[A]] =
    Schema.set[A].transform(
      set => NonEmptySet.fromSet(SortedSet.from(set)).getOrElse(throw SchemaError.validationFailed("Could not convert to non empty set")),
      _.toSortedSet.toSet
    )
}
