package apparatus.core.fix.alg

import apparatus.core.machines.{ClosedMealy, DeciderMaterializer}
import cats.Monad
import cats.effect.kernel.Ref
import cats.implicits.*

import java.util.UUID

trait AggregateEntry[F[_]]:
  def compileRouter(m: DeciderMaterializer[F])(using Ref.Make[F], Monad[F]): F[ClosedMealy[F, ?, ?]]
