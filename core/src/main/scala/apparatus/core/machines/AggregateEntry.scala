package apparatus.core.machines

import cats.Monad
import cats.effect.kernel.Ref

trait AggregateEntry[F[_]]:
  def compileRouter(m: DeciderMaterializer[F])(using Ref.Make[F], Monad[F]): F[ClosedMealy[F, ?, ?]]
