package apparatus.core.fix.alg

import apparatus.core.{BaseMachineT, DeciderMaterializer}
import cats.Monad

trait DeciderEntry:
  def materialize[F[_] : Monad](m: DeciderMaterializer[F]): F[BaseMachineT[F, ?, ?]]
