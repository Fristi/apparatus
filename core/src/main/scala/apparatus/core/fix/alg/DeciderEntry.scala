package apparatus.core.fix.alg

import apparatus.core.{BaseMachineT, DeciderMaterializer}

trait DeciderEntry[F[_]]:
  def materialize(m: DeciderMaterializer[F]): F[BaseMachineT[F, ?, ?]]
