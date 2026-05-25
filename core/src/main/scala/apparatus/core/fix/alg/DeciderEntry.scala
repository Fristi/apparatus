package apparatus.core.fix.alg

import apparatus.core.machines.{ClosedMealy, DeciderMaterializer, MealyMachine, OpenMealy}

trait DeciderEntry[F[_]]:
  def materialize(m: DeciderMaterializer[F]): F[MealyMachine[F, ?, ?]]
