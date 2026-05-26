package apparatus.core.fix.alg

import apparatus.core.machines.{DeciderMaterializer, MealyMachine, OpenMealy}

trait DeciderEntry[F[_]]:
  def materialize(m: DeciderMaterializer[F]): F[MealyMachine[F, ?, ?]]
