package apparatus.core.fix.alg

import apparatus.core.Decider
import apparatus.core.fix.ApparatusF

type DeciderRegistry = Map[String, Decider[?, ?, ?]]

def collectStableAlg[F[_], I, O]: ApparatusF[F, I, O, DeciderRegistry] => DeciderRegistry =
  case ApparatusF.DeciderNode(id, decider) => Map(id -> decider)
  case ApparatusF.Sequential(l, r) => l ++ r
  case ApparatusF.Parallel(l, r) => l ++ r
  case ApparatusF.Alternative(l, r) => l ++ r
  case ApparatusF.Feedback(l, r, _, _, _) => l ++ r
  case ApparatusF.FeedbackMany(l, r, _, _, _) => l ++ r
  case ApparatusF.Labeled(inner, _) => inner