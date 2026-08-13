package apparatus.tests

import apparatus.core.machines.Decider

object DeciderTestSupport:
  /** Run `decide` then `evolve` for a single command. */
  def step[S, I, E](decider: Decider[S, I, List[E]], state: S, cmd: I): (List[E], S) =
    val events = decider.decide(cmd, state)
    (events, decider.evolve(events, state))

  /** Fold commands through decide/evolve, accumulating all emitted events. */
  def replay[S, I, E](decider: Decider[S, I, List[E]], initial: S, cmds: List[I]): (List[E], S) =
    cmds.foldLeft((Nil: List[E], initial)) { case ((events, state), cmd) =>
      val (stepEvents, next) = step(decider, state, cmd)
      (events ++ stepEvents, next)
    }
