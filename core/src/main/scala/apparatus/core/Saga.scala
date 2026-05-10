package apparatus.core

import cats.*
import cats.implicits.*
import cats.data.NonEmptySet
import scala.collection.immutable.SortedSet

/** Lifecycle state of a saga.
  *
  * A saga moves through the following states:
  *
  * {{{
  * Waiting ──(Boot)──► Running ──(all steps complete)──► Succeeded
  *                        │
  *                        └──(step fails)──► Compensating ──(all compensations done)──► Failed
  * }}}
  *
  * @tparam Step the step type, which must have `Order`, `Eq`, and `Show` instances
  */
sealed trait SagaState[Step]

object SagaState {

  /** Initial state. The saga has not been started yet. */
  case class Waiting[Step]() extends SagaState[Step]

  /** All forward steps completed successfully. */
  case class Succeeded[Step]() extends SagaState[Step]

  /** Compensation finished (regardless of individual step outcomes). */
  case class Failed[Step]() extends SagaState[Step]

  /** A forward run is in progress.
    *
    * @param current     the step currently executing
    * @param todo        remaining steps to execute in order
    * @param compensation steps that have completed and must be compensated if a later step fails
    */
  case class Running[Step](current: Step, todo: SortedSet[Step], compensation: SortedSet[Step]) extends SagaState[Step]

  /** Compensation is in progress after a forward step failed.
    *
    * @param current the compensation step currently executing
    * @param todo    remaining compensation steps to execute in order
    */
  case class Compensating[Step](current: Step, todo: SortedSet[Step]) extends SagaState[Step]
}

/** Outcome reported by an external service for a single saga step or compensation step. */
enum SagaStepResult { case Completed, Failed }

/** Events emitted by [[SagaBehavior.decide]] and consumed by [[SagaBehavior.evolve]].
  *
  * These events are the persistent record of what happened during the saga. Replaying
  * them against [[SagaBehavior.evolve]] fully reconstructs [[SagaState]].
  *
  * @tparam Step the step type
  */
enum SagaEvent[Step]:
  /** The saga was started. `startStep` is the first step to execute; `todo` is the remaining set. */
  case Booted(startStep: Step, todo: SortedSet[Step])

  /** A forward step has been dispatched to the external service. */
  case StepStarted(name: Step)

  /** An external service reported the result of a forward step. */
  case StepProgressed(name: Step, result: SagaStepResult)

  /** A forward step failed; compensation begins at `startStep` working through `todo`. */
  case CompensationTriggered(startStep: Step, todo: SortedSet[Step])

  /** A compensation step has been dispatched to the external service. */
  case CompensationStarted(name: Step)

  /** An external service reported the result of a compensation step. */
  case CompensationProgressed(name: Step, result: SagaStepResult)

/** Defines the domain-specific shape of a saga.
  *
  * Extend this trait to describe your saga's steps and how incoming commands map to step
  * results.  The trait provides final `decide` and `evolve` implementations that drive the
  * generic [[SagaState]] machine; you only need to supply four things:
  *
  *   - [[startCommand]] — the command that boots the saga
  *   - [[steps]] — the ordered set of forward steps
  *   - [[stepHandler]] — translates incoming commands to `(Step, SagaStepResult)` during a forward run
  *   - [[compensationHandler]] — translates incoming commands to `(Step, SagaStepResult)` during compensation
  *
  * Lift a concrete behavior into an Apparatus via [[decider]]:
  *
  * {{{
  * val fsm: Apparatus[Id, BookingCommand, List[SagaEvent[BookingStep]]] =
  *   Apparatus.Fresh(behavior.decider.toBaseMachine)
  * }}}
  *
  * @tparam Cmd  the command type
  * @tparam Step the step type; must have `Order`, `Eq`, and `Show` instances so steps can be
  *              stored in a `SortedSet` and compared safely
  */
trait SagaBehavior[Cmd, Step : {Order, Eq, Show}]:

  /** The command that transitions [[SagaState.Waiting]] → [[SagaState.Running]]. */
  def startCommand: Cmd

  /** Ordered set of forward steps. The saga executes them head-to-tail. */
  def steps: NonEmptySet[Step]

  /** Translates an incoming command to a `(step, result)` pair during compensation.
    *
    * Return `(step, SagaStepResult.Completed)` when the external service confirms that
    * `step` was rolled back successfully; `SagaStepResult.Failed` otherwise.
    * The partial function should be defined for every command that an external service can
    * send as a compensation acknowledgement — it is silently ignored when undefined.
    */
  def compensationHandler: PartialFunction[Cmd, (Step, SagaStepResult)]

  /** Translates an incoming command to a `(step, result)` pair during the forward run.
    *
    * Return `(step, SagaStepResult.Completed)` when the external service confirms success;
    * `SagaStepResult.Failed` when it reports failure.
    * The partial function should be defined for every command that an external service can
    * send as a forward-step acknowledgement — it is silently ignored when undefined.
    */
  def stepHandler: PartialFunction[Cmd, (Step, SagaStepResult)]

  /** Pure decision function: maps `(state, command)` → list of [[SagaEvent]]s.
    *
    * Rules:
    *   - `Waiting`     — emits [[SagaEvent.Booted]] only when `cmd == startCommand`
    *   - `Running`     — delegates to [[stepHandler]]; on `Completed` advances to next step,
    *                     on `Failed` triggers compensation via [[SagaEvent.CompensationTriggered]]
    *   - `Compensating`— delegates to [[compensationHandler]]; advances through compensation steps
    *   - `Succeeded` / `Failed` — always emits `Nil`
    */
  final def decide(state: SagaState[Step], cmd: Cmd): List[SagaEvent[Step]] = state match {
    case SagaState.Waiting() => if(cmd == startCommand) List(SagaEvent.Booted(steps.head, steps.tail)) else Nil
    case SagaState.Running(current, todo, compensation) =>
      stepHandler.unapply(cmd) match {
        case Some((stepName, result)) =>
          result match {
            case SagaStepResult.Completed =>
              if(current === stepName) {
                List(SagaEvent.StepProgressed(stepName, result)) ++ todo.headOption.map(SagaEvent.StepStarted(_))
              } else {
                Nil
              }
            case SagaStepResult.Failed =>
              if(current === stepName) {
                val progressEvent: List[SagaEvent[Step]] = List(SagaEvent.StepProgressed(stepName, result))
                val triggeredEvent: List[SagaEvent[Step]] = compensation.headOption.map(step => SagaEvent.CompensationTriggered(step, compensation.tail)).toList

                progressEvent  ++ triggeredEvent
              } else {
                Nil
              }

          }
        case None => Nil
      }
    case SagaState.Compensating(current, todo) =>
      compensationHandler.unapply(cmd) match {
        case Some((stepName, result)) =>
          result match {
            case SagaStepResult.Completed =>
              if(current === stepName) {
                List(SagaEvent.CompensationProgressed(stepName, result)) ++ todo.headOption.map(SagaEvent.CompensationStarted(_))
              } else {
                Nil
              }
            case SagaStepResult.Failed =>
              if(current === stepName) {
                val progressEvent = List(SagaEvent.CompensationProgressed(stepName, result))
                val startEvent = todo.headOption.map(SagaEvent.CompensationStarted(_)).toList
                progressEvent ++ startEvent
              } else {
                Nil
              }
          }
        case None => Nil
      }
    case _ => Nil
  }

  /** Pure evolution function: folds a [[SagaEvent]] into the current [[SagaState]].
    *
    * This is the replay function used both during normal execution and event-log replay.
    * It is total: unknown event/state combinations return the state unchanged.
    */
  final def evolve(state: SagaState[Step], evt: SagaEvent[Step]): SagaState[Step] =
    state match {
      case SagaState.Waiting() =>
        evt match {
          case SagaEvent.Booted(startStep, todo) => SagaState.Running(startStep, todo, SortedSet.empty)
          case _ => state
        }
      case SagaState.Running(_, todo, compensation) =>
        evt match {
          case SagaEvent.StepProgressed(name, SagaStepResult.Completed) =>
            if todo.isEmpty then SagaState.Succeeded()
            else SagaState.Running(todo.head, todo.tail, compensation + name)
          case SagaEvent.CompensationTriggered(startStep, compensTodo) =>
            SagaState.Compensating(startStep, compensTodo)
          case _ => state
        }
      case SagaState.Compensating(_, todo) =>
        evt match {
          case SagaEvent.CompensationProgressed(_, SagaStepResult.Completed) =>
            if todo.isEmpty then SagaState.Failed()
            else SagaState.Compensating(todo.head, todo.tail)
          case _ => state
        }

      case _ => state
    }

  /** Builds a [[Decider]] that wraps [[decide]] and [[evolve]], seeded at [[SagaState.Waiting]].
    *
    * Lift it into an Apparatus with `behavior.decider.toBaseMachine`.
    */
  def decider: Decider[SagaState[Step], Cmd, List[SagaEvent[Step]]] =
    DeciderBuilder.seed[SagaState[Step]](SagaState.Waiting())
      .decide[Cmd, List[SagaEvent[Step]]]((s, i) => decide(s, i))
      .evolveList((s, e) => evolve(s, e))
