package apparatus.core

import cats.*
import cats.implicits.*
import cats.data.NonEmptySet
import scala.collection.immutable.SortedSet

sealed trait SagaState[Step]

object SagaState {
  case class Waiting[Step]() extends SagaState[Step]
  case class Succeeded[Step]() extends SagaState[Step]
  case class Failed[Step]() extends SagaState[Step]

  case class Running[Step](current: Step, todo: SortedSet[Step], compensation: SortedSet[Step]) extends SagaState[Step]
  case class Compensating[Step](current: Step, todo: SortedSet[Step]) extends SagaState[Step]
}

enum SagaStepResult { case Completed, Failed }

enum SagaEvent[Step]:
  case Booted(startStep: Step, todo: SortedSet[Step])
  case StepStarted(name: Step)
  case StepProgressed(name: Step, result: SagaStepResult)
  case CompensationTriggered(startStep: Step, todo: SortedSet[Step])
  case CompensationStarted(name: Step)
  case CompensationProgressed(name: Step, result: SagaStepResult)

trait SagaBehavior[Cmd, Step : {Order, Eq, Show}]:
  def startCommand: Cmd
  def steps: NonEmptySet[Step]
  def compensationHandler: PartialFunction[Cmd, (Step, SagaStepResult)]
  def stepHandler: PartialFunction[Cmd, (Step, SagaStepResult)]

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
  final def evolve(state: SagaState[Step], evt: SagaEvent[Step]): SagaState[Step] =
    state match {
      case SagaState.Waiting() =>
        evt match {
          case SagaEvent.Booted(startStep, todo) => SagaState.Running(startStep, todo, SortedSet.empty)
          case _ => state
        }
      case SagaState.Running(current, todo, compensation) =>
        evt match {
          case SagaEvent.StepStarted(name) => SagaState.Running(name, todo - name, compensation)
          case SagaEvent.StepProgressed(name, result) =>
            result match {
              case SagaStepResult.Completed =>
                SagaState.Running(current, todo - name, compensation + name)
              case _ => state
            }
          case SagaEvent.CompensationTriggered(current, todo) => SagaState.Compensating(current, todo)
          case _ => state
        }
      case SagaState.Compensating(current, todo) =>
        evt match {
          case SagaEvent.CompensationStarted(name) => SagaState.Compensating(name, todo - name)
          case SagaEvent.CompensationProgressed(name, result) =>
            result match {
              case SagaStepResult.Completed =>
                SagaState.Compensating(current, todo - name)
              case _ => state
            }
          case _ => state
        }

      case _ => state
    }

  def decider: Decider[SagaState[Step], Cmd, List[SagaEvent[Step]]] =
    DeciderBuilder.seed[SagaState[Step]](SagaState.Waiting())
      .decide[Cmd, List[SagaEvent[Step]]]((s, i) => decide(s, i))
      .evolveList((s, e) => evolve(s, e))