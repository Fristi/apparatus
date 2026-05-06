package apparatus.core

import cats.*
import cats.data.NonEmptySet

enum SagaState:
  case Waiting
  case Succeeded
  case Failed
  case Running(current: String, todo: Set[String], compensation: Set[String])
  case Compensating(current: String, todo: Set[String])

enum SagaStepResult { case Completed, Failed }

enum SagaEvent:
  case Booted(startStep: String, todo: Set[String])
  case StepStarted(name: String)
  case StepProgressed(name: String, result: SagaStepResult)
  case CompensationTriggered(startStep: String, todo: Set[String])
  case CompensationStarted(name: String)
  case CompensationProgressed(name: String, result: SagaStepResult)

trait SagaBehavior[Cmd]:
  def startCommand: Cmd
  def steps: NonEmptySet[String]
  def compensationHandler: PartialFunction[Cmd, (String, SagaStepResult)]
  def stepHandler: PartialFunction[Cmd, (String, SagaStepResult)]

  final def decide(state: SagaState, cmd: Cmd): List[SagaEvent] = state match {
    case SagaState.Waiting => if(cmd == startCommand) List(SagaEvent.Booted(steps.head, steps.tail)) else Nil
    case SagaState.Running(current, todo, compensation) =>
      stepHandler.unapply(cmd) match {
        case Some((stepName, result)) =>
          result match {
            case SagaStepResult.Completed =>
              if(current == stepName) {
                List(SagaEvent.StepProgressed(stepName, result)) ++ todo.headOption.map(SagaEvent.StepStarted(_))
              } else {
                Nil
              }
            case SagaStepResult.Failed =>
              if(current == stepName) {
                val progressEvent = List(SagaEvent.StepProgressed(stepName, result))
                val triggeredEvent = compensation.headOption.map(SagaEvent.CompensationTriggered(_, compensation.tail)).toList

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
              if(current == stepName) {
                List(SagaEvent.CompensationProgressed(stepName, result)) ++ todo.headOption.map(SagaEvent.CompensationStarted(_))
              } else {
                Nil
              }
            case SagaStepResult.Failed =>
              if(current == stepName) {
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
  final def evolve(state: SagaState, evt: SagaEvent): SagaState = state match {
    case SagaState.Waiting =>
      evt match {
        case SagaEvent.Booted(startStep, todo) => SagaState.Running(startStep, todo, Set.empty)
        case _ => state
      }
    case SagaState.Running(current, todo, compensation) =>
      evt match {
        case SagaEvent.StepStarted(name) => SagaState.Running(name, todo, compensation)
        case SagaEvent.StepProgressed(name, result) =>
          result match {
            case SagaStepResult.Completed => SagaState.Running(current, todo - name, compensation + name)
            case _ => state
          }
        case SagaEvent.CompensationTriggered(current, todo) => SagaState.Compensating(current, todo)
        case _ => state
      }
    case SagaState.Compensating(current, todo) =>
      evt match {
        case SagaEvent.CompensationStarted(name) => SagaState.Compensating(name, todo)
        case SagaEvent.CompensationProgressed(name, result) =>
          result match {
            case SagaStepResult.Completed => SagaState.Compensating(current, todo - name)
            case _ => state
          }
        case _ => state
      }

    case _ => state
  }

  def decider: Decider[SagaState, Cmd, List[SagaEvent]] =
    DeciderBuilder.seed[SagaState](SagaState.Waiting)
      .decide[Cmd, List[SagaEvent]]((s, i) => decide(s, i))
      .evolveList((s, e) => evolve(s, e))