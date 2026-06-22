package apparatus.core.patterns

import apparatus.core.*
import apparatus.core.machines.{Decider, DeciderBuilder, evolveList}
import cats.*
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.implicits.*
import zio.blocks.schema.Schema

import java.util.UUID

/** Distinguishes which direction a saga is currently running.
  *
  * A saga always starts in the [[Forward]] phase, executing its steps in order.
  * If any step fails the saga switches to [[Compensation]], replaying completed
  * steps in reverse to undo their side effects.
  *
  * `SagaPhase` is embedded in every [[SagaAdvanceCodec]] round-trip so that a
  * single command type (e.g. `BookingCommand.Advance`) can carry both forward
  * acknowledgements and compensation acknowledgements without ambiguity.
  */
enum SagaPhase {
  case Forward
  case Compensation
}

/** Generates command ids for saga step dispatches. */
trait SagaStepCorrelationIdGenerator[-Step]:
  def next(step: Step): UUID

object SagaStepCorrelationIdGenerator:
  def random[Step]: SagaStepCorrelationIdGenerator[Step] = new SagaStepCorrelationIdGenerator[Step]:
    override def next(step: Step): UUID = UUID.randomUUID()

/** Bidirectional codec between a saga command type and an advance acknowledgement.
  *
  * Maps between a saga's command type `Cmd` and the quadruple
  * `(correlationId, step, phase, result)` that describes a single acknowledgement from an
  * external service.
  *
  * Implement once per saga command type and pass it to [[SagaBehaviorFactory]]
  * and to every [[SagaStepAdapter.adapt]] call so the saga orchestrator and the
  * individual service adapters share a consistent encoding.
  */
trait SagaAdvanceCodec[Cmd, Stp]:
  def decode(cmd: Cmd): Option[(UUID, Stp, SagaPhase, SagaStepResult)]
  def encode(id: UUID, step: Stp, phase: SagaPhase, result: SagaStepResult): Cmd

/** @deprecated use [[SagaAdvanceCodec]] */
type SagaAdvancePrism[Cmd, Stp] = SagaAdvanceCodec[Cmd, Stp]

/** @deprecated use [[SagaAdvanceCodec]] */
trait Prism[S, A]:
  def getOption(input: S): Option[A]
  def reverseGet(input: A): S

object SagaAdvanceCodec:
  def apply[Cmd, Stp](
    decodeFn: Cmd => Option[(UUID, Stp, SagaPhase, SagaStepResult)],
    encodeFn: (UUID, Stp, SagaPhase, SagaStepResult) => Cmd
  ): SagaAdvanceCodec[Cmd, Stp] =
    new SagaAdvanceCodec[Cmd, Stp]:
      override def decode(cmd: Cmd): Option[(UUID, Stp, SagaPhase, SagaStepResult)] = decodeFn(cmd)
      override def encode(id: UUID, step: Stp, phase: SagaPhase, result: SagaStepResult): Cmd =
        encodeFn(id, step, phase, result)

/** Service domain events that embed the saga instance correlation id. */
trait SagaCorrelated:
  def correlationId: UUID

object SagaStepAdapter:

  /** Shared [[classify]] for search-reserve-cancel service event shapes. */
  def classifyReserveSearch[Evt](
    reserved:    PartialFunction[Evt, ?],
    failed:      PartialFunction[Evt, ?],
    compensated: PartialFunction[Evt, ?]
  ): Evt => Option[(SagaPhase, SagaStepResult)] =
    event =>
      reserved.lift(event).as(SagaPhase.Forward      -> SagaStepResult.Completed)
        .orElse(failed.lift(event).as(SagaPhase.Forward      -> SagaStepResult.Failed))
        .orElse(compensated.lift(event).as(SagaPhase.Compensation -> SagaStepResult.Completed))

/** Bridges a single external service into the saga orchestration machinery. */
trait SagaStepAdapter[Cmd, Evt <: SagaCorrelated, SagaStep, SagaData]:

  def step: SagaStep

  def start(id: UUID, sagaState: SagaData, correlationId: UUID): Cmd

  def compensate(id: UUID): Cmd

  def classify(event: Evt): Option[(SagaPhase, SagaStepResult)]

  final def correlationId(event: Evt): UUID = event.correlationId

  final def lmapOrEmpty[F[_], O : Monoid](apparatus: Apparatus[F, Cmd, O]): Apparatus[F, SagaEvent[SagaStep, SagaData], O] =
    apparatus
      .lmapOrEmpty[SagaEvent[SagaStep, SagaData]] {
        case SagaEvent.StepStarted(corrId, sagaState, s, id) if s == step =>
          start(id, sagaState, corrId)
        case SagaEvent.CompensationStarted(_, s, id) if s == step => compensate(id)
      }
      .label(s"${step} event router")

  final def rmap[F[_]: Applicative, I, SagaCmd](
    apparatus: Apparatus[F, I, List[Evt]],
    codec:     SagaAdvanceCodec[SagaCmd, SagaStep]
  ): Apparatus[F, I, List[SagaCmd]] =
    apparatus.rmap(evs =>
      evs.flatMap { ev =>
        classify(ev).map { (phase, result) =>
          codec.encode(correlationId(ev), step, phase, result)
        }
      }
    )

  /** Wires a service machine into the saga event bus (input filter + output encode). */
  final def adapt[F[_]: Applicative, SagaCmd](
    machine: Apparatus[F, Cmd, List[Evt]],
    codec:   SagaAdvanceCodec[SagaCmd, SagaStep]
  ): Apparatus[F, SagaEvent[SagaStep, SagaData], List[SagaCmd]] =
    rmap(lmapOrEmpty(machine), codec)

/** Convenience constructor for [[SagaBehavior]] that derives [[advanceHandler]] from a [[SagaAdvanceCodec]]. */
case class SagaBehaviorFactory[Cmd, SagaStep : {Eq, Order, Show}, SagaData](
  name: String,
  startCommand: PartialFunction[Cmd, (UUID, SagaData)],
  compensateCommand: PartialFunction[Cmd, UUID],
  commandCorrelationId: Cmd => UUID,
  codec: SagaAdvanceCodec[Cmd, SagaStep],
  steps: NonEmptySet[SagaStep],
  uuidGen: SagaStepCorrelationIdGenerator[SagaStep] = SagaStepCorrelationIdGenerator.random
) extends SagaBehavior[Cmd, SagaStep, SagaData]:
  override def advanceHandler: PartialFunction[Cmd, (SagaStep, SagaPhase, SagaStepResult)] =
    Function.unlift(cmd => codec.decode(cmd).map((_, stp, phase, result) => (stp, phase, result)))

/** Lifecycle state of a saga. */
sealed trait SagaState[SagaStep, +SagaData]

object SagaState:

  final case class StepDispatch[SagaStep](name: SagaStep, id: UUID)

  object StepDispatch:
    given [SagaStep: Schema]: Schema[StepDispatch[SagaStep]] = Schema.derived

  case class Waiting[SagaStep, SagaData]() extends SagaState[SagaStep, SagaData]

  case class Prepared[SagaStep, SagaData](sagaData: SagaData, steps: NonEmptyList[StepDispatch[SagaStep]]) extends SagaState[SagaStep, SagaData]

  case class Succeeded[SagaStep, SagaData](sagaData: SagaData, steps: NonEmptyList[StepDispatch[SagaStep]]) extends SagaState[SagaStep, SagaData]

  case class CompensationPrepared[SagaStep, SagaData](steps: NonEmptyList[StepDispatch[SagaStep]]) extends SagaState[SagaStep, SagaData]

  case class Failed[SagaStep, SagaData]() extends SagaState[SagaStep, SagaData]

  case class Running[SagaStep, SagaData](
    sagaData: SagaData,
    current: StepDispatch[SagaStep],
    todo: List[StepDispatch[SagaStep]],
    completed: List[StepDispatch[SagaStep]],
    steps: NonEmptyList[StepDispatch[SagaStep]]
  ) extends SagaState[SagaStep, SagaData]

  case class Compensating[SagaStep, SagaData](current: StepDispatch[SagaStep], todo: List[StepDispatch[SagaStep]]) extends SagaState[SagaStep, SagaData]

/** Outcome reported by an external service for a single saga step or compensation step. */
enum SagaStepResult derives Schema { case Completed, Failed }

sealed trait SagaEvent[+SagaStep, +SagaData]

object SagaEvent:

  final case class Booted[SagaStep, SagaData](
    correlationId: UUID,
    sagaState: SagaData,
    steps: NonEmptyList[apparatus.core.patterns.SagaState.StepDispatch[SagaStep]]
  ) extends SagaEvent[SagaStep, SagaData]

  final case class StepStarted[SagaStep, SagaData](
    correlationId: UUID,
    sagaState: SagaData,
    name: SagaStep,
    id: UUID
  ) extends SagaEvent[SagaStep, SagaData]

  final case class StepProgressed[SagaStep](
    correlationId: UUID,
    name: SagaStep,
    result: SagaStepResult
  ) extends SagaEvent[SagaStep, Nothing]

  final case class CompensationTriggered[SagaStep](
    correlationId: UUID,
    steps: NonEmptyList[apparatus.core.patterns.SagaState.StepDispatch[SagaStep]]
  ) extends SagaEvent[SagaStep, Nothing]

  final case class CompensationStarted[SagaStep](
    correlationId: UUID,
    name: SagaStep,
    id: UUID
  ) extends SagaEvent[SagaStep, Nothing]

  final case class CompensationProgressed[SagaStep](
    correlationId: UUID,
    name: SagaStep,
    result: SagaStepResult
  ) extends SagaEvent[SagaStep, Nothing]

  given [SagaStep: {Schema, Order}, SagaData: Schema]: Schema[SagaEvent[SagaStep, SagaData]] =
    Schema.derived

object SagaBehavior:

  private def compensationEvents[SagaStep, SagaData](
    correlationId: UUID,
    dispatches: NonEmptyList[SagaState.StepDispatch[SagaStep]]
  ): List[SagaEvent[SagaStep, SagaData]] =
    List(
      SagaEvent.CompensationTriggered(correlationId, dispatches),
      SagaEvent.CompensationStarted(correlationId, dispatches.head.name, dispatches.head.id)
    )

  private def handleStepProgress[SagaStep: Eq, SagaData](
    current: SagaState.StepDispatch[SagaStep],
    stepName: SagaStep,
    result: SagaStepResult,
    todo: List[SagaState.StepDispatch[SagaStep]],
    correlationId: UUID,
    progress: (UUID, SagaStep, SagaStepResult) => SagaEvent[SagaStep, SagaData],
    startNext: SagaState.StepDispatch[SagaStep] => SagaEvent[SagaStep, SagaData],
    onFailedContinue: Boolean
  ): List[SagaEvent[SagaStep, SagaData]] =
    if current.name =!= stepName then Nil
    else
      val progressEvent = List(progress(correlationId, stepName, result))
      result match
        case SagaStepResult.Completed =>
          progressEvent ++ todo.headOption.map(startNext).toList
        case SagaStepResult.Failed if onFailedContinue =>
          progressEvent ++ todo.headOption.map(startNext).toList
        case SagaStepResult.Failed =>
          progressEvent

/** Defines the domain-specific shape of a saga. */
trait SagaBehavior[Cmd, SagaStep : {Order, Eq, Show}, SagaData]:
  def name: String

  def startCommand: PartialFunction[Cmd, (UUID, SagaData)]

  def compensateCommand: PartialFunction[Cmd, UUID]

  def commandCorrelationId: Cmd => UUID

  def steps: NonEmptySet[SagaStep]

  def uuidGen: SagaStepCorrelationIdGenerator[SagaStep]

  def advanceHandler: PartialFunction[Cmd, (SagaStep, SagaPhase, SagaStepResult)]

  final def decide(state: apparatus.core.patterns.SagaState[SagaStep, SagaData], cmd: Cmd): List[SagaEvent[SagaStep, SagaData]] = state match {
    case SagaState.Waiting() =>
      startCommand.lift(cmd).toList.flatMap { (correlationId, sagaState) =>
        val dispatches = NonEmptyList.fromListUnsafe(steps.toSortedSet.toList.map(step => SagaState.StepDispatch(step, uuidGen.next(step))))
        List(
          SagaEvent.Booted(correlationId, sagaState, dispatches),
          SagaEvent.StepStarted(correlationId, sagaState, dispatches.head.name, dispatches.head.id)
        )
      }
    case SagaState.Running(sagaData, current, todo, completed, _) =>
      val correlationId = commandCorrelationId(cmd)
      compensateCommand.lift(cmd) match
        case Some(_) =>
          NonEmptyList.fromList(completed).toList.flatMap(SagaBehavior.compensationEvents(correlationId, _))
        case None =>
          advanceHandler.lift(cmd) match
            case Some((stepName, SagaPhase.Forward, SagaStepResult.Completed)) =>
              SagaBehavior.handleStepProgress(
                current, stepName, SagaStepResult.Completed, todo, correlationId,
                SagaEvent.StepProgressed(_, _, _),
                next => SagaEvent.StepStarted(correlationId, sagaData, next.name, next.id),
                onFailedContinue = false
              )
            case Some((stepName, SagaPhase.Forward, SagaStepResult.Failed)) if current.name === stepName =>
              List(SagaEvent.StepProgressed(correlationId, stepName, SagaStepResult.Failed)) ++
                NonEmptyList.fromList(completed).toList.flatMap(SagaBehavior.compensationEvents(correlationId, _))
            case _ => Nil
    case SagaState.Compensating(current, todo) =>
      val correlationId = commandCorrelationId(cmd)
      advanceHandler.lift(cmd).flatMap { (stepName, phase, result) =>
        phase match
          case SagaPhase.Compensation =>
            Some(
              SagaBehavior.handleStepProgress(
                current, stepName, result, todo, correlationId,
                SagaEvent.CompensationProgressed(_, _, _),
                next => SagaEvent.CompensationStarted(correlationId, next.name, next.id),
                onFailedContinue = true
              )
            )
          case SagaPhase.Forward => None
      }.getOrElse(Nil)
    case SagaState.Succeeded(_, steps) =>
      compensateCommand.lift(cmd).toList.flatMap { correlationId =>
        SagaBehavior.compensationEvents(correlationId, steps)
      }
    case _ => Nil
  }

  final def evolve(state: apparatus.core.patterns.SagaState[SagaStep, SagaData], evt: SagaEvent[SagaStep, SagaData]): apparatus.core.patterns.SagaState[SagaStep, SagaData] =
    state match {
      case SagaState.Waiting() =>
        evt match {
          case booted: SagaEvent.Booted[SagaStep @unchecked, SagaData @unchecked] =>
            SagaState.Prepared(booted.sagaState, booted.steps)
          case _ => state
        }
      case SagaState.Prepared(sagaData, steps) =>
        evt match {
          case SagaEvent.StepStarted(_, _, _, _) =>
            SagaState.Running(sagaData, steps.head, steps.tail.toList, Nil, steps)
          case _ => state
        }
      case SagaState.Running(sagaData, current, todo, completed, steps) =>
        evt match {
          case SagaEvent.StepProgressed(_, _, SagaStepResult.Completed) =>
            if todo.isEmpty then SagaState.Succeeded(sagaData, steps)
            else SagaState.Running(sagaData, todo.head, todo.tail, completed :+ current, steps)
          case compensationTriggered: SagaEvent.CompensationTriggered[SagaStep @unchecked] =>
            SagaState.CompensationPrepared(compensationTriggered.steps)
          case _ => state
        }
      case SagaState.CompensationPrepared(steps) =>
        evt match {
          case SagaEvent.CompensationStarted(_, _, _) => SagaState.Compensating(steps.head, steps.tail.toList)
          case _ => state
        }
      case SagaState.Compensating(_, todo) =>
        evt match {
          case SagaEvent.CompensationProgressed(_, _, SagaStepResult.Completed) =>
            if todo.isEmpty then SagaState.Failed()
            else SagaState.Compensating(todo.head, todo.tail)
          case _ => state
        }
      case _ => state
    }

  def decider: Decider[apparatus.core.patterns.SagaState[SagaStep, SagaData], Cmd, List[SagaEvent[SagaStep, SagaData]]] =
    DeciderBuilder.seed[apparatus.core.patterns.SagaState[SagaStep, SagaData]](name, SagaState.Waiting())
      .decide[Cmd, List[SagaEvent[SagaStep, SagaData]]]((s, i) => decide(s, i))
      .evolveList((s, e) => evolve(s, e))
