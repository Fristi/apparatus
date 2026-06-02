package apparatus.core.patterns

import apparatus.core.*
import apparatus.core.machines.{Decider, DeciderBuilder, evolveList}
import cats.*
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.implicits.*
import zio.blocks.schema.Schema

import java.util.UUID
import scala.collection.immutable.SortedSet

/** Distinguishes which direction a saga is currently running.
  *
  * A saga always starts in the [[Forward]] phase, executing its steps in order.
  * If any step fails the saga switches to [[Compensation]], replaying completed
  * steps in reverse to undo their side effects.
  *
  * `SagaPhase` is embedded in every [[SagaAdvancePrism]] round-trip so that a
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

/** Bidirectional optic between a saga command type `S` and a focus type `A`.
  *
  * A `Prism` is the read/write pair needed to embed structured data inside a
  * sum type:
  *   - [[getOption]] extracts the focus, returning `None` for variants that do
  *     not carry the focus.
  *   - [[reverseGet]] reconstructs the outer type from a focus value.
  *
  * This is a minimal optics interface; you do not need a full optics library
  * to implement it.
  *
  * @tparam S the outer (sum) type — typically a command enum
  * @tparam A the inner focus type — typically a tuple of structured data
  */
trait Prism[S, A] {
  /** Extracts the focus `A` from `input`, or `None` if `input` belongs to a different variant. */
  def getOption(input: S): Option[A]

  /** Reconstructs the outer type `S` from a focus value `A`. */
  def reverseGet(input: A): S
}

/** A [[Prism]] specialised for advancing a saga.
  *
  * Maps between a saga's command type `Cmd` and the quadruple
  * `(correlationId, step, phase, result)` that describes a single acknowledgement from an
  * external service:
  *
  *   - `correlationId` — saga instance id (e.g. booking id propagated from `InitSearch`)
  *   - `step`          — which saga step is being acknowledged (e.g. `BookingStep.Car`)
  *   - `phase`         — [[SagaPhase.Forward]] or [[SagaPhase.Compensation]]
  *   - `result`        — [[SagaStepResult.Completed]] or [[SagaStepResult.Failed]]
  *
  * Implement once per saga command type and pass it to [[SagaBehaviorFactory]]
  * and to every [[SagaStepAdapter.rmap]] call so the saga orchestrator and the
  * individual service adapters share a consistent encoding.
  *
  * Example (from the booking saga):
  * {{{
  * val advancePrism: SagaAdvancePrism[BookingCommand, BookingStep] =
  *   new Prism[BookingCommand, (UUID, BookingStep, SagaPhase, SagaStepResult)] {
  *     def getOption(cmd: BookingCommand) = cmd match {
  *       case BookingCommand.Advance(id, step, phase, result) => Some((id, step, phase, result))
  *       case _ => None
  *     }
  *     def reverseGet(t: (UUID, BookingStep, SagaPhase, SagaStepResult)) =
  *       BookingCommand.Advance(t._1, t._2, t._3, t._4)
  *   }
  * }}}
  *
  * @tparam Cmd the command type of the saga (e.g. `BookingCommand`)
  * @tparam Stp the step type of the saga (e.g. `BookingStep`)
  */
type SagaAdvancePrism[Cmd, Stp] = Prism[Cmd, (UUID, Stp, SagaPhase, SagaStepResult)]

/** Service domain events that embed the saga instance correlation id. */
trait SagaCorrelated:
  def correlationId: UUID

/** Bridges a single external service into the saga orchestration machinery.
  *
  * Each service participating in a saga needs three pieces of information that
  * only the domain knows:
  *   1. Which command starts (or re-starts) this step.
  *   2. Which command rolls this step back.
  *   3. How to interpret the service's domain events as saga progress signals.
  *
  * `SagaStepAdapter` captures those three concerns and provides two derived
  * combinators — [[lmapOrEmpty]] and [[rmap]] — that wire a raw service
  * [[Apparatus]] into the orchestrator's event/command vocabulary.
  *
  * == Usage ==
  *
  * Implement one adapter per step and keep them as `val`s alongside the saga
  * definition:
  * {{{
  * val carStep = new SagaStepAdapter[CarCommand, CarEvent, BookingStep] {
  *   def step       = BookingStep.Car
  *   def start      = CarCommand.Reserve
  *   def compensate = CarCommand.Compensate
  *   def classify(ev: CarEvent) = ev match {
  *     case CarEvent.Reserved           => Some(SagaPhase.Forward      -> SagaStepResult.Completed)
  *     case CarEvent.Failed             => Some(SagaPhase.Forward      -> SagaStepResult.Failed)
  *     case CarEvent.Compensated        => Some(SagaPhase.Compensation -> SagaStepResult.Completed)
  *     case CarEvent.CompensationFailed => Some(SagaPhase.Compensation -> SagaStepResult.Failed)
  *   }
  * }
  * }}}
  *
  * Then compose the service machine into the saga's event bus:
  * {{{
  * def carServiceFSM[F[_]: Applicative]: Apparatus[SagaEvent[BookingStep], List[BookingCommand]] =
  *   carStep.rmap[F](carStep.lmapOrEmpty(carDecider().toApparatus[F]("car")), BookingCommand.advancePrism)
  * }}}
  *
  * @tparam Cmd the command type of the external service (e.g. `CarCommand`)
  * @tparam Evt the event type of the external service (e.g. `CarEvent`)
  * @tparam Stp the step type shared with the saga orchestrator (e.g. `BookingStep`)
  */
trait SagaStepAdapter[Cmd, Evt <: SagaCorrelated, SagaStep, SagaData] {

  /** The saga step this adapter represents. Used to filter incoming [[SagaEvent]]s. */
  def step: SagaStep

  /** Command sent to the external service to begin the forward step.
    *
    * `sagaState` and `correlationId` come from [[SagaEvent.StepStarted]].
    */
  def start(id: UUID, sagaState: SagaData, correlationId: UUID): Cmd

  /** Command sent to the external service to roll back the forward step. */
  def compensate(id: UUID): Cmd

  /** Interprets a domain event from the external service as a saga signal.
    *
    * Return `Some((phase, result))` when the event is a saga-relevant
    * acknowledgement; `None` for events the saga does not care about.
    *
    * @param event a raw event emitted by the external service's [[Apparatus]]
    * @return `Some` with the phase and result, or `None` to ignore the event
    */
  def classify(event: Evt): Option[(SagaPhase, SagaStepResult)]

  /** Saga correlation id embedded in service events (e.g. `bookingId` on [[SagaCorrelated]] events). */
  final def correlationId(event: Evt): UUID = event.correlationId

  /** Translates saga orchestration events into service commands (input side).
    *
    * Wraps a service [[Apparatus]] so it only fires when the orchestrator emits
    * a [[SagaEvent]] that targets this step.  All other saga events produce the
    * empty output (via `lmapOrEmpty`), leaving the service machine untouched.
    *
    * The resulting apparatus is labelled `"<step> event router"` so it appears
    * clearly in Mermaid diagrams.
    *
    * @param apparatus the raw service machine keyed on `Cmd`
    * @return a machine keyed on `SagaEvent[Stp]`, silent for unrelated events
    */
  final def lmapOrEmpty[F[_], O : Monoid](apparatus: Apparatus[F, Cmd, O]): Apparatus[F, SagaEvent[SagaStep, SagaData], O] =
      apparatus
        .lmapOrEmpty[SagaEvent[SagaStep, SagaData]] {
          case SagaEvent.StepStarted(corrId, sagaState, s, id) if s == step =>
            start(id, sagaState, corrId)
          case SagaEvent.CompensationStarted(_, s, id) if s == step => compensate(id)
        }
        .label(s"${step} event router")

  /** Translates service domain events into saga advance commands (output side).
    *
    * Wraps a service [[Apparatus]] so its `List[Evt]` output is mapped to
    * `List[SagaCmd]` using [[classify]], [[correlationId]], and the provided [[SagaAdvancePrism]].
    * Events for which [[classify]] returns `None` are silently dropped.
    *
    * Pair with [[lmapOrEmpty]] to get a fully adapted service machine:
    * {{{
    * carStep.rmap(carStep.lmapOrEmpty(rawMachine), advancePrism)
    * }}}
    *
    * @param apparatus the machine to adapt (input type `I`, output `List[Evt]`)
    * @param prism     the saga's advance prism used to construct saga commands
    * @return a machine with the same input type but output `List[SagaCmd]`
    */
  final def rmap[F[_]: Applicative, I, SagaCmd](
    apparatus: Apparatus[F, I, List[Evt]],
    prism:     SagaAdvancePrism[SagaCmd, SagaStep]
  ): Apparatus[F, I, List[SagaCmd]] =
    apparatus.rmap(evs =>
      evs.flatMap { ev =>
        classify(ev).map { (phase, result) =>
          prism.reverseGet((correlationId(ev), step, phase, result))
        }
      }
    )
}

/** Convenience constructor for [[SagaBehavior]] that derives [[stepHandler]] and
  * [[compensationHandler]] from a [[SagaAdvancePrism]].
  *
  * Use this instead of manually implementing the trait when a prism already encodes the
  * command ↔ `(correlationId, step, phase, result)` mapping.
  *
  * @param name         aggregate name for the saga decider
  * @param startCommand command that boots the saga from [[SagaState.Waiting]]
  * @param prism        bidirectional mapping between commands and advance triples
  * @param steps        ordered set of forward steps for this saga
  * @tparam Cmd command type
  * @tparam Stp step type; must have `Eq`, `Order`, and `Show` instances
  */
case class SagaBehaviorFactory[Cmd, SagaStep : {Eq, Order, Show}, SagaData](
  name: String,
  startCommandClass: Class[? <: Cmd],
  compensateCommandClass: Class[? <: Cmd],
  sagaIdExtractor: Cmd => UUID,
  sagaStateExtractor: PartialFunction[Cmd, SagaData],
  prism: SagaAdvancePrism[Cmd, SagaStep],
  steps: NonEmptySet[SagaStep],
  uuidGen: SagaStepCorrelationIdGenerator[SagaStep] = SagaStepCorrelationIdGenerator.random
) extends SagaBehavior[Cmd, SagaStep, SagaData] {
    override def commandSagaId(cmd: Cmd): UUID = sagaIdExtractor(cmd)
    override def commandSagaState: PartialFunction[Cmd, SagaData] = sagaStateExtractor
    override val stepHandler: PartialFunction[Cmd, (SagaStep, SagaStepResult)] =
      Function.unlift(cmd =>
        prism.getOption(cmd).filter((_, _, phase, _) => phase == SagaPhase.Forward).map((_, stp, _, result) => (stp, result))
      )
    override val compensationHandler: PartialFunction[Cmd, (SagaStep, SagaStepResult)] =
      Function.unlift(cmd =>
        prism.getOption(cmd).filter((_, _, phase, _) => phase == SagaPhase.Compensation).map((_, stp, _, result) => (stp, result))
      )
}

/** Lifecycle state of a saga.
  *
  * A saga moves through the following states:
  *
  * {{{
  * Waiting ──(Boot)──► Prepared ──(StepStarted)──► Running ──(all steps complete)──► Succeeded
  *                                                      │
  *                                          (step fails)──► CompensationPrepared ──(CompensationStarted)──► Compensating ──(all compensations done)──► Failed
  * }}}
  *
  * `Prepared` and `CompensationPrepared` are brief intermediate states that record which steps
  * are scheduled before the first dispatch event (`StepStarted` / `CompensationStarted`) fires.
  *
  * @tparam Step the step type, which must have `Order`, `Eq`, and `Show` instances
  */
sealed trait SagaState[SagaStep, +SagaData]

object SagaState {

  /** Persisted dispatch target for a saga step command. */
  final case class StepDispatch[SagaStep](name: SagaStep, id: UUID)

  object StepDispatch:
    given [SagaStep: Schema]: Schema[StepDispatch[SagaStep]] = Schema.derived

  /** Initial state. The saga has not been started yet. */
  case class Waiting[SagaStep, SagaData]() extends SagaState[SagaStep, SagaData]

  /** Steps have been scheduled; awaiting dispatch of the first [[SagaEvent.StepStarted]]. */
  case class Prepared[SagaStep, SagaData](sagaData: SagaData, steps: NonEmptyList[StepDispatch[SagaStep]]) extends SagaState[SagaStep, SagaData]

  /** All forward steps completed successfully. */
  case class Succeeded[SagaStep, SagaData](sagaData: SagaData, steps: NonEmptyList[StepDispatch[SagaStep]]) extends SagaState[SagaStep, SagaData]

  /** Compensation steps scheduled; awaiting dispatch of the first [[SagaEvent.CompensationStarted]]. */
  case class CompensationPrepared[SagaStep, SagaData](steps: NonEmptyList[StepDispatch[SagaStep]]) extends SagaState[SagaStep, SagaData]

  /** Compensation finished (regardless of individual step outcomes). */
  case class Failed[SagaStep, SagaData]() extends SagaState[SagaStep, SagaData]

  /** A forward run is in progress.
    *
    * @param current     the step currently executing
    * @param todo        remaining steps to execute in order
    * @param compensation steps that have completed and must be compensated if a later step fails
    */
  case class Running[SagaStep, SagaData](
    sagaData: SagaData,
    current: StepDispatch[SagaStep],
    todo: List[StepDispatch[SagaStep]],
    compensation: SortedSet[SagaStep],
    steps: NonEmptyList[StepDispatch[SagaStep]]
  ) extends SagaState[SagaStep, SagaData]

  /** Compensation is in progress after a forward step failed.
    *
    * @param current the compensation step currently executing
    * @param todo    remaining compensation steps to execute in order
    */
  case class Compensating[SagaStep, SagaData](current: StepDispatch[SagaStep], todo: List[StepDispatch[SagaStep]]) extends SagaState[SagaStep, SagaData]
}

/** Outcome reported by an external service for a single saga step or compensation step. */
enum SagaStepResult derives Schema { case Completed, Failed }

/** Events emitted by [[SagaBehavior.decide]] and consumed by [[SagaBehavior.evolve]].
  *
  * These events are the persistent record of what happened during the saga. Replaying
  * them against [[SagaBehavior.evolve]] fully reconstructs [[SagaState]].
  *
  * @tparam Step the step type
  */
sealed trait SagaEvent[+SagaStep, +SagaData]

object SagaEvent:

  /** The saga was started. `steps` is the full ordered set of forward steps to execute. */
  final case class Booted[SagaStep, SagaData](
    correlationId: UUID,
    sagaState: SagaData,
    steps: NonEmptyList[apparatus.core.patterns.SagaState.StepDispatch[SagaStep]]
  ) extends SagaEvent[SagaStep, SagaData]

  /** A forward step has been dispatched to the external service. */
  final case class StepStarted[SagaStep, SagaData](
    correlationId: UUID,
    sagaState: SagaData,
    name: SagaStep,
    id: UUID
  ) extends SagaEvent[SagaStep, SagaData]

  /** An external service reported the result of a forward step. */
  final case class StepProgressed[SagaStep](
    correlationId: UUID,
    name: SagaStep,
    result: SagaStepResult
  ) extends SagaEvent[SagaStep, Nothing]

  /** A forward step failed; compensation will proceed through `steps` in order. */
  final case class CompensationTriggered[SagaStep](
    correlationId: UUID,
    steps: NonEmptyList[apparatus.core.patterns.SagaState.StepDispatch[SagaStep]]
  ) extends SagaEvent[SagaStep, Nothing]

  /** A compensation step has been dispatched to the external service. */
  final case class CompensationStarted[SagaStep](
    correlationId: UUID,
    name: SagaStep,
    id: UUID
  ) extends SagaEvent[SagaStep, Nothing]

  /** An external service reported the result of a compensation step. */
  final case class CompensationProgressed[SagaStep](
    correlationId: UUID,
    name: SagaStep,
    result: SagaStepResult
  ) extends SagaEvent[SagaStep, Nothing]

  given [SagaStep: {Schema, Order}, SagaData: Schema]: Schema[SagaEvent[SagaStep, SagaData]] =
    Schema.derived

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
trait SagaBehavior[Cmd, SagaStep : {Order, Eq, Show}, SagaData]:
  /** Aggregate name used as the [[Decider]] name and for logging / Mermaid diagrams. */
  def name: String

  /** The command that transitions [[SagaState.Waiting]] → [[SagaState.Running]]. */
  def startCommandClass: Class[? <: Cmd]

  /** Command that asks the saga to enter compensation mode. */
  def compensateCommandClass: Class[? <: Cmd]

  /** Extracts saga/aggregate correlation id from any command consumed by the saga. */
  def commandSagaId(cmd: Cmd): UUID

  /** Extracts the boot state from the start command. */
  def commandSagaState: PartialFunction[Cmd, SagaData]

  /** Ordered set of forward steps. The saga executes them head-to-tail. */
  def steps: NonEmptySet[SagaStep]

  /** UUID generator used when dispatching step and compensation commands. */
  def uuidGen: SagaStepCorrelationIdGenerator[SagaStep]

  /** Translates an incoming command to a `(step, result)` pair during compensation.
    *
    * Return `(step, SagaStepResult.Completed)` when the external service confirms that
    * `step` was rolled back successfully; `SagaStepResult.Failed` otherwise.
    * The partial function should be defined for every command that an external service can
    * send as a compensation acknowledgement — it is silently ignored when undefined.
    */
  def compensationHandler: PartialFunction[Cmd, (SagaStep, SagaStepResult)]

  /** Translates an incoming command to a `(step, result)` pair during the forward run.
    *
    * Return `(step, SagaStepResult.Completed)` when the external service confirms success;
    * `SagaStepResult.Failed` when it reports failure.
    * The partial function should be defined for every command that an external service can
    * send as a forward-step acknowledgement — it is silently ignored when undefined.
    */
  def stepHandler: PartialFunction[Cmd, (SagaStep, SagaStepResult)]

  /** Pure decision function: maps `(state, command)` → list of [[SagaEvent]]s.
    *
    * Rules:
    *   - `Waiting`              — emits [[SagaEvent.Booted]] + [[SagaEvent.StepStarted]] when `cmd == startCommand`
    *   - `Running`              — delegates to [[stepHandler]]; on `Completed` advances to next step via `StepStarted`,
    *                             on `Failed` emits `CompensationTriggered` + `CompensationStarted` for the compensation set
    *   - `Compensating`        — delegates to [[compensationHandler]]; advances through compensation steps
    *   - `Prepared` / `CompensationPrepared` / `Succeeded` / `Failed` — always emits `Nil`
    */
  final def decide(state: apparatus.core.patterns.SagaState[SagaStep, SagaData], cmd: Cmd): List[SagaEvent[SagaStep, SagaData]] = state match {
    case SagaState.Waiting() =>
      if startCommandClass.isInstance(cmd) && commandSagaState.isDefinedAt(cmd) then
        val correlationId = commandSagaId(cmd)
        val sagaState = commandSagaState(cmd)
        val dispatches = NonEmptyList.fromListUnsafe(steps.toSortedSet.toList.map(step => SagaState.StepDispatch(step, uuidGen.next(step))))
        List(
          SagaEvent.Booted(correlationId, sagaState, dispatches),
          SagaEvent.StepStarted(correlationId, sagaState, dispatches.head.name, dispatches.head.id)
        )
      else Nil
    case SagaState.Running(sagaData, current, todo, compensation, steps) =>
      val correlationId = commandSagaId(cmd)
      if compensateCommandClass.isInstance(cmd) then
        NonEmptyList.fromList(steps.toList.filter(dispatch => compensation.contains(dispatch.name))).toList.flatMap { dispatches =>
          List(
            SagaEvent.CompensationTriggered(correlationId, dispatches),
            SagaEvent.CompensationStarted(correlationId, dispatches.head.name, dispatches.head.id)
          )
        }
      else
        stepHandler.unapply(cmd) match {
          case Some((stepName, result)) =>
            result match {
              case SagaStepResult.Completed =>
                if(current.name === stepName) {
                  List(SagaEvent.StepProgressed(correlationId, stepName, result)) ++ todo.headOption.map(next => SagaEvent.StepStarted(correlationId, sagaData, next.name, next.id))
                } else {
                  Nil
                }
              case SagaStepResult.Failed =>
                if(current.name === stepName) {
                  val progressEvent: List[SagaEvent[SagaStep, SagaData]] = List(SagaEvent.StepProgressed(correlationId, stepName, result))
                  val compEvents: List[SagaEvent[SagaStep, SagaData]] = NonEmptyList.fromList(steps.toList.filter(dispatch => compensation.contains(dispatch.name))).toList.flatMap { dispatches =>
                    List(
                      SagaEvent.CompensationTriggered(correlationId, dispatches),
                      SagaEvent.CompensationStarted(correlationId, dispatches.head.name, dispatches.head.id)
                    )
                  }
                  progressEvent ++ compEvents
                } else {
                  Nil
                }
            }
          case None => Nil
        }
    case SagaState.Compensating(current, todo) =>
      val correlationId = commandSagaId(cmd)
      compensationHandler.unapply(cmd) match {
        case Some((stepName, result)) =>
          result match {
            case SagaStepResult.Completed =>
              if(current.name === stepName) {
                List(SagaEvent.CompensationProgressed(correlationId, stepName, result)) ++ todo.headOption.map(next => SagaEvent.CompensationStarted(correlationId, next.name, next.id))
              } else {
                Nil
              }
            case SagaStepResult.Failed =>
              if(current.name === stepName) {
                val progressEvent = List(SagaEvent.CompensationProgressed(correlationId, stepName, result))
                val startEvent = todo.headOption.map(next => SagaEvent.CompensationStarted(correlationId, next.name, next.id)).toList
                progressEvent ++ startEvent
              } else {
                Nil
              }
          }
        case None => Nil
      }
    case SagaState.Succeeded(sagaData, steps) =>
      val correlationId = commandSagaId(cmd)
      if compensateCommandClass.isInstance(cmd) then
        List(
          SagaEvent.CompensationTriggered(correlationId, steps),
          SagaEvent.CompensationStarted(correlationId, steps.head.name, steps.head.id)
        )
      else
        Nil
    case _ => Nil
  }

  /** Pure evolution function: folds a [[SagaEvent]] into the current [[SagaState]].
    *
    * This is the replay function used both during normal execution and event-log replay.
    * It is total: unknown event/state combinations return the state unchanged.
    */
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
            SagaState.Running(sagaData, steps.head, steps.tail.toList, SortedSet.empty[SagaStep], steps)
          case _ => state
        }
      case SagaState.Running(sagaData, current, todo, compensation, steps) =>
        evt match {
          case SagaEvent.StepProgressed(_, name, SagaStepResult.Completed) =>
            if todo.isEmpty then SagaState.Succeeded(sagaData, steps)
            else SagaState.Running(sagaData, todo.head, todo.tail, compensation + current.name, steps)
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

  /** Builds a [[Decider]] that wraps [[decide]] and [[evolve]], seeded at [[SagaState.Waiting]].
    *
    * Lift it into an Apparatus with `behavior.decider.toBaseMachine`.
    */
  def decider: Decider[apparatus.core.patterns.SagaState[SagaStep, SagaData], Cmd, List[SagaEvent[SagaStep, SagaData]]] =
    DeciderBuilder.seed[apparatus.core.patterns.SagaState[SagaStep, SagaData]](name, SagaState.Waiting())
      .decide[Cmd, List[SagaEvent[SagaStep, SagaData]]]((s, i) => decide(s, i))
      .evolveList((s, e) => evolve(s, e))
