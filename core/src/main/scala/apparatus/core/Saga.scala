package apparatus.core

import cats.*
import cats.data.NonEmptySet
import cats.implicits.*
import zio.blocks.schema.Schema

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
  def getOption(input: S): Option[A]
  def reverseGet(input: A): S
}

/** A [[Prism]] specialised for advancing a saga.
  *
  * Maps between a saga's command type `Cmd` and the triple
  * `(step, phase, result)` that describes a single acknowledgement from an
  * external service:
  *
  *   - `step`   — which saga step is being acknowledged (e.g. `BookingStep.Car`)
  *   - `phase`  — [[SagaPhase.Forward]] or [[SagaPhase.Compensation]]
  *   - `result` — [[SagaStepResult.Completed]] or [[SagaStepResult.Failed]]
  *
  * Implement once per saga command type and pass it to [[SagaBehaviorFactory]]
  * and to every [[SagaStepAdapter.rmap]] call so the saga orchestrator and the
  * individual service adapters share a consistent encoding.
  *
  * Example (from the booking saga):
  * {{{
  * val advancePrism: SagaAdvancePrism[BookingCommand, BookingStep] =
  *   new Prism[BookingCommand, (BookingStep, SagaPhase, SagaStepResult)] {
  *     def getOption(cmd: BookingCommand) = cmd match {
  *       case BookingCommand.Advance(step, phase, result) => Some((step, phase, result))
  *       case _ => None
  *     }
  *     def reverseGet(t: (BookingStep, SagaPhase, SagaStepResult)) =
  *       BookingCommand.Advance(t._1, t._2, t._3)
  *   }
  * }}}
  *
  * @tparam Cmd the command type of the saga (e.g. `BookingCommand`)
  * @tparam Stp the step type of the saga (e.g. `BookingStep`)
  */
type SagaAdvancePrism[Cmd, Stp] = Prism[Cmd, (Stp, SagaPhase, SagaStepResult)]

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
trait SagaStepAdapter[Cmd, Evt, Stp] {

  /** The saga step this adapter represents. Used to filter incoming [[SagaEvent]]s. */
  def step: Stp

  /** Command sent to the external service to begin the forward step. */
  def start: Cmd

  /** Command sent to the external service to roll back the forward step. */
  def compensate: Cmd

  /** Interprets a domain event from the external service as a saga signal.
    *
    * Return `Some((phase, result))` when the event is a saga-relevant
    * acknowledgement; `None` for events the saga does not care about.
    *
    * @param event a raw event emitted by the external service's [[Apparatus]]
    * @return `Some` with the phase and result, or `None` to ignore the event
    */
  def classify(event: Evt): Option[(SagaPhase, SagaStepResult)]

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
  final def lmapOrEmpty[F[_], O : Monoid](apparatus: Apparatus[F, Cmd, O]): Apparatus[F, SagaEvent[Stp], O] =
      apparatus
        .lmapOrEmpty[SagaEvent[Stp]] {
          case SagaEvent.StepStarted(s) if s == step => start
          case SagaEvent.CompensationStarted(s) if s == step => compensate
        }
        .label(s"${step} event router")

  /** Translates service domain events into saga advance commands (output side).
    *
    * Wraps a service [[Apparatus]] so its `List[Evt]` output is mapped to
    * `List[SagaCmd]` using [[classify]] and the provided [[SagaAdvancePrism]].
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
  final def rmap[F[_] : Applicative, I, SagaCmd](apparatus: Apparatus[F, I, List[Evt]], prism: SagaAdvancePrism[SagaCmd, Stp]): Apparatus[F, I, List[SagaCmd]] =
    apparatus.rmap(evs => evs.flatMap((ev: Evt) => classify(ev).map((phase, result) => prism.reverseGet(step, phase, result))))
}

case class SagaBehaviorFactory[Cmd, Stp : {Eq, Order, Show}](startCommand: Cmd, prism: SagaAdvancePrism[Cmd, Stp], steps: NonEmptySet[Stp]) extends SagaBehavior[Cmd, Stp] {
    override val stepHandler: PartialFunction[Cmd, (Stp, SagaStepResult)] =
      Function.unlift(cmd => prism.getOption(cmd).filter((_, phase, _) => phase == SagaPhase.Forward).map((stp, _, result) => (stp, result)))
    override val compensationHandler: PartialFunction[Cmd, (Stp, SagaStepResult)] =
      Function.unlift(cmd => prism.getOption(cmd).filter((_, phase, _) => phase == SagaPhase.Compensation).map((stp, _, result) => (stp, result)))
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
sealed trait SagaState[Step]

object SagaState {

  /** Initial state. The saga has not been started yet. */
  case class Waiting[Step]() extends SagaState[Step]

  /** Steps have been scheduled; awaiting dispatch of the first [[SagaEvent.StepStarted]]. */
  case class Prepared[Step](steps: NonEmptySet[Step]) extends SagaState[Step]

  /** All forward steps completed successfully. */
  case class Succeeded[Step]() extends SagaState[Step]

  /** Compensation steps scheduled; awaiting dispatch of the first [[SagaEvent.CompensationStarted]]. */
  case class CompensationPrepared[Step](steps: NonEmptySet[Step]) extends SagaState[Step]

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
enum SagaStepResult derives Schema { case Completed, Failed }

/** Events emitted by [[SagaBehavior.decide]] and consumed by [[SagaBehavior.evolve]].
  *
  * These events are the persistent record of what happened during the saga. Replaying
  * them against [[SagaBehavior.evolve]] fully reconstructs [[SagaState]].
  *
  * @tparam Step the step type
  */
sealed trait SagaEvent[Step]

object SagaEvent:

  /** The saga was started. `steps` is the full ordered set of forward steps to execute. */
  final case class Booted[Step](
                                 steps: NonEmptySet[Step]
                               ) extends SagaEvent[Step]

  /** A forward step has been dispatched to the external service. */
  final case class StepStarted[Step](
                                      name: Step
                                    ) extends SagaEvent[Step]

  /** An external service reported the result of a forward step. */
  final case class StepProgressed[Step](
                                         name: Step,
                                         result: SagaStepResult
                                       ) extends SagaEvent[Step]

  /** A forward step failed; compensation will proceed through `steps` in order. */
  final case class CompensationTriggered[Step](
                                                steps: NonEmptySet[Step]
                                              ) extends SagaEvent[Step]

  /** A compensation step has been dispatched to the external service. */
  final case class CompensationStarted[Step](
                                              name: Step
                                            ) extends SagaEvent[Step]

  /** An external service reported the result of a compensation step. */
  final case class CompensationProgressed[Step](
                                                 name: Step,
                                                 result: SagaStepResult
                                               ) extends SagaEvent[Step]

  given [Step: {Schema, Order}]: Schema[SagaEvent[Step]] =
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
    *   - `Waiting`              — emits [[SagaEvent.Booted]] + [[SagaEvent.StepStarted]] when `cmd == startCommand`
    *   - `Running`              — delegates to [[stepHandler]]; on `Completed` advances to next step via `StepStarted`,
    *                             on `Failed` emits `CompensationTriggered` + `CompensationStarted` for the compensation set
    *   - `Compensating`        — delegates to [[compensationHandler]]; advances through compensation steps
    *   - `Prepared` / `CompensationPrepared` / `Succeeded` / `Failed` — always emits `Nil`
    */
  final def decide(state: SagaState[Step], cmd: Cmd): List[SagaEvent[Step]] = state match {
    case SagaState.Waiting() =>
      if(cmd == startCommand) List(SagaEvent.Booted(steps), SagaEvent.StepStarted(steps.head)) else Nil
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
                val compEvents: List[SagaEvent[Step]] = NonEmptySet.fromSet(compensation).toList.flatMap { cs =>
                  List(SagaEvent.CompensationTriggered(cs), SagaEvent.CompensationStarted(cs.head))
                }
                progressEvent ++ compEvents
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
          case SagaEvent.Booted(steps) => SagaState.Prepared(steps)
          case _ => state
        }
      case SagaState.Prepared(steps) =>
        evt match {
          case SagaEvent.StepStarted(_) => SagaState.Running(steps.head, steps.tail, SortedSet.empty)
          case _ => state
        }
      case SagaState.Running(_, todo, compensation) =>
        evt match {
          case SagaEvent.StepProgressed(name, SagaStepResult.Completed) =>
            if todo.isEmpty then SagaState.Succeeded()
            else SagaState.Running(todo.head, todo.tail, compensation + name)
          case SagaEvent.CompensationTriggered(steps) =>
            SagaState.CompensationPrepared(steps)
          case _ => state
        }
      case SagaState.CompensationPrepared(steps) =>
        evt match {
          case SagaEvent.CompensationStarted(_) => SagaState.Compensating(steps.head, steps.tail)
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
