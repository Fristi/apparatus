package apparatus.tests

import apparatus.core.*
import apparatus.core.machines.*
import apparatus.core.patterns.*
import apparatus.examples.*
import cats.data.NonEmptyList
import cats.effect.SyncIO
import cats.implicits.*

class BookingSagaIdentitySpec extends munit.FunSuite:

  test("civilian happy path: all steps complete in one run"):
    assertEquals(
      Apparatus.runA(saga[SyncIO](BookingServices.default[SyncIO], BookingDomain.testBehavior), BookingCommand.Start(BookingDomain.bookingId, BookingDomain.sagaState), DeciderMaterializer.syncIO).unsafeRunSync(),
      List(
        SagaEvent.Booted(BookingDomain.bookingId, BookingDomain.sagaState, NonEmptyList.of(
          SagaState.StepDispatch(BookingStep.Hotel, BookingDomain.hotelId),
          SagaState.StepDispatch(BookingStep.Car, BookingDomain.carId),
          SagaState.StepDispatch(BookingStep.Flight, BookingDomain.flightId)
        )),
        SagaEvent.StepStarted(BookingDomain.bookingId, BookingDomain.sagaState, BookingStep.Hotel, BookingDomain.hotelId),
        SagaEvent.StepProgressed(BookingDomain.bookingId, BookingStep.Hotel, SagaStepResult.Completed),
        SagaEvent.StepStarted(BookingDomain.bookingId, BookingDomain.sagaState, BookingStep.Car, BookingDomain.carId),
        SagaEvent.StepProgressed(BookingDomain.bookingId, BookingStep.Car, SagaStepResult.Completed),
        SagaEvent.StepStarted(BookingDomain.bookingId, BookingDomain.sagaState, BookingStep.Flight, BookingDomain.flightId),
        SagaEvent.StepProgressed(BookingDomain.bookingId, BookingStep.Flight, SagaStepResult.Completed)
      )
    )

  test("flight fails: compensation triggered for completed steps"):
    val events =
      Apparatus
        .runA(
          saga[SyncIO](bookingServices = BookingSagaMocks.failingFlightSearch[SyncIO], BookingDomain.testBehavior),
          BookingCommand.Start(BookingDomain.bookingId, BookingDomain.sagaState),
          DeciderMaterializer.syncIO
        )
        .unsafeRunSync()
    assert(events.contains(SagaEvent.StepProgressed(BookingDomain.bookingId, BookingStep.Flight, SagaStepResult.Failed)))
    assert(events.exists { case SagaEvent.CompensationTriggered(_, _) => true; case _ => false })
    assert(events.exists { case SagaEvent.CompensationProgressed(_, BookingStep.Car,    SagaStepResult.Completed) => true; case _ => false })
    assert(events.exists { case SagaEvent.CompensationProgressed(_, BookingStep.Hotel, SagaStepResult.Completed) => true; case _ => false })

  test("flight fails: car compensated, flight not"):
    val events =
      Apparatus
        .runA(
          saga[SyncIO](bookingServices = BookingSagaMocks.failingFlightSearch[SyncIO], BookingDomain.testBehavior),
          BookingCommand.Start(BookingDomain.bookingId, BookingDomain.sagaState),
          DeciderMaterializer.syncIO
        )
        .unsafeRunSync()
    assert(events.contains(SagaEvent.StepProgressed(BookingDomain.bookingId, BookingStep.Flight, SagaStepResult.Failed)))
    assert(events.exists { case SagaEvent.CompensationProgressed(_, BookingStep.Car, SagaStepResult.Completed) => true; case _ => false })
    assert(!events.exists { case SagaEvent.CompensationProgressed(_, BookingStep.Flight, _) => true; case _ => false })

  test("diplomat start: hotel step pauses awaiting background check"):
    val events =
      Apparatus
        .runA(
          saga[SyncIO](BookingServices.default[SyncIO], BookingDomain.testBehavior),
          BookingCommand.Start(BookingDomain.bookingId, BookingDomain.diplomatSagaState),
          DeciderMaterializer.syncIO
        )
        .unsafeRunSync()
    assertEquals(
      events,
      List(
        SagaEvent.Booted(BookingDomain.bookingId, BookingDomain.diplomatSagaState, NonEmptyList.of(
          SagaState.StepDispatch(BookingStep.Hotel, BookingDomain.hotelId),
          SagaState.StepDispatch(BookingStep.Car, BookingDomain.carId),
          SagaState.StepDispatch(BookingStep.Flight, BookingDomain.flightId)
        )),
        SagaEvent.StepStarted(BookingDomain.bookingId, BookingDomain.diplomatSagaState, BookingStep.Hotel, BookingDomain.hotelId)
      )
    )

  test("car fails: no compensation needed"):
    val events =
      Apparatus
        .runA(
          saga[SyncIO](bookingServices = BookingSagaMocks.failingCarSearch[SyncIO], BookingDomain.testBehavior),
          BookingCommand.Start(BookingDomain.bookingId, BookingDomain.sagaState),
          DeciderMaterializer.syncIO
        )
        .unsafeRunSync()
    assert(events.contains(SagaEvent.StepProgressed(BookingDomain.bookingId, BookingStep.Car, SagaStepResult.Failed)))
    assert(!events.exists {
      case SagaEvent.CompensationTriggered(_, steps) => steps.exists(_.name == BookingStep.Car)
      case _ => false
    })
