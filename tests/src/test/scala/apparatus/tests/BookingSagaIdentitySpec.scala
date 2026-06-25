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
      Apparatus.runA(bookingSaga[SyncIO](BookingServices.default[SyncIO], BookingDomain.testBehavior), BookingCommand.Start(BookingDomain.bookingId, BookingDomain.sagaState), DeciderMaterializer.syncIO).unsafeRunSync(),
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

  test("diplomat start: hotel step pauses awaiting background check"):
    val events =
      Apparatus
        .runA(
          bookingSaga[SyncIO](BookingServices.default[SyncIO], BookingDomain.testBehavior),
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
