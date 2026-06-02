package apparatus.tests

import apparatus.core.*
import apparatus.core.machines.*
import apparatus.core.patterns.*
import apparatus.examples.*
import cats.data.NonEmptyList
import cats.effect.SyncIO
import cats.data.NonEmptySet
import cats.implicits.*

class BookingSagaIdentitySpec extends munit.FunSuite:

  test("happy path: all steps complete in order"):
    assertEquals(
      Apparatus.runA(saga[SyncIO](BookingServices.default[SyncIO]), BookingCommand.Start(bookingId, BookingDomain.sagaState), DeciderMaterializer.syncIO).unsafeRunSync(),
      List(
        SagaEvent.Booted(bookingId, BookingDomain.sagaState, NonEmptyList.of(
          SagaState.StepDispatch(BookingStep.Hotel, hotelId),
          SagaState.StepDispatch(BookingStep.Car, carId),
          SagaState.StepDispatch(BookingStep.Flight, flightId)
        )),
        SagaEvent.StepStarted(bookingId, BookingDomain.sagaState, BookingStep.Hotel, hotelId),
        SagaEvent.StepProgressed(bookingId, BookingStep.Hotel, SagaStepResult.Completed),
        SagaEvent.StepStarted(bookingId, BookingDomain.sagaState, BookingStep.Car, carId),
        SagaEvent.StepProgressed(bookingId, BookingStep.Car, SagaStepResult.Completed),
        SagaEvent.StepStarted(bookingId, BookingDomain.sagaState, BookingStep.Flight, flightId),
        SagaEvent.StepProgressed(bookingId, BookingStep.Flight, SagaStepResult.Completed)
      )
    )

  test("flight fails: compensation triggered for completed steps"):
    val events =
      Apparatus
        .runA(
          saga[SyncIO](bookingServices = BookingSagaMocks.failingFlightSearch[SyncIO]),
          BookingCommand.Start(bookingId, BookingDomain.sagaState),
          DeciderMaterializer.syncIO
        )
        .unsafeRunSync()
    assert(events.contains(SagaEvent.StepProgressed(bookingId, BookingStep.Flight, SagaStepResult.Failed)))
    assert(events.exists { case SagaEvent.CompensationTriggered(_, _) => true; case _ => false })
    assert(events.exists { case SagaEvent.CompensationProgressed(_, BookingStep.Car,    SagaStepResult.Completed) => true; case _ => false })
    assert(events.exists { case SagaEvent.CompensationProgressed(_, BookingStep.Hotel, SagaStepResult.Completed) => true; case _ => false })

  test("flight fails: car compensated, flight not"):
    val events =
      Apparatus
        .runA(
          saga[SyncIO](bookingServices = BookingSagaMocks.failingFlightSearch[SyncIO]),
          BookingCommand.Start(bookingId, BookingDomain.sagaState),
          DeciderMaterializer.syncIO
        )
        .unsafeRunSync()
    assert(events.contains(SagaEvent.StepProgressed(bookingId, BookingStep.Flight, SagaStepResult.Failed)))
    assert(events.exists { case SagaEvent.CompensationProgressed(_, BookingStep.Car, SagaStepResult.Completed) => true; case _ => false })
    assert(!events.exists { case SagaEvent.CompensationProgressed(_, BookingStep.Flight, _) => true; case _ => false })

  test("car fails: no compensation needed"):
    val events =
      Apparatus
        .runA(
          saga[SyncIO](bookingServices = BookingSagaMocks.failingCarSearch[SyncIO]),
          BookingCommand.Start(bookingId, BookingDomain.sagaState),
          DeciderMaterializer.syncIO
        )
        .unsafeRunSync()
    assert(events.contains(SagaEvent.StepProgressed(bookingId, BookingStep.Car, SagaStepResult.Failed)))
    assert(!events.exists {
      case SagaEvent.CompensationTriggered(_, steps) => steps.exists(_.name == BookingStep.Car)
      case _ => false
    })

  test("rerooted at car: happy path — car reserved, flight follows"):
    val defaultBookingAtCarEvents: List[SagaEvent[BookingStep, BookingSagaState]] = List(
      SagaEvent.Booted(bookingId, BookingDomain.sagaState, NonEmptyList.of(
        SagaState.StepDispatch(BookingStep.Hotel, hotelId),
        SagaState.StepDispatch(BookingStep.Car, carId),
        SagaState.StepDispatch(BookingStep.Flight, flightId)
      )),
      SagaEvent.StepStarted(bookingId, BookingDomain.sagaState, BookingStep.Hotel, hotelId),
      SagaEvent.StepProgressed(bookingId, BookingStep.Hotel, SagaStepResult.Completed),
      SagaEvent.StepStarted(bookingId, BookingDomain.sagaState, BookingStep.Car, carId)
    )
    val bookingAtCar = behavior.decider.evolveFrom(defaultBookingAtCarEvents)
    assertEquals(
      Apparatus.runMultipleA(
        sagaRerootedAtCar[SyncIO](booking = bookingAtCar, bookingServices = BookingServices.default[SyncIO]),
        List(CarCommand.InitSearch(carId, BookingDomain.carQuery, bookingId)),
        DeciderMaterializer.syncIO
      ).unsafeRunSync(),
      List(
        SagaEvent.StepProgressed(bookingId, BookingStep.Car, SagaStepResult.Completed),
        SagaEvent.StepStarted(bookingId, BookingDomain.sagaState, BookingStep.Flight, flightId),
        SagaEvent.StepProgressed(bookingId, BookingStep.Flight, SagaStepResult.Completed)
      )
    )

  test("rerooted at car: car fails — hotel compensated, flight skipped"):
    val defaultBookingAtCarEvents: List[SagaEvent[BookingStep, BookingSagaState]] = List(
      SagaEvent.Booted(bookingId, BookingDomain.sagaState, NonEmptyList.of(
        SagaState.StepDispatch(BookingStep.Hotel, hotelId),
        SagaState.StepDispatch(BookingStep.Car, carId),
        SagaState.StepDispatch(BookingStep.Flight, flightId)
      )),
      SagaEvent.StepStarted(bookingId, BookingDomain.sagaState, BookingStep.Hotel, hotelId),
      SagaEvent.StepProgressed(bookingId, BookingStep.Hotel, SagaStepResult.Completed),
      SagaEvent.StepStarted(bookingId, BookingDomain.sagaState, BookingStep.Car, carId)
    )
    val bookingAtCar = behavior.decider.evolveFrom(defaultBookingAtCarEvents)
    val events = Apparatus.runMultipleA(
      sagaRerootedAtCar[SyncIO](
        booking         = bookingAtCar,
        bookingServices = BookingSagaMocks.failingCarSearch[SyncIO],
        flight          = flightMachine[SyncIO](),
        car             = carMachine[SyncIO](),
        hotel           = BookingSagaMocks.hotelReservedMachine[SyncIO]
      ),
      List(CarCommand.InitSearch(carId, BookingDomain.carQuery, bookingId)),
      DeciderMaterializer.syncIO
    ).unsafeRunSync()
    assert(events.contains(SagaEvent.StepProgressed(bookingId, BookingStep.Car, SagaStepResult.Failed)))
    assert(events.exists { case SagaEvent.CompensationProgressed(_, BookingStep.Hotel, SagaStepResult.Completed) => true; case _ => false })
    assert(!events.exists { case SagaEvent.CompensationProgressed(_, BookingStep.Flight, _) => true; case _ => false })

  test("rerooted at car: flight fails — car and hotel both compensated"):
    val defaultBookingAtCarEvents: List[SagaEvent[BookingStep, BookingSagaState]] = List(
      SagaEvent.Booted(bookingId, BookingDomain.sagaState, NonEmptyList.of(
        SagaState.StepDispatch(BookingStep.Hotel, hotelId),
        SagaState.StepDispatch(BookingStep.Car, carId),
        SagaState.StepDispatch(BookingStep.Flight, flightId)
      )),
      SagaEvent.StepStarted(bookingId, BookingDomain.sagaState, BookingStep.Hotel, hotelId),
      SagaEvent.StepProgressed(bookingId, BookingStep.Hotel, SagaStepResult.Completed)
    )
    val bookingAtCar = behavior.decider.evolveFrom(defaultBookingAtCarEvents)
    val events = Apparatus.runMultipleA(
      sagaRerootedAtCar[SyncIO](
        booking         = bookingAtCar,
        bookingServices = BookingSagaMocks.failingFlightSearch[SyncIO],
        flight          = flightMachine[SyncIO](),
        car             = carMachine[SyncIO](),
        hotel           = BookingSagaMocks.hotelReservedMachine[SyncIO]
      ),
      List(CarCommand.InitSearch(carId, BookingDomain.carQuery, bookingId)),
      DeciderMaterializer.syncIO
    ).unsafeRunSync()
    assert(events.exists { case SagaEvent.CompensationProgressed(_, BookingStep.Car,   SagaStepResult.Completed) => true; case _ => false })
    assert(events.exists { case SagaEvent.CompensationProgressed(_, BookingStep.Hotel, SagaStepResult.Completed) => true; case _ => false })
    assert(!events.exists { case SagaEvent.CompensationProgressed(_, BookingStep.Flight, _) => true; case _ => false })
