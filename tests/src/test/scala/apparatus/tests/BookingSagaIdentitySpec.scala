package apparatus.tests

import apparatus.core.*
import apparatus.core.machines.*
import apparatus.core.patterns.*
import apparatus.examples.*
import cats.effect.SyncIO
import cats.data.NonEmptySet
import cats.implicits.*

import scala.collection.immutable.SortedSet

class BookingSagaIdentitySpec extends munit.FunSuite:

  test("happy path: all steps complete in order"):
    assertEquals(
      Apparatus.runA(saga[SyncIO](), BookingCommand.Start, DeciderMaterializer.syncIO).unsafeRunSync(),
      List(
        SagaEvent.Booted(NonEmptySet.of(BookingStep.Hotel, BookingStep.Car, BookingStep.Flight)),
        SagaEvent.StepStarted(BookingStep.Hotel),
        SagaEvent.StepProgressed(BookingStep.Hotel, SagaStepResult.Completed),
        SagaEvent.StepStarted(BookingStep.Car),
        SagaEvent.StepProgressed(BookingStep.Car, SagaStepResult.Completed),
        SagaEvent.StepStarted(BookingStep.Flight),
        SagaEvent.StepProgressed(BookingStep.Flight, SagaStepResult.Completed)
      )
    )

  test("flight fails: compensation triggered for completed steps"):
    val events = Apparatus.runA(saga[SyncIO](flight = flightDecider(failsOnReserve = true)), BookingCommand.Start, DeciderMaterializer.syncIO).unsafeRunSync()
    assert(events.contains(SagaEvent.StepProgressed(BookingStep.Flight, SagaStepResult.Failed)))
    assert(events.exists { case SagaEvent.CompensationTriggered(_) => true; case _ => false })
    assert(events.exists { case SagaEvent.CompensationProgressed(BookingStep.Car,    SagaStepResult.Completed) => true; case _ => false })
    assert(events.exists { case SagaEvent.CompensationProgressed(BookingStep.Hotel, SagaStepResult.Completed) => true; case _ => false })

  test("flight fails: car compensated, flight not"):
    val events = Apparatus.runA(saga[SyncIO](flight = flightDecider(failsOnReserve = true)), BookingCommand.Start, DeciderMaterializer.syncIO).unsafeRunSync()
    assert(events.contains(SagaEvent.StepProgressed(BookingStep.Flight, SagaStepResult.Failed)))
    assert(events.exists { case SagaEvent.CompensationProgressed(BookingStep.Car, SagaStepResult.Completed) => true; case _ => false })
    assert(!events.exists { case SagaEvent.CompensationProgressed(BookingStep.Flight, _) => true; case _ => false })

  test("car fails: no compensation needed"):
    val events = Apparatus.runA(saga[SyncIO](car = carDecider(failsOnReserve = true)), BookingCommand.Start, DeciderMaterializer.syncIO).unsafeRunSync()
    assert(events.contains(SagaEvent.StepProgressed(BookingStep.Car, SagaStepResult.Failed)))
    assert(!events.exists { case SagaEvent.CompensationTriggered(BookingStep.Car) => true; case _ => false })

  test("rerooted at car: happy path — car reserved, flight follows"):
    val defaultBookingAtCarEvents: List[SagaEvent[BookingStep]] = List(
      SagaEvent.Booted(NonEmptySet.of(BookingStep.Hotel, BookingStep.Car, BookingStep.Flight)),
      SagaEvent.StepStarted(BookingStep.Hotel),
      SagaEvent.StepProgressed(BookingStep.Hotel, SagaStepResult.Completed),
      SagaEvent.StepStarted(BookingStep.Car)
    )
    val bookingAtCar = behavior.decider.evolveFrom(defaultBookingAtCarEvents)
    assertEquals(
      Apparatus.runA(sagaRerootedAtCar[SyncIO](booking = bookingAtCar), CarCommand.Reserve, DeciderMaterializer.syncIO).unsafeRunSync(),
      List(
        SagaEvent.StepProgressed(BookingStep.Car, SagaStepResult.Completed),
        SagaEvent.StepStarted(BookingStep.Flight),
        SagaEvent.StepProgressed(BookingStep.Flight, SagaStepResult.Completed)
      )
    )

  test("rerooted at car: car fails — hotel compensated, flight skipped"):
    val defaultBookingAtCarEvents: List[SagaEvent[BookingStep]] = List(
      SagaEvent.Booted(NonEmptySet.of(BookingStep.Hotel, BookingStep.Car, BookingStep.Flight)),
      SagaEvent.StepStarted(BookingStep.Hotel),
      SagaEvent.StepProgressed(BookingStep.Hotel, SagaStepResult.Completed),
      SagaEvent.StepStarted(BookingStep.Car)
    )
    val bookingAtCar = behavior.decider.evolveFrom(defaultBookingAtCarEvents)
    val events = Apparatus.runA(
      sagaRerootedAtCar[SyncIO](
        booking = bookingAtCar,
        car     = carDecider(failsOnReserve = true),
        hotel   = hotelDecider().evolveFrom(List(HotelEvent.Reserved)),
      ),
      CarCommand.Reserve,
      DeciderMaterializer.syncIO
    ).unsafeRunSync()
    assert(events.contains(SagaEvent.StepProgressed(BookingStep.Car, SagaStepResult.Failed)))
    assert(events.exists { case SagaEvent.CompensationProgressed(BookingStep.Hotel, SagaStepResult.Completed) => true; case _ => false })
    assert(!events.exists { case SagaEvent.CompensationProgressed(BookingStep.Flight, _) => true; case _ => false })

  test("rerooted at car: flight fails — car and hotel both compensated"):
    val defaultBookingAtCarEvents: List[SagaEvent[BookingStep]] = List(
      SagaEvent.Booted(NonEmptySet.of(BookingStep.Hotel, BookingStep.Car, BookingStep.Flight)),
      SagaEvent.StepStarted(BookingStep.Hotel),
      SagaEvent.StepProgressed(BookingStep.Hotel, SagaStepResult.Completed)
    )
    val bookingAtCar = behavior.decider.evolveFrom(defaultBookingAtCarEvents)
    val events = Apparatus.runA(
      sagaRerootedAtCar[SyncIO](
        booking = bookingAtCar,
        flight  = flightDecider(failsOnReserve = true),
        hotel   = hotelDecider().evolveFrom(List(HotelEvent.Reserved)),
        car = carDecider()
      ),
      CarCommand.Reserve,
      DeciderMaterializer.syncIO
    ).unsafeRunSync()
    assert(events.exists { case SagaEvent.CompensationProgressed(BookingStep.Car,   SagaStepResult.Completed) => true; case _ => false })
    assert(events.exists { case SagaEvent.CompensationProgressed(BookingStep.Hotel, SagaStepResult.Completed) => true; case _ => false })
    assert(!events.exists { case SagaEvent.CompensationProgressed(BookingStep.Flight, _) => true; case _ => false })
