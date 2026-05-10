package apparatus.tests

import apparatus.core.*
import apparatus.examples.*
import cats.Id
import cats.implicits.*

import scala.collection.immutable.SortedSet

class BookingSagaSpec extends munit.FunSuite:

  test("happy path: all steps complete in order"):
    assertEquals(
      Apparatus.runA(saga(), BookingCommand.Start),
      List(
        SagaEvent.Booted(BookingStep.Hotel, SortedSet(BookingStep.Car, BookingStep.Flight)),
        SagaEvent.StepProgressed(BookingStep.Hotel, SagaStepResult.Completed),
        SagaEvent.StepStarted(BookingStep.Car),
        SagaEvent.StepProgressed(BookingStep.Car, SagaStepResult.Completed),
        SagaEvent.StepStarted(BookingStep.Flight),
        SagaEvent.StepProgressed(BookingStep.Flight, SagaStepResult.Completed)
      )
    )

  test("flight fails: compensation triggered for completed steps"):
    val events = Apparatus.runA(saga(flight = flightDecider(failsOnReserve = true)), BookingCommand.Start)
    assert(events.contains(SagaEvent.StepProgressed(BookingStep.Flight, SagaStepResult.Failed)))
    assert(events.exists { case SagaEvent.CompensationTriggered(_, _) => true; case _ => false })
    assert(events.exists { case SagaEvent.CompensationProgressed(BookingStep.Car,    SagaStepResult.Completed) => true; case _ => false })
    assert(events.exists { case SagaEvent.CompensationProgressed(BookingStep.Hotel, SagaStepResult.Completed) => true; case _ => false })

  test("flight fails: car compensated, flight not"):
    val events = Apparatus.runA(saga(flight = flightDecider(failsOnReserve = true)), BookingCommand.Start)
    assert(events.contains(SagaEvent.StepProgressed(BookingStep.Flight, SagaStepResult.Failed)))
    assert(events.exists { case SagaEvent.CompensationProgressed(BookingStep.Car, SagaStepResult.Completed) => true; case _ => false })
    assert(!events.exists { case SagaEvent.CompensationProgressed(BookingStep.Flight, _) => true; case _ => false })

  test("car fails: no compensation needed"):
    val events = Apparatus.runA(saga(car = carDecider(failsOnReserve = true)), BookingCommand.Start)
    assert(events.contains(SagaEvent.StepProgressed(BookingStep.Car, SagaStepResult.Failed)))
    assert(!events.exists { case SagaEvent.CompensationStarted(_) => true; case _ => false })

  // ── feedbackMany (Feedback-level reroot) ─────────────────────────────────────

  test("feedbackMany: accepts List[BookingCommand] as entry point"):
    assertEquals(
      Apparatus.runA(bookingDecider.feedbackMany(makeServices()), List(BookingCommand.Start)),
      List(
        SagaEvent.Booted(BookingStep.Hotel, SortedSet(BookingStep.Car, BookingStep.Flight)),
        SagaEvent.StepProgressed(BookingStep.Hotel, SagaStepResult.Completed),
        SagaEvent.StepStarted(BookingStep.Car),
        SagaEvent.StepProgressed(BookingStep.Car, SagaStepResult.Completed),
        SagaEvent.StepStarted(BookingStep.Flight),
        SagaEvent.StepProgressed(BookingStep.Flight, SagaStepResult.Completed)
      )
    )

  test("rerooted at car: happy path — car reserved, flight follows"):
    val defaultBookingAtCarEvents: List[SagaEvent[BookingStep]] = List(
      SagaEvent.Booted(BookingStep.Hotel, SortedSet(BookingStep.Car, BookingStep.Flight)),
      SagaEvent.StepProgressed(BookingStep.Hotel, SagaStepResult.Completed),
      SagaEvent.StepStarted(BookingStep.Car)
    )
    val bookingAtCar = behavior.decider.evolveFrom(defaultBookingAtCarEvents)
    assertEquals(
      Apparatus.runA(sagaRerootedAtCar(booking = bookingAtCar), CarCommand.Reserve),
      List(
        SagaEvent.StepProgressed(BookingStep.Car,    SagaStepResult.Completed),
        SagaEvent.StepStarted(BookingStep.Flight),
        SagaEvent.StepProgressed(BookingStep.Flight, SagaStepResult.Completed)
      )
    )

  test("rerooted at car: car fails — hotel compensated, flight skipped"):
    val defaultBookingAtCarEvents: List[SagaEvent[BookingStep]] = List(
      SagaEvent.Booted(BookingStep.Hotel, SortedSet(BookingStep.Car, BookingStep.Flight)),
      SagaEvent.StepProgressed(BookingStep.Hotel, SagaStepResult.Completed),
      SagaEvent.StepStarted(BookingStep.Car)
    )
    val bookingAtCar = behavior.decider.evolveFrom(defaultBookingAtCarEvents)
    val events = Apparatus.runA(
      sagaRerootedAtCar(
        booking = bookingAtCar,
        car     = carDecider(failsOnReserve = true),
        hotel   = hotelDecider(initialState = HotelState.Reserved)
      ),
      CarCommand.Reserve
    )
    assert(events.contains(SagaEvent.StepProgressed(BookingStep.Car, SagaStepResult.Failed)))
    assert(events.exists { case SagaEvent.CompensationProgressed(BookingStep.Hotel, SagaStepResult.Completed) => true; case _ => false })
    assert(!events.exists { case SagaEvent.CompensationProgressed(BookingStep.Flight, _) => true; case _ => false })

  test("rerooted at car: flight fails — car and hotel both compensated"):
    val defaultBookingAtCarEvents: List[SagaEvent[BookingStep]] = List(
      SagaEvent.Booted(BookingStep.Hotel, SortedSet(BookingStep.Car, BookingStep.Flight)),
      SagaEvent.StepProgressed(BookingStep.Hotel, SagaStepResult.Completed)
    )
    val bookingAtCar = behavior.decider.evolveFrom(defaultBookingAtCarEvents)
    // Car succeeds, flight fails → compensation must roll back both car (carFeedback at Reserved)
    // and hotel (pre-seeded at Reserved because hotel booked in the earlier part of the saga).
    val events = Apparatus.runA(
      sagaRerootedAtCar(
        booking = bookingAtCar,
        flight  = flightDecider(failsOnReserve = true),
        hotel   = hotelDecider(initialState = HotelState.Reserved),
        car = carDecider()
      ),
      CarCommand.Reserve
    )
    assert(events.exists { case SagaEvent.CompensationProgressed(BookingStep.Car,   SagaStepResult.Completed) => true; case _ => false })
    assert(events.exists { case SagaEvent.CompensationProgressed(BookingStep.Hotel, SagaStepResult.Completed) => true; case _ => false })
    assert(!events.exists { case SagaEvent.CompensationProgressed(BookingStep.Flight, _) => true; case _ => false })
