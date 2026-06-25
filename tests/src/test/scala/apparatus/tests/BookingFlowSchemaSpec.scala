package apparatus.tests

import apparatus.core.patterns.*
import apparatus.examples.*
import apparatus.examples.saga.*
import cats.data.NonEmptyList
import munit.FunSuite
import zio.blocks.schema.*

class BookingFlowSchemaSpec extends FunSuite:

  test("BookingFlow round-trips through JSON") {
    assertEquals(BookingFlow.Civilian.toJsonString.fromJson[BookingFlow], Right(BookingFlow.Civilian))
    assertEquals(BookingFlow.Diplomat.toJsonString.fromJson[BookingFlow], Right(BookingFlow.Diplomat))
  }

  test("BookingSagaState with diplomat flow round-trips") {
    val json = BookingDomain.diplomatSagaState.toJsonString
    assertEquals(json.fromJson[BookingSagaState], Right(BookingDomain.diplomatSagaState))
  }

  test("HotelEvent.SearchStarted with diplomat flow round-trips") {
    val event = HotelEvent.SearchStarted(
      BookingDomain.hotelId,
      BookingDomain.bookingId,
      BookingDomain.hotelQuery,
      BookingFlow.Diplomat
    )
    val json = event.toJsonString
    assertEquals(json.fromJson[HotelEvent], Right(event))
  }

  test("SagaEvent.Booted with diplomat state round-trips") {
    val event: SagaEvent[BookingStep, BookingSagaState] = SagaEvent.Booted(
      BookingDomain.bookingId,
      BookingDomain.diplomatSagaState,
      cats.data.NonEmptyList.of(SagaState.StepDispatch(BookingStep.Hotel, BookingDomain.hotelId))
    )
    val json = event.toJsonString
    assertEquals(json.fromJson[SagaEvent[BookingStep, BookingSagaState]], Right(event))
  }

  test("HotelEvent.BackgroundCheckRequired round-trips") {
    val event = HotelEvent.BackgroundCheckRequired(BookingDomain.hotelId, BookingDomain.bookingId, "Grand Hotel")
    val json = event.toJsonString
    assertEquals(json.fromJson[HotelEvent], Right(event))
  }
