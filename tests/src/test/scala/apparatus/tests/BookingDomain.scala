package apparatus.tests

import apparatus.examples.*

import java.time.LocalDate
import java.util.UUID

object BookingDomain:
  val flightId: UUID = UUID.fromString("00000000-0000-0000-0000-000000000001")
  val carId: UUID = UUID.fromString("00000000-0000-0000-0000-000000000002")
  val hotelId: UUID = UUID.fromString("00000000-0000-0000-0000-000000000003")
  val bookingId: UUID = UUID.fromString("00000000-0000-0000-0000-000000000004")

  val sagaState = BookingSagaState(fromCity = "Amsterdam", toCity = "London", fromDate = LocalDate.of(2026, 11, 1), toDate = LocalDate.of(2026, 11, 8))
  val carQuery = CarQuery(sagaState.toCity, sagaState.fromDate, sagaState.toDate)
  val hotelQuery = HotelQuery(sagaState.toCity, sagaState.fromDate, sagaState.toDate)
  val flightQuery = FlightQuery(sagaState.fromCity, sagaState.toCity, sagaState.fromDate, sagaState.toDate)
