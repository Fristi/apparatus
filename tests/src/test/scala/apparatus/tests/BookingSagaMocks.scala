package apparatus.tests

import apparatus.core.*
import apparatus.core.machines.evolveFrom
import apparatus.examples.*
import cats.Applicative
import cats.implicits.*

/** Test doubles for [[BookingServices]]; configure per-offer failures to drive saga rollbacks. */
object BookingSagaMocks:

  final class MockFlightService[F[_]: Applicative](
    defaultFlightNumber: String = "BA123",
    failOnFlightNumber:  Option[String] = None
  ) extends FlightService[F]:
    def searchFlight(query: FlightQuery): F[Option[String]] =
      (failOnFlightNumber match
        case Some(number) if number == defaultFlightNumber => None
        case _                                             => Some(defaultFlightNumber)
      ).pure[F]

  final class MockCarService[F[_]: Applicative](
    defaultCarModel: String = "Tesla Model 3",
    failOnCarModel:  Option[String] = None
  ) extends CarService[F]:
    def searchCar(query: CarQuery): F[Option[String]] =
      (failOnCarModel match
        case Some(model) if model == defaultCarModel => None
        case _                                       => Some(defaultCarModel)
      ).pure[F]

  final class MockHotelService[F[_]: Applicative](
    defaultHotelName: String = "Grand Hotel",
    failOnHotelName:  Option[String] = None
  ) extends HotelService[F]:
    def searchHotel(query: HotelQuery): F[Option[String]] =
      (failOnHotelName match
        case Some(name) if name == defaultHotelName => None
        case _                                      => Some(defaultHotelName)
      ).pure[F]

  def services[F[_]: Applicative](
    flight: FlightService[F],
    car:    CarService[F],
    hotel:  HotelService[F]
  ): BookingServices[F] =
    BookingServices(flight, car, hotel)

  def defaultServices[F[_]: Applicative]: BookingServices[F] =
    services(new MockFlightService[F](), new MockCarService[F](), new MockHotelService[F]())

  /** Simulates a failed flight search (no offers). */
  def failingFlightSearch[F[_]: Applicative]: BookingServices[F] =
    services(
      new MockFlightService[F](failOnFlightNumber = Some("BA123")),
      new MockCarService[F](),
      new MockHotelService[F]()
    )

  /** Simulates a failed car search (no offers). */
  def failingCarSearch[F[_]: Applicative]: BookingServices[F] =
    services(
      new MockFlightService[F](),
      new MockCarService[F](failOnCarModel = Some("Tesla Model 3")),
      new MockHotelService[F]()
    )

  /** Simulates a failed hotel search (no offers). */
  def failingHotelSearch[F[_]: Applicative]: BookingServices[F] =
    services(
      new MockFlightService[F](),
      new MockCarService[F](),
      new MockHotelService[F](failOnHotelName = Some("Grand Hotel"))
    )

  /** Hotel sub-aggregate already reserved (for rerooted saga scenarios). */
  def hotelReservedMachine[F[_]: Applicative]: Apparatus[F, HotelCommand, List[HotelEvent]] =
    hotelMachine(
      hotelDecider.evolveFrom(List(
        HotelEvent.SearchStarted(hotelId, bookingId, BookingDomain.hotelQuery),
        HotelEvent.Reserved(hotelId, bookingId)
      ))
    )
