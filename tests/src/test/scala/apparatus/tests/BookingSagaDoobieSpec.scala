package apparatus.tests

import apparatus.core.*
import apparatus.core.patterns.{SagaEvent, SagaState, SagaStepResult}
import apparatus.examples.*
import apparatus.examples.saga.*
import apparatus.{EventStore, PostgresEventStore}
import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits.*
import com.dimafeng.testcontainers.PostgreSQLContainer
import com.dimafeng.testcontainers.munit.TestContainersForAll
import doobie.*
import doobie.implicits.*
import munit.CatsEffectSuite
import org.testcontainers.utility.DockerImageName

class BookingSagaDoobieSpec extends CatsEffectSuite with TestContainersForAll:

  override type Containers = PostgreSQLContainer

  override def startContainers(): PostgreSQLContainer =
    PostgreSQLContainer.Def(
      dockerImageName = DockerImageName.parse("postgres:15.1"),
      databaseName = "testcontainer-scala",
      username = "scala",
      password = "scala"
    ).start()

  def makeTransactor(c: Containers): Transactor[IO] =
    Transactor.fromDriverManager[IO](
      driver = "org.postgresql.Driver",
      url = c.jdbcUrl,
      user = c.username,
      password = c.password,
      logHandler = None
    )

  /** Fresh schema plus empty event store — required because TestContainersForAll shares one DB across tests. */
  val resetStore: ConnectionIO[Unit] =
    for
      _ <- PostgresEventStore.create()
      _ <- sql"TRUNCATE eventstreams".update.run.void
    yield ()

  private def runBookingCommand(
    xa:              Transactor[IO],
    bookingServices: BookingServices[ConnectionIO] = BookingServices.default[ConnectionIO]
  )(cmd: BookingCommand): IO[List[SagaEvent[BookingStep, BookingSagaState]]] =
    val prg = bookingEntrypoint[ConnectionIO](bookingServices, BookingDomain.testBehavior)
    val mat = EventStore.deciderMaterializer(PostgresEventStore)
    Apparatus.runA(prg, cmd, mat).transact(xa)

  private def runCarCommand(
    xa:              Transactor[IO],
    bookingServices: BookingServices[ConnectionIO] = BookingServices.default[ConnectionIO]
  )(cmd: CarCommand): IO[List[SagaEvent[BookingStep, BookingSagaState]]] =
    val prg = carEntrypoint[ConnectionIO](bookingServices, BookingDomain.testBehavior)
    val mat = EventStore.deciderMaterializer(PostgresEventStore)
    Apparatus.runA(prg, cmd, mat).transact(xa)

  private def runHotelCommand(
    xa:              Transactor[IO],
    bookingServices: BookingServices[ConnectionIO] = BookingServices.default[ConnectionIO]
  )(cmd: HotelCommand): IO[List[SagaEvent[BookingStep, BookingSagaState]]] =
    val prg = hotelEntrypoint[ConnectionIO](bookingServices, BookingDomain.testBehavior)
    val mat = EventStore.deciderMaterializer(PostgresEventStore)
    Apparatus.runA(prg, cmd, mat).transact(xa)

  private def runFlightCommand(
    xa:              Transactor[IO],
    bookingServices: BookingServices[ConnectionIO] = BookingServices.default[ConnectionIO]
  )(cmd: FlightCommand): IO[List[SagaEvent[BookingStep, BookingSagaState]]] =
    val prg = flightEntrypoint[ConnectionIO](bookingServices, BookingDomain.testBehavior)
    val mat = EventStore.deciderMaterializer(PostgresEventStore)
    Apparatus.runA(prg, cmd, mat).transact(xa)

  test("civilian happy path: all steps complete in one command") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _      <- resetStore.transact(xa)
        events <- runBookingCommand(xa)(BookingCommand.Start(BookingDomain.bookingId, BookingDomain.sagaState))
      yield assertEquals(
        events,
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
    }
  }

  test("diplomat happy path: async verification commands drive each step") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _           <- resetStore.transact(xa)
        startEvents <- runBookingCommand(xa)(BookingCommand.Start(BookingDomain.bookingId, BookingDomain.diplomatSagaState))
        _ = assertEquals(
          startEvents,
          List(
            SagaEvent.Booted(BookingDomain.bookingId, BookingDomain.diplomatSagaState, NonEmptyList.of(
              SagaState.StepDispatch(BookingStep.Hotel, BookingDomain.hotelId),
              SagaState.StepDispatch(BookingStep.Car, BookingDomain.carId),
              SagaState.StepDispatch(BookingStep.Flight, BookingDomain.flightId)
            )),
            SagaEvent.StepStarted(BookingDomain.bookingId, BookingDomain.diplomatSagaState, BookingStep.Hotel, BookingDomain.hotelId)
          )
        )
        hotelEvents <- runHotelCommand(xa)(HotelCommand.VerifyBackgroundCheck(BookingDomain.hotelId))
        _ = assertEquals(
          hotelEvents,
          List(
            SagaEvent.StepProgressed(BookingDomain.bookingId, BookingStep.Hotel, SagaStepResult.Completed),
            SagaEvent.StepStarted(BookingDomain.bookingId, BookingDomain.diplomatSagaState, BookingStep.Car, BookingDomain.carId)
          )
        )
        carEvents <- runCarCommand(xa)(CarCommand.VerifyDriverLicense(BookingDomain.carId))
        _ = assertEquals(
          carEvents,
          List(
            SagaEvent.StepProgressed(BookingDomain.bookingId, BookingStep.Car, SagaStepResult.Completed),
            SagaEvent.StepStarted(BookingDomain.bookingId, BookingDomain.diplomatSagaState, BookingStep.Flight, BookingDomain.flightId)
          )
        )
        flightEvents <- runFlightCommand(xa)(FlightCommand.VerifyClearance(BookingDomain.flightId))
      yield assertEquals(
        flightEvents,
        List(SagaEvent.StepProgressed(BookingDomain.bookingId, BookingStep.Flight, SagaStepResult.Completed))
      )
    }
  }

  test("diplomat car license rejected: hotel compensated after async car step fails") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _      <- resetStore.transact(xa)
        _      <- runBookingCommand(xa)(BookingCommand.Start(BookingDomain.bookingId, BookingDomain.diplomatSagaState))
        _      <- runHotelCommand(xa)(HotelCommand.VerifyBackgroundCheck(BookingDomain.hotelId))
        events <- runCarCommand(xa)(CarCommand.RejectDriverLicense(BookingDomain.carId))
      yield
        assert(events.contains(SagaEvent.StepProgressed(BookingDomain.bookingId, BookingStep.Car, SagaStepResult.Failed)))
        assert(events.exists { case SagaEvent.CompensationProgressed(_, BookingStep.Hotel, SagaStepResult.Completed) => true; case _ => false })
        assert(!events.exists { case SagaEvent.CompensationProgressed(_, BookingStep.Flight, _) => true; case _ => false })
    }
  }

  test("diplomat flight clearance rejected: car and hotel compensated") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _      <- resetStore.transact(xa)
        _      <- runBookingCommand(xa)(BookingCommand.Start(BookingDomain.bookingId, BookingDomain.diplomatSagaState))
        _      <- runHotelCommand(xa)(HotelCommand.VerifyBackgroundCheck(BookingDomain.hotelId))
        _      <- runCarCommand(xa)(CarCommand.VerifyDriverLicense(BookingDomain.carId))
        events <- runFlightCommand(xa)(FlightCommand.RejectClearance(BookingDomain.flightId))
      yield
        assert(events.contains(SagaEvent.StepProgressed(BookingDomain.bookingId, BookingStep.Flight, SagaStepResult.Failed)))
        assert(events.exists { case SagaEvent.CompensationProgressed(_, BookingStep.Car, SagaStepResult.Completed) => true; case _ => false })
        assert(events.exists { case SagaEvent.CompensationProgressed(_, BookingStep.Hotel, SagaStepResult.Completed) => true; case _ => false })
    }
  }
