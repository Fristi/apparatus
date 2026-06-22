package apparatus.tests

import apparatus.core.*
import apparatus.core.patterns.{SagaEvent, SagaState, SagaStepResult}
import apparatus.examples.*
import apparatus.{EventStore, PostgresEventStore}
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.effect.IO
import cats.implicits.*
import com.dimafeng.testcontainers.PostgreSQLContainer
import com.dimafeng.testcontainers.munit.TestContainersForAll
import doobie.*
import doobie.implicits.*
import munit.CatsEffectSuite
import org.testcontainers.utility.DockerImageName

import java.util.UUID

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

  val createSchema: ConnectionIO[Unit] =
    PostgresEventStore.create().void

  private def runCommand(
    xa:              Transactor[IO],
    bookingServices: BookingServices[ConnectionIO] = BookingServices.default[ConnectionIO]
  )(cmd: BookingCommand): IO[List[SagaEvent[BookingStep, BookingSagaState]]] =
    val prg = saga[ConnectionIO](bookingServices, BookingDomain.testBehavior)
    val mat = EventStore.deciderMaterializer(PostgresEventStore)
    Apparatus.runA(prg, cmd, mat).transact(xa)

  test("happy path: all steps complete in order") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _      <- createSchema.transact(xa)
        events <- runCommand(xa)(BookingCommand.Start(BookingDomain.bookingId, BookingDomain.sagaState))
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
