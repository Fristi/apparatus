package apparatus.tests

import apparatus.core.*
import apparatus.core.patterns.{SagaEvent, SagaStepResult}
import apparatus.examples.*
import apparatus.{EventStore, PostgresEventStore}
import cats.data.NonEmptySet
import cats.effect.IO
import cats.implicits.*
import com.dimafeng.testcontainers.PostgreSQLContainer
import com.dimafeng.testcontainers.munit.TestContainersForAll
import doobie.*
import doobie.implicits.*
import munit.CatsEffectSuite
import org.testcontainers.utility.DockerImageName

import java.time.Instant
import java.time.temporal.ChronoUnit
import java.util.UUID

class BookingSagaDoobieSpec extends CatsEffectSuite with TestContainersForAll:

  override type Containers = PostgreSQLContainer

  val now = Instant.now().truncatedTo(ChronoUnit.MICROS)

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

  def runCommand(xa: Transactor[IO])(id: UUID, cmd: BookingCommand): IO[List[SagaEvent[BookingStep]]] =
    val prg: Apparatus[ConnectionIO, BookingCommand, List[SagaEvent[BookingStep]]] = saga[ConnectionIO]()
    val deciderMaterializer = EventStore.deciderMaterializer(PostgresEventStore)
    Apparatus.runA(prg, cmd, deciderMaterializer).transact(xa)

  test("happy path: all steps complete in order") {

    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _ <- createSchema.transact(xa)
        id = UUID.randomUUID()
        events <- runCommand(xa)(id, BookingCommand.Start)
      yield assertEquals(events, List(
        SagaEvent.Booted(NonEmptySet.of(BookingStep.Hotel, BookingStep.Car, BookingStep.Flight)),
        SagaEvent.StepStarted(BookingStep.Hotel),
        SagaEvent.StepProgressed(BookingStep.Hotel, SagaStepResult.Completed),
        SagaEvent.StepStarted(BookingStep.Car),
        SagaEvent.StepProgressed(BookingStep.Car, SagaStepResult.Completed),
        SagaEvent.StepStarted(BookingStep.Flight),
        SagaEvent.StepProgressed(BookingStep.Flight, SagaStepResult.Completed)
      ))
    }
  }

