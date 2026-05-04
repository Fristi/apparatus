package apparatus

import apparatus.core.*
import cats.effect.IO
import cats.implicits.*
import com.dimafeng.testcontainers.PostgreSQLContainer
import com.dimafeng.testcontainers.munit.TestContainersForAll
import doobie.*
import doobie.free.connection.ConnectionIO
import doobie.implicits.*
import munit.CatsEffectSuite
import org.testcontainers.utility.DockerImageName

import java.util.UUID

class BankAccountSpec extends CatsEffectSuite with TestContainersForAll:

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
    sql"""
      CREATE TABLE IF NOT EXISTS eventstreams (
        aggregate_id UUID NOT NULL,
        sequence_nr  INT  NOT NULL,
        body         TEXT NOT NULL,
        PRIMARY KEY (aggregate_id, sequence_nr)
      )
    """.update.run.map(_ => ())

  // Run one command against a fresh aggregate id, returning the events produced.
  def runCommand(xa: Transactor[IO])(id: UUID, cmd: BankAccountCommand): IO[List[BankAccountEvent]] =
    (for
      fsm          <- bankAccount.transactionalDecider(id)
      (events, _)  <- FSM.run(fsm, cmd)
    yield events).transact(xa)

  test("open account emits Opened") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _      <- createSchema.transact(xa)
        id      = UUID.randomUUID()
        events <- runCommand(xa)(id, BankAccountCommand.Open)
      yield assertEquals(events, List(BankAccountEvent.Opened))
    }
  }

  test("deposit on uninitialized account is rejected") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _      <- createSchema.transact(xa)
        id      = UUID.randomUUID()
        events <- runCommand(xa)(id, BankAccountCommand.Deposit(BigDecimal(100)))
      yield assertEquals(events, List(BankAccountEvent.Rejected("invalid command for current state")))
    }
  }

  test("deposit after open increases recorded amount") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _      <- createSchema.transact(xa)
        id      = UUID.randomUUID()
        _      <- runCommand(xa)(id, BankAccountCommand.Open)
        events <- runCommand(xa)(id, BankAccountCommand.Deposit(BigDecimal(250)))
      yield assertEquals(events, List(BankAccountEvent.Deposited(BigDecimal(250))))
    }
  }

  test("withdraw within balance succeeds") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _      <- createSchema.transact(xa)
        id      = UUID.randomUUID()
        _      <- runCommand(xa)(id, BankAccountCommand.Open)
        _      <- runCommand(xa)(id, BankAccountCommand.Deposit(BigDecimal(500)))
        events <- runCommand(xa)(id, BankAccountCommand.Withdraw(BigDecimal(200)))
      yield assertEquals(events, List(BankAccountEvent.Withdrawn(BigDecimal(200))))
    }
  }

  test("withdraw exceeding balance is rejected") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _      <- createSchema.transact(xa)
        id      = UUID.randomUUID()
        _      <- runCommand(xa)(id, BankAccountCommand.Open)
        _      <- runCommand(xa)(id, BankAccountCommand.Deposit(BigDecimal(100)))
        events <- runCommand(xa)(id, BankAccountCommand.Withdraw(BigDecimal(999)))
      yield assertEquals(events, List(BankAccountEvent.Rejected("insufficient funds")))
    }
  }

  test("close open account emits ClosedAccount") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _      <- createSchema.transact(xa)
        id      = UUID.randomUUID()
        _      <- runCommand(xa)(id, BankAccountCommand.Open)
        events <- runCommand(xa)(id, BankAccountCommand.Close)
      yield assertEquals(events, List(BankAccountEvent.ClosedAccount))
    }
  }

  test("event store replays history so state is rebuilt across sessions") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _      <- createSchema.transact(xa)
        id      = UUID.randomUUID()
        // Session 1: open
        _      <- runCommand(xa)(id, BankAccountCommand.Open)
        // Session 2: deposit — decider must reload Opened event from store
        _      <- runCommand(xa)(id, BankAccountCommand.Deposit(BigDecimal(300)))
        // Session 3: withdraw — must see balance of 300
        events <- runCommand(xa)(id, BankAccountCommand.Withdraw(BigDecimal(300)))
      yield assertEquals(events, List(BankAccountEvent.Withdrawn(BigDecimal(300))))
    }
  }

  test("commands after close are rejected") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _      <- createSchema.transact(xa)
        id      = UUID.randomUUID()
        _      <- runCommand(xa)(id, BankAccountCommand.Open)
        _      <- runCommand(xa)(id, BankAccountCommand.Close)
        events <- runCommand(xa)(id, BankAccountCommand.Deposit(BigDecimal(50)))
      yield assertEquals(events, List(BankAccountEvent.Rejected("invalid command for current state")))
    }
  }
