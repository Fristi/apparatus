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

import java.time.Instant
import java.time.temporal.ChronoUnit
import java.util.UUID

class BankAccountSpec extends CatsEffectSuite with TestContainersForAll:

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
    for
      _ <- PostgresEventStore[BankAccountEvent]().create()
      _ <- DoobieBankAccountTransactionRepository.create()
    yield ()

  // Run one command against a fresh aggregate id, returning the events produced.
  def runCommand(xa: Transactor[IO])(id: UUID, cmd: BankAccountCommand): IO[List[BankAccountEvent]] =
    bankAccount.transactionalDecider(id).flatMap(x => Apparatus.runA(x.tap(transactionsProjection(id, DoobieBankAccountTransactionRepository)), cmd)).transact(xa)

  test("open account emits Opened") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _ <- createSchema.transact(xa)
        id = UUID.randomUUID()
        events <- runCommand(xa)(id, BankAccountCommand.Open(now))
      yield assertEquals(events, List(BankAccountEvent.Opened(now)))
    }
  }

  test("deposit on uninitialized account is rejected") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _ <- createSchema.transact(xa)
        id = UUID.randomUUID()
        events <- runCommand(xa)(id, BankAccountCommand.Deposit(BigDecimal(100), now))
      yield assertEquals(events, List(BankAccountEvent.Rejected("invalid command for current state")))
    }
  }

  test("deposit after open increases recorded amount") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _ <- createSchema.transact(xa)
        id = UUID.randomUUID()
        _ <- runCommand(xa)(id, BankAccountCommand.Open(now))
        events <- runCommand(xa)(id, BankAccountCommand.Deposit(BigDecimal(250), now))
      yield assertEquals(events, List(BankAccountEvent.Deposited(BigDecimal(250), now)))
    }
  }

  test("withdraw within balance succeeds") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _ <- createSchema.transact(xa)
        id = UUID.randomUUID()
        _ <- runCommand(xa)(id, BankAccountCommand.Open(now))
        _ <- runCommand(xa)(id, BankAccountCommand.Deposit(BigDecimal(500), now))
        events <- runCommand(xa)(id, BankAccountCommand.Withdraw(BigDecimal(200), now))
      yield assertEquals(events, List(BankAccountEvent.Withdrawn(BigDecimal(200), now)))
    }
  }

  test("withdraw exceeding balance is rejected") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _ <- createSchema.transact(xa)
        id = UUID.randomUUID()
        _ <- runCommand(xa)(id, BankAccountCommand.Open(now))
        _ <- runCommand(xa)(id, BankAccountCommand.Deposit(BigDecimal(100), now))
        events <- runCommand(xa)(id, BankAccountCommand.Withdraw(BigDecimal(999), now))
      yield assertEquals(events, List(BankAccountEvent.Rejected("insufficient funds")))
    }
  }

  test("close open account emits ClosedAccount") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _ <- createSchema.transact(xa)
        id = UUID.randomUUID()
        _ <- runCommand(xa)(id, BankAccountCommand.Open(now))
        events <- runCommand(xa)(id, BankAccountCommand.Close(now))
      yield assertEquals(events, List(BankAccountEvent.ClosedAccount(now)))
    }
  }

  test("event store replays history so state is rebuilt across sessions") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _ <- createSchema.transact(xa)
        id = UUID.randomUUID()
        // Session 1: open
        _ <- runCommand(xa)(id, BankAccountCommand.Open(now))
        // Session 2: deposit — decider must reload Opened event from store
        _ <- runCommand(xa)(id, BankAccountCommand.Deposit(BigDecimal(300), now))
        // Session 3: withdraw — must see balance of 300
        events <- runCommand(xa)(id, BankAccountCommand.Withdraw(BigDecimal(300), now))
      yield assertEquals(events, List(BankAccountEvent.Withdrawn(BigDecimal(300), now)))
    }
  }

  test("commands after close are rejected") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _ <- createSchema.transact(xa)
        id = UUID.randomUUID()
        _ <- runCommand(xa)(id, BankAccountCommand.Open(now))
        _ <- runCommand(xa)(id, BankAccountCommand.Close(now))
        events <- runCommand(xa)(id, BankAccountCommand.Deposit(BigDecimal(50), now))
      yield assertEquals(events, List(BankAccountEvent.Rejected("invalid command for current state")))
    }
  }

  test("projection records deposit and withdrawal transactions") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _ <- createSchema.transact(xa)
        id = UUID.randomUUID()
        _ <- runCommand(xa)(id, BankAccountCommand.Open(now))
        _ <- runCommand(xa)(id, BankAccountCommand.Deposit(BigDecimal(500), now))
        _ <- runCommand(xa)(id, BankAccountCommand.Withdraw(BigDecimal(200), now))
        txs <- DoobieBankAccountTransactionRepository.listTransactions(id).transact(xa)
      yield assertEquals(
        txs,
        List(
          Transaction(TransactionType.Deposit,    BigDecimal(500), now),
          Transaction(TransactionType.Withdrawal, BigDecimal(200), now)
        )
      )
    }
  }
