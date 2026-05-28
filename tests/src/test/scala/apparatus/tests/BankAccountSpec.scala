package apparatus.tests

import apparatus.given
import apparatus.*
import apparatus.core.*
import apparatus.examples.*
import cats.effect.IO
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
      _ <- PostgresEventStore.create()
      _ <- DoobieBankAccountTransactionRepository.create()
    yield ()

  def runCommand(xa: Transactor[IO])(cmd: BankAccountCommand): IO[List[BankAccountEvent]] = {
    val prg: Apparatus[ConnectionIO, BankAccountCommand, List[BankAccountEvent]] =
      Apparatus.aggregateMachine("bank-account", bankAccount, _.id)
        .tap(transactionsProjection(DoobieBankAccountTransactionRepository))
    val mat = EventStore.deciderMaterializer(PostgresEventStore)

    Apparatus.runA(prg, cmd, mat).transact(xa)
  }

  test("open account emits Opened") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _ <- createSchema.transact(xa)
        id = UUID.randomUUID()
        events <- runCommand(xa)(BankAccountCommand.Open(id, now))
      yield assertEquals(events, List(BankAccountEvent.Opened(id, now)))
    }
  }

  test("deposit on uninitialized account is rejected") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _ <- createSchema.transact(xa)
        id = UUID.randomUUID()
        events <- runCommand(xa)(BankAccountCommand.Deposit(id, BigDecimal(100), now))
      yield assertEquals(events, List(BankAccountEvent.Rejected(id, "invalid command for current state")))
    }
  }

  test("deposit after open increases recorded amount") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _ <- createSchema.transact(xa)
        id = UUID.randomUUID()
        _ <- runCommand(xa)(BankAccountCommand.Open(id, now))
        events <- runCommand(xa)(BankAccountCommand.Deposit(id, BigDecimal(250), now))
      yield assertEquals(events, List(BankAccountEvent.Deposited(id, BigDecimal(250), now)))
    }
  }

  test("withdraw within balance succeeds") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _ <- createSchema.transact(xa)
        id = UUID.randomUUID()
        _ <- runCommand(xa)(BankAccountCommand.Open(id, now))
        _ <- runCommand(xa)(BankAccountCommand.Deposit(id, BigDecimal(500), now))
        events <- runCommand(xa)(BankAccountCommand.Withdraw(id, BigDecimal(200), now))
      yield assertEquals(events, List(BankAccountEvent.Withdrawn(id, BigDecimal(200), now)))
    }
  }

  test("withdraw exceeding balance is rejected") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _ <- createSchema.transact(xa)
        id = UUID.randomUUID()
        _ <- runCommand(xa)(BankAccountCommand.Open(id, now))
        _ <- runCommand(xa)(BankAccountCommand.Deposit(id, BigDecimal(100), now))
        events <- runCommand(xa)(BankAccountCommand.Withdraw(id, BigDecimal(999), now))
      yield assertEquals(events, List(BankAccountEvent.Rejected(id, "insufficient funds")))
    }
  }

  test("close open account emits ClosedAccount") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _ <- createSchema.transact(xa)
        id = UUID.randomUUID()
        _ <- runCommand(xa)(BankAccountCommand.Open(id, now))
        events <- runCommand(xa)(BankAccountCommand.Close(id, now))
      yield assertEquals(events, List(BankAccountEvent.ClosedAccount(id, now)))
    }
  }

  test("event store replays history so state is rebuilt across sessions") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _ <- createSchema.transact(xa)
        id = UUID.randomUUID()
        _ <- runCommand(xa)(BankAccountCommand.Open(id, now))
        _ <- runCommand(xa)(BankAccountCommand.Deposit(id, BigDecimal(300), now))
        events <- runCommand(xa)(BankAccountCommand.Withdraw(id, BigDecimal(300), now))
      yield assertEquals(events, List(BankAccountEvent.Withdrawn(id, BigDecimal(300), now)))
    }
  }

  test("commands after close are rejected") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _ <- createSchema.transact(xa)
        id = UUID.randomUUID()
        _ <- runCommand(xa)(BankAccountCommand.Open(id, now))
        _ <- runCommand(xa)(BankAccountCommand.Close(id, now))
        events <- runCommand(xa)(BankAccountCommand.Deposit(id, BigDecimal(50), now))
      yield assertEquals(events, List(BankAccountEvent.Rejected(id, "invalid command for current state")))
    }
  }

  test("projection records deposit and withdrawal transactions") {
    withContainers { c =>
      val xa = makeTransactor(c)
      for
        _ <- createSchema.transact(xa)
        id = UUID.randomUUID()
        _ <- runCommand(xa)(BankAccountCommand.Open(id, now))
        _ <- runCommand(xa)(BankAccountCommand.Deposit(id, BigDecimal(500), now))
        _ <- runCommand(xa)(BankAccountCommand.Withdraw(id, BigDecimal(200), now))
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
