package apparatus.tests

import apparatus.given
import apparatus.*
import apparatus.core.*
import apparatus.core.machines.DeciderMaterializer
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

  def prg: Apparatus[ConnectionIO, BankAccountCommand, List[BankAccountEvent]] =
    Apparatus.aggregateMachineE[ConnectionIO, BankAccountState, BankAccountCommand, BankAccountError, BankAccountEvent](bankAccount, _.id)
      .tap(transactionsProjection(DoobieBankAccountTransactionRepository))

  // Not used for routing but required by Apparatus.runSteps signature; aggregateMachineE
  // registers no AggregateMachine nodes so materialize is never called.
  val mat: DeciderMaterializer[ConnectionIO] = EventStore.deciderMaterializer(PostgresEventStore)

  /** Run commands in one transaction so state is shared across all steps.
   *  On domain error, the whole IO fails with the BankAccountError. */
  def run(xa: Transactor[IO])(cmds: BankAccountCommand*): IO[List[List[BankAccountEvent]]] =
    Apparatus.runSteps(prg, cmds.toList, mat).transact(xa)

  test("open account emits Opened") {
    withContainers { c =>
      val xa = makeTransactor(c)
      val id = UUID.randomUUID()
      for
        _ <- createSchema.transact(xa)
        results <- run(xa)(BankAccountCommand.Open(id, now))
      yield assertEquals(results, List(List(BankAccountEvent.Opened(id, now))))
    }
  }

  test("deposit on uninitialized account raises NotInitialized") {
    withContainers { c =>
      val xa = makeTransactor(c)
      val id = UUID.randomUUID()
      for
        _ <- createSchema.transact(xa)
        result <- run(xa)(BankAccountCommand.Deposit(id, BigDecimal(100), now)).attempt
      yield assert(result.left.exists(_.isInstanceOf[BankAccountError.NotInitialized.type]))
    }
  }

  test("deposit after open increases recorded amount") {
    withContainers { c =>
      val xa = makeTransactor(c)
      val id = UUID.randomUUID()
      for
        _ <- createSchema.transact(xa)
        results <- run(xa)(
          BankAccountCommand.Open(id, now),
          BankAccountCommand.Deposit(id, BigDecimal(250), now)
        )
      yield assertEquals(results.last, List(BankAccountEvent.Deposited(id, BigDecimal(250), now)))
    }
  }

  test("withdraw within balance succeeds") {
    withContainers { c =>
      val xa = makeTransactor(c)
      val id = UUID.randomUUID()
      for
        _ <- createSchema.transact(xa)
        results <- run(xa)(
          BankAccountCommand.Open(id, now),
          BankAccountCommand.Deposit(id, BigDecimal(500), now),
          BankAccountCommand.Withdraw(id, BigDecimal(200), now)
        )
      yield assertEquals(results.last, List(BankAccountEvent.Withdrawn(id, BigDecimal(200), now)))
    }
  }

  test("withdraw exceeding balance raises InsufficientFunds") {
    withContainers { c =>
      val xa = makeTransactor(c)
      val id = UUID.randomUUID()
      for
        _ <- createSchema.transact(xa)
        result <- run(xa)(
          BankAccountCommand.Open(id, now),
          BankAccountCommand.Deposit(id, BigDecimal(100), now),
          BankAccountCommand.Withdraw(id, BigDecimal(999), now)
        ).attempt
      yield assert(result.left.exists(_.isInstanceOf[BankAccountError.InsufficientFunds.type]))
    }
  }

  test("close open account emits ClosedAccount") {
    withContainers { c =>
      val xa = makeTransactor(c)
      val id = UUID.randomUUID()
      for
        _ <- createSchema.transact(xa)
        results <- run(xa)(
          BankAccountCommand.Open(id, now),
          BankAccountCommand.Close(id, now)
        )
      yield assertEquals(results.last, List(BankAccountEvent.ClosedAccount(id, now)))
    }
  }

  test("commands after close raise AlreadyClosed") {
    withContainers { c =>
      val xa = makeTransactor(c)
      val id = UUID.randomUUID()
      for
        _ <- createSchema.transact(xa)
        result <- run(xa)(
          BankAccountCommand.Open(id, now),
          BankAccountCommand.Close(id, now),
          BankAccountCommand.Deposit(id, BigDecimal(50), now)
        ).attempt
      yield assert(result.left.exists(_.isInstanceOf[BankAccountError.AlreadyClosed.type]))
    }
  }

  test("projection records deposit and withdrawal transactions") {
    withContainers { c =>
      val xa = makeTransactor(c)
      val id = UUID.randomUUID()
      for
        _ <- createSchema.transact(xa)
        _ <- run(xa)(
          BankAccountCommand.Open(id, now),
          BankAccountCommand.Deposit(id, BigDecimal(500), now),
          BankAccountCommand.Withdraw(id, BigDecimal(200), now)
        )
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
