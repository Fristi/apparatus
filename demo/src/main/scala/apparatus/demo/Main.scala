package apparatus.demo

import apparatus.core.*
import apparatus.core.fix.alg.compile
import apparatus.core.patterns.SagaEvent
import apparatus.examples.{BookingCommand, BookingSagaState, BookingServices, BookingStep, saga}
import apparatus.proteus.{Proteus, ProteusServiceConfig}
import apparatus.{EventStore, PostgresEventStore}
import cats.effect.std.Dispatcher
import cats.effect.{IO, IOApp}
import cats.implicits.*
import ciris.*
import doobie.Transactor
import doobie.free.connection.ConnectionIO
import doobie.implicits.*
import doobie.util.log.LogHandler
import io.grpc.netty.shaded.io.grpc.netty.NettyServerBuilder
import io.grpc.protobuf.services.ProtoReflectionService
import proteus.ProtobufDeriver.DerivationFlag
import proteus.server.{Fs2ServerBackend, GrpcContext}
import proteus.{ProtobufCodec, ProtobufDeriver}
import zio.blocks.schema.Schema

import java.time.{Instant, LocalDate, ZoneOffset}
import java.util.UUID

// ── Server ───────────────────────────────────────────────────────────────────



object Main extends IOApp.Simple:

  final case class PgConfig(
    host:     String,
    port:     Int,
    database: String,
    user:     String,
    password: Secret[String]
  )

  val pgConfig: ConfigValue[Effect, PgConfig] =
    (
      env("PG_HOST").as[String].default("localhost"),
      env("PG_PORT").as[Int].default(5432),
      env("PG_DATABASE").as[String].default("apparatus"),
      env("PG_USER").as[String].default("apparatus"),
      env("PG_PASSWORD").secret.default(Secret("apparatus"))
    ).parMapN(PgConfig.apply)

  def transactor(c: PgConfig): Transactor[IO] =
    Transactor.fromDriverManager[IO](
      driver     = "org.postgresql.Driver",
      url        = s"jdbc:postgresql://${c.host}:${c.port}/${c.database}",
      user       = c.user,
      password   = c.password.value,
      logHandler = Some(LogHandler.jdkLogHandler[IO])
    )

  val run: IO[Unit] =
    pgConfig.load[IO].flatMap { cfg =>
      val xa  = transactor(cfg)
      val mat = EventStore.deciderMaterializer(PostgresEventStore)
      val svcCfg = ProteusServiceConfig(
        rpcName     = "Book",
        serviceName = "BookingService",
        packageName = "apparatus.demo",
        comment = Some("A service to book a holiday")
      )

      Dispatcher.parallel[IO].use { dispatcher =>
        given Fs2ServerBackend[IO, IO, GrpcContext] = Fs2ServerBackend[IO](dispatcher, 16)

        lazy val uuidCodec: ProtobufCodec[UUID] =
          ProtobufCodec.derived[String].transform(UUID.fromString, _.toString())

        lazy val localDateCodec: ProtobufCodec[LocalDate] =
          ProtobufCodec
            .derived[Long]
            .transform[LocalDate](
              millis => LocalDate.ofInstant(Instant.ofEpochMilli(millis), ZoneOffset.UTC),
              _.toEpochDay
            )

        given ProtobufDeriver = ProtobufDeriver
          .enable(DerivationFlag.NestedOneOf)
          .instance(uuidCodec)
          .instance(localDateCodec)

        given ProtobufCodec[BookingCommand] = ProtobufCodec.derived[BookingCommand]

        final case class AggregateSummary(events: List[SagaEvent[BookingStep, BookingSagaState]]) derives ProtobufCodec

        for
          _ <- PostgresEventStore.create().transact(xa)

          s = saga[ConnectionIO](BookingServices.default[ConnectionIO]).rmap(evs => AggregateSummary(evs))

          // Compile the saga network in ConnectionIO, then lift to IO
          networkCIO <- compile(mat)(s).transact(xa)
          network     = networkCIO.mapK(xa.trans)

          svcDef = Proteus.defFromCompiledNetwork(svcCfg, network)

          server <- IO.blocking {
            NettyServerBuilder
              .forPort(9000)
              .addService(svcDef)
              .addService(ProtoReflectionService.newInstance())
              .build()
              .start()
          }

          _ <- IO.println("gRPC server listening on port 9000")
          _ <- IO.blocking(server.awaitTermination())
        yield ()
      }
    }
