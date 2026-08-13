package apparatus.proteus

import apparatus.core.Apparatus
import apparatus.core.fix.alg.{CompiledNetwork, compile}
import apparatus.core.machines.DeciderMaterializer
import cats.effect.Sync
import cats.implicits.*
import io.grpc.ServerServiceDefinition
import proteus.server.{ServerBackend, ServerService}
import proteus.{ProtobufCodec, Rpc, Service}

final case class ProteusServiceConfig(
  rpcName: String,
  serviceName: String,
  packageName: String,
  comment: Option[String] = None
)

object Proteus {
  def defFromNetwork[F[_], G[_], C, I, O](cfg: ProteusServiceConfig, apparatus: Apparatus[F, I, O], mat: DeciderMaterializer[F])
                                         (implicit B: ServerBackend[F, G, C], F: Sync[F], I : ProtobufCodec[I], O: ProtobufCodec[O]): F[ServerServiceDefinition] = {
    val rpc: Rpc.Unary[I, O] = Rpc.unary[I, O](cfg.rpcName)
    val svc = Service(cfg.packageName, cfg.serviceName, cfg.comment.orNull).rpc(rpc)

    for {
      network <- compile(mat)(apparatus)
    } yield ServerService.apply[F, G, C](using B)
      .rpc(rpc, network.run(_))
      .build(svc)

  }

  def defFromCompiledNetwork[F[_], G[_], C, I, O](cfg: ProteusServiceConfig, network: CompiledNetwork[F, I, O])
                                                  (implicit B: ServerBackend[F, G, C], I: ProtobufCodec[I], O: ProtobufCodec[O]): ServerServiceDefinition = {
    val rpc: Rpc.Unary[I, O] = Rpc.unary[I, O](cfg.rpcName)
    val svc = Service(cfg.packageName, cfg.serviceName, cfg.comment.orNull).rpc(rpc)

    ServerService.apply[F, G, C](using B)
      .rpc(rpc, network.run(_))
      .build(svc)
  }
}