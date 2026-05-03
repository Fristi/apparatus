package apparatus

import apparatus.core.*
import doobie.free.connection
import doobie.{Read, Write}
import doobie.free.connection.ConnectionIO

import java.util.UUID

final case class EventEntry[O](sequenceNr: Int, body: O)

trait EventStore[F[_], O]:
  def lockAggregate(id: UUID): F[Boolean]
  def loadAggregateStream(id: UUID): F[List[EventEntry[O]]]
  def appendAggregateStream(id: UUID, events: List[EventEntry[O]]): F[Int]


extension [S, I, O : {Read, Write}](decider: Decider[S, I, List[O]]) {
  def transactionalDecider(id: UUID): ConnectionIO[FSM[ConnectionIO, I, List[O]]] =
    for {
      store = PostgresEventStore[O]()
      acquired <- store.lockAggregate(id)
      _ <- if(!acquired) connection.raiseError(new Throwable("Cannot acquire lock")) else connection.unit
      events <- store.loadAggregateStream(id)
      evolvedDecider = decider.evolveFrom(events.map(_.body))
    } yield {
      val maxSequenceNr = events.maxByOption(_.sequenceNr).map(_.sequenceNr).getOrElse(0)
      val baseMachineT = new BaseMachineT[ConnectionIO, I, List[O]] {
        override type State = S
        override def initialState: S = evolvedDecider.state
        override def action(state: State, input: I): ConnectionIO[(List[O], State)] =
          val o = evolvedDecider.decide(input, state)
          val eventStreamToAppend = o.zipWithIndex.map((o, idx) => EventEntry(maxSequenceNr + idx, o))
          val ns = evolvedDecider.evolve(o, state)

          store.appendAggregateStream(id, eventStreamToAppend).map(_ => (o, ns))
      }

      FSM.Basic(baseMachineT)
    }
}