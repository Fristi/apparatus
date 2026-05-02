package apparatus.core

import cats.Monad
import cats.implicits.*

sealed trait FSM[F[_], I, O]

object FSM:
  case class Basic[F[_], I, O](machine: BaseMachineT[F, I, O])                          extends FSM[F, I, O]
  case class Sequential[F[_], A, B, C](left: FSM[F, A, B], right: FSM[F, B, C])         extends FSM[F, A, C]
  case class Parallel[F[_], A, B, C, D](left: FSM[F, A, B], right: FSM[F, C, D])        extends FSM[F, (A, C), (B, D)]
  case class Alternative[F[_], A, B, C, D](left: FSM[F, A, B], right: FSM[F, C, D])     extends FSM[F, Either[A, C], Either[B, D]]

  def run[F[_]: Monad, I, O](fsm: FSM[F, I, O], input: I): F[(O, FSM[F, I, O])] = fsm match
    case Basic(m) =>
      m.step(input).map((o, s) => (o, Basic(BaseMachineT(s, (s, i) => m.action(s, i)))))

    case Sequential(left, right) =>
      for
        (o1, l2) <- run(left, input)
        (o2, r2) <- run(right, o1)
      yield (o2, Sequential(l2, r2))

    case Parallel(left, right) =>
      for
        (o1, l2) <- run(left, input._1)
        (o2, r2) <- run(right, input._2)
      yield ((o1, o2), Parallel(l2, r2))

    case Alternative(left, right) =>
      input match
        case Left(l)  => run(left, l).map  { case (o, l2) => (Left(o),  Alternative(l2, right)) }
        case Right(r) => run(right, r).map { case (o, r2) => (Right(o), Alternative(left, r2)) }
