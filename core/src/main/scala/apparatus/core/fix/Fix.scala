package apparatus.core.fix

import cats.Functor

final case class Fix[F[_]](unfix: F[Fix[F]])

def cata[F[_]: Functor, A](term: Fix[F])(alg: F[A] => A): A =
  alg(Functor[F].map(term.unfix)(cata(_)(alg)))