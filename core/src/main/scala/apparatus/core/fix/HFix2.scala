package apparatus.core.fix

import apparatus.core.patterns

final case class HFix2[H[_[_, _], _, _], I, O](unfix: H[[A, B] =>> HFix2[H, A, B], I, O])

trait FunctionK2[F[_, _], G[_, _]] {
  def apply[I, O](f: F[I, O]): G[I, O]
}

/** An algebra: collapses one layer, preserving both indices */
type HAlgebra2[H[_[_, _], _, _], F[_, _]] =
  [I, O] => H[F, I, O] => F[I, O]

trait HFunctor2[H[_[_, _], _, _]]:
  // cleaner standalone form
  def hfmap[F[_, _], G[_, _], I, O](nt: FunctionK2[F, G])(hfio: H[F, I, O]): H[G, I, O]

def cata2[H[_[_, _], _, _], F[_, _], I, O](alg: HAlgebra2[H, F])(hf: HFix2[H, I, O])(using hfunctor: HFunctor2[H]): F[I, O] =
  def go[A, B]: HFix2[H, A, B] => F[A, B] =
    (fix: HFix2[H, A, B]) =>
      alg[A, B](hfunctor.hfmap[[x, y] =>> HFix2[H, x, y], F, A, B](new FunctionK2[[x, y] =>> HFix2[H, x, y], F] {
        override def apply[I, O](f: HFix2[H, I, O]): F[I, O] = go(f)
      })(fix.unfix))
  go[I, O](hf)