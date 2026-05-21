package apparatus.core.fix

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



// ─── The pattern functor ────────────────────────────────────────────────────

enum ArrowF[F[_, _], I, O]:
  case Arr[F[_, _], I, O](f: I => O) extends ArrowF[F, I, O]
  case Compose[F[_, _], I, M, O](g: F[M, O], h: F[I, M]) extends ArrowF[F, I, O]


// ─── HFunctor2 instance ─────────────────────────────────────────────────────

given HFunctor2[ArrowF] with
  def hfmap[F[_, _], G[_, _], I, O](
                                     nt: FunctionK2[F, G]
                                   )(h: ArrowF[F, I, O]): ArrowF[G, I, O] =
    h match
      case ArrowF.Arr(f)        => ArrowF.Arr(f)
      case ArrowF.Compose(g, h) => ArrowF.Compose(nt.apply(g), nt.apply(h))


// ─── Smart constructors into HFix2 ─────────────────────────────────────────

type Arrow[I, O] = HFix2[ArrowF, I, O]

def arr[I, O](f: I => O): Arrow[I, O] =
  HFix2(ArrowF.Arr(f))

def compose[I, M, O](g: Arrow[M, O], h: Arrow[I, M]): Arrow[I, O] =
  HFix2(ArrowF.Compose(g, h))


// ─── Algebra: evaluate to a plain function ──────────────────────────────────

val evalAlg: HAlgebra2[ArrowF, Function1] =
  [I, O] => (node: ArrowF[Function1, I, O]) => node match
    case ArrowF.Arr(f)        => f
    case ArrowF.Compose(g, h) => g compose h

val prg = compose(arr[Int, Int](_ * 2), arr[Int, Int](_ + 10))

def eval[I, O]: Arrow[I, O] => (I => O) =
  cata2[ArrowF, Function1, I, O](evalAlg)

@main def main = println(eval(prg)(2))