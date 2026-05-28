package apparatus.examples

import apparatus.core.*
import apparatus.core.machines.*
import cats.Applicative
import cats.implicits.*
import zio.blocks.schema.Schema

import java.util.UUID

// ── Domain ────────────────────────────────────────────────────────────────────

enum CartState:
  case WaitingForPayment
  case InitiatingPayment
  case PaymentComplete

  def decide(cmd: CartCommand): List[CartEvent] = this match
    case CartState.WaitingForPayment =>
      cmd match
        case CartCommand.PayCart        => List(CartEvent.CartPaymentInitiated)
        case CartCommand.MarkCartAsPaid => Nil
    case CartState.InitiatingPayment =>
      cmd match
        case CartCommand.MarkCartAsPaid => List(CartEvent.CartPaymentCompleted)
        case CartCommand.PayCart        => Nil
    case CartState.PaymentComplete => Nil

  def evolve(ev: CartEvent): CartState = this match
    case CartState.WaitingForPayment =>
      ev match
        case CartEvent.CartPaymentInitiated => CartState.InitiatingPayment
        case _                              => this
    case CartState.InitiatingPayment =>
      ev match
        case CartEvent.CartPaymentCompleted => CartState.PaymentComplete
        case _                              => this
    case CartState.PaymentComplete => this

enum CartCommand:
  case PayCart
  case MarkCartAsPaid

enum CartEvent derives Schema:
  case CartPaymentInitiated
  case CartPaymentCompleted

enum CartView:
  case Initiated
  case Completed

// ── Cart aggregate ────────────────────────────────────────────────────────────

val cartDecider: Decider[CartState, CartCommand, List[CartEvent]] =
  DeciderBuilder
    .seed[CartState](CartState.WaitingForPayment)
    .decide[CartCommand, List[CartEvent]](_.decide(_))
    .evolveList(_.evolve(_))

def cartMachine[F[_]: Applicative](cartId: UUID): Apparatus[F, CartCommand, List[CartEvent]] =
  Apparatus.aggregateMachine[F, CartCommand, CartEvent]("cart", cartDecider, _ => cartId)

// ── Policy: payment gateway ───────────────────────────────────────────────────
// In this world payments always succeed: CartPaymentInitiated triggers MarkCartAsPaid.

def paymentGateway[F[_]: Applicative]: Apparatus[F, CartEvent, List[CartCommand]] =
  Apparatus.closedMealy(ClosedMealy.stateless[F, CartEvent, List[CartCommand]] {
    case CartEvent.CartPaymentInitiated => List(CartCommand.MarkCartAsPaid).pure[F]
    case CartEvent.CartPaymentCompleted => Nil.pure[F]
  })

// ── Projection: payment status ────────────────────────────────────────────────

def paymentStatus[F[_]: Applicative]: Apparatus[F, CartEvent, List[CartView]] =
  Apparatus.closedMealy(ClosedMealy.stateless[F, CartEvent, List[CartView]] {
    case CartEvent.CartPaymentInitiated => List(CartView.Initiated).pure[F]
    case CartEvent.CartPaymentCompleted => List(CartView.Completed).pure[F]
  })

// ── Write model: cart + payment gateway feedback loop ─────────────────────────
// Haskell: writeModel = Feedback cart paymentGateway

def writeModel[F[_]: Applicative](cartId: UUID): Apparatus[F, CartCommand, List[CartEvent]] =
  cartMachine[F](cartId).feedback(paymentGateway[F])

// ── Application: write model piped through read projection ────────────────────
// Haskell: application = Kleisli writeModel paymentStatus
// Since paymentStatus is stateless, Kleisli reduces to rmap + flatMap.

def cartApplication[F[_]: Applicative](cartId: UUID): Apparatus[F, CartCommand, List[CartView]] =
  writeModel[F](cartId).rmap(_.flatMap {
    case CartEvent.CartPaymentInitiated => List(CartView.Initiated)
    case CartEvent.CartPaymentCompleted => List(CartView.Completed)
  })

// ── Shipping domain ───────────────────────────────────────────────────────────
// Haskell: shippingBasic and shippingInfo were left undefined — implemented here.

enum ShippingState:
  case Idle
  case Shipping

  def decide(cmd: ShippingCommand): List[ShippingEvent] = this match
    case ShippingState.Idle     => List(ShippingEvent.ShippingStarted)
    case ShippingState.Shipping => Nil // already shipping, ignore

  def evolve(ev: ShippingEvent): ShippingState = this match
    case ShippingState.Idle => ShippingState.Shipping
    case s                  => s

enum ShippingCommand:
  case StartShipping

enum ShippingEvent derives Schema:
  case ShippingStarted

enum ShippingInfo:
  case Started

val shippingDecider: Decider[ShippingState, ShippingCommand, List[ShippingEvent]] =
  DeciderBuilder
    .seed[ShippingState](ShippingState.Idle)
    .decide[ShippingCommand, List[ShippingEvent]](_.decide(_))
    .evolveList(_.evolve(_))

def shippingMachine[F[_]: Applicative](shippingId: UUID): Apparatus[F, ShippingCommand, List[ShippingEvent]] =
  Apparatus.aggregateMachine[F, ShippingCommand, ShippingEvent]("shipping", shippingDecider, _ => shippingId)

// Haskell: shippingInfo = undefined
def shippingInfo[F[_]: Applicative]: Apparatus[F, ShippingEvent, List[ShippingInfo]] =
  Apparatus.closedMealy(ClosedMealy.stateless[F, ShippingEvent, List[ShippingInfo]] {
    case ShippingEvent.ShippingStarted => List(ShippingInfo.Started).pure[F]
  })

// ── Combined cart + shipping ──────────────────────────────────────────────────
// Haskell: aggregateWithShipping = rmap (fmap Left ||| fmap Right) $ cart +++ shipping

def aggregateWithShipping[F[_]: Applicative](
  cartId:     UUID,
  shippingId: UUID
): Apparatus[F, Either[CartCommand, ShippingCommand], List[Either[CartEvent, ShippingEvent]]] =
  (cartMachine[F](cartId) ||| shippingMachine[F](shippingId))
    .rmap {
      case Left(evts)  => evts.map(Left(_))
      case Right(evts) => evts.map(Right(_))
    }

// Policy: on CartPaymentCompleted, kick off shipping.
// Haskell: paymentCompletePolicy = stateless \case
//   CartPaymentInitiated -> []
//   CartPaymentCompleted -> [StartShipping]
def paymentCompletePolicy[F[_]: Applicative]: Apparatus[F, CartEvent, List[ShippingCommand]] =
  Apparatus.closedMealy(ClosedMealy.stateless[F, CartEvent, List[ShippingCommand]] {
    case CartEvent.CartPaymentInitiated => Nil.pure[F]
    case CartEvent.CartPaymentCompleted => List(ShippingCommand.StartShipping).pure[F]
  })

// Feedback right-hand side: Either[CartEvent, ShippingEvent] → List[Either[CartCommand, ShippingCommand]]
//
// Haskell has two separate feedback policies:
//   - paymentGateway: CartPaymentInitiated → MarkCartAsPaid  (payment always succeeds)
//   - paymentCompletePolicy: CartPaymentCompleted → StartShipping
//
// Combined here so a single PayCart drives the full flow:
//   PayCart → CartPaymentInitiated → MarkCartAsPaid → CartPaymentCompleted → StartShipping
private def combinedFeedback[F[_]: Applicative]: Apparatus[F, Either[CartEvent, ShippingEvent], List[Either[CartCommand, ShippingCommand]]] =
  (Apparatus.closedMealy(ClosedMealy.stateless[F, CartEvent, List[Either[CartCommand, ShippingCommand]]] {
    case CartEvent.CartPaymentInitiated => List(Left(CartCommand.MarkCartAsPaid)).pure[F]
    case CartEvent.CartPaymentCompleted => List(Right(ShippingCommand.StartShipping)).pure[F]
  }) ||| Apparatus.closedMealy(
    ClosedMealy.stateless[F, ShippingEvent, List[Either[CartCommand, ShippingCommand]]](_ => Nil.pure[F])
  )).rmap {
    case Left(cmds)  => cmds
    case Right(cmds) => cmds
  }

// Haskell: writeModelWithShipping = Feedback aggregateWithShipping (...)
def writeModelWithShipping[F[_]: Applicative](
  cartId:     UUID,
  shippingId: UUID
): Apparatus[F, Either[CartCommand, ShippingCommand], List[Either[CartEvent, ShippingEvent]]] =
  aggregateWithShipping[F](cartId, shippingId).feedback(combinedFeedback[F])

// Read model: fan out events to their respective projections.
// Haskell: readModel = rmap (fmap Left ||| fmap Right) $ paymentStatus +++ shippingInfo
def cartShippingReadModel[F[_]: Applicative]: Apparatus[F, Either[CartEvent, ShippingEvent], List[Either[CartView, ShippingInfo]]] =
  (paymentStatus[F] ||| shippingInfo[F])
    .rmap {
      case Left(views) => views.map(Left(_))
      case Right(info) => info.map(Right(_))
    }

// Full pipeline: write model piped through read model (Kleisli composition).
// Haskell: cartAndShipping = Kleisli writeModelWithShipping readModel
def cartAndShipping[F[_]: Applicative](
  cartId:     UUID,
  shippingId: UUID
): Apparatus[F, Either[CartCommand, ShippingCommand], List[Either[CartView, ShippingInfo]]] =
  writeModelWithShipping[F](cartId, shippingId).rmap(_.flatMap {
    case Left(CartEvent.CartPaymentInitiated)  => List(Left(CartView.Initiated))
    case Left(CartEvent.CartPaymentCompleted)  => List(Left(CartView.Completed))
    case Right(ShippingEvent.ShippingStarted)  => List(Right(ShippingInfo.Started))
  })
