package apparatus.tests

import apparatus.core.*
import apparatus.core.machines.*
import apparatus.examples.*
import cats.effect.SyncIO
import cats.implicits.*

import java.util.UUID

class CartSpec extends munit.FunSuite:

  private val cartId     = UUID.fromString("00000000-0000-0000-0000-000000000001")
  private val shippingId = UUID.fromString("00000000-0000-0000-0000-000000000002")
  private val mat        = DeciderMaterializer.syncIO

  // ── Pure decider tests ────────────────────────────────────────────────────

  test("PayCart from WaitingForPayment emits CartPaymentInitiated"):
    val evts = cartDecider.decide(CartCommand.PayCart(cartId), CartState.WaitingForPayment)
    assertEquals(evts, List(CartEvent.CartPaymentInitiated(cartId)))

  test("MarkCartAsPaid from WaitingForPayment emits nothing"):
    val evts = cartDecider.decide(CartCommand.MarkCartAsPaid(cartId), CartState.WaitingForPayment)
    assertEquals(evts, Nil)

  test("MarkCartAsPaid from InitiatingPayment emits CartPaymentCompleted"):
    val evts = cartDecider.decide(CartCommand.MarkCartAsPaid(cartId), CartState.InitiatingPayment)
    assertEquals(evts, List(CartEvent.CartPaymentCompleted(cartId)))

  test("PayCart from InitiatingPayment emits nothing (already in-flight)"):
    val evts = cartDecider.decide(CartCommand.PayCart(cartId), CartState.InitiatingPayment)
    assertEquals(evts, Nil)

  test("any command from PaymentComplete emits nothing"):
    assertEquals(cartDecider.decide(CartCommand.PayCart(cartId),        CartState.PaymentComplete), Nil)
    assertEquals(cartDecider.decide(CartCommand.MarkCartAsPaid(cartId), CartState.PaymentComplete), Nil)

  test("evolve: CartPaymentInitiated advances to InitiatingPayment"):
    val next = CartState.WaitingForPayment.evolve(CartEvent.CartPaymentInitiated(cartId))
    assertEquals(next, CartState.InitiatingPayment)

  test("evolve: CartPaymentCompleted advances to PaymentComplete"):
    val next = CartState.InitiatingPayment.evolve(CartEvent.CartPaymentCompleted(cartId))
    assertEquals(next, CartState.PaymentComplete)

  // ── Shipping decider tests ────────────────────────────────────────────────

  test("StartShipping from Idle emits ShippingStarted"):
    val evts = shippingDecider.decide(ShippingCommand.StartShipping(shippingId), ShippingState.Idle)
    assertEquals(evts, List(ShippingEvent.ShippingStarted(shippingId)))

  test("StartShipping from Shipping emits nothing (already shipping)"):
    val evts = shippingDecider.decide(ShippingCommand.StartShipping(shippingId), ShippingState.Shipping)
    assertEquals(evts, Nil)

  test("evolve: ShippingStarted advances Idle to Shipping"):
    val next = ShippingState.Idle.evolve(ShippingEvent.ShippingStarted(shippingId))
    assertEquals(next, ShippingState.Shipping)

  // ── Apparatus network tests ───────────────────────────────────────────────

  def runSteps[O](fsm: Apparatus[SyncIO, CartCommand, O], cmds: CartCommand*): List[O] =
    Apparatus.runSteps(fsm, cmds.toList, mat).unsafeRunSync()

  def runStepsEither[A, B](
    fsm:  Apparatus[SyncIO, Either[CartCommand, ShippingCommand], B],
    cmds: Either[CartCommand, ShippingCommand]*
  ): List[B] =
    Apparatus.runSteps(fsm, cmds.toList, mat).unsafeRunSync()

  test("writeModel: PayCart triggers payment gateway and emits both events"):
    // feedback loop: PayCart → CartPaymentInitiated → gateway → MarkCartAsPaid → CartPaymentCompleted
    val result = runSteps(writeModel[SyncIO], CartCommand.PayCart(cartId))
    assertEquals(result.flatten, List(CartEvent.CartPaymentInitiated(cartId), CartEvent.CartPaymentCompleted(cartId)))

  test("writeModel: duplicate PayCart after completion emits nothing"):
    val results = runSteps(writeModel[SyncIO],
      CartCommand.PayCart(cartId),
      CartCommand.PayCart(cartId) // cart is in PaymentComplete state, ignored
    )
    // second PayCart: state is PaymentComplete, decide returns Nil
    assertEquals(results.last, Nil)

  test("cartApplication: PayCart produces Initiated then Completed views"):
    val result = runSteps(cartApplication[SyncIO], CartCommand.PayCart(cartId))
    assertEquals(result.flatten, List(CartView.Initiated, CartView.Completed))

  test("cartApplication: two PayCart commands — second produces no views"):
    val results = runSteps(cartApplication[SyncIO],
      CartCommand.PayCart(cartId),
      CartCommand.PayCart(cartId)
    )
    assertEquals(results.head.toSet, Set(CartView.Initiated, CartView.Completed))
    assertEquals(results.last, Nil)

  // ── Full cart + shipping pipeline tests ───────────────────────────────────

  test("cartAndShipping: PayCart triggers shipping on payment complete"):
    val results = runStepsEither(cartAndShipping[SyncIO](shippingId),
      Left(CartCommand.PayCart(cartId))
    )
    val views = results.flatten
    assert(views.contains(Left(CartView.Initiated)),  "should contain Initiated")
    assert(views.contains(Left(CartView.Completed)),  "should contain Completed")
    assert(views.contains(Right(ShippingInfo.Started)), "should start shipping on payment")

  test("cartAndShipping: StartShipping directly produces Started info"):
    val results = runStepsEither(cartAndShipping[SyncIO](shippingId),
      Right(ShippingCommand.StartShipping(shippingId))
    )
    assertEquals(results.flatten, List(Right(ShippingInfo.Started)))

  test("cartAndShipping: second StartShipping is idempotent (already shipping)"):
    val results = runStepsEither(cartAndShipping[SyncIO](shippingId),
      Right(ShippingCommand.StartShipping(shippingId)),
      Right(ShippingCommand.StartShipping(shippingId))
    )
    assertEquals(results.last, Nil)

  test("writeModelWithShipping: PayCart emits cart + shipping events"):
    val results = runStepsEither(writeModelWithShipping[SyncIO](shippingId),
      Left(CartCommand.PayCart(cartId))
    )
    val evts = results.flatten
    assert(evts.contains(Left(CartEvent.CartPaymentInitiated(cartId))))
    assert(evts.contains(Left(CartEvent.CartPaymentCompleted(cartId))))
    assert(evts.contains(Right(ShippingEvent.ShippingStarted(shippingId))))
