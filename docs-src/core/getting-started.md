# Getting Started

This page walks through a self-contained example: a shopping cart aggregate with a
payment gateway feedback loop.

```scala mdoc:silent
import apparatus.core.*
import apparatus.core.machines.*
import cats.effect.SyncIO
import cats.implicits.*
import zio.blocks.schema.Schema
import java.util.UUID
```

## 1 — Define the domain

Start with plain Scala types for state, commands, and events. The event type needs a `Schema`
instance so Apparatus can serialise it (used by durable materialisers).

```scala mdoc:silent
enum CartState:
  case WaitingForPayment
  case InitiatingPayment
  case PaymentComplete

sealed trait CartCommand:
  val id: UUID
object CartCommand:
  case class PayCart(id: UUID)        extends CartCommand
  case class MarkCartAsPaid(id: UUID) extends CartCommand

enum CartEvent derives Schema:
  case CartPaymentInitiated(id: UUID)
  case CartPaymentCompleted(id: UUID)
```

## 2 — Build a Decider

Use `DeciderBuilder` to assemble the pure decide/evolve pair. `partiallyDecide` returns `Nil`
for any `(state, command)` combination not listed — a safe default for guards.

```scala mdoc:silent
val cartDecider: Decider[CartState, CartCommand, List[CartEvent]] =
  DeciderBuilder
    .seed[CartState]("cart", CartState.WaitingForPayment)
    .decide[CartCommand, List[CartEvent]] { (state, cmd) =>
      (state, cmd) match
        case (CartState.WaitingForPayment,  cmd: CartCommand.PayCart)        => List(CartEvent.CartPaymentInitiated(cmd.id))
        case (CartState.InitiatingPayment,  cmd: CartCommand.MarkCartAsPaid) => List(CartEvent.CartPaymentCompleted(cmd.id))
        case _                                                                => Nil
    }
    .evolveList { (state, evt) =>
      (state, evt) match
        case (CartState.WaitingForPayment, CartEvent.CartPaymentInitiated(_)) => CartState.InitiatingPayment
        case (CartState.InitiatingPayment, CartEvent.CartPaymentCompleted(_)) => CartState.PaymentComplete
        case _                                                                 => state
    }
```

Both `decide` and `evolve` are **pure functions** — you can test them in isolation without any
Apparatus machinery:

```scala mdoc
val initEvents = cartDecider.decide(CartCommand.PayCart(UUID.randomUUID()), CartState.WaitingForPayment)
val nextState  = cartDecider.evolve(initEvents, CartState.WaitingForPayment)
```

## 3 — Lift into an Apparatus node

`Apparatus.aggregateMachine` wraps the decider in a per-UUID routed node. Each aggregate ID
gets its own `cats.effect.Ref`-backed state, allocated lazily on first use.

```scala mdoc:silent
def cartMachine[F[_]: cats.Applicative]: Apparatus[F, CartCommand, List[CartEvent]] =
  Apparatus.aggregateMachine[F, CartCommand, CartEvent](cartDecider, _.id)
```

## 4 — Add a feedback node

A **feedback loop** connects a "policy" machine back into the aggregate. Here the payment
gateway auto-confirms every `CartPaymentInitiated` event by emitting a `MarkCartAsPaid`
command, which is immediately fed back into `cartMachine`.

```scala mdoc:silent
def paymentGateway[F[_]: cats.Applicative]: Apparatus[F, CartEvent, List[CartCommand]] =
  Apparatus.closedMealy(ClosedMealy.stateless[F, CartEvent, List[CartCommand]] {
    case CartEvent.CartPaymentInitiated(id) => List(CartCommand.MarkCartAsPaid(id)).pure[F]
    case CartEvent.CartPaymentCompleted(_)  => Nil.pure[F]
  })

// writeModel: send a PayCart command, the loop completes the payment atomically
def writeModel[F[_]: cats.Applicative]: Apparatus[F, CartCommand, List[CartEvent]] =
  cartMachine[F].feedback(paymentGateway[F])
```

The `feedback` combinator runs to **quiescence**: it keeps feeding outputs of `paymentGateway`
back into `cartMachine` until no more commands are produced. All emitted events are collected
and returned.

## 5 — Run the network

Compile the network with a `DeciderMaterializer` and step through inputs.

```scala mdoc
val mat    = DeciderMaterializer.syncIO
val cartId = UUID.fromString("00000000-0000-0000-0000-000000000001")

// Single PayCart drives the full feedback loop synchronously
val events: List[CartEvent] =
  Apparatus.run(writeModel[SyncIO], CartCommand.PayCart(cartId), mat).unsafeRunSync()
```

`events` contains both `CartPaymentInitiated` and `CartPaymentCompleted` — the full causal
chain completed in one `run` call.

## 6 — Visualise the network

Call `.mermaid` on any `Apparatus` to get a Mermaid diagram:

```scala mdoc:silent
val diagram: String = writeModel[SyncIO].mermaid
```

Paste the result into [mermaid.live](https://mermaid.live) to see the topology.

## Next steps

- [Machines](./machines.md) — understand the three machine types in depth
- [Decider](./decider.md) — builder DSL, event sourcing, materialization
- [Apparatus](./apparatus.md) — full combinator reference and network patterns
