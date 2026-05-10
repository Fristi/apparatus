# Mermaid

`Mermaid` renders any `Apparatus` network as a [Mermaid](https://mermaid.js.org)
flowchart. Only the structural topology is shown — internal state and transition
functions are erased at runtime — but labels make a diagram easier to digest how the network is setup.

```scala mdoc:silent
import apparatus.core.*
import apparatus.examples.*
import cats.Id
import cats.implicits.*
```

## Basic usage

```scala
val diagram: String = Mermaid.print(myMachine)
```

Paste the result into [mermaid.live](https://mermaid.live) or drop it into a Markdown renderer
that supports Mermaid fenced code blocks.

## Attaching labels

Call `.label("…")` on any sub-machine before passing the network to `print`.
The booking saga example already bakes labels into every service machine and the orchestrator,
so `saga()` produces a fully annotated network:

```scala mdoc:silent
val diagram = Mermaid.print(saga())
```

Label semantics depend on what the node is:

| Expression | Effect in diagram |
|---|---|
| `basic.label("X")` | Renames the rectangle to `X` |
| `lmapOrEmpty{…}.label("X")` | Renames the filter diamond to `X` |
| `composite.label("X")` | Wraps the entire sub-graph in `subgraph "X"` |

## Shape legend

| Mermaid shape | Apparatus node |
|---|---|
| `["name"]` rectangle | `Apparatus.Basic` — a single machine |
| `{"name"}` diamond | `Apparatus.Alternative` (routing) or `Apparatus.LmapOrEmpty` (filter) |
| `(["name"])` stadium | Structural `split`, `join`, `fan-out`, `combine` connectors |

## Example output

`Mermaid.print(saga())` for the three-service booking saga:

```mermaid
graph TD
    node_1["Booking Saga"]
    fan_2(["fan-out"])
    combine_3(["combine"])
    fan_4(["fan-out"])
    combine_5(["combine"])
    subgraph "Flight Service"
      filter_6{"Flight Event Router"}
      node_7["Flight Decider"]
      node_8["Machine"]
    end
    subgraph "Car Service"
      filter_9{"Car Event Router"}
      node_10["Car Decider"]
      node_11["Machine"]
    end
    subgraph "Hotel Service"
      filter_12{"Hotel Event Router"}
      node_13["Hotel Decider"]
      node_14["Machine"]
    end
    filter_6 -->|defined| node_7
    node_7 --> node_8
    filter_9 -->|defined| node_10
    node_10 --> node_11
    fan_4 --> filter_6
    fan_4 --> filter_9
    node_8 --> combine_5
    node_11 --> combine_5
    filter_12 -->|defined| node_13
    node_13 --> node_14
    fan_2 --> fan_4
    fan_2 --> filter_12
    combine_5 --> combine_3
    node_14 --> combine_3
    node_1 -->|B| fan_2
    combine_3 -->|A ↺| node_1
```

## Feedback back-edges

`Apparatus.Feedback` (`<->`) and `Apparatus.FeedbackMany` produce back-edges labelled `A ↺`
and `N[A] ↺` respectively. Mermaid renders these as curved arrows that close the loop visually.

## Labeling intermediate nodes

The most useful pattern is to label every `Basic` that wraps a domain decider and every
`lmapOrEmpty` filter:

```scala
def labeledFlightService(flight: Decider[FlightState, FlightCommand, List[FlightEvent]]) = {
  val flightDecider = Apparatus.Basic(flight.toBaseMachine[Id]).label("Flight Decider")
  val flightService = flightDecider
    .lmapOrEmpty[SagaEvent[BookingStep]] {
      case SagaEvent.StepStarted(BookingStep.Flight) => FlightCommand.Reserve
    }
    .label("Flight Event Router")
    .rmap(_.collect {
      case FlightEvent.Reserved => BookingCommand.MarkFlightComplete
    })

  flightService.label("Flight Service")
}
```
