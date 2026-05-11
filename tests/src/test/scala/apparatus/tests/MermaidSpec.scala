package apparatus.tests

import apparatus.core.*
import apparatus.examples.*
import cats.Id
import cats.implicits.*

import scala.collection.immutable.SortedSet

class MermaidSpec extends munit.FunSuite:

  // ── Determinism ───────────────────────────────────────────────────────────────
  // The renderer traverses an immutable Apparatus tree with a monotonic counter.
  // Same tree → same traversal order → same IDs → identical output.

  // The booking orchestrator rehydrated to: Running(Car, todo=Set(Flight), compensation=Set(Hotel))
  val defaultBookingAtCarEvents: List[SagaEvent[BookingStep]] = List(
    SagaEvent.Booted(BookingStep.Hotel, SortedSet(BookingStep.Car, BookingStep.Flight)),
    SagaEvent.StepProgressed(BookingStep.Hotel, SagaStepResult.Completed),
    SagaEvent.StepStarted(BookingStep.Car)
  )
  val bookingAtCar = behavior.decider.evolveFrom(defaultBookingAtCarEvents)

  test("saga: output is deterministic"):
    assertEquals(Mermaid.print(saga[Id]()), Mermaid.print(saga[Id]()))

  test("sagaRerootedAtCar: output is deterministic"):
    assertEquals(
      Mermaid.print(sagaRerootedAtCar[Id](booking = bookingAtCar)),
      Mermaid.print(sagaRerootedAtCar[Id](booking = bookingAtCar))
    )

  // ── saga() snapshot ───────────────────────────────────────────────────────────
  // Note: Mermaid.openSubgraph appends a trailing space after the closing quote,
  // so each subgraph line ends with `" ` (quote + space).

  private val sg = " " // trailing space from openSubgraph — makes the intent explicit

  private val expectedSaga: String =
    s"""graph TD
       |    node_1["booking"]
       |    fan_2(["fan-out"])
       |    combine_3(["combine"])
       |    subgraph "Flight service"$sg
       |    filter_4{"Flight event router"}
       |    node_5["flight"]
       |    node_6["Machine"]
       |    end
       |    fan_7(["fan-out"])
       |    combine_8(["combine"])
       |    subgraph "Car service"$sg
       |    filter_9{"Car event router"}
       |    node_10["car"]
       |    node_11["Machine"]
       |    end
       |    subgraph "Hotel service"$sg
       |    filter_12{"Hotel event router"}
       |    node_13["hotel"]
       |    node_14["Machine"]
       |    end
       |    filter_4 -->|defined| node_5
       |    node_5 --> node_6
       |    filter_9 -->|defined| node_10
       |    node_10 --> node_11
       |    filter_12 -->|defined| node_13
       |    node_13 --> node_14
       |    fan_7 --> filter_9
       |    fan_7 --> filter_12
       |    node_11 --> combine_8
       |    node_14 --> combine_8
       |    fan_2 --> filter_4
       |    fan_2 --> fan_7
       |    node_6 --> combine_3
       |    combine_8 --> combine_3
       |    node_1 -->|B| fan_2
       |    combine_3 -->|A ↺| node_1""".stripMargin

  test("saga: full snapshot"):
    assertEquals(Mermaid.print(saga[Id]()), expectedSaga)

  // ── sagaRerootedAtCar() snapshot ──────────────────────────────────────────────
  // Entry point is CarCommand, not BookingCommand.
  // carCore is Stable("car") so node_1 shows its id "car" rather than "Machine".
  // node_2 is the stateless rmap machine chained after carCore via >>>.
  // node_3 is the bookingDecider (Stable("booking")) → shows as "booking".
  // All three services appear in the feedback reactor:
  //   - Flight and Hotel as forward services
  //   - Car (Stable("car"), shares state with carCore) to handle compensation

  private val expectedRerooted: String =
    s"""graph TD
       |    node_1["car"]
       |    node_2["Machine"]
       |    node_3["booking"]
       |    fan_4(["fan-out"])
       |    combine_5(["combine"])
       |    subgraph "Flight service"$sg
       |    filter_6{"Flight event router"}
       |    node_7["flight"]
       |    node_8["Machine"]
       |    end
       |    fan_9(["fan-out"])
       |    combine_10(["combine"])
       |    subgraph "Car service"$sg
       |    filter_11{"Car event router"}
       |    node_12["car"]
       |    node_13["Machine"]
       |    end
       |    subgraph "Hotel service"$sg
       |    filter_14{"Hotel event router"}
       |    node_15["hotel"]
       |    node_16["Machine"]
       |    end
       |    node_1 --> node_2
       |    filter_6 -->|defined| node_7
       |    node_7 --> node_8
       |    filter_11 -->|defined| node_12
       |    node_12 --> node_13
       |    filter_14 -->|defined| node_15
       |    node_15 --> node_16
       |    fan_9 --> filter_11
       |    fan_9 --> filter_14
       |    node_13 --> combine_10
       |    node_16 --> combine_10
       |    fan_4 --> filter_6
       |    fan_4 --> fan_9
       |    node_8 --> combine_5
       |    combine_10 --> combine_5
       |    node_3 -->|N[B]| fan_4
       |    combine_5 -->|N[A] ↺| node_3
       |    node_2 --> node_3""".stripMargin

  test("sagaRerootedAtCar: full snapshot"):
    assertEquals(Mermaid.print(sagaRerootedAtCar[Id](booking = bookingAtCar)), expectedRerooted)

  // ── Structural spot-checks ────────────────────────────────────────────────────

  test("saga: contains all three service subgraphs"):
    val d = Mermaid.print(saga[Id]())
    assert(d.contains("""subgraph "Flight service""""))
    assert(d.contains("""subgraph "Car service""""))
    assert(d.contains("""subgraph "Hotel service""""))

  test("saga: Booking Saga orchestrator node labeled"):
    assert(Mermaid.print(saga[Id]()).contains("""node_1["booking"]"""))

  test("saga: feedback back-edge present"):
    assert(Mermaid.print(saga[Id]()).contains("↺"))

  test("sagaRerootedAtCar: all three service subgraphs present in feedback reactor"):
    // Car is included via Stable("car") shared with carCore to handle compensation
    val d = Mermaid.print(sagaRerootedAtCar[Id](booking = bookingAtCar))
    assert(d.contains("""subgraph "Flight service""""))
    assert(d.contains("""subgraph "Car service""""))
    assert(d.contains("""subgraph "Hotel service""""))

  test("sagaRerootedAtCar: FeedbackMany back-edge uses N[A] notation"):
    assert(Mermaid.print(sagaRerootedAtCar[Id](booking = bookingAtCar)).contains("N[A] ↺"))
