package apparatus.tests

import apparatus.core.*
import apparatus.examples.*
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
    assertEquals(Mermaid.print(saga()), Mermaid.print(saga()))

  test("sagaRerootedAtCar: output is deterministic"):
    assertEquals(
      Mermaid.print(sagaRerootedAtCar(booking = bookingAtCar)),
      Mermaid.print(sagaRerootedAtCar(booking = bookingAtCar))
    )

  // ── saga() snapshot ───────────────────────────────────────────────────────────
  // Note: Mermaid.openSubgraph appends a trailing space after the closing quote,
  // so each subgraph line ends with `" ` (quote + space).

  private val sg = " " // trailing space from openSubgraph — makes the intent explicit

  private val expectedSaga: String =
    s"""graph TD
       |    node_1["Booking Saga"]
       |    fan_2(["fan-out"])
       |    combine_3(["combine"])
       |    fan_4(["fan-out"])
       |    combine_5(["combine"])
       |    subgraph "Flight Service"$sg
       |    filter_6{"Flight Event Router"}
       |    node_7["Flight Decider"]
       |    node_8["Machine"]
       |    end
       |    subgraph "Car Service"$sg
       |    filter_9{"Car Event Router"}
       |    node_10["Car Decider"]
       |    node_11["Machine"]
       |    end
       |    subgraph "Hotel Service"$sg
       |    filter_12{"Hotel Event Router"}
       |    node_13["Hotel Decider"]
       |    node_14["Machine"]
       |    end
       |    filter_6 -->|defined| node_7
       |    node_7 --> node_8
       |    filter_9 -->|defined| node_10
       |    node_10 --> node_11
       |    fan_4 --> filter_6
       |    fan_4 --> filter_9
       |    node_8 --> combine_5
       |    node_11 --> combine_5
       |    filter_12 -->|defined| node_13
       |    node_13 --> node_14
       |    fan_2 --> fan_4
       |    fan_2 --> filter_12
       |    combine_5 --> combine_3
       |    node_14 --> combine_3
       |    node_1 -->|B| fan_2
       |    combine_3 -->|A ↺| node_1""".stripMargin

  test("saga: full snapshot"):
    assertEquals(Mermaid.print(saga()), expectedSaga)

  // ── sagaRerootedAtCar() snapshot ──────────────────────────────────────────────
  // Entry point is CarCommand, not BookingCommand.
  // bookingDecider is an unlabeled Basic (rehydrated from events) → shows as "Machine".
  // All three services appear in the feedback reactor:
  //   - Flight and Hotel as forward services
  //   - Car (via carFeedback pre-seeded at Reserved) to handle compensation after car succeeds

  private val expectedRerooted: String =
    s"""graph TD
       |    node_1["Machine"]
       |    node_2["Machine"]
       |    fan_3(["fan-out"])
       |    combine_4(["combine"])
       |    fan_5(["fan-out"])
       |    combine_6(["combine"])
       |    subgraph "Flight Service"$sg
       |    filter_7{"Flight Event Router"}
       |    node_8["Flight Decider"]
       |    node_9["Machine"]
       |    end
       |    subgraph "Car Service"$sg
       |    filter_10{"Car Event Router"}
       |    node_11["Car Decider"]
       |    node_12["Machine"]
       |    end
       |    subgraph "Hotel Service"$sg
       |    filter_13{"Hotel Event Router"}
       |    node_14["Hotel Decider"]
       |    node_15["Machine"]
       |    end
       |    filter_7 -->|defined| node_8
       |    node_8 --> node_9
       |    filter_10 -->|defined| node_11
       |    node_11 --> node_12
       |    fan_5 --> filter_7
       |    fan_5 --> filter_10
       |    node_9 --> combine_6
       |    node_12 --> combine_6
       |    filter_13 -->|defined| node_14
       |    node_14 --> node_15
       |    fan_3 --> fan_5
       |    fan_3 --> filter_13
       |    combine_6 --> combine_4
       |    node_15 --> combine_4
       |    node_2 -->|N[B]| fan_3
       |    combine_4 -->|N[A] ↺| node_2
       |    node_1 --> node_2""".stripMargin

  test("sagaRerootedAtCar: full snapshot"):
    assertEquals(Mermaid.print(sagaRerootedAtCar(booking = bookingAtCar)), expectedRerooted)

  // ── Structural spot-checks ────────────────────────────────────────────────────

  test("saga: contains all three service subgraphs"):
    val d = Mermaid.print(saga())
    assert(d.contains("""subgraph "Flight Service""""))
    assert(d.contains("""subgraph "Car Service""""))
    assert(d.contains("""subgraph "Hotel Service""""))

  test("saga: Booking Saga orchestrator node labeled"):
    assert(Mermaid.print(saga()).contains("""node_1["Booking Saga"]"""))

  test("saga: feedback back-edge present"):
    assert(Mermaid.print(saga()).contains("↺"))

  test("sagaRerootedAtCar: all three service subgraphs present in feedback reactor"):
    // Car is included via carFeedback (pre-seeded at Reserved) to handle compensation
    val d = Mermaid.print(sagaRerootedAtCar(booking = bookingAtCar))
    assert(d.contains("""subgraph "Flight Service""""))
    assert(d.contains("""subgraph "Car Service""""))
    assert(d.contains("""subgraph "Hotel Service""""))

  test("sagaRerootedAtCar: FeedbackMany back-edge uses N[A] notation"):
    assert(Mermaid.print(sagaRerootedAtCar(booking = bookingAtCar)).contains("N[A] ↺"))
