package apparatus.tests

import apparatus.examples.*
import munit.FunSuite

class FlightSpec extends FunSuite:

  private val flightNumber = "BA123"

  test("decide: InitSearch from Seed emits SearchStarted"):
    val cmd = FlightCommand.InitSearch(BookingDomain.flightId, BookingDomain.flightQuery, BookingDomain.bookingId, BookingFlow.Civilian)
    val evts = flightDecider.decide(cmd, FlightState.Seed)
    assertEquals(evts, List(FlightEvent.SearchStarted(BookingDomain.flightId, BookingDomain.bookingId, BookingDomain.flightQuery, BookingFlow.Civilian)))

  test("decide: SelectFlight from Searching emits Reserved"):
    val state = FlightState.Searching(BookingDomain.bookingId, BookingFlow.Civilian)
    val evts = flightDecider.decide(FlightCommand.SelectFlight(BookingDomain.flightId, flightNumber), state)
    assertEquals(evts, List(FlightEvent.Reserved(BookingDomain.flightId, BookingDomain.bookingId)))

  test("decide: RequestClearance from Searching emits ClearanceRequired"):
    val state = FlightState.Searching(BookingDomain.bookingId, BookingFlow.Diplomat)
    val evts = flightDecider.decide(FlightCommand.RequestClearance(BookingDomain.flightId, flightNumber), state)
    assertEquals(evts, List(FlightEvent.ClearanceRequired(BookingDomain.flightId, BookingDomain.bookingId, flightNumber)))

  test("decide: NoFlightFound from Searching emits Failed"):
    val state = FlightState.Searching(BookingDomain.bookingId, BookingFlow.Civilian)
    val evts = flightDecider.decide(FlightCommand.NoFlightFound(BookingDomain.flightId), state)
    assertEquals(evts, List(FlightEvent.Failed(BookingDomain.flightId, BookingDomain.bookingId)))

  test("decide: unknown command from Cancelled emits nothing"):
    val cmd = FlightCommand.InitSearch(BookingDomain.flightId, BookingDomain.flightQuery, BookingDomain.bookingId, BookingFlow.Civilian)
    assertEquals(flightDecider.decide(cmd, FlightState.Cancelled), Nil)

  test("evolve: SearchStarted advances Seed to Searching"):
    val event = FlightEvent.SearchStarted(BookingDomain.flightId, BookingDomain.bookingId, BookingDomain.flightQuery, BookingFlow.Diplomat)
    assertEquals(FlightState.Seed.evolve(event), FlightState.Searching(BookingDomain.bookingId, BookingFlow.Diplomat))

  test("evolve: ClearanceRequired advances Searching to AwaitingClearance"):
    val event = FlightEvent.ClearanceRequired(BookingDomain.flightId, BookingDomain.bookingId, flightNumber)
    val state = FlightState.Searching(BookingDomain.bookingId, BookingFlow.Diplomat)
    assertEquals(state.evolve(event), FlightState.AwaitingClearance(BookingDomain.bookingId, flightNumber))

  test("evolve: Failed from Searching returns to Seed"):
    val state = FlightState.Searching(BookingDomain.bookingId, BookingFlow.Civilian)
    assertEquals(state.evolve(FlightEvent.Failed(BookingDomain.flightId, BookingDomain.bookingId)), FlightState.Seed)

  test("civilian flow: search and reserve in one go"):
    val cmds = List(
      FlightCommand.InitSearch(BookingDomain.flightId, BookingDomain.flightQuery, BookingDomain.bookingId, BookingFlow.Civilian),
      FlightCommand.SelectFlight(BookingDomain.flightId, flightNumber)
    )
    val (events, state) = DeciderTestSupport.replay(flightDecider, FlightState.Seed, cmds)
    assertEquals(
      events,
      List(
        FlightEvent.SearchStarted(BookingDomain.flightId, BookingDomain.bookingId, BookingDomain.flightQuery, BookingFlow.Civilian),
        FlightEvent.Reserved(BookingDomain.flightId, BookingDomain.bookingId)
      )
    )
    assertEquals(state, FlightState.Reserved(BookingDomain.bookingId))

  test("diplomat flow: clearance check then reserve in one go"):
    val cmds = List(
      FlightCommand.InitSearch(BookingDomain.flightId, BookingDomain.flightQuery, BookingDomain.bookingId, BookingFlow.Diplomat),
      FlightCommand.RequestClearance(BookingDomain.flightId, flightNumber),
      FlightCommand.VerifyClearance(BookingDomain.flightId)
    )
    val (events, state) = DeciderTestSupport.replay(flightDecider, FlightState.Seed, cmds)
    assertEquals(
      events,
      List(
        FlightEvent.SearchStarted(BookingDomain.flightId, BookingDomain.bookingId, BookingDomain.flightQuery, BookingFlow.Diplomat),
        FlightEvent.ClearanceRequired(BookingDomain.flightId, BookingDomain.bookingId, flightNumber),
        FlightEvent.Reserved(BookingDomain.flightId, BookingDomain.bookingId)
      )
    )
    assertEquals(state, FlightState.Reserved(BookingDomain.bookingId))

  test("diplomat flow: rejected clearance returns to Seed"):
    val cmds = List(
      FlightCommand.InitSearch(BookingDomain.flightId, BookingDomain.flightQuery, BookingDomain.bookingId, BookingFlow.Diplomat),
      FlightCommand.RequestClearance(BookingDomain.flightId, flightNumber),
      FlightCommand.RejectClearance(BookingDomain.flightId)
    )
    val (events, state) = DeciderTestSupport.replay(flightDecider, FlightState.Seed, cmds)
    assert(events.contains(FlightEvent.Failed(BookingDomain.flightId, BookingDomain.bookingId)))
    assertEquals(state, FlightState.Seed)
