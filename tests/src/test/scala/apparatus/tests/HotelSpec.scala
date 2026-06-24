package apparatus.tests

import apparatus.examples.*
import munit.FunSuite

class HotelSpec extends FunSuite:

  private val hotelName = "Grand Hotel"

  test("decide: InitSearch from Seed emits SearchStarted"):
    val cmd = HotelCommand.InitSearch(BookingDomain.hotelId, BookingDomain.hotelQuery, BookingDomain.bookingId, BookingFlow.Civilian)
    val evts = hotelDecider.decide(cmd, HotelState.Seed)
    assertEquals(evts, List(HotelEvent.SearchStarted(BookingDomain.hotelId, BookingDomain.bookingId, BookingDomain.hotelQuery, BookingFlow.Civilian)))

  test("decide: SelectHotel from Searching emits Reserved"):
    val state = HotelState.Searching(BookingDomain.bookingId, BookingFlow.Civilian)
    val evts = hotelDecider.decide(HotelCommand.SelectHotel(BookingDomain.hotelId, hotelName), state)
    assertEquals(evts, List(HotelEvent.Reserved(BookingDomain.hotelId, BookingDomain.bookingId)))

  test("decide: RequestBackgroundCheck from Searching emits BackgroundCheckRequired"):
    val state = HotelState.Searching(BookingDomain.bookingId, BookingFlow.Diplomat)
    val evts = hotelDecider.decide(HotelCommand.RequestBackgroundCheck(BookingDomain.hotelId, hotelName), state)
    assertEquals(evts, List(HotelEvent.BackgroundCheckRequired(BookingDomain.hotelId, BookingDomain.bookingId, hotelName)))

  test("decide: Cancel from Reserved emits Compensated"):
    val state = HotelState.Reserved(BookingDomain.bookingId)
    val evts = hotelDecider.decide(HotelCommand.Cancel(BookingDomain.hotelId), state)
    assertEquals(evts, List(HotelEvent.Compensated(BookingDomain.hotelId, BookingDomain.bookingId)))

  test("decide: unknown command from Cancelled emits nothing"):
    val cmd = HotelCommand.InitSearch(BookingDomain.hotelId, BookingDomain.hotelQuery, BookingDomain.bookingId, BookingFlow.Civilian)
    assertEquals(hotelDecider.decide(cmd, HotelState.Cancelled), Nil)

  test("evolve: SearchStarted advances Seed to Searching"):
    val event = HotelEvent.SearchStarted(BookingDomain.hotelId, BookingDomain.bookingId, BookingDomain.hotelQuery, BookingFlow.Diplomat)
    assertEquals(HotelState.Seed.evolve(event), HotelState.Searching(BookingDomain.bookingId, BookingFlow.Diplomat))

  test("evolve: BackgroundCheckRequired advances Searching to AwaitingBackgroundCheck"):
    val event = HotelEvent.BackgroundCheckRequired(BookingDomain.hotelId, BookingDomain.bookingId, hotelName)
    val state = HotelState.Searching(BookingDomain.bookingId, BookingFlow.Diplomat)
    assertEquals(state.evolve(event), HotelState.AwaitingBackgroundCheck(BookingDomain.bookingId, hotelName))

  test("evolve: Compensated advances Reserved to Cancelled"):
    val state = HotelState.Reserved(BookingDomain.bookingId)
    assertEquals(state.evolve(HotelEvent.Compensated(BookingDomain.hotelId, BookingDomain.bookingId)), HotelState.Cancelled)

  test("civilian flow: search and reserve in one go"):
    val cmds = List(
      HotelCommand.InitSearch(BookingDomain.hotelId, BookingDomain.hotelQuery, BookingDomain.bookingId, BookingFlow.Civilian),
      HotelCommand.SelectHotel(BookingDomain.hotelId, hotelName)
    )
    val (events, state) = DeciderTestSupport.replay(hotelDecider, HotelState.Seed, cmds)
    assertEquals(
      events,
      List(
        HotelEvent.SearchStarted(BookingDomain.hotelId, BookingDomain.bookingId, BookingDomain.hotelQuery, BookingFlow.Civilian),
        HotelEvent.Reserved(BookingDomain.hotelId, BookingDomain.bookingId)
      )
    )
    assertEquals(state, HotelState.Reserved(BookingDomain.bookingId))

  test("diplomat flow: background check then reserve in one go"):
    val cmds = List(
      HotelCommand.InitSearch(BookingDomain.hotelId, BookingDomain.hotelQuery, BookingDomain.bookingId, BookingFlow.Diplomat),
      HotelCommand.RequestBackgroundCheck(BookingDomain.hotelId, hotelName),
      HotelCommand.VerifyBackgroundCheck(BookingDomain.hotelId)
    )
    val (events, state) = DeciderTestSupport.replay(hotelDecider, HotelState.Seed, cmds)
    assertEquals(
      events,
      List(
        HotelEvent.SearchStarted(BookingDomain.hotelId, BookingDomain.bookingId, BookingDomain.hotelQuery, BookingFlow.Diplomat),
        HotelEvent.BackgroundCheckRequired(BookingDomain.hotelId, BookingDomain.bookingId, hotelName),
        HotelEvent.Reserved(BookingDomain.hotelId, BookingDomain.bookingId)
      )
    )
    assertEquals(state, HotelState.Reserved(BookingDomain.bookingId))

  test("diplomat flow: rejected background check returns to Seed"):
    val cmds = List(
      HotelCommand.InitSearch(BookingDomain.hotelId, BookingDomain.hotelQuery, BookingDomain.bookingId, BookingFlow.Diplomat),
      HotelCommand.RequestBackgroundCheck(BookingDomain.hotelId, hotelName),
      HotelCommand.RejectBackgroundCheck(BookingDomain.hotelId)
    )
    val (events, state) = DeciderTestSupport.replay(hotelDecider, HotelState.Seed, cmds)
    assert(events.contains(HotelEvent.Failed(BookingDomain.hotelId, BookingDomain.bookingId)))
    assertEquals(state, HotelState.Seed)
