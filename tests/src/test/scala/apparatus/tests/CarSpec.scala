package apparatus.tests

import apparatus.examples.BookingFlow
import apparatus.examples.saga.*
import munit.FunSuite

class CarSpec extends FunSuite:

  private val carModel = "Tesla Model 3"

  test("decide: InitSearch from Seed emits SearchStarted"):
    val cmd = CarCommand.InitSearch(BookingDomain.carId, BookingDomain.carQuery, BookingDomain.bookingId, BookingFlow.Civilian)
    val evts = carDecider.decide(cmd, CarState.Seed)
    assertEquals(evts, List(CarEvent.SearchStarted(BookingDomain.carId, BookingDomain.bookingId, BookingDomain.carQuery, BookingFlow.Civilian)))

  test("decide: SelectCar from Searching emits Reserved"):
    val state = CarState.Searching(BookingDomain.bookingId, BookingFlow.Civilian)
    val evts = carDecider.decide(CarCommand.SelectCar(BookingDomain.carId, carModel), state)
    assertEquals(evts, List(CarEvent.Reserved(BookingDomain.carId, BookingDomain.bookingId)))

  test("decide: RequestLicenseCheck from Searching emits LicenseCheckRequired"):
    val state = CarState.Searching(BookingDomain.bookingId, BookingFlow.Diplomat)
    val evts = carDecider.decide(CarCommand.RequestLicenseCheck(BookingDomain.carId, carModel), state)
    assertEquals(evts, List(CarEvent.LicenseCheckRequired(BookingDomain.carId, BookingDomain.bookingId, carModel)))

  test("decide: Cancel from Reserved emits Compensated"):
    val state = CarState.Reserved(BookingDomain.bookingId)
    val evts = carDecider.decide(CarCommand.Cancel(BookingDomain.carId), state)
    assertEquals(evts, List(CarEvent.Compensated(BookingDomain.carId, BookingDomain.bookingId)))

  test("decide: unknown command from Cancelled emits nothing"):
    val cmd = CarCommand.InitSearch(BookingDomain.carId, BookingDomain.carQuery, BookingDomain.bookingId, BookingFlow.Civilian)
    assertEquals(carDecider.decide(cmd, CarState.Cancelled), Nil)

  test("evolve: SearchStarted advances Seed to Searching"):
    val event = CarEvent.SearchStarted(BookingDomain.carId, BookingDomain.bookingId, BookingDomain.carQuery, BookingFlow.Diplomat)
    assertEquals(CarState.Seed.evolve(event), CarState.Searching(BookingDomain.bookingId, BookingFlow.Diplomat))

  test("evolve: LicenseCheckRequired advances Searching to AwaitingLicenseCheck"):
    val event = CarEvent.LicenseCheckRequired(BookingDomain.carId, BookingDomain.bookingId, carModel)
    val state = CarState.Searching(BookingDomain.bookingId, BookingFlow.Diplomat)
    assertEquals(state.evolve(event), CarState.AwaitingLicenseCheck(BookingDomain.bookingId, carModel))

  test("evolve: Failed from AwaitingLicenseCheck returns to Seed"):
    val state = CarState.AwaitingLicenseCheck(BookingDomain.bookingId, carModel)
    assertEquals(state.evolve(CarEvent.Failed(BookingDomain.carId, BookingDomain.bookingId)), CarState.Seed)

  test("civilian flow: search and reserve in one go"):
    val init = CarCommand.InitSearch(BookingDomain.carId, BookingDomain.carQuery, BookingDomain.bookingId, BookingFlow.Civilian)
    val select = CarCommand.SelectCar(BookingDomain.carId, carModel)
    val (events, state) = DeciderTestSupport.replay(carDecider, CarState.Seed, List(init, select))
    assertEquals(
      events,
      List(
        CarEvent.SearchStarted(BookingDomain.carId, BookingDomain.bookingId, BookingDomain.carQuery, BookingFlow.Civilian),
        CarEvent.Reserved(BookingDomain.carId, BookingDomain.bookingId)
      )
    )
    assertEquals(state, CarState.Reserved(BookingDomain.bookingId))

  test("diplomat flow: license check then reserve in one go"):
    val cmds = List(
      CarCommand.InitSearch(BookingDomain.carId, BookingDomain.carQuery, BookingDomain.bookingId, BookingFlow.Diplomat),
      CarCommand.RequestLicenseCheck(BookingDomain.carId, carModel),
      CarCommand.VerifyDriverLicense(BookingDomain.carId)
    )
    val (events, state) = DeciderTestSupport.replay(carDecider, CarState.Seed, cmds)
    assertEquals(
      events,
      List(
        CarEvent.SearchStarted(BookingDomain.carId, BookingDomain.bookingId, BookingDomain.carQuery, BookingFlow.Diplomat),
        CarEvent.LicenseCheckRequired(BookingDomain.carId, BookingDomain.bookingId, carModel),
        CarEvent.Reserved(BookingDomain.carId, BookingDomain.bookingId)
      )
    )
    assertEquals(state, CarState.Reserved(BookingDomain.bookingId))

  test("diplomat flow: rejected license returns to Seed"):
    val cmds = List(
      CarCommand.InitSearch(BookingDomain.carId, BookingDomain.carQuery, BookingDomain.bookingId, BookingFlow.Diplomat),
      CarCommand.RequestLicenseCheck(BookingDomain.carId, carModel),
      CarCommand.RejectDriverLicense(BookingDomain.carId)
    )
    val (events, state) = DeciderTestSupport.replay(carDecider, CarState.Seed, cmds)
    assert(events.contains(CarEvent.Failed(BookingDomain.carId, BookingDomain.bookingId)))
    assertEquals(state, CarState.Seed)
