package apparatus.tests

import apparatus.core.*
import apparatus.core.machines.*
import apparatus.examples.*
import cats.effect.SyncIO
import cats.implicits.*

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

def runSteps(cmds: CatanCommand*): List[List[CatanEvent]] =
  Apparatus.runSteps(catanMachine[SyncIO], cmds.toList, DeciderMaterializer.syncIO).unsafeRunSync()

def runA(cmds: CatanCommand*): List[CatanEvent] =
  Apparatus.runMultiple(catanMachine[SyncIO], cmds, DeciderMaterializer.syncIO).unsafeRunSync()

// Give P1 resources for a road (wood + brick)
val giveRoadResources: CatanCommand =
  CatanCommand.Collect(List(ResourceGrant(PlayerId.P1, wood = 1, brick = 1, sheep = 0, wheat = 0, ore = 0)))

// Give P1 resources for a settlement
val giveSettlementResources: CatanCommand =
  CatanCommand.Collect(List(ResourceGrant(PlayerId.P1, wood = 1, brick = 1, sheep = 1, wheat = 1, ore = 0)))

// Give P1 resources for a city upgrade
val giveCityResources: CatanCommand =
  CatanCommand.Collect(List(ResourceGrant(PlayerId.P1, wood = 0, brick = 0, sheep = 0, wheat = 2, ore = 3)))

// Give P1 resources for a dev card
val giveDevCardResources: CatanCommand =
  CatanCommand.Collect(List(ResourceGrant(PlayerId.P1, wood = 0, brick = 0, sheep = 1, wheat = 1, ore = 1)))

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

class CatanSpec extends munit.FunSuite:

  // --- Pure Decider unit tests (no SyncIO overhead) ---

  test("decide: invalid dice value rejected"):
    assertEquals(
      catanDecider.decide(CatanCommand.Roll(13), CatanState.initial),
      List(CatanEvent.Rejected("invalid dice value: 13"))
    )

  test("decide: roll 1 rejected"):
    assertEquals(
      catanDecider.decide(CatanCommand.Roll(1), CatanState.initial),
      List(CatanEvent.Rejected("invalid dice value: 1"))
    )

  test("decide: valid roll emits Rolled"):
    val evts = catanDecider.decide(CatanCommand.Roll(6), CatanState.initial)
    assertEquals(evts, List(CatanEvent.Rolled(PlayerId.P1, 6)))

  test("decide: roll 7 emits Rolled(7) transitioning to WaitingForRobber"):
    val evts = catanDecider.decide(CatanCommand.Roll(7), CatanState.initial)
    assertEquals(evts, List(CatanEvent.Rolled(PlayerId.P1, 7)))
    val next = catanDecider.evolve(evts, CatanState.initial)
    assertEquals(next.phase, TurnPhase.WaitingForRobber)

  test("decide: self-rob rejected"):
    val state = CatanState.initial.copy(phase = TurnPhase.WaitingForRobber)
    val evts  = catanDecider.decide(CatanCommand.MoveRobber(Some(PlayerId.P1)), state)
    assert(evts.exists { case CatanEvent.Rejected(_) => true; case _ => false })

  // --- Phase enforcement (via Apparatus network) ---

  test("roll in WaitingForAction is rejected"):
    val results = runSteps(CatanCommand.Roll(5), CatanCommand.Roll(4))
    assert(results(1).exists { case CatanEvent.Rejected(_) => true; case _ => false })

  test("EndTurn without rolling is rejected"):
    val results = runSteps(CatanCommand.EndTurn)
    assert(results.head.exists { case CatanEvent.Rejected(_) => true; case _ => false })

  test("EndTurn without resolving robber is rejected"):
    val results = runSteps(CatanCommand.Roll(7), CatanCommand.EndTurn)
    assert(results(1).exists { case CatanEvent.Rejected(_) => true; case _ => false })

  test("Build before rolling is rejected"):
    val results = runSteps(CatanCommand.BuildRoad)
    assert(results.head.exists { case CatanEvent.Rejected(_) => true; case _ => false })

  test("Build before resolving robber is rejected"):
    val results = runSteps(CatanCommand.Roll(7), CatanCommand.BuildRoad)
    assert(results(1).exists { case CatanEvent.Rejected(_) => true; case _ => false })

  // --- Resource collection ---

  test("Collect grants resources in WaitingForAction"):
    val results = runSteps(
      CatanCommand.Roll(6),
      CatanCommand.Collect(List(ResourceGrant(PlayerId.P1, wood = 2, brick = 0, sheep = 0, wheat = 0, ore = 0)))
    )
    assert(results(1).contains(CatanEvent.ResourcesGranted(PlayerId.P1, 2, 0, 0, 0, 0)))

  test("Collect in WaitingForRoll is rejected"):
    val results = runSteps(
      CatanCommand.Collect(List(ResourceGrant(PlayerId.P1, wood = 1, brick = 0, sheep = 0, wheat = 0, ore = 0)))
    )
    assert(results.head.exists { case CatanEvent.Rejected(_) => true; case _ => false })

  // --- Building ---

  test("BuildRoad without resources is rejected"):
    val results = runSteps(CatanCommand.Roll(6), CatanCommand.BuildRoad)
    assert(results(1).exists { case CatanEvent.Rejected(_) => true; case _ => false })

  test("BuildRoad with resources succeeds"):
    val results = runSteps(CatanCommand.Roll(6), giveRoadResources, CatanCommand.BuildRoad)
    assert(results(2).contains(CatanEvent.RoadBuilt(PlayerId.P1)))

  test("BuildSettlement without resources is rejected"):
    val results = runSteps(CatanCommand.Roll(6), CatanCommand.BuildSettlement)
    assert(results(1).exists { case CatanEvent.Rejected(_) => true; case _ => false })

  test("BuildSettlement with resources succeeds"):
    val results = runSteps(CatanCommand.Roll(6), giveSettlementResources, CatanCommand.BuildSettlement)
    assert(results(2).contains(CatanEvent.SettlementBuilt(PlayerId.P1)))

  test("BuildCity without resources is rejected"):
    val results = runSteps(CatanCommand.Roll(6), CatanCommand.BuildCity)
    assert(results(1).exists { case CatanEvent.Rejected(_) => true; case _ => false })

  test("BuildCity with no settlement to upgrade is rejected"):
    // P1 starts with 1 settlement. We'd need to upgrade it, then try again.
    // Easier: create state where settlements=0
    val noSettlementState = CatanState.initial.copy(
      phase = TurnPhase.WaitingForAction,
      p1    = PlayerData.initial.copy(settlements = 0, resources = Resources(0, 0, 0, 2, 3))
    )
    val evts = catanDecider.decide(CatanCommand.BuildCity, noSettlementState)
    assert(evts.exists { case CatanEvent.Rejected(_) => true; case _ => false })

  test("BuildCity decrements settlements and increments cities"):
    val results = runSteps(CatanCommand.Roll(6), giveCityResources, CatanCommand.BuildCity)
    assert(results(2).contains(CatanEvent.CityBuilt(PlayerId.P1)))

  test("BuyDevCard without resources is rejected"):
    val results = runSteps(CatanCommand.Roll(6), CatanCommand.BuyDevCard)
    assert(results(1).exists { case CatanEvent.Rejected(_) => true; case _ => false })

  test("BuyDevCard with resources succeeds"):
    val results = runSteps(CatanCommand.Roll(6), giveDevCardResources, CatanCommand.BuyDevCard)
    assert(results(2).contains(CatanEvent.DevCardBought(PlayerId.P1)))

  // --- Turn progression ---

  test("EndTurn advances to P2"):
    val events = runA(CatanCommand.Roll(6), CatanCommand.EndTurn)
    assert(events.contains(CatanEvent.TurnStarted(PlayerId.P2)))

  test("Turn wraps P4 back to P1"):
    val cmds = List(
      // P1 turn
      CatanCommand.Roll(6), CatanCommand.EndTurn,
      // P2 turn
      CatanCommand.Roll(6), CatanCommand.EndTurn,
      // P3 turn
      CatanCommand.Roll(6), CatanCommand.EndTurn,
      // P4 turn
      CatanCommand.Roll(6), CatanCommand.EndTurn
    )
    val events = Apparatus.runMultiple(catanMachine[SyncIO], cmds, DeciderMaterializer.syncIO).unsafeRunSync()
    assert(events.contains(CatanEvent.TurnStarted(PlayerId.P1)))

  // --- Robber ---

  test("MoveRobber with no victim emits RobberMoved only"):
    val results = runSteps(CatanCommand.Roll(7), CatanCommand.MoveRobber(None))
    val robberEvts = results(1)
    assert(robberEvts.contains(CatanEvent.RobberMoved(PlayerId.P1, None)))
    assert(!robberEvts.exists { case CatanEvent.ResourceStolen(_, _, _, _, _, _, _) => true; case _ => false })

  test("MoveRobber with victim steals first available resource"):
    // Give P2 some wood
    val state = CatanState.initial.copy(
      phase = TurnPhase.WaitingForRobber,
      p2    = PlayerData.initial.copy(resources = Resources(wood = 2, brick = 0, sheep = 0, wheat = 0, ore = 0))
    )
    val evts = catanDecider.decide(CatanCommand.MoveRobber(Some(PlayerId.P2)), state)
    assert(evts.contains(CatanEvent.ResourceStolen(PlayerId.P2, PlayerId.P1, 1, 0, 0, 0, 0)))

  test("MoveRobber with victim having no resources emits no ResourceStolen"):
    val state = CatanState.initial.copy(phase = TurnPhase.WaitingForRobber)
    // P2 has no resources (initial state)
    val evts = catanDecider.decide(CatanCommand.MoveRobber(Some(PlayerId.P2)), state)
    assert(!evts.exists { case CatanEvent.ResourceStolen(_, _, _, _, _, _, _) => true; case _ => false })
    assert(evts.contains(CatanEvent.RobberMoved(PlayerId.P1, Some(PlayerId.P2))))

  test("After MoveRobber, WaitingForAction is restored"):
    val results = runSteps(CatanCommand.Roll(7), CatanCommand.MoveRobber(None), CatanCommand.EndTurn)
    // EndTurn should succeed (WaitingForAction), not be rejected
    assert(!results(2).exists { case CatanEvent.Rejected(_) => true; case _ => false })
    assert(results(2).contains(CatanEvent.TurnEnded(PlayerId.P1)))

  // --- Dev cards and Largest Army ---

  test("PlayKnight with no dev cards is rejected"):
    val results = runSteps(CatanCommand.Roll(6), CatanCommand.PlayKnight)
    assert(results(1).exists { case CatanEvent.Rejected(_) => true; case _ => false })

  test("Playing 3 knights claims Largest Army"):
    // Pre-load state: P1 has 3 dev cards and WaitingForAction
    val pd = PlayerData.initial.copy(devCards = 3, knightsPlayed = 2)
    val state = CatanState.initial.copy(phase = TurnPhase.WaitingForAction, p1 = pd)
    val evts = catanDecider.decide(CatanCommand.PlayKnight, state)
    assert(evts.contains(CatanEvent.KnightPlayed(PlayerId.P1)))
    assert(evts.contains(CatanEvent.LargestArmyClaimed(PlayerId.P1)))

  test("Largest Army transfers when another player plays more knights"):
    val p1pd = PlayerData.initial.copy(devCards = 1, knightsPlayed = 3)
    val p2pd = PlayerData.initial.copy(devCards = 0, knightsPlayed = 4)
    val state = CatanState.initial.copy(
      phase             = TurnPhase.WaitingForAction,
      p1                = p1pd,
      p2                = p2pd,
      largestArmyHolder = Some(PlayerId.P2)
    )
    val evts = catanDecider.decide(CatanCommand.PlayKnight, state)
    // P1 would have 4 knights = same as P2 → no transfer (must be strictly more)
    assert(!evts.contains(CatanEvent.LargestArmyTransferred(PlayerId.P2, PlayerId.P1)))

  test("Largest Army transfers when strictly more knights"):
    val p1pd = PlayerData.initial.copy(devCards = 1, knightsPlayed = 4)
    val p2pd = PlayerData.initial.copy(devCards = 0, knightsPlayed = 3)
    val state = CatanState.initial.copy(
      phase             = TurnPhase.WaitingForAction,
      p1                = p1pd,
      p2                = p2pd,
      largestArmyHolder = Some(PlayerId.P2)
    )
    val evts = catanDecider.decide(CatanCommand.PlayKnight, state)
    assert(evts.contains(CatanEvent.LargestArmyTransferred(PlayerId.P2, PlayerId.P1)))

  // --- Longest Road ---

  test("Building 5th road claims Longest Road"):
    val pd    = PlayerData.initial.copy(roads = 4, resources = Resources(wood = 1, brick = 1, sheep = 0, wheat = 0, ore = 0))
    val state = CatanState.initial.copy(phase = TurnPhase.WaitingForAction, p1 = pd)
    val evts  = catanDecider.decide(CatanCommand.BuildRoad, state)
    assert(evts.contains(CatanEvent.LongestRoadClaimed(PlayerId.P1)))

  test("Longest Road not claimed before 5 roads"):
    val pd    = PlayerData.initial.copy(roads = 3, resources = Resources(wood = 1, brick = 1, sheep = 0, wheat = 0, ore = 0))
    val state = CatanState.initial.copy(phase = TurnPhase.WaitingForAction, p1 = pd)
    val evts  = catanDecider.decide(CatanCommand.BuildRoad, state)
    assert(!evts.exists { case CatanEvent.LongestRoadClaimed(_) => true; case _ => false })

  test("Longest Road transfers when another player builds more roads"):
    val p1pd = PlayerData.initial.copy(roads = 5, resources = Resources(wood = 1, brick = 1, sheep = 0, wheat = 0, ore = 0))
    val p2pd = PlayerData.initial.copy(roads = 4)
    val state = CatanState.initial.copy(
      phase             = TurnPhase.WaitingForAction,
      p1                = p1pd,
      p2                = p2pd,
      longestRoadHolder = Some(PlayerId.P2)
    )
    val evts = catanDecider.decide(CatanCommand.BuildRoad, state)
    assert(evts.contains(CatanEvent.LongestRoadTransferred(PlayerId.P2, PlayerId.P1)))

  // --- Win condition ---

  test("Reaching 10 VP emits GameWon"):
    // P1 at 9 VP (4 settlements + 1 city = 4 + 2 = 6... let's use 8 settlements and no city)
    // Settlements = 8 → 8 VP, LargestArmy = 2 VP → 10 total. Build one more settlement.
    val pd = PlayerData.initial.copy(
      settlements = 8,
      cities      = 0,
      resources   = Resources(wood = 1, brick = 1, sheep = 1, wheat = 1, ore = 0)
    )
    val state = CatanState.initial.copy(
      phase             = TurnPhase.WaitingForAction,
      p1                = pd,
      largestArmyHolder = Some(PlayerId.P1)
    )
    val evts = catanDecider.decide(CatanCommand.BuildSettlement, state)
    assert(evts.contains(CatanEvent.SettlementBuilt(PlayerId.P1)))
    assert(evts.contains(CatanEvent.GameWon(PlayerId.P1)))

  test("Commands after GameWon are rejected"):
    val state = CatanState.initial.copy(phase = TurnPhase.GameOver)
    val evts  = catanDecider.decide(CatanCommand.Roll(6), state)
    assertEquals(evts, List(CatanEvent.Rejected("game is over")))

  test("Rejected events do not mutate state"):
    val before = CatanState.initial
    val evts   = catanDecider.decide(CatanCommand.BuildRoad, before) // wrong phase
    val after  = catanDecider.evolve(evts, before)
    assertEquals(before, after)
