package apparatus.examples

import apparatus.core.Apparatus
import apparatus.core.machines.*
import zio.blocks.schema.Schema

// ---------------------------------------------------------------------------
// Resources — pure state helper, never appears in events
// ---------------------------------------------------------------------------

case class Resources(wood: Int, brick: Int, sheep: Int, wheat: Int, ore: Int):
  def canAfford(cost: Resources): Boolean =
    wood >= cost.wood && brick >= cost.brick && sheep >= cost.sheep &&
    wheat >= cost.wheat && ore >= cost.ore

  def pay(cost: Resources): Resources =
    Resources(wood - cost.wood, brick - cost.brick, sheep - cost.sheep, wheat - cost.wheat, ore - cost.ore)

  def add(r: Resources): Resources =
    Resources(wood + r.wood, brick + r.brick, sheep + r.sheep, wheat + r.wheat, ore + r.ore)

object Resources:
  val empty: Resources        = Resources(0, 0, 0, 0, 0)
  val roadCost: Resources       = Resources(wood = 1, brick = 1, sheep = 0, wheat = 0, ore = 0)
  val settlementCost: Resources = Resources(wood = 1, brick = 1, sheep = 1, wheat = 1, ore = 0)
  val cityCost: Resources       = Resources(wood = 0, brick = 0, sheep = 0, wheat = 2, ore = 3)
  val devCardCost: Resources    = Resources(wood = 0, brick = 0, sheep = 1, wheat = 1, ore = 1)

// ---------------------------------------------------------------------------
// Player identity
// ---------------------------------------------------------------------------

enum PlayerId derives Schema:
  case P1, P2, P3, P4

object PlayerId:
  val all: List[PlayerId] = List(P1, P2, P3, P4)

// ---------------------------------------------------------------------------
// Per-player data
// ---------------------------------------------------------------------------

case class PlayerData(
  resources:    Resources,
  roads:        Int,
  settlements:  Int,
  cities:       Int,
  devCards:     Int,
  knightsPlayed: Int
):
  def victoryPoints: Int = settlements + cities * 2

object PlayerData:
  val initial: PlayerData = PlayerData(Resources.empty, roads = 0, settlements = 1, cities = 0, devCards = 0, knightsPlayed = 0)

// ---------------------------------------------------------------------------
// Turn phase
// ---------------------------------------------------------------------------

enum TurnPhase:
  case WaitingForRoll, WaitingForAction, WaitingForRobber, GameOver

// ---------------------------------------------------------------------------
// Game state
// ---------------------------------------------------------------------------

case class CatanState(
  phase:             TurnPhase,
  currentPlayer:     PlayerId,
  lastRoll:          Int,
  p1:                PlayerData,
  p2:                PlayerData,
  p3:                PlayerData,
  p4:                PlayerData,
  largestArmyHolder: Option[PlayerId],
  longestRoadHolder: Option[PlayerId]
):
  def playerData(p: PlayerId): PlayerData = p match
    case PlayerId.P1 => p1
    case PlayerId.P2 => p2
    case PlayerId.P3 => p3
    case PlayerId.P4 => p4

  def updatePlayer(p: PlayerId, d: PlayerData): CatanState = p match
    case PlayerId.P1 => copy(p1 = d)
    case PlayerId.P2 => copy(p2 = d)
    case PlayerId.P3 => copy(p3 = d)
    case PlayerId.P4 => copy(p4 = d)

  def nextPlayer: PlayerId =
    val idx = PlayerId.all.indexOf(currentPlayer)
    PlayerId.all((idx + 1) % 4)

  // Projected VP including bonus cards (used pre-evolve for win checks)
  def totalVP(p: PlayerId, extraSettlements: Int = 0, extraCities: Int = 0): Int =
    val base = playerData(p).victoryPoints + extraSettlements + extraCities * 2
    val army = if largestArmyHolder.contains(p) then 2 else 0
    val road = if longestRoadHolder.contains(p) then 2 else 0
    base + army + road

  def decide(cmd: CatanCommand): List[CatanEvent] =
    import CatanCommand.*
    import CatanEvent.*
    import TurnPhase.*

    phase match
      case GameOver =>
        List(Rejected("game is over"))

      case WaitingForRoll =>
        cmd match
          case Roll(v) if v < 2 || v > 12 =>
            List(Rejected(s"invalid dice value: $v"))
          case Roll(7) =>
            List(Rolled(currentPlayer, 7))
          case Roll(v) =>
            List(Rolled(currentPlayer, v))
          case _ =>
            List(Rejected("must roll first"))

      case WaitingForRobber =>
        cmd match
          case MoveRobber(Some(victim)) if victim == currentPlayer =>
            List(Rejected("cannot rob yourself"))
          case MoveRobber(victim) =>
            val stolenEvents = victim.flatMap { v =>
              val res = playerData(v).resources
              // Deterministic steal: first non-zero resource
              if res.wood  > 0 then Some(ResourceStolen(v, currentPlayer, 1, 0, 0, 0, 0))
              else if res.brick > 0 then Some(ResourceStolen(v, currentPlayer, 0, 1, 0, 0, 0))
              else if res.sheep > 0 then Some(ResourceStolen(v, currentPlayer, 0, 0, 1, 0, 0))
              else if res.wheat > 0 then Some(ResourceStolen(v, currentPlayer, 0, 0, 0, 1, 0))
              else if res.ore   > 0 then Some(ResourceStolen(v, currentPlayer, 0, 0, 0, 0, 1))
              else None
            }.toList
            RobberMoved(currentPlayer, victim) :: stolenEvents
          case _ =>
            List(Rejected("must move robber after rolling 7"))

      case WaitingForAction =>
        cmd match
          case Roll(_) =>
            List(Rejected("already rolled this turn"))

          case Collect(grants) =>
            grants.map(g => ResourcesGranted(g.player, g.wood, g.brick, g.sheep, g.wheat, g.ore))

          case MoveRobber(_) =>
            List(Rejected("no robber to move"))

          case BuildRoad =>
            val pd = playerData(currentPlayer)
            if !pd.resources.canAfford(Resources.roadCost) then
              List(Rejected("insufficient resources: road costs 1 wood + 1 brick"))
            else
              val newRoads = pd.roads + 1
              val roadEvents: List[CatanEvent] =
                if newRoads >= 5 then
                  longestRoadHolder match
                    case None =>
                      List(LongestRoadClaimed(currentPlayer))
                    case Some(holder) if holder != currentPlayer && newRoads > playerData(holder).roads =>
                      List(LongestRoadTransferred(holder, currentPlayer))
                    case _ =>
                      Nil
                else Nil
              val winEvents =
                val bonusRoad = if roadEvents.nonEmpty && !longestRoadHolder.contains(currentPlayer) then 2 else 0
                if totalVP(currentPlayer) + bonusRoad >= 10 then List(GameWon(currentPlayer)) else Nil
              RoadBuilt(currentPlayer) :: roadEvents ::: winEvents

          case BuildSettlement =>
            val pd = playerData(currentPlayer)
            if !pd.resources.canAfford(Resources.settlementCost) then
              List(Rejected("insufficient resources: settlement costs 1 wood + 1 brick + 1 sheep + 1 wheat"))
            else
              val winEvents =
                if totalVP(currentPlayer, extraSettlements = 1) >= 10 then List(GameWon(currentPlayer)) else Nil
              SettlementBuilt(currentPlayer) :: winEvents

          case BuildCity =>
            val pd = playerData(currentPlayer)
            if pd.settlements == 0 then
              List(Rejected("no settlement to upgrade to city"))
            else if !pd.resources.canAfford(Resources.cityCost) then
              List(Rejected("insufficient resources: city costs 2 wheat + 3 ore"))
            else
              val winEvents =
                if totalVP(currentPlayer, extraSettlements = -1, extraCities = 1) >= 10 then List(GameWon(currentPlayer)) else Nil
              CityBuilt(currentPlayer) :: winEvents

          case BuyDevCard =>
            val pd = playerData(currentPlayer)
            if !pd.resources.canAfford(Resources.devCardCost) then
              List(Rejected("insufficient resources: dev card costs 1 sheep + 1 wheat + 1 ore"))
            else
              List(DevCardBought(currentPlayer))

          case PlayKnight =>
            val pd = playerData(currentPlayer)
            if pd.devCards == 0 then
              List(Rejected("no dev cards to play"))
            else
              val newKnights = pd.knightsPlayed + 1
              val armyEvents: List[CatanEvent] =
                if newKnights >= 3 then
                  largestArmyHolder match
                    case None =>
                      List(LargestArmyClaimed(currentPlayer))
                    case Some(holder) if holder != currentPlayer && newKnights > playerData(holder).knightsPlayed =>
                      List(LargestArmyTransferred(holder, currentPlayer))
                    case _ =>
                      Nil
                else Nil
              val winEvents =
                val bonusArmy = if armyEvents.nonEmpty && !largestArmyHolder.contains(currentPlayer) then 2 else 0
                if totalVP(currentPlayer) + bonusArmy >= 10 then List(GameWon(currentPlayer)) else Nil
              KnightPlayed(currentPlayer) :: armyEvents ::: winEvents

          case EndTurn =>
            List(TurnEnded(currentPlayer), TurnStarted(nextPlayer))

  def evolve(event: CatanEvent): CatanState =
    import CatanEvent.*
    import TurnPhase.*

    event match
      case Rolled(_, v) =>
        copy(lastRoll = v, phase = if v == 7 then WaitingForRobber else WaitingForAction)

      case ResourcesGranted(p, w, b, s, wh, o) =>
        val pd = playerData(p)
        updatePlayer(p, pd.copy(resources = pd.resources.add(Resources(w, b, s, wh, o))))

      case RobberMoved(_, _) =>
        copy(phase = WaitingForAction) // phase restored here; ResourceStolen (if any) re-applies it

      case ResourceStolen(from, to, w, b, s, wh, o) =>
        val stolen = Resources(w, b, s, wh, o)
        val fromPd = playerData(from)
        val toPd   = playerData(to)
        updatePlayer(from, fromPd.copy(resources = fromPd.resources.pay(stolen)))
          .updatePlayer(to, toPd.copy(resources = toPd.resources.add(stolen)))
          .copy(phase = WaitingForAction)

      case RoadBuilt(p) =>
        val pd = playerData(p)
        updatePlayer(p, pd.copy(roads = pd.roads + 1, resources = pd.resources.pay(Resources.roadCost)))

      case SettlementBuilt(p) =>
        val pd = playerData(p)
        updatePlayer(p, pd.copy(settlements = pd.settlements + 1, resources = pd.resources.pay(Resources.settlementCost)))

      case CityBuilt(p) =>
        val pd = playerData(p)
        updatePlayer(p, pd.copy(
          cities      = pd.cities + 1,
          settlements = pd.settlements - 1,
          resources   = pd.resources.pay(Resources.cityCost)
        ))

      case DevCardBought(p) =>
        val pd = playerData(p)
        updatePlayer(p, pd.copy(devCards = pd.devCards + 1, resources = pd.resources.pay(Resources.devCardCost)))

      case KnightPlayed(p) =>
        val pd = playerData(p)
        updatePlayer(p, pd.copy(knightsPlayed = pd.knightsPlayed + 1, devCards = pd.devCards - 1))

      case LargestArmyClaimed(p)          => copy(largestArmyHolder = Some(p))
      case LargestArmyTransferred(_, to)  => copy(largestArmyHolder = Some(to))
      case LongestRoadClaimed(p)          => copy(longestRoadHolder = Some(p))
      case LongestRoadTransferred(_, to)  => copy(longestRoadHolder = Some(to))

      case TurnEnded(_) => this
      case TurnStarted(p) =>
        copy(currentPlayer = p, phase = WaitingForRoll, lastRoll = 0)

      case GameWon(_) => copy(phase = GameOver)
      case Rejected(_) => this

object CatanState:
  def initial: CatanState =
    CatanState(
      phase             = TurnPhase.WaitingForRoll,
      currentPlayer     = PlayerId.P1,
      lastRoll          = 0,
      p1                = PlayerData.initial,
      p2                = PlayerData.initial,
      p3                = PlayerData.initial,
      p4                = PlayerData.initial,
      largestArmyHolder = None,
      longestRoadHolder = None
    )

// ---------------------------------------------------------------------------
// Commands
// ---------------------------------------------------------------------------

case class ResourceGrant(player: PlayerId, wood: Int, brick: Int, sheep: Int, wheat: Int, ore: Int)

enum CatanCommand:
  case Roll(value: Int)
  case Collect(grants: List[ResourceGrant])
  case MoveRobber(victim: Option[PlayerId])
  case BuildRoad
  case BuildSettlement
  case BuildCity
  case BuyDevCard
  case PlayKnight
  case EndTurn

// ---------------------------------------------------------------------------
// Events — must derive Schema for DeciderMaterializer
// ---------------------------------------------------------------------------

enum CatanEvent derives Schema:
  case Rolled(player: PlayerId, value: Int)
  case ResourcesGranted(player: PlayerId, wood: Int, brick: Int, sheep: Int, wheat: Int, ore: Int)
  case RobberMoved(by: PlayerId, victim: Option[PlayerId])
  case ResourceStolen(from: PlayerId, to: PlayerId, wood: Int, brick: Int, sheep: Int, wheat: Int, ore: Int)
  case RoadBuilt(player: PlayerId)
  case SettlementBuilt(player: PlayerId)
  case CityBuilt(player: PlayerId)
  case DevCardBought(player: PlayerId)
  case KnightPlayed(player: PlayerId)
  case LargestArmyClaimed(player: PlayerId)
  case LargestArmyTransferred(from: PlayerId, to: PlayerId)
  case LongestRoadClaimed(player: PlayerId)
  case LongestRoadTransferred(from: PlayerId, to: PlayerId)
  case TurnEnded(player: PlayerId)
  case TurnStarted(player: PlayerId)
  case GameWon(player: PlayerId)
  case Rejected(reason: String)

// ---------------------------------------------------------------------------
// Decider + Apparatus wiring
// ---------------------------------------------------------------------------

val catanDecider: Decider[CatanState, CatanCommand, List[CatanEvent]] =
  DeciderBuilder
    .seed[CatanState](CatanState.initial)
    .decide[CatanCommand, List[CatanEvent]](_.decide(_))
    .evolveList(_.evolve(_))

def catanMachine[F[_]]: Apparatus[F, CatanCommand, List[CatanEvent]] =
  Apparatus.deciderMachine[F, CatanCommand, CatanEvent]("catan-game", catanDecider)
