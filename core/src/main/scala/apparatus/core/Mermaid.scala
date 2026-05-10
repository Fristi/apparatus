package apparatus.core

/** Renders an [[Apparatus]] graph structure as a Mermaid `graph TD` flowchart.
  *
  * Only the **structural topology** is captured — internal state and transition
  * functions are erased at runtime and cannot be inspected. Attach human-readable
  * names with `.label("…")` before calling [[print]]:
  *
  *   - `.label("…")` on an [[Apparatus.Fresh]] or [[Apparatus.Stable]] → renames the node box.
  *   - `.label("…")` on an [[Apparatus.LmapOrEmpty]]                   → renames the filter diamond.
  *   - `.label("…")` on any composite                                   → wraps it in a Mermaid `subgraph`.
  *
  * === Shape legend ===
  *
  * {{{
  *   ["name"]   rectangle — Apparatus.Fresh / Apparatus.Stable (machine nodes)
  *   {"name"}   diamond   — Apparatus.Alternative (routing), Apparatus.LmapOrEmpty (filter)
  *   (["name"]) stadium   — structural join/split/fan-out/combine nodes
  * }}}
  *
  * === Usage ===
  *
  * {{{
  *   val diagram = ApparatusMermaid.print(
  *     bookingDecider.label("Booking Saga") <-> (
  *       flightService.label("Flight Service")
  *         merge carService.label("Car Service")
  *     )
  *   )
  *   // paste into https://mermaid.live
  * }}}
  *
  * The output is a self-contained Mermaid `graph TD` string. Paste it into
  * [mermaid.live](https://mermaid.live) or embed it in any Markdown renderer that
  * supports Mermaid fenced code blocks.
  */
object Mermaid:

  def print[F[_], I, O](fsm: Apparatus[F, I, O]): String =
    val ctx = Context()
    render(fsm, ctx)
    "graph TD\n" + ctx.declarations.mkString("\n") + "\n" + ctx.edges.mkString("\n")

  // ── internals ──────────────────────────────────────────────────────────────

  private class Context:
    private var counter = 0
    val declarations = collection.mutable.ListBuffer.empty[String]
    val edges        = collection.mutable.ListBuffer.empty[String]

    def fresh(prefix: String): String =
      counter += 1
      s"${prefix}_$counter"

    def node(id: String, label: String, shape: Shape): Unit =
      declarations += shape.mermaid(id, label)

    def edge(from: String, to: String, label: String = ""): Unit =
      if label.isEmpty then edges += s"    $from --> $to"
      else edges += s"    $from -->|$label| $to"

    def openSubgraph(label: String): Unit =
      declarations += s"""    subgraph "$label" """

    def closeSubgraph(): Unit =
      declarations += "    end"

  private enum Shape:
    case Box, Diamond, Stadium
    def mermaid(id: String, label: String): String = this match
      case Box     => s"""    $id["$label"]"""
      case Diamond => s"""    $id{"$label"}"""
      case Stadium => s"""    $id(["$label"])"""

  /** Returns (inputPortId, outputPortId) so callers can wire edges. */
  private def render[F[_], I, O](fsm: Apparatus[F, I, O], ctx: Context): (String, String) =
    fsm match

      case Apparatus.Labeled(Apparatus.Fresh(_), name) =>
        val id = ctx.fresh("node")
        ctx.node(id, name, Shape.Box)
        (id, id)

      case Apparatus.Labeled(Apparatus.Stable(stableId, _), name) =>
        val id = ctx.fresh("node")
        ctx.node(id, name, Shape.Box)
        (id, id)

      case Apparatus.Labeled(Apparatus.LmapOrEmpty(inner, _, _), name) =>
        val filterId = ctx.fresh("filter")
        ctx.node(filterId, name, Shape.Diamond)
        val (iIn, iOut) = render(inner, ctx)
        ctx.edge(filterId, iIn, "defined")
        (filterId, iOut)

      case Apparatus.Labeled(inner, name) =>
        ctx.openSubgraph(name)
        val result = render(inner, ctx)
        ctx.closeSubgraph()
        result

      case Apparatus.Fresh(_) =>
        val id = ctx.fresh("node")
        ctx.node(id, "Machine", Shape.Box)
        (id, id)

      case Apparatus.Stable(stableId, _) =>
        val id = ctx.fresh("node")
        ctx.node(id, stableId, Shape.Box)
        (id, id)

      case Apparatus.Sequential(left, right) =>
        val (lIn, lOut) = render(left, ctx)
        val (rIn, rOut) = render(right, ctx)
        ctx.edge(lOut, rIn)
        (lIn, rOut)

      case Apparatus.Parallel(left, right) =>
        val splitId = ctx.fresh("split")
        val joinId  = ctx.fresh("join")
        ctx.node(splitId, "split", Shape.Stadium)
        ctx.node(joinId,  "join",  Shape.Stadium)
        val (lIn, lOut) = render(left, ctx)
        val (rIn, rOut) = render(right, ctx)
        ctx.edge(splitId, lIn,  "_1")
        ctx.edge(splitId, rIn,  "_2")
        ctx.edge(lOut, joinId, "_1")
        ctx.edge(rOut, joinId, "_2")
        (splitId, joinId)

      case Apparatus.Alternative(left, right) =>
        val routeId = ctx.fresh("route")
        val mergeId = ctx.fresh("merge")
        ctx.node(routeId, "Either?", Shape.Diamond)
        ctx.node(mergeId, "merge",   Shape.Stadium)
        val (lIn, lOut) = render(left, ctx)
        val (rIn, rOut) = render(right, ctx)
        ctx.edge(routeId, lIn,  "Left")
        ctx.edge(routeId, rIn,  "Right")
        ctx.edge(lOut, mergeId, "Left")
        ctx.edge(rOut, mergeId, "Right")
        (routeId, mergeId)

      case Apparatus.Feedback(left, right) =>
        val (lIn, lOut) = render(left, ctx)
        val (rIn, rOut) = render(right, ctx)
        ctx.edge(lOut, rIn,  "B")
        ctx.edge(rOut, lIn,  "A ↺")
        (lIn, lOut)

      case Apparatus.FeedbackMany(left, right) =>
        val (lIn, lOut) = render(left, ctx)
        val (rIn, rOut) = render(right, ctx)
        ctx.edge(lOut, rIn,  "N[B]")
        ctx.edge(rOut, lIn,  "N[A] ↺")
        (lIn, lOut)

      case Apparatus.LmapOrEmpty(inner, _, _) =>
        val filterId = ctx.fresh("filter")
        ctx.node(filterId, "lmapOrEmpty", Shape.Diamond)
        val (iIn, iOut) = render(inner, ctx)
        ctx.edge(filterId, iIn, "defined")
        (filterId, iOut)

      case Apparatus.Merged(left, right, _) =>
        val fanId  = ctx.fresh("fan")
        val combId = ctx.fresh("combine")
        ctx.node(fanId,  "fan-out", Shape.Stadium)
        ctx.node(combId, "combine", Shape.Stadium)
        val (lIn, lOut) = render(left, ctx)
        val (rIn, rOut) = render(right, ctx)
        ctx.edge(fanId, lIn)
        ctx.edge(fanId, rIn)
        ctx.edge(lOut, combId)
        ctx.edge(rOut, combId)
        (fanId, combId)

      case other =>
        val id = ctx.fresh("node")
        ctx.node(id, other.getClass.getSimpleName, Shape.Box)
        (id, id)
