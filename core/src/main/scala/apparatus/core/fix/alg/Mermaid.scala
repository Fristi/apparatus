package apparatus.core.fix.alg

import apparatus.core.fix.{Apparatus, ApparatusF}

object Mermaid:

  def print[I, O](apparatus: Apparatus[I, O]): String =
    val ctx = Context()
    render(apparatus, ctx)
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
  private def render[I, O](apparatus: Apparatus[I, O], ctx: Context): (String, String) =
    apparatus.unfix match

      case ApparatusF.DeciderNode(networkId, _, _) =>
        val id = ctx.fresh("node")
        ctx.node(id, networkId, Shape.Box)
        (id, id)

      case ApparatusF.DeciderRef(networkId) =>
        val id = ctx.fresh("node")
        ctx.node(id, networkId, Shape.Box)
        (id, id)

      case ApparatusF.Sequential(left, right) =>
        val (lIn, lOut) = render(left, ctx)
        val (rIn, rOut) = render(right, ctx)
        ctx.edge(lOut, rIn)
        (lIn, rOut)

      case ApparatusF.Parallel(left, right) =>
        val splitId = ctx.fresh("split")
        val joinId  = ctx.fresh("join")
        ctx.node(splitId, "split", Shape.Stadium)
        ctx.node(joinId,  "join",  Shape.Stadium)
        val (lIn, lOut) = render(left, ctx)
        val (rIn, rOut) = render(right, ctx)
        ctx.edge(splitId, lIn, "_1")
        ctx.edge(splitId, rIn, "_2")
        ctx.edge(lOut, joinId, "_1")
        ctx.edge(rOut, joinId, "_2")
        (splitId, joinId)

      case ApparatusF.Alternative(left, right) =>
        val routeId = ctx.fresh("route")
        val mergeId = ctx.fresh("merge")
        ctx.node(routeId, "Either?", Shape.Diamond)
        ctx.node(mergeId, "merge",   Shape.Stadium)
        val (lIn, lOut) = render(left, ctx)
        val (rIn, rOut) = render(right, ctx)
        ctx.edge(routeId, lIn, "Left")
        ctx.edge(routeId, rIn, "Right")
        ctx.edge(lOut, mergeId, "Left")
        ctx.edge(rOut, mergeId, "Right")
        (routeId, mergeId)

      case ApparatusF.Feedback(left, right, _, _, _) =>
        val (lIn, lOut) = render(left, ctx)
        val (rIn, rOut) = render(right, ctx)
        ctx.edge(lOut, rIn, "N[B]")
        ctx.edge(rOut, lIn, "A ↺")
        (lIn, lOut)

      case ApparatusF.FeedbackMany(left, right, _, _, _) =>
        val (lIn, lOut) = render(left, ctx)
        val (rIn, rOut) = render(right, ctx)
        ctx.edge(lOut, rIn, "N[B]")
        ctx.edge(rOut, lIn, "N[A] ↺")
        (lIn, lOut)

      // Label directly on a leaf — rename the node box rather than wrapping in subgraph
      case ApparatusF.Labeled(inner, name) if isLeaf(inner) =>
        val id = ctx.fresh("node")
        ctx.node(id, name, Shape.Box)
        (id, id)

      // Label on a composite — wrap in a Mermaid subgraph
      case ApparatusF.Labeled(inner, name) =>
        ctx.openSubgraph(name)
        val result = render(inner, ctx)
        ctx.closeSubgraph()
        result

  private def isLeaf[I, O](apparatus: Apparatus[I, O]): Boolean =
    apparatus.unfix match
      case _: ApparatusF.DeciderNode[?, ?, ?] => true
      case _: ApparatusF.DeciderRef[?, ?, ?]  => true
      case _                                   => false
