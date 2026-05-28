package apparatus.core.fix.alg

import apparatus.core
import apparatus.core.Apparatus
import apparatus.core.fix.ApparatusF
import cats.Monad

object Mermaid:

  /** Renders `apparatus` as a Mermaid `graph TD` diagram string.
    *
    * Normalises the network first (deduplicating aggregate machines into `Ref` nodes), then
    * performs a recursive depth-first traversal to emit node declarations and edge statements.
    *
    * The returned string can be embedded in a Markdown fenced code block tagged `mermaid` to
    * produce a visualisation in tools that support it (GitHub, Notion, etc.).
    *
    * @param apparatus the network to render
    * @return a Mermaid diagram as a plain string
    */
  def print[Eff[_] : Monad, I, O](apparatus: Apparatus[Eff, I, O]): String =
    val (_, _, normalized) = normalize(apparatus)
    val ctx = Context()
    render(normalized, ctx)
    "graph TD\n" + ctx.declarations.mkString("\n") + "\n" + ctx.edges.mkString("\n")

  /** Mutable accumulator used during a single [[render]] traversal.
    *
    * Keeps node declarations and edge statements in insertion order so the emitted Mermaid
    * source is human-readable and deterministic.
    */
  private class Context:
    private var counter = 0
    val declarations = collection.mutable.ListBuffer.empty[String]
    val edges        = collection.mutable.ListBuffer.empty[String]

    /** Allocates a unique Mermaid node ID with the given `prefix` (e.g. `"node"`, `"split"`). */
    def fresh(prefix: String): String =
      counter += 1
      s"${prefix}_$counter"

    /** Emits a node declaration with the given `id`, display `label`, and `shape`. */
    def node(id: String, label: String, shape: Shape): Unit =
      declarations += shape.mermaid(id, label)

    /** Emits a directed edge from `from` to `to`, optionally annotated with `label`. */
    def edge(from: String, to: String, label: String = ""): Unit =
      if label.isEmpty then edges += s"    $from --> $to"
      else edges += s"    $from -->|$label| $to"

    /** Opens a named Mermaid subgraph block. Must be closed with [[closeSubgraph]]. */
    def openSubgraph(label: String): Unit =
      declarations += s"""    subgraph "$label" """

    /** Closes the most recently opened subgraph block. */
    def closeSubgraph(): Unit =
      declarations += "    end"

  /** Mermaid node shape variants.
    *
    *   - `Box`     — rectangular node `[label]` used for machines and references
    *   - `Diamond` — decision/routing node `{label}` used for aggregate machines and filters
    *   - `Stadium` — rounded-rectangle node `([label])` used for split/join and feedback ports
    */
  private enum Shape:
    case Box, Diamond, Stadium
    /** Returns the Mermaid syntax string for a node with this shape. */
    def mermaid(id: String, label: String): String = this match
      case Box     => s"""    $id["$label"]"""
      case Diamond => s"""    $id{"$label"}"""
      case Stadium => s"""    $id(["$label"])"""

  /** Returns (inputPortId, outputPortId) so callers can wire edges. */
  private def render[Eff[_], I, O](apparatus: Apparatus[Eff, I, O], ctx: Context): (String, String) =
    apparatus.unfix match

      case ApparatusF.AggregateMachine(name, _) =>
        val id = ctx.fresh("node")
        ctx.node(id, name, Shape.Diamond)
        (id, id)

      case ApparatusF.Ref(networkId) =>
        val id = ctx.fresh("node")
        ctx.node(id, networkId, Shape.Box)
        (id, id)

      case ApparatusF.OpenMachine(_) =>
        val id = ctx.fresh("node")
        ctx.node(id, "Machine", Shape.Box)
        (id, id)

      case ApparatusF.ClosedMachine(_) =>
        val id = ctx.fresh("node")
        ctx.node(id, "ClosedMachine", Shape.Stadium)
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
        ctx.edge(lOut, rIn, "B")
        ctx.edge(rOut, lIn, "A ↺")
        (lIn, lOut)

      case ApparatusF.FeedbackMany(left, right, _, _, _) =>
        val (lIn, lOut) = render(left, ctx)
        val (rIn, rOut) = render(right, ctx)
        ctx.edge(lOut, rIn, "N[B]")
        ctx.edge(rOut, lIn, "N[A] ↺")
        (lIn, lOut)

      case ApparatusF.LmapOrEmpty(inner, _, _) =>
        val filterId = ctx.fresh("filter")
        ctx.node(filterId, "lmapOrEmpty", Shape.Diamond)
        val (iIn, iOut) = render(inner, ctx)
        ctx.edge(filterId, iIn, "defined")
        (filterId, iOut)

      case ApparatusF.Merged(left, right, _) =>
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

      case ApparatusF.Labeled(inner, name) =>
        inner.unfix match
          case ApparatusF.LmapOrEmpty(innerInner, _, _) =>
            val filterId = ctx.fresh("filter")
            ctx.node(filterId, name, Shape.Diamond)
            val (iIn, iOut) = render(innerInner, ctx)
            ctx.edge(filterId, iIn, "defined")
            (filterId, iOut)
          case _ if isLeaf(inner) =>
            val id = ctx.fresh("node")
            ctx.node(id, name, Shape.Box)
            (id, id)
          case _ =>
            ctx.openSubgraph(name)
            val result = render(inner, ctx)
            ctx.closeSubgraph()
            result

  /** Returns `true` when `apparatus` is a leaf node that renders as a single Mermaid node.
    *
    * Used by the [[render]] `Labeled` case to decide whether to emit an inline label or a
    * subgraph block: leaf nodes get a simple renamed box; composite nodes get a subgraph.
    */
  private def isLeaf[Eff[_], I, O](apparatus: Apparatus[Eff, I, O]): Boolean =
    apparatus.unfix match
      case _: ApparatusF.Ref[?, ?, ?, ?]         => true
      case _: ApparatusF.OpenMachine[?, ?, ?, ?] => true
      case _                                     => false
