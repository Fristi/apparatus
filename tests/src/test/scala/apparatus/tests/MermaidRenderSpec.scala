package apparatus.tests

import apparatus.core.{Apparatus}
import apparatus.core.fix.alg.Mermaid
import apparatus.core.machines.OpenMealy
import cats.Id
import cats.implicits.*
import munit.FunSuite

import sys.process.*
import java.nio.charset.StandardCharsets
import java.nio.file.Files

/** Validates each Apparatus combinator's Mermaid output actually renders via
 *  minlag/mermaid-cli. Each test writes a .mmd file, runs the CLI container,
 *  and asserts a valid SVG is produced.
 */
class MermaidRenderSpec extends FunSuite:

  // ── helpers ──────────────────────────────────────────────────────────────────

  private def node[I, O](f: I => O): Apparatus[Id, I, O] =
    Apparatus.openMealy(OpenMealy.stateless[Id, I, O](i => f(i)))

  private def named[I, O](label: String)(f: I => O): Apparatus[Id, I, O] =
    Apparatus.labeled(label)(node(f))

  /** Writes `diagram` to a temp dir, runs mermaid-cli, asserts SVG output. */
  private def assertRenders(diagram: String)(using loc: munit.Location): Unit =
    val tmp    = Files.createTempDirectory("mermaid-render-")
    val input  = tmp.resolve("input.mmd")
    val output = tmp.resolve("output.svg")
    Files.writeString(input, diagram, StandardCharsets.UTF_8)

    val exit =
      Seq(
        "docker", "run", "--rm",
        "-v", s"${tmp.toAbsolutePath}:/data",
        "minlag/mermaid-cli:8.8.0",
        "-i", "/data/input.mmd",
        "-o", "/data/output.svg"
      ).!


    assert(Files.exists(output), s"SVG not created.\nDiagram:\n$diagram")
    val svg = Files.readString(output, StandardCharsets.UTF_8)
    assert(svg.contains("<svg"), s"Output not valid SVG (first 300 chars):\n${svg.take(300)}\nDiagram:\n$diagram")

  // ── tests ─────────────────────────────────────────────────────────────────

  test("sequential renders") {
    assertRenders(Mermaid.print(Apparatus.sequential(
      named[Int, Int]("StepA")(identity),
      named[Int, Int]("StepB")(identity)
    )))
  }

  test("parallel renders") {
    assertRenders(Mermaid.print(Apparatus.parallel(
      named[Int, Int]("Left")(identity),
      named[Int, Int]("Right")(identity)
    )))
  }

  test("alternative renders") {
    assertRenders(Mermaid.print(Apparatus.alternative(
      named[Int, Int]("LeftBranch")(identity),
      named[Int, Int]("RightBranch")(identity)
    )))
  }

  test("merged renders") {
    assertRenders(Mermaid.print(Apparatus.merged(
      named[Int, List[Int]]("A")(i => List(i)),
      named[Int, List[Int]]("B")(i => List(i))
    )))
  }

  test("feedback renders") {
    assertRenders(Mermaid.print(Apparatus.feedback(
      named[Int, List[Int]]("Forward")(i => List(i)),
      named[Int, List[Int]]("Reverse")(i => List(i))
    )))
  }

  test("feedbackMany renders") {
    assertRenders(Mermaid.print(Apparatus.feedbackMany(
      named[Int, List[Int]]("Forward")(i => List(i)),
      named[Int, List[Int]]("Reverse")(i => List(i))
    )))
  }

  test("lmapOrEmpty renders") {
    assertRenders(Mermaid.print(Apparatus.lmapOrEmpty[Id, Int, List[Int], String](
      named[Int, List[Int]]("Inner")(i => List(i)),
      { case s => s.length }
    )))
  }

  test("labeled subgraph renders") {
    assertRenders(Mermaid.print(Apparatus.labeled("Group")(
      Apparatus.sequential(
        named[Int, Int]("A")(identity),
        named[Int, Int]("B")(identity)
      )
    )))
  }
