package apparatus.core

import cats.Id
import cats.implicits.*

class FeedbackSpec extends munit.FunSuite:

  private def stateless[A, B](f: A => List[B]): FSM[Id, A, List[B]] =
    FSM.Basic(BaseMachineT.stateless[Id, A, List[B]](a => f(a)))

  test("no feedback: right returns empty, output is single left result"):
    val fsm = FSM.feedback(
      stateless((n: Int) => List(n)),
      stateless((_: Int) => List.empty[Int])
    )
    val (out, _) = FSM.run(fsm, 5)
    assertEquals(out, List(5))

  test("countdown: input n expands to List(n, n-1, ..., 0)"):
    val fsm = FSM.feedback(
      stateless((n: Int) => List(n)),
      stateless((n: Int) => if n > 0 then List(n - 1) else Nil)
    )
    val (out, _) = FSM.run(fsm, 3)
    assertEquals(out, List(3, 2, 1, 0))

  test("right machine state is preserved across feedback iterations within one step"):
    // right fires back 99 only on its first invocation
    val right = FSM.Basic(BaseMachineT[Id, Int, Int, List[Int]](
      0,
      (count, _) => if count == 0 then (List(99), 1) else (Nil, count)
    ))
    val fsm = FSM.feedback(stateless((n: Int) => List(n)), right)

    val (out, _) = FSM.run(fsm, 5)
    assertEquals(out, List(5, 99))

  test("right machine state carries over to the next top-level step"):
    val right = FSM.Basic(BaseMachineT[Id, Int, Int, List[Int]](
      0,
      (count, _) => if count == 0 then (List(99), 1) else (Nil, count)
    ))
    val fsm          = FSM.feedback(stateless((n: Int) => List(n)), right)
    val (_, fsm2)    = FSM.run(fsm, 5)   // right fires once here
    val (out2, _)    = FSM.run(fsm2, 5)  // right exhausted, no feedback
    assertEquals(out2, List(5))

  test("dfs ordering: right produces two feedback items, matches Haskell runMultiple semantics"):
    // right(n) = [n-1, n-2] if n > 1 else []
    // input 3 → DFS: [3, 2, 1, 0, 1],  BFS would give [3, 2, 1, 1, 0]
    val fsm = FSM.feedback(
      stateless((n: Int) => List(n)),
      stateless((n: Int) => if n > 1 then List(n - 1, n - 2) else Nil)
    )
    val (out, _) = FSM.run(fsm, 3)
    assertEquals(out, List(3, 2, 1, 0, 1))

  test("multiple inputs processed sequentially each with independent feedback"):
    val fsm = FSM.feedback(
      stateless((n: Int) => List(n)),
      stateless((n: Int) => if n > 0 then List(n - 1) else Nil)
    )
    val (out1, fsm2) = FSM.run(fsm, 2)
    val (out2, _)    = FSM.run(fsm2, 1)
    assertEquals(out1, List(2, 1, 0))
    assertEquals(out2, List(1, 0))
