package grit.projects.method

import grit.projects.method.Example.{Expect, Move}
import grit.projects.method.MethodFixtures.*

import utest.*

object MethodBenchTests extends TestSuite {

  private val declared: Method =
    right(
      Method
        .of(method, 1, Vector(change, note), transitions, views, TransitionChecks.all)
        .left
        .map(_.mkString("; "))
    )

  private val why = artifact(head("p/why", change, "proposed"), "## Why\n\nBecause.")
  private val bare = artifact(head("p/bare", change, "proposed"), "No reason given.")

  val tests = Tests {
    test("examples whose moves come out as expected pass, an allowed move seen by the next") {
      val examples = Vector(
        Example(
          "a change with its why is done",
          project(why),
          Vector(
            Move(key("p/why"), state("building"), Expect.Allowed),
            Move(key("p/why"), state("done"), Expect.Allowed)
          )
        ),
        Example(
          "a change without its why is held",
          project(bare),
          Vector(
            Move(key("p/bare"), state("building"), Expect.Allowed),
            Move(key("p/bare"), state("done"), Expect.RefusedBy(check("sections-present")))
          )
        )
      )
      assert(MethodBench.failures(declared, examples) == Vector.empty)
    }
    test("an example is reported at its first move to come out otherwise, with what it met") {
      val wrong = Move(key("p/bare"), state("done"), Expect.Allowed)
      val examples = Vector(
        Example(
          "passes",
          project(why),
          Vector(Move(key("p/why"), state("building"), Expect.Allowed))
        ),
        Example(
          "expects a bare change done",
          project(bare),
          Vector(
            Move(key("p/bare"), state("building"), Expect.Allowed),
            wrong,
            Move(key("p/bare"), state("proposed"), Expect.Allowed)
          )
        )
      )
      val building = bare.head.copy(state = state("building"))
      assert(
        MethodBench.failures(declared, examples) == Vector(
          MethodBench.Failed(
            "expects a bare change done",
            2,
            wrong,
            Some(
              MoveRefusal.Unheld(
                building,
                state("done"),
                Vector(check("sections-present") -> "its body lacks ## Why")
              )
            )
          )
        )
      )
    }
  }
}
