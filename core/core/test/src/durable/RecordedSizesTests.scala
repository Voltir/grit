package grit.core.durable

import grit.core.durable.InMemoryDurable.{Outcome, Step}
import grit.core.id.WorkflowId

import utest.*

object RecordedSizesTests extends TestSuite {

  private def history(steps: Step*): History =
    History("w", WorkflowId("w:1"), "e", "s", steps.toVector)

  /** A step recording `bytes` bytes of output. */
  private def sized(name: String, bytes: Int): Step = Step(name, Outcome.Output("x" * bytes))

  private def breaches(budgets: Map[String, Int], whole: Int)(steps: Step*): Vector[String] =
    RecordedSizes.breaches(
      Vector("h.json" -> history(steps*)),
      RecordedSizes.familyOf,
      budgets,
      whole
    )

  val tests = Tests {
    test("a step records its output's UTF-8 bytes, its error's message, and nothing as a marker") {
      (
        RecordedSizes.of(Step("s", Outcome.Output("é1"))),
        RecordedSizes.of(Step("s", Outcome.Threw(Some("boom")))),
        RecordedSizes.of(Step("s", Outcome.Threw(None))),
        RecordedSizes.of(Step("s", Outcome.Marker))
      ) ==> (3, 4, 0, 0)
    }

    test(
      "a step over its family's budget is a breach naming its history, step, bytes and budget; one at it is none"
    ) {
      breaches(Map("post" -> 10), 1000)(sized("post:1", 10), sized("post:2", 11)) ==> Vector(
        "h.json: step post:2 records 11 bytes, over the budget of 10 for post: content with a " +
          "content-addressed home is recorded by its id"
      )
    }

    test("a step whose family has no budget is a breach naming it, whatever its size") {
      breaches(Map("post" -> 10), 1000)(sized("post:1", 1), Step("close", Outcome.Marker)) ==>
        Vector("h.json: step close, of family close, has no budget: give it one")
    }

    test(
      "a history over the whole budget in all is a breach, though each step fits; one at it is none"
    ) {
      val budgets = Map("post" -> 10)
      (
        breaches(budgets, 20)(sized("post:1", 10), sized("post:2", 10)),
        breaches(budgets, 19)(sized("post:1", 10), sized("post:2", 10))
      ) ==> (
        Vector.empty,
        Vector("h.json: its steps record 20 bytes in all, over the budget of 19")
      )
    }

    test("a step's family is its name's first part, with a move's phase kept from its last") {
      Vector("close", "post:3", "ask:a:record", "ask:a:b:settle").map(RecordedSizes.familyOf) ==>
        Vector("close", "post", "ask:record", "ask:settle")
    }
  }
}
