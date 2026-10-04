package grit.eval.harness.jev

import grit.core.id.QuestionName
import grit.core.triage.{Earning, Tags}

import utest.*

/** The question sets a comparison names, each by the gate live triage drafted by while it was
  * live.
  */
object SetsTests extends TestSuite {

  val tests = Tests {
    test("v1 to v4 are named, each with its own gate, its durable and the to it asks") {
      Sets.all.map(s => (s.name, s.speak, s.durable, s.to.map(QuestionName.value))) ==> Vector(
        ("v1", Tags.V1.gate, Some(Earning.Durable), None),
        ("v2", Tags.V2.drafts, Some(Earning.Durable), Some("to")),
        ("v3", Tags.V3.drafts, Some(Earning.Durable), Some("to")),
        ("v4", Tags.V4.drafts, Some(Earning.Durable), Some("to"))
      )
    }
  }
}
