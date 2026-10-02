package grit.core.id

import utest.*

object ShadowRefTests extends TestSuite {

  private def period(n: Long): PeriodSeq =
    PeriodSeq.of(n).getOrElse(throw new java.lang.AssertionError(n))

  private def name(s: String): ShadowName =
    ShadowName.of(s).getOrElse(throw new java.lang.AssertionError(s))

  val tests = Tests {
    test("a shadow's workflow id is shadow:conversation:period:turn:name, and parses back") {
      val p = PeriodRef(ConversationId("0199-abc"), period(2))
      val shadow = ShadowRef(TriageRef(p, TurnSeq(7)), name("words-2"))
      // A pin of a recorded name: DBOS keeps every shadow workflow under this id, and the
      // purge finds a period's shadows by its prefix.
      WorkflowId.value(shadow.workflowId) ==> "shadow:0199-abc:2:7:words-2"
      ShadowRef.fromWorkflowId(shadow.workflowId) ==> Some(shadow)
      ShadowRef.prefix(p) ==> "shadow:0199-abc:2:"
    }

    test("an id that is not a shadow's parses to None, a triage's among them") {
      val not =
        List(
          "triage:c:2:7",
          "shadow:c:2:7",
          "shadow:c:0:7:w",
          "shadow:c:2:-1:w",
          "shadow::2:7:w",
          "shadow:c:2:7:Words",
          "shadow:c:2:7:"
        )
      not.map(s => ShadowRef.fromWorkflowId(WorkflowId(s))) ==> not.map(_ => None)
    }

    test("a shadow's name is lowercase letters, digits and dashes, so it holds no colon") {
      List("words", "replica-2", "0").map(ShadowName.of(_).isRight) ==> List(true, true, true)
      List("", "Words", "a:b", "a b", "a_b").map(ShadowName.of(_).isLeft) ==>
        List(true, true, true, true, true)
    }
  }
}
