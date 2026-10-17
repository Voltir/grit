package grit.core.id

import utest.*

object PeriodRefTests extends TestSuite {

  private def period(n: Long): PeriodSeq =
    PeriodSeq.of(n).getOrElse(throw new java.lang.AssertionError(n))

  val tests = Tests {
    test("a period's workflow prefix is name:conversation:period:, for each workflow name") {
      // A pin of recorded names: DBOS keeps every close, settle, triage and shadow workflow
      // under these ids, and the purge finds a period's workflows by these prefixes.
      val p = PeriodRef(ConversationId("0199-abc"), period(2))
      Vector("close", "settle", "triage", "shadow").map(p.workflowPrefix) ==> Vector(
        "close:0199-abc:2:",
        "settle:0199-abc:2:",
        "triage:0199-abc:2:",
        "shadow:0199-abc:2:"
      )
    }
  }
}
