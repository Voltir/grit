package grit.core.plugin

import grit.core.id.WorkflowId
import grit.core.period.CloseOrdinal

import utest.*

object PluginTests extends TestSuite {

  private def name(s: String): PluginName =
    PluginName.of(s).getOrElse(throw new java.lang.AssertionError(s))

  val tests = Tests {
    test("a plugin's name is lowercase letters, digits and dashes, from a letter") {
      Vector("digest", "wiki-2").map(PluginName.of(_).map(PluginName.value)) ==> Vector(
        Right("digest"),
        Right("wiki-2")
      )
      val why = "a plugin's name is lowercase letters, digits and dashes, starting with a letter"
      Vector("", "Digest", "2wiki", "a:b", "a b").map(PluginName.of) ==> Vector.fill(5)(Left(why))
    }

    test("a post's workflow id is post:plugin:version:cursor, and parses back") {
      val ref = PostRef(
        name("digest"),
        2,
        CloseOrdinal.of(17).getOrElse(throw new java.lang.AssertionError("o"))
      )
      // A pin of a recorded name: DBOS keeps every post workflow under this id.
      WorkflowId.value(ref.workflowId) ==> "post:digest:2:17"
      PostRef.fromWorkflowId(ref.workflowId) ==> Some(ref)
      Vector(
        "post:digest:2",
        "post:Digest:2:1",
        "post:digest:x:1",
        "post:digest:1:-1",
        "close:c:1:2"
      )
        .map(s => PostRef.fromWorkflowId(WorkflowId(s))) ==> Vector.fill(5)(None)
    }
  }
}
