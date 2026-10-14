package grit.turn

import grit.core.id.{DocumentVersion, EntrySeq, ToolCallId}
import grit.core.tool.{Retry, ToolName, ToolSet}
import grit.core.visibility.Integrity

import utest.*

/** What the model is told about hidden items and what reading them costs is in words it may
  * repeat to a person: none of grit's own terms, and no tool's name except where it is told to
  * call one.
  */
object PlainWordsTests extends TestSuite {

  private def entry(name: ToolName, action: String): ToolSet.Entry =
    ToolSet.Entry(
      name,
      "does",
      ujson.Obj(),
      false,
      Retry.Rerun,
      None,
      Integrity.Member,
      Some(action)
    )

  private val offered: ToolSet =
    ToolSet
      .of(
        Vector(
          entry(ToolName("slack_post"), "post to channels"),
          entry(ToolName("remind_me"), "set reminders")
        )
      )
      .getOrElse(throw new java.lang.AssertionError("distinct names"))

  private val version: DocumentVersion =
    DocumentVersion.of(3).getOrElse(throw new java.lang.AssertionError("3 is a version"))

  private val issue =
    Item(ItemRef.Result(ToolCallId("c1")), Integrity.Untrusted, "github_get_issue")
  private val cards = Item(ItemRef.Document(version), Integrity.Untrusted, "Cards")
  private val guest = Item(ItemRef.Own(EntrySeq(41)), Integrity.Outside, "a guest")

  private val h1: HiddenName =
    HiddenName.read("h1").fold(e => throw new java.lang.AssertionError(e), identity)

  /** Each word or name of `text` the model should not repeat to a person, once each. Removed
    * first: an instruction to call read_hidden or extract with a name, and anything quoted.
    */
  private def unplain(text: String): Vector[String] = {
    val said = text
      .replaceAll("call (read_hidden|extract) with name h[0-9]+", "")
      .replaceAll("\"[^\"]*\"", "")
      .toLowerCase
    val terms = Vector("integrity", "stub", "withdraw", "hidden item", "untrusted", "member")
    val tools =
      Vector("slack_post", "remind_me", "github_get_issue", "read_hidden", "extract", "requires")
    (terms ++ tools).filter(said.contains) ++
      Vector("turn").filter(w => s"\\b$w\\b".r.findFirstIn(said).isDefined)
  }

  val tests = Tests {

    test("a stub and a withdrawal say nothing a person would not follow") {
      val costs = Vector(ToolName("slack_post"), ToolName("remind_me"))
      val stubs = Vector(issue, cards, guest).map(TurnGate.stub(_, Fate.Hidden(h1, costs), offered))
      val before = TurnGate.decide(Integrity.Member, offered, Vector.empty, Set.empty, Map.empty)
      val now =
        TurnGate.decide(Integrity.Member, offered, Vector(issue), Set(h1), Map(issue.ref -> h1))
      val line = TurnGate.withdrawn(before, now, offered)
      assert(line.isDefined)
      (stubs ++ line).map(unplain) ==> Vector.fill(4)(Vector.empty[String])
    }
  }
}
