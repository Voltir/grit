package grit.turn

import grit.core.flow.TestTainted
import grit.core.id.{EntryId, EntrySeq, TestCallSlots, ToolCallId}
import grit.core.message.AssistantBlock
import grit.core.store.{Entry, Payload}
import grit.core.tool.{Believed, Bound, Outcome, Repairs, ToolName, Toolbox}
import grit.core.visibility.Integrity

import utest.*

/** [[ReadHidden]]: what a call of `read_hidden` is answered, and the names a turn's replies
  * read through it.
  */
object ReadHiddenTests extends TestSuite {

  private def named(text: String): HiddenName =
    HiddenName.read(text).fold(e => throw new java.lang.AssertionError(e), identity)

  private val issue =
    Item(ItemRef.Result(ToolCallId("c1")), Integrity.Untrusted, "github_get_issue")

  private val gated = Gated(
    Integrity.Member,
    Vector(Decided(issue, Fate.Hidden(named("h1"), Vector(ToolName("slack_post"))))),
    Vector.empty
  )

  /** What a call of the tool `gated` offers, with `args`, is answered. */
  private def asked(gated: Option[Gated], args: ujson.Value): Outcome =
    Toolbox
      .of(ReadHidden.tool(gated))
      .fold(d => throw new java.lang.AssertionError(s"duplicate $d"), identity)
      .bind(
        TestTainted.at(
          AssistantBlock.ToolCall(ToolCallId("c2"), "read_hidden", args),
          Integrity.Untrusted
        ),
        Repairs.All
      ) match {
      case Right(b: Bound.Free) => b(TestCallSlots.First)
      case other => throw new java.lang.AssertionError(s"not free: $other")
    }

  /** A reply of the turn's tool loop calling `calls` (tool name, arguments). */
  private def exchange(seq: Long, calls: (String, ujson.Value)*): Entry =
    Entry(
      EntryId(s"call:$seq"),
      grit.core.id.ConversationId("c"),
      grit.core.id.TurnSeq(1),
      None,
      EntrySeq(seq),
      Payload.Exchange(
        TurnFixtures.said(
          "",
          calls.zipWithIndex.map { case ((name, args), i) =>
            AssistantBlock.ToolCall(ToolCallId(s"t$seq$i"), name, args)
          }*
        )
      ),
      java.time.Instant.EPOCH,
      Integrity.Member
    )

  val tests = Tests {
    test("a call naming a hidden item is answered that it is shown next, believed at the item") {
      asked(Some(gated), ujson.Obj("name" -> "h1")) ==> Outcome.Done(
        "h1 is shown in full in its place from your next request on.",
        Some(Integrity.Untrusted)
      )
    }

    test("a call naming nothing hidden is answered so, as grit's own words") {
      val none = Outcome.Done("Nothing in the request you answered is hidden as h2.")
      asked(Some(gated), ujson.Obj("name" -> "h2")) ==> none
      asked(None, ujson.Obj("name" -> "h2")) ==> none
      ReadHidden.tool(None).output ==> Believed.Own
    }

    test("its description says what reading costs and when to read, in plain words") {
      ReadHidden.tool(None).spec.does ==>
        "Shows in full something your request shows only as a note (\"Hidden here as h1: " +
        "…\"), in its place, from your next request on. After you read it, you can't do what " +
        "that note says you'd lose until you've answered. Read it when your answer needs what " +
        "it says."
    }

    test("the names read are those read_hidden was called with, unreadable arguments skipped") {
      ReadHidden.read(
        Vector(
          exchange(1, "read_hidden" -> ujson.Obj("name" -> "h2"), "peek" -> ujson.Obj()),
          exchange(
            2,
            "read_hidden" -> ujson.Obj("name" -> "x"),
            "peek" -> ujson.Obj("name" -> "h3")
          ),
          exchange(3, "read_hidden" -> ujson.Obj("name" -> "h1"))
        )
      ) ==> Set(named("h1"), named("h2"))
    }
  }
}
