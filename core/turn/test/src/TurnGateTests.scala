package grit.turn

import grit.core.id.{ConversationId, DocumentVersion, EntrySeq, ToolCallId}
import grit.core.tool.{Retry, ToolName, ToolSet}
import grit.core.visibility.Integrity.Level
import grit.core.visibility.{Compartment, Integrity}

import utest.*

object TurnGateTests extends TestSuite {

  private val team: Compartment =
    Compartment.of("team").getOrElse(throw new java.lang.AssertionError("team is a compartment"))

  private def entry(name: ToolName, requires: Integrity): ToolSet.Entry =
    ToolSet.Entry(name, "does", ujson.Obj(), false, Retry.Rerun, None, requires)

  private def set(entries: ToolSet.Entry*): ToolSet =
    ToolSet.of(entries.toVector).getOrElse(throw new java.lang.AssertionError("distinct names"))

  private val post = ToolName("slack_post")
  private val read = ToolName("read")
  private val note = ToolName("note")

  private val offered =
    set(
      entry(post, Integrity.Member),
      entry(read, Integrity.Untrusted),
      entry(note, Integrity.Outside)
    )

  private val version: DocumentVersion =
    DocumentVersion.of(3).getOrElse(throw new java.lang.AssertionError("3 is a version"))

  private val issue =
    Item(ItemRef.Result(ToolCallId("c1")), Integrity.Untrusted, "github_get_issue")
  private val guest = Item(ItemRef.Own(EntrySeq(41)), Integrity.Outside, "a guest")

  val tests = Tests {
    test(
      "an item below the prior level is shown and lowers it to the meet; one above is not listed"
    ) {
      val prior = Integrity.at(Level.Member, team)
      // Authority with no compartment does not dominate Member in team: it lowers to Member.
      val wide = Item(ItemRef.Document(version), Integrity.Authority, "notes")
      val own = Item(
        ItemRef.Nearby(ConversationId("c2"), EntrySeq(4)),
        Integrity.at(Level.Authority, team),
        "Ann"
      )
      TurnGate.decide(prior, ToolSet.Empty, Vector(own, wide, guest)) ==>
        Gated(
          Integrity.Outside,
          Vector(Decided(wide, Fate.Lowered), Decided(guest, Fate.Lowered)),
          Vector.empty
        )
    }

    test("the offered tools whose requirement the level reached does not dominate are withheld") {
      TurnGate.decide(Integrity.Member, offered, Vector(guest)).withheld ==>
        Vector(Withheld(post, Integrity.Member))
      TurnGate.decide(Integrity.Member, offered, Vector.empty).withheld ==> Vector.empty
    }

    test("a tool withheld now and offered the round before is named, with the item that cost it") {
      val before = TurnGate.decide(Integrity.Member, offered, Vector.empty)
      val now = TurnGate.decide(Integrity.Member, offered, Vector(issue))
      TurnGate.withdrawn(before, now) ==> Some(
        "Withdrawn for the rest of this turn: slack_post, which requires [integrity: member], " +
          "since you read github_get_issue [integrity: untrusted]; note, which requires " +
          "[integrity: outside], since you read github_get_issue [integrity: untrusted]."
      )
      // Withheld already, it is not withdrawn again; nothing newly withheld says nothing.
      TurnGate.withdrawn(now, TurnGate.decide(Integrity.Untrusted, offered, Vector(guest))) ==> None
    }

    test("the item named is the first whose integrity does not dominate the tool's requirement") {
      val before = TurnGate.decide(Integrity.Member, offered, Vector.empty)
      val now = TurnGate.decide(Integrity.Member, offered, Vector(guest, issue))
      TurnGate.withdrawn(before, now) ==> Some(
        "Withdrawn for the rest of this turn: slack_post, which requires [integrity: member], " +
          "since you read a guest [integrity: outside]; note, which requires " +
          "[integrity: outside], since you read github_get_issue [integrity: untrusted]."
      )
    }

    test("a hidden item's name is h and a number from 1, with no leading zero") {
      Vector("h1", "h12").map(HiddenName.read(_).map(HiddenName.value)) ==>
        Vector(Right("h1"), Right("h12"))
      Vector("h0", "h01", "h", "hx", "x1").map(HiddenName.read(_).isLeft) ==>
        Vector(true, true, true, true, true)
    }
  }
}
