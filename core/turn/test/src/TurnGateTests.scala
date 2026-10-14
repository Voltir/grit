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

  private def named(text: String): HiddenName =
    HiddenName.read(text).fold(e => throw new java.lang.AssertionError(e), identity)

  /** The gate from `prior` over `items`, none read and none named before. */
  private def unread(prior: Integrity, tools: ToolSet, items: Vector[Item]): Gated =
    TurnGate.decide(prior, tools, items, Set.empty, Map.empty)

  /** The gate from `prior` over `items`, each of `read` read through `read_hidden` under the
    * name an earlier round gave it, `h1` onward in order.
    */
  private def reading(prior: Integrity, tools: ToolSet, items: Vector[Item], read: Item*): Gated = {
    val names = read.zipWithIndex.map((item, i) => item.ref -> named(s"h${i + 1}")).toMap
    TurnGate.decide(prior, tools, items, names.values.toSet, names)
  }

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
      unread(prior, ToolSet.Empty, Vector(own, wide, guest)) ==>
        Gated(
          Integrity.Outside,
          Vector(Decided(wide, Fate.Lowered), Decided(guest, Fate.Lowered)),
          Vector.empty
        )
    }

    test("the offered tools whose requirement the level reached does not dominate are withheld") {
      reading(Integrity.Member, offered, Vector(guest), guest).withheld ==>
        Vector(Withheld(post, Integrity.Member))
      unread(Integrity.Member, offered, Vector.empty).withheld ==> Vector.empty
    }

    test("a tool withheld now and offered the round before is named, with the item that cost it") {
      val before = unread(Integrity.Member, offered, Vector.empty)
      val now = reading(Integrity.Member, offered, Vector(issue), issue)
      TurnGate.withdrawn(before, now) ==> Some(
        "Withdrawn for the rest of this turn: slack_post, which requires [integrity: member], " +
          "since you read github_get_issue [integrity: untrusted]; note, which requires " +
          "[integrity: outside], since you read github_get_issue [integrity: untrusted]."
      )
      // Withheld already, it is not withdrawn again; nothing newly withheld says nothing.
      TurnGate.withdrawn(now, unread(Integrity.Untrusted, offered, Vector(guest))) ==> None
    }

    test("the item named is the first whose integrity does not dominate the tool's requirement") {
      val before = unread(Integrity.Member, offered, Vector.empty)
      val now = reading(Integrity.Member, offered, Vector(guest, issue), guest, issue)
      TurnGate.withdrawn(before, now) ==> Some(
        "Withdrawn for the rest of this turn: slack_post, which requires [integrity: member], " +
          "since you read a guest [integrity: outside]; note, which requires " +
          "[integrity: outside], since you read github_get_issue [integrity: untrusted]."
      )
    }

    test("an item whose reading would withdraw a tool offered now is hidden, naming those tools") {
      unread(Integrity.Member, offered, Vector(issue)) ==>
        Gated(
          Integrity.Member,
          Vector(Decided(issue, Fate.Hidden(named("h1"), Vector(post, note)))),
          Vector.empty
        )
    }

    test("an item read through read_hidden is shown and lowers, though it costs a tool") {
      reading(Integrity.Member, offered, Vector(issue), issue) ==>
        Gated(
          Integrity.Untrusted,
          Vector(Decided(issue, Fate.Lowered)),
          Vector(Withheld(post, Integrity.Member), Withheld(note, Integrity.Outside))
        )
    }

    test("an item's cost is the tools offered at the level the items before it reached") {
      // The guest, read, withholds post: the issue then costs note alone.
      reading(Integrity.Member, offered, Vector(guest, issue), guest).decided ==>
        Vector(
          Decided(guest, Fate.Lowered),
          Decided(issue, Fate.Hidden(named("h2"), Vector(note)))
        )
    }

    test("a hidden item keeps the name an earlier round gave it; a newly hidden one, the next") {
      val earlier = Map(guest.ref -> named("h2"))
      TurnGate
        .decide(Integrity.Member, offered, Vector(issue, guest), Set.empty, earlier)
        .hidden ==>
        Vector(
          issue -> Fate.Hidden(named("h3"), Vector(post, note)),
          guest -> Fate.Hidden(named("h2"), Vector(post))
        )
    }

    test("a withdrawal names an item that was shown, never one hidden") {
      val before = unread(Integrity.Member, offered, Vector.empty)
      // The issue is hidden; the guest, read, withholds post.
      val now = TurnGate.decide(
        Integrity.Member,
        offered,
        Vector(issue, guest),
        Set(named("h1")),
        Map(guest.ref -> named("h1"))
      )
      TurnGate.withdrawn(before, now) ==> Some(
        "Withdrawn for the rest of this turn: slack_post, which requires [integrity: member], " +
          "since you read a guest [integrity: outside]."
      )
    }

    test("a stub names the item, its source and level, what reading it withdraws, and how") {
      TurnGate.stub(issue, Fate.Hidden(named("h1"), Vector(post, note))) ==>
        "Hidden here as h1: something from github_get_issue, believed only at " +
        "[integrity: untrusted]. Reading it would withdraw slack_post, note for the rest of " +
        "this turn; to read it, call read_hidden with name h1."
    }

    test("a hidden item's name is h and a number from 1, with no leading zero") {
      Vector("h1", "h12").map(HiddenName.read(_).map(HiddenName.value)) ==>
        Vector(Right("h1"), Right("h12"))
      Vector("h0", "h01", "h", "hx", "x1").map(HiddenName.read(_).isLeft) ==>
        Vector(true, true, true, true, true)
    }
  }
}
