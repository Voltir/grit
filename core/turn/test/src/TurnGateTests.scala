package grit.turn

import grit.core.id.{ConversationId, DocumentVersionId, EntrySeq, ToolCallId}
import grit.core.tool.{Retry, ToolName, ToolSet}
import grit.core.visibility.Integrity.Level
import grit.core.visibility.{Compartment, Integrity}

import utest.*

object TurnGateTests extends TestSuite {

  private val team: Compartment =
    Compartment.of("team").getOrElse(throw new java.lang.AssertionError("team is a compartment"))

  private def entry(name: ToolName, requires: Integrity, action: Option[String]): ToolSet.Entry =
    ToolSet.Entry(name, "does", ujson.Obj(), false, Retry.Rerun, None, requires, action)

  private def set(entries: ToolSet.Entry*): ToolSet =
    ToolSet.of(entries.toVector).getOrElse(throw new java.lang.AssertionError("distinct names"))

  private val post = ToolName("slack_post")
  private val read = ToolName("read")
  private val note = ToolName("note")

  private val offered =
    set(
      entry(post, Integrity.Member, Some("post to channels")),
      entry(read, Integrity.Untrusted, None),
      entry(note, Integrity.Outside, None)
    )

  private val version: DocumentVersionId =
    DocumentVersionId.of(3).getOrElse(throw new java.lang.AssertionError("3 is a version"))

  private val issue =
    Item(ItemRef.Result(ToolCallId("c1")), Integrity.Untrusted, "github_get_issue")
  private val guest = Item(ItemRef.Own(EntrySeq(41)), Integrity.Outside, "a guest")

  private def named(text: String): HiddenName =
    HiddenName.read(text).fold(e => throw new java.lang.AssertionError(e), identity)

  /** The gate from `prior` over `items`, none read and none named before. */
  private def unread(prior: Integrity, tools: ToolSet, items: Vector[Item]): Gated =
    TurnGate.decide(prior, tools, items, Map.empty, Map.empty)

  /** The gate from `prior` over `items`, each of `read` an answer read_hidden gave of a name
    * nothing hides, none hidden before.
    */
  private def reading(prior: Integrity, tools: ToolSet, items: Vector[Item], read: Item*): Gated =
    TurnGate.decide(prior, tools, items, read.map(_.ref -> named("h9")).toMap, Map.empty)

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
      TurnGate.withdrawn(before, now, offered) ==> Some(
        "From now until you answer, you can't post to channels or use note, because you read " +
          "a result from a place people outside the team can write in."
      )
      // Withheld already, it is not withdrawn again; nothing newly withheld says nothing.
      TurnGate.withdrawn(now, unread(Integrity.Untrusted, offered, Vector(guest)), offered) ==> None
    }

    test("the item named is the first whose integrity does not dominate the tool's requirement") {
      val before = unread(Integrity.Member, offered, Vector.empty)
      val now = reading(Integrity.Member, offered, Vector(guest, issue), guest, issue)
      TurnGate.withdrawn(before, now, offered) ==> Some(
        "From now until you answer, you can't post to channels, because you read something " +
          "from a guest, written from outside the team; and you can't use note, because you read a " +
          "result from a place people outside the team can write in."
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
          Decided(issue, Fate.Hidden(named("h1"), Vector(note)))
        )
    }

    test(
      "a hidden item stays as an earlier round recorded it, though the level fell to it; a newly hidden one takes the next name"
    ) {
      // The level has fallen to Outside, which the guest dominates; its costs are as recorded.
      val earlier: Map[ItemRef, Fate.Hidden] =
        Map(guest.ref -> Fate.Hidden(named("h2"), Vector(post, note)))
      TurnGate.decide(Integrity.Outside, offered, Vector(guest), Map.empty, earlier).decided ==>
        Vector(Decided(guest, Fate.Hidden(named("h2"), Vector(post, note))))
      TurnGate
        .decide(Integrity.Member, offered, Vector(issue, guest), Map.empty, earlier)
        .hidden ==>
        Vector(
          issue -> Fate.Hidden(named("h3"), Vector(post, note)),
          guest -> Fate.Hidden(named("h2"), Vector(post, note))
        )
    }

    test("a withdrawal names an item that was shown, never one hidden") {
      val before = unread(Integrity.Member, offered, Vector.empty)
      // The issue is hidden; the guest, read, withholds post.
      val now = TurnGate.decide(
        Integrity.Member,
        offered,
        Vector(issue, guest),
        Map(guest.ref -> named("h9")),
        Map.empty
      )
      TurnGate.withdrawn(before, now, offered) ==> Some(
        "From now until you answer, you can't post to channels, because you read something " +
          "from a guest, written from outside the team."
      )
    }

    test(
      "a stub says where the item comes from, what reading it costs, how to read it, and how to ask it"
    ) {
      TurnGate.stub(issue, Fate.Hidden(named("h1"), Vector(post, note)), offered) ==>
        "Hidden here as h1: a result from a place people outside the team can write in. If " +
        "your answer needs what it says, read it: call read_hidden with name h1. After that " +
        "you can't post to channels or use note until you've answered. To ask it a short " +
        "question instead, answered yes or no, with a whole number, a date, one of options " +
        "you list or a short code, call extract with name h1: that never stops you doing " +
        "anything."
      TurnGate.stub(guest, Fate.Hidden(named("h2"), Vector(post)), offered) ==>
        "Hidden here as h2: something from a guest, written from outside the team. If your " +
        "answer needs what it says, read it: call read_hidden with name h2. After that you " +
        "can't post to channels until you've answered. To ask it a short question instead, " +
        "answered yes or no, with a whole number, a date, one of options you list or a short " +
        "code, call extract with name h2: that never stops you doing anything."
    }

    test("a hidden item's name is h and a number from 1, with no leading zero") {
      Vector("h1", "h12").map(HiddenName.read(_).map(HiddenName.value)) ==>
        Vector(Right("h1"), Right("h12"))
      Vector("h0", "h01", "h", "hx", "x1").map(HiddenName.read(_).isLeft) ==>
        Vector(true, true, true, true, true)
    }
  }
}
