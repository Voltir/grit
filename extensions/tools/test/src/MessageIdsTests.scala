package grit.tools

import java.time.Instant

import grit.core.flow.TestTainted
import grit.core.id.{ConversationId, EntryId, Href, SourceId, TestCallSlots, ToolCallId, TurnRef}
import grit.core.identity.{Principal, TestAccounts}
import grit.core.inbox.InboundId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.recipe.InMemoryRoomReads
import grit.core.store.{
  Askers,
  Db,
  Entry,
  InMemoryEntryStore,
  InMemoryPrincipals,
  Origin,
  Payload,
  StoreError,
  Tx
}
import grit.core.tool.{Bound, Outcome, Repairs, Toolbox}
import grit.core.visibility.{Clearance, Integrity, Label, Subject}
import grit.dbos.sql.TestTx

import utest.*

/** [[MessageIds]]: this conversation's id, and the asker's first or last message in the room,
  * across its conversations, with its link.
  */
object MessageIdsTests extends TestSuite {

  private val ana = TestAccounts.sourced("slack:T1/U1")
  private val ben = TestAccounts.account("slack:T1/U2")

  /** Ana's direct messages: each top-level message a conversation of its own. */
  private def dm(thread: String): Origin = Origin.Direct(ana, thread)

  private val room = dm("1.0").room

  private val Said = Instant.parse("2026-10-21T14:03:00Z")

  private def link(p: String) = Href.of(s"https://acme.slack.com/archives/D1/$p")

  /** A world of Ana's direct messages: what each conversation holds, who wrote each message and
    * the link it was recorded with.
    */
  private final class World {
    val entries = new InMemoryEntryStore
    val principals = new InMemoryPrincipals
    // Replaced whole by the test's own calls, on its one thread; nothing else holds it.
    @caps.unsafe.untrackedCaptures
    private var origins = Map.empty[ConversationId, Origin]
    val rooms = new InMemoryRoomReads(
      entries,
      c => origins.getOrElse(c, Origin.Task("none", ConversationId.value(c))),
      principals
    )

    /** `text`, said in `thread`'s conversation `minutes` after [[Said]], by `by` with `linked`,
      * as that conversation's turn `turn`.
      */
    def say(
        thread: String,
        text: String,
        minutes: Int,
        linked: Option[Href],
        by: grit.core.identity.Account = ana,
        turn: Int = 0
    ): Entry = {
      val c = ConversationId(dm(thread).place.written)
      origins = origins.updated(c, dm(thread))
      val e = Entry(
        InboundId.of(c, SourceId(s"$thread.$turn")),
        c,
        grit.core.id.TurnSeq(turn.toLong),
        None,
        grit.core.id.EntrySeq(turn.toLong),
        Payload.Message(Message.User(text)),
        Said.plusSeconds(60L * minutes),
        Integrity.Member
      )
      entries.insert(e)(using TestTx.fake).fold(err => sys.error(err.toString), identity)
      principals.authored(e.id, by, linked)
      e
    }

    /** grit's reply in `thread`'s conversation, at its turn `turn`. */
    def reply(thread: String, minutes: Int, turn: Int): Unit = {
      val c = ConversationId(dm(thread).place.written)
      val usage = Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None)
      val m = Message.Assistant(Vector(AssistantBlock.Text("Hi.")), StopReason.EndTurn, usage, "m")
      entries
        .insert(
          Entry(
            EntryId(s"reply:$thread:$turn"),
            c,
            grit.core.id.TurnSeq(turn.toLong),
            None,
            grit.core.id.EntrySeq(turn.toLong + 1),
            Payload.Message(m),
            Said.plusSeconds(60L * minutes),
            Integrity.Member
          )
        )(using TestTx.fake)
        .fold(err => sys.error(err.toString), identity)
    }

    /** `message_ids` asked for `message`, in the turn `answering` begins, by `asker`, read in
      * Ana's direct room unless `roomless`, from a store that fails when `down`.
      */
    def ask(
        message: String,
        answering: Entry,
        asker: Option[Principal] = Some(Principal.Person(TestAccounts.principalId(ana), Set.empty)),
        roomless: Boolean = false,
        down: Boolean = false
    ): Outcome = {
      val at = TestCallSlots.at(TurnRef(answering.conversationId, answering.turnSeq))
      val askers: Askers = new Askers {
        def of(turn: TurnRef)(using Tx^): Either[StoreError, Option[grit.core.identity.Principal]] =
          Right(asker)
      }
      val clearance =
        if (roomless) Clearance.of(Label.Public)
        else Clearance.inRoom(room, Label.Public, Label.Public)
      val store: Db^ = new Db {
        def read[A](
            subject: Subject
        )(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
          if (down) Left(StoreError.DatabaseError("down"))
          else body(using TestTx.fake(clearance))
      }
      Toolbox
        .of(MessageIds.tool(MessageIds.Stores(entries, rooms, principals), askers))
        .fold(d => throw new java.lang.AssertionError(s"duplicate $d"), identity)
        .bind(
          TestTainted.authority(
            AssistantBlock
              .ToolCall(ToolCallId("c1"), "message_ids", ujson.Obj("message" -> message))
          ),
          Repairs.All
        ) match {
        case Right(r: Bound.Reading) =>
          r.through(store.as(Subject.Turn(at.turn))) match {
            case b: Bound.Free => b(at)
            case other => throw new java.lang.AssertionError(s"not free: $other")
          }
        case other => throw new java.lang.AssertionError(s"not reading: $other")
      }
    }
  }

  private val Answering = s"This conversation's id is ${dm("5.0").place.written}."

  val tests = Tests {
    test(
      "first is the asker's earliest message in the room, across its conversations, with its link and its integrity"
    ) {
      val w = new World
      w.say("0.5", "Ben here", 0, link("p05"), by = ben)
      w.say("1.0", "hello   there,\nfirst one", 1, link("p10"))
      w.reply("1.0", 2, 0)
      val now = w.say("5.0", "what is my first message's id?", 5, link("p50"))
      w.ask("first", now) ==> Outcome.Done(
        s"$Answering\n" +
          "Of their messages in their direct messages with you (every thread counted), the first grit still keeps:\n" +
          s"entry id: in:${dm("1.0").place.written}:1.0.0\n" +
          s"conversation id: ${dm("1.0").place.written}\n" +
          "said: Wed 2026-10-21 14:04 UTC\n" +
          "link: https://acme.slack.com/archives/D1/p10\n" +
          "it begins: \"hello there, first one\"",
        Some(Integrity.Member)
      )
    }

    test("last is the message being answered when it is the asker's latest, not one said since") {
      val w = new World
      w.say("1.0", "earlier", 1, link("p10"))
      val now = w.say("5.0", "and this one's id?", 5, link("p50"))
      w.say("9.0", "said while the turn ran", 9, link("p90"))
      w.ask("last", now) ==> Outcome.Done(
        s"$Answering\n" +
          "Of their messages in their direct messages with you (every thread counted), the last grit still keeps:\n" +
          s"entry id: in:${dm("5.0").place.written}:5.0.0\n" +
          s"conversation id: ${dm("5.0").place.written}\n" +
          "said: Wed 2026-10-21 14:08 UTC\n" +
          "link: https://acme.slack.com/archives/D1/p50\n" +
          "it begins: \"and this one's id?\"",
        Some(Integrity.Member)
      )
    }

    test("a message recorded with no link is said to have none from its app") {
      val w = new World
      val now = w.say("5.0", "my id?", 5, None)
      w.ask("first", now) ==> Outcome.Done(
        s"$Answering\n" +
          "Of their messages in their direct messages with you (every thread counted), the first grit still keeps:\n" +
          s"entry id: in:${dm("5.0").place.written}:5.0.0\n" +
          s"conversation id: ${dm("5.0").place.written}\n" +
          "said: Wed 2026-10-21 14:08 UTC\n" +
          "link: none; the app it came from (Slack, say) gives no link to it\n" +
          "it begins: \"my id?\"",
        Some(Integrity.Member)
      )
    }

    test("an asker with no message kept here is told that older messages are deleted") {
      val w = new World
      val now = w.say("5.0", "my id?", 5, link("p50"))
      w.ask(
        "first",
        now,
        asker = Some(Principal.Person(TestAccounts.principalId(ben), Set.empty))
      ) ==>
        Outcome.Done(
          s"$Answering grit keeps no such message of theirs in their direct messages with you " +
            "(every thread counted): older messages are deleted after a while."
        )
    }

    test("a store it cannot read is an error that names nothing of what failed") {
      val w = new World
      val now = w.say("5.0", "my id?", 5, link("p50"))
      w.ask("first", now, down = true) ==> Outcome.Failed(MessageIds.Unread)
    }

    test("an asker it cannot tell, or a turn with no room, gets the conversation's id alone") {
      val w = new World
      val now = w.say("5.0", "my id?", 5, link("p50"))
      (w.ask("first", now, asker = None), w.ask("first", now, roomless = true)) ==> (
        Outcome.Done(
          s"$Answering I can't tell which messages here are the person's, so I can't look for " +
            "theirs."
        ),
        Outcome.Done(s"$Answering It belongs to no room, so I can't look for other messages.")
      )
    }
  }
}
