package grit.tools

import java.time.Instant

import grit.core.flow.TestTainted
import grit.core.id.{
  ConversationId,
  EntryId,
  Href,
  PeriodSeq,
  SourceId,
  TestCallSlots,
  ToolCallId,
  TurnRef,
  TurnSeq
}
import grit.core.identity.{Principal, TestAccounts}
import grit.core.inbox.InboundId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.period.{CloseReason, TestClosings}
import grit.core.place.Place
import grit.core.recipe.InMemoryRoomReads
import grit.core.store.{
  Askers,
  Db,
  Entry,
  EntrySearch,
  InMemoryEntryStore,
  InMemoryPrincipals,
  OpenPeriod,
  Origin,
  Payload,
  StoreError,
  Tx
}
import grit.core.tool.{Bound, Outcome, Repairs, Tool, Toolbox}
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

    /** What the room's search finds for any query, best first, as a test scripts it. */
    // Replaced whole by the test's own calls, on its one thread; nothing else holds it.
    @caps.unsafe.untrackedCaptures
    var hits: Vector[EntrySearch.Hit] = Vector.empty

    /** The room, range, query and limit each search was asked for. */
    // Replaced whole by the test's own calls, on its one thread; nothing else holds it.
    @caps.unsafe.untrackedCaptures
    var searched: Vector[(Place, Instant, Instant, String, Int)] = Vector.empty

    private val search = new EntrySearch {
      private def none = Right(Vector.empty)
      def search(c: ConversationId, from: TurnSeq, before: TurnSeq, query: String, limit: Int)(using
          Tx^
      ): Either[StoreError, Vector[EntrySearch.Hit]] = none
      def nearby(open: Vector[OpenPeriod], query: String, limit: Int)(using
          Tx^
      ): Either[StoreError, Vector[EntrySearch.Hit]] = none
      def closings(conversations: Vector[ConversationId], query: String, limit: Int)(using
          Tx^
      ): Either[StoreError, Vector[EntrySearch.Hit]] = none
      def room(room: Place, from: Instant, until: Instant, query: String, limit: Int)(using
          Tx^
      ): Either[StoreError, Vector[EntrySearch.Hit]] = {
        searched = searched :+ (room, from, until, query, limit)
        Right(hits)
      }
    }

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

    /** `payload`, grit's own, in `thread`'s conversation at its turn `turn`, as a search hit. */
    def kept(thread: String, payload: Payload, minutes: Int, turn: Int): EntrySearch.Hit = {
      val c = ConversationId(dm(thread).place.written)
      val e = Entry(
        EntryId(s"kept:$thread:$turn"),
        c,
        TurnSeq(turn.toLong),
        None,
        grit.core.id.EntrySeq(turn.toLong + 1),
        payload,
        Said.plusSeconds(60L * minutes),
        Integrity.Member
      )
      entries.insert(e)(using TestTx.fake).fold(err => sys.error(err.toString), identity)
      hitOf(e)
    }

    /** `e` as the search finds it. */
    def hitOf(e: Entry): EntrySearch.Hit =
      EntrySearch.Hit(e.id, TurnRef(e.conversationId, e.turnSeq), 1.0)

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

    /** Whom the askers resolve the turn's asker to; [[ask]] sets it. */
    // Replaced whole by the test's own calls, on its one thread; nothing else holds it.
    @caps.unsafe.untrackedCaptures
    private var asker: Option[Principal] = None

    private val askers: Askers = new Askers {
      def of(turn: TurnRef)(using Tx^): Either[StoreError, Option[Principal]] = Right(asker)
    }

    /** `message_ids` over this world's stores. */
    val tool: Tool[MessageIds.Pointed] =
      MessageIds.tool(MessageIds.Stores(entries, rooms, search, principals), askers)

    /** `message_ids` asked for `message`, in the turn `answering` begins, by `asker`, read in
      * Ana's direct room unless `roomless`, from a store that fails when `down`.
      */
    def ask(
        message: String,
        answering: Entry,
        asker: Option[Principal] = Some(Principal.Person(TestAccounts.principalId(ana), Set.empty)),
        roomless: Boolean = false,
        down: Boolean = false,
        quote: Option[String] = None
    ): Outcome = {
      val at = TestCallSlots.at(TurnRef(answering.conversationId, answering.turnSeq))
      this.asker = asker
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
        .of(tool)
        .fold(d => throw new java.lang.AssertionError(s"duplicate $d"), identity)
        .bind(
          TestTainted.authority(
            AssistantBlock.ToolCall(
              ToolCallId("c1"),
              "message_ids",
              ujson.Obj.from(
                Vector("message" -> ujson.Str(message)) ++ quote.map(q => "quote" -> ujson.Str(q))
              )
            )
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
      // A later message recorded before the earliest, so first is by time, not by record.
      w.say("3.0", "a later one", 3, link("p30"))
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

    test("a message kept with no link is said to have none, without guessing why") {
      val w = new World
      val now = w.say("5.0", "my id?", 5, None)
      w.ask("first", now) ==> Outcome.Done(
        s"$Answering\n" +
          "Of their messages in their direct messages with you (every thread counted), the first grit still keeps:\n" +
          s"entry id: in:${dm("5.0").place.written}:5.0.0\n" +
          s"conversation id: ${dm("5.0").place.written}\n" +
          "said: Wed 2026-10-21 14:08 UTC\n" +
          "link: none kept for it (grit keeps none for messages from before links were recorded, " +
          "or from an app that gives none)\n" +
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

    test(
      "quoted is the first message among the room's best matches, anyone's, a closing passed over, searched up to the message answered"
    ) {
      val w = new World
      val bens = w.say("0.5", "the deploy moved to Friday", 0, link("p05"), by = ben)
      val anas = w.say("1.0", "deploy on Friday then", 1, link("p10"))
      val now = w.say("5.0", "id of Ben's deploy message?", 5, link("p50"))
      val closed = w.kept(
        "0.5",
        Payload.Closed(PeriodSeq.First, CloseReason.Lapsed, TestClosings.prose("deploy moved")),
        2,
        1
      )
      w.hits = Vector(closed, w.hitOf(bens), w.hitOf(anas))
      (w.ask("quoted", now, quote = Some("  deploy moved  ")), w.searched) ==> (
        Outcome.Done(
          s"$Answering\n" +
            "Of the messages in their direct messages with you (every thread counted), the one " +
            "that best matches:\n" +
            s"entry id: in:${dm("0.5").place.written}:0.5.0\n" +
            s"conversation id: ${dm("0.5").place.written}\n" +
            "said: Wed 2026-10-21 14:03 UTC\n" +
            "link: https://acme.slack.com/archives/D1/p05\n" +
            "it begins: \"the deploy moved to Friday\"",
          Some(Integrity.Member)
        ),
        Vector((room, Instant.EPOCH, now.createdAt.plusNanos(1000), "deploy moved", 5))
      )
    }

    test(
      "quoted with no message among the matches says none is kept; with no quote it is refused"
    ) {
      val w = new World
      val now = w.say("5.0", "id of the deploy message?", 5, link("p50"))
      val args = w.tool.spec.args
      (
        w.ask("quoted", now, quote = Some("deploy")),
        args.read(ujson.Obj("message" -> "quoted")).left.map(_.message),
        args.read(ujson.Obj("message" -> "quoted", "quote" -> " ")).left.map(_.message),
        args.read(ujson.Obj("message" -> "first", "quote" -> "deploy")).left.map(_.message)
      ) ==> (
        Outcome.Done(
          s"$Answering grit keeps no message in their direct messages with you (every thread " +
            "counted) that matches those words: older messages are deleted after a while."
        ),
        Left("`quote` is missing: it takes the words to look for, for quoted."),
        Left("`quote` is missing: it takes the words to look for, for quoted."),
        Left("`quote` is only for `message` quoted.")
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
