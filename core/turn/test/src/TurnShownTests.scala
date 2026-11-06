package grit.turn

import java.time.{Instant, ZoneId, ZoneOffset}

import grit.core.context.{Shown, Window}
import grit.core.document.{DocLabel, DocText, Document, Placement}
import grit.core.id.{
  ConversationId,
  DocKey,
  DocumentVersionId,
  EntryId,
  EntrySeq,
  PluginName,
  ToolCallId,
  TurnRef,
  TurnSeq
}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{AfterToolResult, ToolGuidance}
import grit.core.place.Place
import grit.core.provider.{ModelRequest, Repeats, ToolSchema, ToolUse}
import grit.core.store.{Entry, Payload, Speakers}
import grit.core.tool.ToolSet
import grit.core.visibility.{Integrity, Label}

import utest.*

/** What [[TurnShown]] tells a turn's model besides its window: the line dating its message, and
  * how each call of its loop is shaped from the plain request.
  */
object TurnShownTests extends TestSuite {

  private val At = Instant.parse("2026-10-21T15:03:00Z")

  private val (pacific, eastern) = (ZoneId.of("America/Los_Angeles"), ZoneId.of("America/New_York"))

  /** The tail every zoned line carries, its times read as `short`. */
  private def tail(short: String): String =
    s"People here give times in their own zone, not UTC: read the times the person gives as $short " +
      s"unless they say otherwise, and answer with times in $short. Times grit shows you in your " +
      "view are UTC."

  private def got[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  /** A brief, version 7, as a boundary of a claim holder's turn holds it. */
  private val brief = Document(
    got(DocumentVersionId.of(7).toRight("version")),
    got(PluginName.of("projects")),
    got(DocKey.of("brief.md")),
    Label.Public,
    Integrity.Member,
    got(Place.read("slack:T1/C1")),
    got(DocText.of("Move the commands to tools.")),
    ujson.Obj(),
    None,
    At,
    Placement(0, At),
    None
  )

  /** A person's message `text`, the first entry of turn `n` at seq `n`. */
  private def said(n: Long, text: String): Entry = Entry(
    EntryId(s"e$n"),
    ConversationId("c"),
    TurnSeq(n),
    None,
    EntrySeq(n),
    Payload.Message(Message.User(text)),
    At,
    Integrity.Member
  )

  /** Turn 3's request, rooted on "now", over one part drawing `window` from `entries`, with
    * [[brief]] when `window` names it, shaped for an unasked loop.
    */
  private def boundary(
      window: Window,
      entries: Vector[Entry]
  ): Either[TurnFailure, ModelRequest] = {
    val showing = TurnShown.Showing(
      Vector(
        TurnShown.Part(
          window,
          entries,
          Vector.empty,
          Vector.empty,
          Vector.empty,
          None,
          window.brief.map(_ => brief -> got(DocLabel.of("Projects")))
        )
      ),
      Vector(said(3, "now")),
      Speakers.none
    )
    val shape = TurnShown.loopShape(
      None,
      ToolUse.Auto,
      Vector.empty,
      AfterToolResult.UserMessage,
      ToolGuidance.SchemaOnly,
      Vector.empty
    )
    TurnShown
      .requestOf("sys", showing, TurnRef(ConversationId("c"), TurnSeq(3)), Map.empty, None, false)
      .map(_.shaped(shape))
  }

  val tests = Tests {
    test(
      "the answering line dates the message in the person's zone, says whose, then UTC, and how to read times"
    ) {
      val personal = "The message you are answering was said Wed 2026-10-21 08:03 PDT, the " +
        s"person's time (America/Los_Angeles); 15:03 UTC. ${tail("PDT")}"
      (
        TurnShown.answering(At, Some(TurnOffer.Zoned(Some(pacific), eastern)), direct = true),
        TurnShown.answering(At, Some(TurnOffer.Zoned(None, pacific)), direct = true),
        TurnShown.answering(At, None, direct = true),
        TurnShown.answering(At, Some(TurnOffer.Zoned(Some(pacific), eastern)), direct = false)
      ) ==> (
        personal,
        "The message you are answering was said Wed 2026-10-21 08:03 PDT, this deployment's " +
          "time (America/Los_Angeles); the person's own zone is not known; 15:03 UTC. " +
          tail("PDT"),
        "The message you are answering was said Wed 2026-10-21 15:03 UTC.",
        s"$personal Read times other people here give as EDT, this deployment's time, unless " +
          "they say otherwise."
      )
    }

    test("the UTC time carries its date when it falls on another day than the dated one") {
      TurnShown.answering(
        Instant.parse("2026-10-22T06:30:00Z"),
        Some(TurnOffer.Zoned(Some(pacific), eastern)),
        direct = true
      ) ==> "The message you are answering was said Wed 2026-10-21 23:30 PDT, the person's time " +
        s"(America/Los_Angeles); 06:30 UTC Thu 2026-10-22. ${tail("PDT")}"
    }

    test("a deployment in UTC, under any of its names, is said as UTC") {
      Vector(ZoneOffset.UTC, ZoneId.of("UTC"), ZoneId.of("Etc/UTC")).map(z =>
        TurnShown.answering(At, Some(TurnOffer.Zoned(None, z)), direct = true)
      ) ==> Vector.fill(3)(
        "The message you are answering was said Wed 2026-10-21 15:03 UTC, this deployment's " +
          "time (UTC); the person's own zone is not known; 15:03 UTC. " + tail("UTC")
      )
    }

    test(
      "a turn asked about its item tags the same message on every call, so each call repeats the one before it"
    ) {
      def reply(text: String, calls: String*): Message.Assistant = Message.Assistant(
        AssistantBlock.Text(text) +: calls.toVector.map(id =>
          AssistantBlock.ToolCall(ToolCallId(id), "peek", ujson.Obj())
        ),
        if (calls.isEmpty) StopReason.EndTurn else StopReason.ToolUse,
        Usage(Tokens(1), Tokens(1), Tokens.Zero, None),
        "m"
      )
      val c = TurnAgenda.Classification(Vector.empty, None, Vector.empty, None, None)
      // The person's message, then the loop's exchange: a reply calling a tool, its result, a
      // declined call's reason as a user message after it, then a second round's.
      val asked = Vector(Message.User("earlier"), reply("before"), Message.User("read it"))
      val exchange = Vector(
        reply("", "t1"),
        Message.ToolResult(ToolCallId("t1"), "alpha", isError = false),
        Message.User("No, not that one.")
      )
      val second = Vector(reply("", "t2"), Message.ToolResult(ToolCallId("t2"), "beta", false))
      def shaped(use: ToolUse, messages: Vector[Message]): Vector[Message] =
        TurnShown
          .loopShape(
            Some(c),
            use,
            Vector.empty,
            AfterToolResult.UserMessage,
            ToolGuidance.SchemaOnly,
            Vector.empty
          )(ModelRequest("sys", messages))
          .messages
      val first = shaped(ToolUse.Auto, asked)
      first.lastOption ==> Some(Message.User(s"read it\n\n${TurnVerdict.tag(c)}"))
      (
        shaped(ToolUse.Auto, asked ++ exchange),
        shaped(ToolUse.Off, asked ++ exchange ++ second)
      ) ==> (
        first ++ exchange,
        first ++ exchange ++ second :+ Message.User(TurnLoop.LastCall)
      )
    }

    test(
      "a round's note sits before that round's reply on every later call, round 1's after the asker's message, which keeps the tag"
    ) {
      val c = TurnAgenda.Classification(Vector.empty, None, Vector.empty, None, None)
      def reply(id: String) = Message.Assistant(
        Vector(AssistantBlock.ToolCall(ToolCallId(id), "peek", ujson.Obj())),
        StopReason.ToolUse,
        Usage(Tokens(1), Tokens(1), Tokens.Zero, None),
        "m"
      )
      val notes = Vector(TurnLoop.Round.First -> "first", TurnLoop.Round.First.next.next -> "third")
      val messages = Vector(
        Message.User("earlier"),
        Message.User("read it"),
        reply("t1"),
        Message.ToolResult(ToolCallId("t1"), "alpha", isError = false),
        reply("t2"),
        Message.ToolResult(ToolCallId("t2"), "beta", isError = false)
      )
      TurnShown
        .loopShape(
          Some(c),
          ToolUse.Auto,
          Vector.empty,
          AfterToolResult.UserMessage,
          ToolGuidance.SchemaOnly,
          notes
        )(ModelRequest("sys", messages))
        .messages ==> Vector(
        Message.User("earlier"),
        Message.User(s"read it\n\n${TurnVerdict.tag(c)}"),
        Message.User("first"),
        reply("t1"),
        Message.ToolResult(ToolCallId("t1"), "alpha", isError = false),
        reply("t2"),
        Message.ToolResult(ToolCallId("t2"), "beta", isError = false),
        Message.User("third")
      )
    }

    test("the loop's request marks stable every message but the last call's note") {
      val asked = Vector(Message.User("earlier"), Message.User("read it"))
      val call = Message.Assistant(
        Vector(AssistantBlock.ToolCall(ToolCallId("t1"), "peek", ujson.Obj())),
        StopReason.ToolUse,
        Usage(Tokens(1), Tokens(1), Tokens.Zero, None),
        "m"
      )
      val result = Message.ToolResult(ToolCallId("t1"), "alpha", isError = false)
      def marked(use: ToolUse, after: AfterToolResult): (Int, Int) = {
        val r = TurnShown.loopShape(
          None,
          use,
          Vector.empty,
          after,
          ToolGuidance.SchemaOnly,
          Vector.empty
        )(ModelRequest("sys", asked ++ Vector(call, result)))
        (r.repeats.stable, r.messages.size)
      }
      // A later call repeats every message a call with tools is sent; the last call's note is
      // its own, after the messages or, where a pair refuses a user there, ending the last
      // result, which no later call repeats either.
      Vector(
        marked(ToolUse.Auto, AfterToolResult.UserMessage),
        marked(ToolUse.Off, AfterToolResult.UserMessage),
        marked(ToolUse.Auto, AfterToolResult.InLastResult),
        marked(ToolUse.Off, AfterToolResult.InLastResult)
      ) ==> Vector((4, 4), (4, 5), (4, 4), (3, 4))
    }

    test("a note put among the established messages moves their end past it, not the task's") {
      val call = Message.Assistant(
        Vector(AssistantBlock.ToolCall(ToolCallId("t1"), "peek", ujson.Obj())),
        StopReason.ToolUse,
        Usage(Tokens(1), Tokens(1), Tokens.Zero, None),
        "m"
      )
      val messages = Vector(
        Message.User("the task"),
        call,
        Message.ToolResult(ToolCallId("t1"), "alpha", isError = false),
        Message.User("go on")
      )
      // No asker: round 1's note goes before the first reply, inside the established part.
      val r = TurnShown.loopShape(
        None,
        ToolUse.Auto,
        Vector.empty,
        AfterToolResult.UserMessage,
        ToolGuidance.SchemaOnly,
        Vector(TurnLoop.Round.First -> "first")
      )(ModelRequest("sys", messages, repeats = Repeats.of(1, 3, 4)))
      (r.repeats.task, r.repeats.established, r.messages.lift(r.repeats.established)) ==>
        (1, 4, Some(Message.User("go on")))
    }

    test(
      "a request repeats its brief as its task part, and every message before its dated line as established"
    ) {
      val drawn = Vector(said(1, "earlier"), said(2, "later"))
      boundary(Window(drawn.map(_.seq), brief = Some(brief.version)), drawn).map(q =>
        (
          q.repeats.task,
          q.repeats.established,
          q.messages.lift(q.repeats.task - 1),
          q.messages.lift(q.repeats.established)
        )
      ) ==> Right(
        // The brief, a gap line (turn 1 is not the first), then the two drawn messages.
        (1, 4, Some(Shown.brief(brief)), Some(Message.User(TurnShown.answering(At, None, false))))
      )
    }

    test("a boundary's digest follows its entries") {
      // What the session did changes at every boundary: after the entries, it leaves what
      // earlier turns established a prefix the next boundary can repeat.
      val drawn = Vector(said(1, "earlier"), said(2, "later"))
      boundary(Window(drawn.map(_.seq), sessionLog = Some("Committed a1b2c3d.")), drawn)
        .map(_.messages.take(4)) ==> Right(
        Vector(
          Shown.Gap,
          Message.User("earlier"),
          Message.User("later"),
          Shown.sessionLog("Committed a1b2c3d.")
        )
      )
    }

    test(
      "a boundary's task follows its brief, inside its task part, and is not shown again among its entries"
    ) {
      val task = said(0, "Move the commands.")
      val drawn = Vector(said(1, "earlier"), said(2, "later"))
      boundary(
        Window(drawn.map(_.seq), brief = Some(brief.version), task = Some(task.seq)),
        task +: drawn
      ).map(q => (q.repeats.task, q.messages.take(3))) ==> Right(
        // The brief, the task, then a gap line: turn 1 is not the first.
        (2, Vector(Shown.brief(brief), Message.User("Move the commands."), Shown.Gap))
      )
    }

    test(
      "the offer is the tool set's: read_hidden and extract whenever a tool requires above the least, whatever the gates decide; a withheld tool stays offered"
    ) {
      // post requires member, so a gate may withhold it; peek and poke require nothing.
      val gating = TurnFixtures.gatedTools(TurnFixtures.NoCheckout)
      val plain = TurnFixtures.tools(TurnFixtures.NoCheckout)
      def offer(set: ToolSet, schemas: Vector[ToolSchema]) =
        TurnShown.gatedOffer(set, schemas, false).map(_.name)
      Vector(
        offer(gating.set, gating.schemas(false)),
        offer(plain.set, plain.schemas(false))
      ) ==> Vector(Vector("read_hidden", "extract", "peek", "post"), Vector("peek", "poke"))
    }
  }
}
