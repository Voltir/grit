package grit.turn

import java.time.Instant

import grit.core.classify.StateJson
import grit.core.context.Window
import grit.core.document.{DocLabel, DocText, Document, Placement, Pointer}
import grit.core.files.{FormatName, KeptPiece, Locus, Piece, TestPieces}
import grit.core.id.{
  ConversationId,
  DocKey,
  DocumentVersionId,
  EntryId,
  EntrySeq,
  PeriodSeq,
  PieceId,
  PluginName,
  ToolCallId,
  TurnSeq
}
import grit.core.message.{AssistantBlock, Message, StopReason, Usage}
import grit.core.period.{CloseReason, TestClosings}
import grit.core.place.Place
import grit.core.stitch.{Said, Strand}
import grit.core.store.{Entry, Nearby, Payload, Speakers}
import grit.core.tool.ToolName
import grit.core.visibility.Integrity

import utest.*

object TurnJudgeTests extends TestSuite {

  private def entry(n: Long, payload: Payload): Entry =
    Entry(
      EntryId(s"e$n"),
      ConversationId("c"),
      TurnSeq(n),
      None,
      EntrySeq(n),
      payload,
      Instant.EPOCH,
      Integrity.Member
    )

  private val record =
    entry(
      0,
      Payload.Closed(PeriodSeq.First, CloseReason.Lapsed, TestClosings.prose("The freeze moved."))
    )
  private val asked = entry(1, Payload.Message(Message.User("when is the freeze?")))
  private val answered = entry(
    2,
    Payload.Message(
      Message.Assistant(
        Vector(AssistantBlock.Text("Thursday.")),
        StopReason.EndTurn,
        Usage.Zero,
        "m"
      )
    )
  )
  private val root = entry(3, Payload.Heard("is it still on?"))
  private val later = entry(4, Payload.Heard("said after"))
  private val all: Vector[Entry] = Vector(record, asked, answered, root, later)
  private val names = Speakers(Map(EntryId("e1") -> "Ana", EntryId("e3") -> "Ben"))

  /** [[TurnJudge.state]] over a request that showed `all`'s entries at `window`'s seqs,
    * `near`, `documents` and the turn's own `mine`, each named as `speakers` names them.
    */
  private def judging(
      all: Vector[Entry],
      window: Window,
      near: Vector[Entry],
      strand: Strand.Read,
      strandChars: Int,
      speakers: Speakers,
      draft: String,
      documents: Vector[(Document, DocLabel)] = Vector.empty,
      mine: Vector[Entry] = Vector.empty,
      pieces: Vector[(KeptPiece, DocLabel, Int)] = Vector.empty
  ): TurnJudge.State =
    TurnJudge.state(
      all,
      TurnShown.Showing(
        all.filter(e => window.entries.contains(e.seq)),
        mine,
        near,
        speakers,
        documents,
        pieces
      ),
      window,
      strand,
      strandChars,
      speakers,
      draft
    )

  val tests = Tests {
    test(
      "the judge sees every message so far under names, replies after the root included, and only the records it recalled"
    ) {
      val state = judging(
        all,
        Window(all.map(_.seq)),
        Vector.empty,
        Strand.Read.empty,
        800,
        names,
        "draft"
      )
      state.thread ==>
        "Ana: when is the freeze?\nAssistant: Thursday.\nBen: is it still on?\nSomeone: said after"
      assert(state.recalled.contains("The freeze moved."), !state.recalled.contains("when is"))
      state.recalled.split("\n\n").size ==> 1
    }

    test(
      "a heard question's thread is its own messages; what its window recalled, own or nearby, is only in recalled"
    ) {
      val place = Place.read("slack:T1/C1/1.0").fold(e => sys.error(e), identity)
      def other(n: Long, conversation: String, payload: Payload): Entry =
        Entry(
          EntryId(s"o$n"),
          ConversationId(conversation),
          TurnSeq(0),
          None,
          EntrySeq(0),
          payload,
          Instant.EPOCH,
          Integrity.Member
        )
      val decided = other(
        1,
        "d",
        Payload.Closed(PeriodSeq.First, CloseReason.Lapsed, TestClosings.prose("A 12-month term."))
      )
      val markup = other(2, "m", Payload.Heard("I marked up the contract."))
      val question = entry(1, Payload.Heard("where did we land on the term?"))
      val state = judging(
        Vector(record, question),
        Window(
          Vector(record.seq, question.seq),
          Vector.empty,
          Vector(
            Nearby.Closed(ConversationId("d"), place, decided.seq),
            Nearby.Open(ConversationId("m"), place, Vector(markup.seq))
          )
        ),
        Vector(decided, markup),
        Strand.Read.empty,
        800,
        Speakers(Map(question.id -> "Nick")),
        "draft"
      )
      state.thread ==> "Nick: where did we land on the term?"
      assert(
        state.recalled.contains("The freeze moved."),
        state.recalled.contains("A 12-month term."),
        state.recalled.contains("I marked up the contract."),
        !state.recalled.contains("where did we land")
      )
      state.recalled.split("\n\n").size ==> 3
    }

    test(
      "a strand is the thread's start, its opening kept when the thread is cut, and never recalled"
    ) {
      val place = Place.read("slack:T1/C1/1.0").fold(e => sys.error(e), identity)
      def said(n: Long, text: String) = Said(
        ConversationId("a"),
        place,
        Entry(
          EntryId(s"a$n"),
          ConversationId("a"),
          TurnSeq(n),
          None,
          EntrySeq(n),
          Payload.Heard(text),
          Instant.EPOCH,
          Integrity.Member
        )
      )
      val opening = said(0, "where did we land on the Engine contract term?")
      val real = entry(1, Payload.Heard("Is this a real question"))
      val strand =
        Strand.Read(Some(opening), Vector(opening), Vector.empty, Set(opening.conversation))
      val long = entry(2, Payload.Heard("x" * TurnJudge.ThreadChars))
      val state = judging(
        Vector(real, long),
        Window(
          Vector(real.seq),
          Vector.empty,
          Vector(Nearby.Along(ConversationId("a"), place, Vector(opening.entry.seq)))
        ),
        Vector(opening.entry),
        strand,
        800,
        Speakers(Map(opening.entry.id -> "Nick", real.id -> "David")),
        "draft"
      )
      assert(
        state.thread.startsWith("Nick: where did we land on the Engine contract term?\n"),
        state.thread.length == TurnJudge.ThreadChars,
        state.recalled == ""
      )
    }

    test("the judge's thread shows grit's post the thread begins with as grit's line") {
      val posted = entry(0, Payload.Posted("The engine's open issues."))
      val heard = entry(1, Payload.Heard("why this?"))
      judging(
        Vector(posted, heard),
        Window(Vector(posted.seq)),
        Vector.empty,
        Strand.Read.empty,
        800,
        Speakers(Map(heard.id -> "Nick")),
        "draft"
      ).thread ==> "Assistant: The engine's open issues.\nNick: why this?"
    }

    test("the judge's thread leaves its oldest messages out whole, a … line where they were") {
      val all = Vector(
        entry(0, Payload.Heard("x" * 3_000)),
        entry(1, Payload.Heard("one two")),
        entry(2, Payload.Heard("y" * 1_000))
      )
      judging(
        all,
        Window(all.map(_.seq)),
        Vector.empty,
        Strand.Read.empty,
        800,
        names,
        "d"
      ).thread ==> s"…\nAna: one two\nSomeone: ${"y" * 1_000}"
    }

    test("a one-message conversation's thread is empty, and its message holds the heard message") {
      val heard = entry(0, Payload.Heard("is it still on?"))
      val state = judging(
        Vector(heard),
        Window(Vector(heard.seq)),
        Vector.empty,
        Strand.Read.empty,
        800,
        Speakers(Map(heard.id -> "Ben")),
        "draft",
        mine = Vector(heard)
      )
      assert(state.thread.isEmpty, state.message == "Ben: is it still on?")
    }

    test("an asked section is recalled: what grit was asked elsewhere grounds a draft") {
      val place = Place.read("slack:T1/C2/3.0").fold(e => sys.error(e), identity)
      val ask = Entry(
        EntryId("o1"),
        ConversationId("asker"),
        TurnSeq(0),
        None,
        EntrySeq(0),
        Payload.Message(Message.User("post the open issues")),
        Instant.EPOCH,
        Integrity.Member
      )
      judging(
        Vector(root),
        Window(
          Vector.empty,
          Vector.empty,
          Vector(Nearby.Asked(ConversationId("asker"), place, Vector(ask.seq)))
        ),
        Vector(ask),
        Strand.Read.empty,
        800,
        Speakers(Map(ask.id -> "Nick")),
        "draft"
      ).recalled
        .contains("Nick, Thu 1970-01-01 00:00 UTC: post the open issues") ==> true
    }

    test(
      "a document the window held is recalled under its label, place and date, its read line left out"
    ) {
      def got[A](e: Either[String, A]): A =
        e.fold(why => throw new java.lang.AssertionError(why), identity)
      val card = Document(
        got(DocumentVersionId.of(7).toRight("version")),
        got(PluginName.of("cards")),
        got(DocKey.of("docs/release.md")),
        grit.core.visibility.Label.Public,
        Integrity.Member,
        got(Place.read("slack:T1/C1")),
        got(DocText.of("Releases are cut on Tuesdays.")),
        ujson.Obj(),
        Some(got(Pointer.of(ToolName("read"), "path" -> ujson.Str("docs/release.md")))),
        Instant.parse("2026-09-30T23:30:00Z"),
        Placement(0, Instant.parse("2026-09-30T23:30:00Z")),
        None
      )
      judging(
        Vector(root),
        Window(Vector.empty, documents = Vector(card.version)),
        Vector.empty,
        Strand.Read.empty,
        800,
        names,
        "draft",
        documents = Vector(card -> got(DocLabel.of("Cards")))
      ).recalled ==> "[doc] Cards, at slack:T1/C1, 2026-09-30:\nReleases are cut on Tuesdays."
    }

    test(
      "a piece the window held is recalled under its label, place, heading path and date, with the count of its file's other matches"
    ) {
      def got[A](e: Either[String, A]): A =
        e.fold(why => throw new java.lang.AssertionError(why), identity)
      val label = got(DocLabel.of("Drive files"))
      val shown = KeptPiece(
        PieceId.of(12L).getOrElse(sys.error("id")),
        got(PluginName.of("files")),
        TestPieces.file("f12", "plan.pdf", FormatName("pdf")),
        got(Place.read("slack:T1/C1")),
        grit.core.visibility.Label.Public,
        Integrity.Member,
        Instant.parse("2026-09-30T23:30:00Z"),
        Piece(
          Vector("Access"),
          grit.prose.form.Doc(
            Vector(
              grit.prose.form.Block.Paragraph(
                grit.prose.form.Text(
                  Vector(
                    grit.prose.form.Span("Accounts are reviewed.", Set.empty, Vector(Locus.Page(2)))
                  )
                )
              )
            )
          ),
          TestPieces.anchor("Accounts are reviewed.")
        )
      )
      judging(
        Vector(root),
        Window(Vector.empty),
        Vector.empty,
        Strand.Read.empty,
        800,
        names,
        "draft",
        pieces = Vector((shown, label, 3))
      ).recalled ==> "[doc] Drive files, part of the file plan.pdf, under Access, page 2, at slack:T1/C1, 2026-09-30:\nAccounts are reviewed.\n3 more parts of this file matched the search." +
        "\nRead it with drive_read {\"file\":\"f12\",\"near\":\"0.0000|⟪Accounts are reviewed.⟫\"}"
    }

    test("what the draft's own tool calls read is recalled under each call; a failed one is not") {
      def result(n: Long, content: String, failed: Boolean, shown: String) = entry(
        n,
        Payload.Result(Message.ToolResult(ToolCallId(s"c$n"), content, failed), shown)
      )
      judging(
        Vector(root),
        Window(Vector.empty),
        Vector.empty,
        Strand.Read.empty,
        800,
        names,
        "draft",
        mine = Vector(
          result(5, "Releases are cut on Tuesdays.", false, "read docs/release.md"),
          result(6, "no such file", true, "read docs/old.md")
        )
      ).recalled ==> "read docs/release.md:\nReleases are cut on Tuesdays."
    }

    test(
      "the conversation's own record is recalled only when the thread cuts a message said before it"
    ) {
      def closing(n: Long) = entry(
        n,
        Payload.Closed(PeriodSeq.First, CloseReason.Lapsed, TestClosings.prose("The freeze moved."))
      )
      def recalled(before: String) = {
        val all =
          Vector(entry(0, Payload.Heard(before)), closing(1), entry(2, Payload.Heard("ok?")))
        judging(
          all,
          Window(all.map(_.seq)),
          Vector.empty,
          Strand.Read.empty,
          800,
          names,
          "d"
        ).recalled
      }
      (recalled("when is the freeze?"), recalled("freeze " * TurnJudge.ThreadChars)) ==> (
        "",
        "[record] this conversation so far, written by grit (closed 1970-01-01): The freeze moved."
      )
    }
    test("a thread whose only message is cut inside recalls the conversation's record") {
      val all = Vector(
        entry(0, Payload.Heard("freeze " * TurnJudge.ThreadChars)),
        entry(
          1,
          Payload.Closed(
            PeriodSeq.First,
            CloseReason.Lapsed,
            TestClosings.prose("The freeze moved.")
          )
        )
      )
      judging(
        all,
        Window(all.map(_.seq)),
        Vector.empty,
        Strand.Read.empty,
        800,
        names,
        "d"
      ).recalled ==> "[record] this conversation so far, written by grit (closed 1970-01-01): The freeze moved."
    }

    test(
      "the judge is shown the heard message apart, and asks whether the draft is grounded in recalled or thread and answers the message"
    ) {
      val state = judging(
        all,
        Window(all.map(_.seq)),
        Vector.empty,
        Strand.Read.empty,
        800,
        names,
        "It moved to Thursday.",
        mine = Vector(root, later)
      )
      val judge = new TurnFixtures.Judge(Some((0.9, 0.9)))
      TurnJudge.judge(judge, grit.assembly.estimate.CharEstimate, state)
      (state.message, judge.states.map(_("message").str), judge.asked) ==> (
        "Ben: is it still on?",
        Vector("Ben: is it still on?"),
        Vector(
          "Read draft, recalled and thread. Is every fact draft states found in recalled or " +
            "thread? Anything neither contains is no, however true it may be.",
          "Read message, thread and draft. Does draft answer what message, or the thread before " +
            "it, asks, and is it not " +
            "already answered in thread? A greeting, an aside, or a reply that only restates " +
            "the question is no."
        )
      )
    }

    test("a window with no record recalls nothing") {
      judging(
        all,
        Window(Vector(asked.seq)),
        Vector.empty,
        Strand.Read.empty,
        800,
        names,
        "d"
      ).recalled ==> ""
    }

    test("the judge is shown the thread's end and the recalled's start, each cut to its limit") {
      val json = StateJson[TurnJudge.State].json(
        TurnJudge.State(
          "m",
          "x" * 10 + "y" * TurnJudge.ThreadChars,
          "d",
          "a" * TurnJudge.RecalledChars + "b"
        )
      )
      (json("thread").str, json("recalled").str) ==> (
        "y" * TurnJudge.ThreadChars,
        "a" * TurnJudge.RecalledChars
      )
    }
  }
}
