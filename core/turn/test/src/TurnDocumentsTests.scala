package grit.turn

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.clock.Clock
import grit.core.context.{AssemblyError, AssemblyRequest, ContextAssembler, Retrieved, Window}
import grit.core.document.{
  DocLabel,
  DocText,
  DocWeight,
  DocumentTerms,
  Following,
  InMemoryDocuments,
  Placement,
  Pointer,
  Written
}
import grit.core.durable.{Durable, FlowFixtures, InMemoryDurable}
import grit.core.files.{FormatName, InMemoryPieces, Locus, Piece, PieceKeeper, TestPieces}
import grit.core.id.{
  ConversationId,
  DocKey,
  DocumentVersionId,
  EntryId,
  EntrySeq,
  PieceId,
  PluginName,
  ToolCallId,
  TurnSeq,
  WorkflowId
}
import grit.core.message.{AssistantBlock, Message, StopReason}
import grit.core.place.Place
import grit.core.provider.Provider
import grit.core.store.{
  Db,
  Entry,
  InMemoryEntryStore,
  InMemoryUsageLedger,
  Nearby,
  Payload,
  StoreError
}
import grit.core.tool.{ToolName, Toolbox}
import grit.core.visibility.Integrity
import grit.dbos.sql.TestTx
import grit.prose.form.{Block, Doc, Span, Text}

import utest.*

/** A turn whose window holds plugins' documents: what the model is shown of them, what
  * `record-window` counts, and what `append` counts as followed.
  */
object TurnDocumentsTests extends TestSuite {
  import TurnFixtures.*

  private def got[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)
  private def ok[A](e: Either[StoreError, A]): A =
    e.fold(err => throw new java.lang.AssertionError(s"$err"), identity)

  private val digest = got(PluginName.of("digest"))
  private val board = got(Place.read("slack:T1/C1"))
  private val written = Instant.parse("2026-09-30T23:30:00Z")

  private def terms(label: String): DocumentTerms =
    got(DocumentTerms.of(got(DocLabel.of(label)), DocWeight.Unscaled, 30.days, 10))

  /** `text` written under `key` by `plugin`'s keeper in `documents`, believed at `integrity`;
    * the version written.
    */
  private def write(
      documents: InMemoryDocuments,
      plugin: PluginName,
      label: String,
      key: String,
      text: String,
      pointer: Option[Pointer] = None,
      integrity: Integrity = Integrity.Member
  ): DocumentVersionId =
    ok(
      documents
        .keeper(plugin, terms(label))
        .write(
          got(DocKey.of(key)),
          grit.core.visibility.Label.Public,
          integrity,
          board,
          got(DocText.of(text)),
          ujson.Obj(),
          pointer,
          written
        )(using
          TestTx.fake
        )
    ) match {
      case Written.Versioned(v, _, _) => v
      case Written.Unchanged(v, _) => v
    }

  /** An assembler whose every window is `drawn`. */
  /** `text`, kept in `pieces` as `plugin`'s one piece of a file of its own (`text` its ref),
    * under the heading Access on page 2, at [[board]], [[written]]: its id.
    */
  private def kept(pieces: InMemoryPieces, plugin: PluginName, text: String): PieceId = {
    val piece = Piece(
      Vector("Access"),
      Doc(Vector(Block.Paragraph(Text(Vector(Span(text, Set.empty, Vector(Locus.Page(2)))))))),
      TestPieces.anchor(text)
    )
    ok(
      pieces
        .keeper(plugin, PieceKeeper.Bound)
        .keep(
          TestPieces.file(text, "plan.pdf", FormatName("pdf")),
          board,
          Integrity.Member,
          Vector(piece),
          0,
          written
        )(using TestTx.fake)
    )
    ok(pieces.search(Vector(plugin), text, 5, written.plusSeconds(1))(using TestTx.fake))
      .map(_.piece.id)
      .headOption
      .getOrElse(sys.error(s"no piece for $text"))
  }

  private def windowing(drawn: Window): ContextAssembler = new Drawing {
    def window(request: AssemblyRequest)(using Db^): Either[AssemblyError, Window] =
      Right(drawn)
  }

  /** A pointer at `path` read with `tool`. */
  private def pointing(tool: String, path: String): Option[Pointer] =
    Some(got(Pointer.of(got(ToolName.of(tool)), "path" -> ujson.Str(path))))

  /** A model whose first reply calls `tool` with `{"path": path}`, `times` times, and whose
    * next one answers.
    */
  private def calling(tool: String, path: String, times: Int = 1): Scripted =
    new Scripted((_, n) =>
      Right(
        if (n == 0)
          said(
            "",
            (1 to times).map { i =>
              AssistantBlock.ToolCall(
                ToolCallId(s"c$i"),
                tool,
                ujson.Obj("path" -> ujson.Str(path))
              )
            }*
          ).copy(stop = StopReason.ToolUse)
        else said("read it")
      )
    )

  private val appendedAt = Instant.parse("2026-10-01T09:00:00Z")

  private def fixed: Clock^ = new Clock {
    def now(): Instant = appendedAt
    def millis(): Long = appendedAt.toEpochMilli
    def sleep(duration: FiniteDuration): Unit = ()
  }

  /** A clock a second later at each read, starting after [[appendedAt]]. */
  private def ticking: Clock^ = new Clock {
    private val reads = new java.util.concurrent.atomic.AtomicLong(0)
    def now(): Instant = appendedAt.plusSeconds(reads.incrementAndGet())
    def millis(): Long = now().toEpochMilli
    def sleep(duration: FiniteDuration): Unit = ()
  }

  /** The turn `turn` run over `documents`, its window holding `held`, its model `provider`,
    * offered `tools`, dated by `clock`.
    */
  private def followingBody(
      entries: InMemoryEntryStore,
      documents: InMemoryDocuments,
      held: Vector[DocumentVersionId],
      provider: Provider^,
      tools: Toolbox[caps.CapSet^{NoCheckout}],
      clock: Clock^ = fixed
  )(id: WorkflowId)(using Durable^): String =
    turnBodyWith(
      entries,
      provider,
      windowing(Window(Vector.empty, Vector.empty, Vector.empty, held)),
      new InMemoryUsageLedger,
      documents = documents,
      clock = clock,
      tools = tools
    )(id)

  private def following(documents: InMemoryDocuments, versions: Vector[DocumentVersionId]) =
    ok(documents.read(versions)(using TestTx.fake)).map(_.following)

  val tests = Tests {
    test(
      "each document is shown under its plugin's label after the nearby sections and before the own turns; one gone or no longer enabled by call time is left out, the turn not failed"
    ) {
      val entries = new InMemoryEntryStore
      val provider = new RecordingProvider
      val documents = new InMemoryDocuments
      documents.declare(Vector(digest -> terms("Weekly digest")))(using TestTx.fake)
      val week = write(documents, digest, "Weekly digest", "week", "Deploys frozen until Friday.")
      val gone = write(documents, digest, "Weekly digest", "old", "Deploys open.")
      // Its plugin wrote it, then was left out of the deployment: no terms in force.
      val dropped = write(documents, got(PluginName.of("notes")), "Notes", "n", "A note.")
      ok(documents.forget(gone)(using TestTx.fake))
      say(entries, "one")
      val web = ConversationId("web")
      entries.insert(
        Entry(
          EntryId("web:u"),
          web,
          TurnSeq(0),
          None,
          EntrySeq(0),
          Payload.Message(Message.User("the login page is blank")),
          Instant.EPOCH,
          Integrity.Member
        )
      )(using TestTx.fake)
      val nearby =
        Vector(Nearby.Open(web, got(Place.read("fs:/home/nick/web")), Vector(EntrySeq(0))))
      val held = Vector(week, gone, dropped)
      val turn = say(entries, "two")
      val output = new InMemoryDurable().run(turn.workflowId)(
        turnBodyWith(
          entries,
          provider,
          windowing(Window(Vector(EntrySeq(0)), Vector.empty, nearby, held)),
          new InMemoryUsageLedger,
          documents = documents
        )
      )
      provider.requests.headOption.map(_.messages) ==> Some(
        Vector(
          Message.User(
            "[afar] another conversation, shown by grit, still open, at fs:/home/nick/web:\n" +
              "Someone, Thu 1970-01-01 00:00 UTC: the login page is blank"
          ),
          Message.User(
            "[doc] Weekly digest, at slack:T1/C1, 2026-09-30:\n" +
              "Deploys frozen until Friday."
          ),
          Message.User("one"),
          dated,
          Message.User("two")
        )
      )
      assert(output.startsWith("replied:"))
      windows(entries).lastOption.map(_._2) ==>
        Some(Payload.Window(Vector(EntrySeq(0)), Vector.empty, nearby, held))
    }

    test(
      "each piece is shown under its plugin's label after the documents and before the own turns, saying how many more of its file matched; one whose plugin is not enabled is left out"
    ) {
      val entries = new InMemoryEntryStore
      val provider = new RecordingProvider
      val documents = new InMemoryDocuments
      val files = got(PluginName.of("files"))
      documents.declare(Vector(digest -> terms("Weekly digest"), files -> terms("Drive files")))(
        using TestTx.fake
      )
      val week = write(documents, digest, "Weekly digest", "week", "Deploys frozen until Friday.")
      val pieces = new InMemoryPieces
      val shown = kept(pieces, files, "Accounts are reviewed.")
      val unlabelled = kept(pieces, got(PluginName.of("dropped")), "Accounts are removed.")
      say(entries, "one")
      val turn = say(entries, "two")
      val output = new InMemoryDurable().run(turn.workflowId)(
        turnBodyWith(
          entries,
          provider,
          windowing(
            Window(
              Vector(EntrySeq(0)),
              Vector.empty,
              Vector.empty,
              Vector(week),
              Vector(Retrieved.Piece(shown, 3), Retrieved.Piece(unlabelled, 0))
            )
          ),
          new InMemoryUsageLedger,
          documents = documents,
          pieces = pieces
        )
      )
      provider.requests.headOption.map(_.messages) ==> Some(
        Vector(
          Message.User(
            "[doc] Weekly digest, at slack:T1/C1, 2026-09-30:\n" +
              "Deploys frozen until Friday."
          ),
          Message.User(
            "[doc] Drive files, part of the file plan.pdf, under Access, page 2, at slack:T1/C1, " +
              "2026-09-30:\nAccounts are reviewed.\n3 more parts of this file matched the search." +
              "\nRead it with drive_read {\"file\":\"Accounts are reviewed.\"," +
              "\"near\":\"0.0000|⟪Accounts are reviewed.⟫\"}"
          ),
          Message.User("one"),
          dated,
          Message.User("two")
        )
      )
      assert(output.startsWith("replied:"))
    }

    test("the window's entry names each piece the window held, by id, after its documents") {
      val entries = new InMemoryEntryStore
      val documents = new InMemoryDocuments
      val files = got(PluginName.of("files"))
      documents.declare(Vector(digest -> terms("Weekly digest"), files -> terms("Drive files")))(
        using TestTx.fake
      )
      val week = write(documents, digest, "Weekly digest", "week", "Deploys frozen until Friday.")
      val pieces = new InMemoryPieces
      val (one, two) =
        (kept(pieces, files, "Accounts are reviewed."), kept(pieces, files, "Keys rotate."))
      val turn = say(entries, "hello")
      val _ = new InMemoryDurable().run(turn.workflowId)(
        turnBodyWith(
          entries,
          new RecordingProvider,
          windowing(
            Window(
              Vector(EntrySeq(0)),
              Vector.empty,
              Vector.empty,
              Vector(week),
              Vector(Retrieved.Piece(two, 0), Retrieved.Piece(one, 1))
            )
          ),
          new InMemoryUsageLedger,
          documents = documents,
          pieces = pieces
        )
      )
      ok(entries.list(conversation)(using TestTx.fake)).collectFirst {
        case Entry(id, _, _, _, _, w: Payload.Window, _, _) if id == Turn.windowId(turn) =>
          (w.documents, w.pieces)
      } ==> Some((Vector(week), Vector((two, 0), (one, 1))))
    }

    test(
      "a document's pointer line is shown only when the turn offers its tool: another name, or one under a prefix, never shows it"
    ) {
      val entries = new InMemoryEntryStore
      val provider = new RecordingProvider
      val documents = new InMemoryDocuments
      documents.declare(Vector(digest -> terms("Cards")))(using TestTx.fake)
      def at(tool: String) =
        Some(got(Pointer.of(got(ToolName.of(tool)), "path" -> ujson.Str("README.md"))))
      val offered = write(documents, digest, "Cards", "peeked", "How to peek.", at("peek"))
      val other = write(documents, digest, "Cards", "fetched", "How to fetch.", at("fetch"))
      val prefixed =
        write(documents, digest, "Cards", "prefixed", "How to peek there.", at("x_peek"))
      val turn = say(entries, "one")
      val peeking = Toolbox
        .of[caps.CapSet^{NoCheckout}](peek(NoCheckout))
        .fold(d => throw new java.lang.AssertionError(d), identity)
      new InMemoryDurable().run(turn.workflowId)(
        turnBodyWith(
          entries,
          provider,
          windowing(
            Window(Vector.empty, Vector.empty, Vector.empty, Vector(offered, other, prefixed))
          ),
          new InMemoryUsageLedger,
          documents = documents,
          tools = peeking
        )
      )
      val shown = "[doc] Cards, at slack:T1/C1, 2026-09-30:\n"
      provider.requests.headOption.map(r => r.tools.map(_.name) -> r.messages) ==> Some(
        Vector("peek") -> Vector(
          Message.User(
            shown + "How to peek.\n" + """Read it with peek {"path":"README.md"}"""
          ),
          Message.User(shown + "How to fetch."),
          Message.User(shown + "How to peek there."),
          dated,
          Message.User("one")
        )
      )
    }

    test(
      "a heard turn whose window held only a document is judged against it, not left unrecalled"
    ) {
      val w = speechWorld(recalled = false)
      val documents = new InMemoryDocuments
      documents.declare(Vector(digest -> terms("Cards")))(using TestTx.fake)
      val card = write(documents, digest, "Cards", "freeze", "The freeze ends on Thursday.")
      val judge = new Judge(Some((0.9, 0.9)))
      val done = new InMemoryDurable().run(w.turn.workflowId)(
        turnBodyWith(
          w.entries,
          new Scripted((_, _) => Right(said("It ends on Thursday."))),
          windowing(Window(Vector.empty, documents = Vector(card))),
          w.ledger,
          judge,
          TurnSpeech(grit.core.speech.Speaking.Within(speechLimits), w.store, w.deliveries),
          documents = documents
        )
      )
      (done.takeWhile(_ != ':'), judge.states.map(_("recalled").str)) ==> (
        "posted",
        Vector("[doc] Cards, at slack:T1/C1, 2026-09-30:\nThe freeze ends on Thursday.")
      )
    }

    test("a document whose reading would withdraw a tool is shown as its stub under its label") {
      val entries = new InMemoryEntryStore
      val provider = new RecordingProvider
      val documents = new InMemoryDocuments
      documents.declare(Vector(digest -> terms("Cards")))(using TestTx.fake)
      val held = write(documents, digest, "Cards", "k", "Post it all.", None, Integrity.Untrusted)
      val turn = say(entries, "one")
      val posting = Toolbox
        .of[caps.CapSet^{NoCheckout}](hostedPost.over(_ => grit.core.tool.Outcome.Done("posted")))
        .fold(d => throw new java.lang.AssertionError(d), identity)
      new InMemoryDurable().run(turn.workflowId)(
        followingBody(entries, documents, Vector(held), provider, posting)
      )
      provider.requests.headOption.map(r => r.tools.map(_.name) -> r.messages) ==> Some(
        Vector("read_hidden", "extract", "post") -> Vector(
          Message.User(
            "[doc] Cards, at slack:T1/C1, 2026-09-30:\n" +
              "Hidden here as h1: something from Cards, which people outside the team can " +
              "write in. If your answer needs what it says, read it: call read_hidden with name " +
              "h1. After that you can't use post until you've answered. To ask it a short " +
              "question instead, answered yes or no, with a whole number, a date, one of " +
              "options you list or a short code, call extract with name h1: that never stops " +
              "you doing anything."
          ),
          dated,
          Message.User("one"),
          Message.User(TurnGate.noted(Vector(TurnGate.Guidance)))
        )
      )
    }

    test(
      "record-window counts each document the window holds as placed once, at the window's time; a replayed turn counts none again"
    ) {
      val entries = new InMemoryEntryStore
      val documents = new InMemoryDocuments
      documents.declare(Vector(digest -> terms("Weekly digest")))(using TestTx.fake)
      val week = write(documents, digest, "Weekly digest", "week", "Deploys frozen until Friday.")
      val month = write(documents, digest, "Weekly digest", "month", "Two releases shipped.")
      val turn = say(entries, "hello")
      val durable = new InMemoryDurable
      val provider = new RecordingProvider
      val at = Instant.parse("2026-10-01T09:00:00Z")
      val clock = new Clock {
        def now(): Instant = at
        def millis(): Long = at.toEpochMilli
        def sleep(duration: FiniteDuration): Unit = ()
      }
      def body(id: WorkflowId)(using Durable^): String =
        turnBodyWith(
          entries,
          provider,
          windowing(Window(Vector(EntrySeq(0)), Vector.empty, Vector.empty, Vector(week, month))),
          new InMemoryUsageLedger,
          documents = documents,
          clock = clock
        )(id)
      durable.run(turn.workflowId)(body(_))
      ok(entries.list(conversation)(using TestTx.fake))
        .collectFirst { case e if e.id == Turn.windowId(turn) => e.createdAt } ==> Some(at)
      def placements = ok(documents.read(Vector(week, month))(using TestTx.fake)).map(_.placement)
      placements ==> Vector.fill(2)(Placement(1, at))
      assert(durable.replay(turn.workflowId, durable.history(turn.workflowId))(body(_)).isRight)
      placements ==> Vector.fill(2)(Placement(1, at))
    }

    test(
      "record-window marks the file of each piece the window holds placed at the window's time, the bound's order"
    ) {
      val entries = new InMemoryEntryStore
      val pieces = new InMemoryPieces
      val files = got(PluginName.of("files"))
      val (one, two, other) = (
        kept(pieces, files, "Accounts are reviewed."),
        kept(pieces, files, "Keys rotate."),
        kept(pieces, files, "Backups run nightly.")
      )
      val turn = say(entries, "hello")
      val at = Instant.parse("2026-10-01T09:00:00Z")
      val clock = new Clock {
        def now(): Instant = at
        def millis(): Long = at.toEpochMilli
        def sleep(duration: FiniteDuration): Unit = ()
      }
      val _ = new InMemoryDurable().run(turn.workflowId)(
        turnBodyWith(
          entries,
          new RecordingProvider,
          windowing(
            Window(
              Vector(EntrySeq(0)),
              Vector.empty,
              Vector.empty,
              Vector.empty,
              Vector(Retrieved.Piece(one, 0), Retrieved.Piece(two, 0))
            )
          ),
          new InMemoryUsageLedger,
          pieces = pieces,
          clock = clock
        )
      )
      Vector(one, two, other).map(pieces.lastPlaced) ==> Vector(Some(at), Some(at), Some(written))
    }

    test(
      "a turn whose calls followed a document's pointer counts it once, at its reply's time; a replayed turn counts none again"
    ) {
      val entries = new InMemoryEntryStore
      val documents = new InMemoryDocuments
      documents.declare(Vector(digest -> terms("Cards")))(using TestTx.fake)
      val followed =
        write(documents, digest, "Cards", "readme", "Readme.", pointing("peek", "README.md"))
      val other = write(documents, digest, "Cards", "other", "Other.", pointing("peek", "OTHER.md"))
      val plain = write(documents, digest, "Cards", "plain", "Plain.")
      val held = Vector(followed, other, plain)
      val turn = say(entries, "what does the readme say?")
      val durable = new InMemoryDurable
      val clock = ticking
      def body(id: WorkflowId)(using Durable^): String =
        followingBody(
          entries,
          documents,
          held,
          calling("peek", "README.md", times = 2),
          succeeding,
          clock
        )(id)
      val output = durable.run(turn.workflowId)(body(_))
      assert(output.startsWith("replied:"))
      val replied = ok(entries.get(turn.replyId)(using TestTx.fake)).map(_.createdAt)
      val once = Vector(replied.flatMap(Following.of(1, _)), None, None)
      (following(documents, held), once.headOption.flatten.isDefined) ==> (once, true)
      assert(durable.replay(turn.workflowId, durable.history(turn.workflowId))(body(_)).isRight)
      following(documents, held) ==> once
    }

    test("the versions a turn's append counted followed read back from the steps it recorded") {
      val entries = new InMemoryEntryStore
      val documents = new InMemoryDocuments
      documents.declare(Vector(digest -> terms("Cards")))(using TestTx.fake)
      val followed =
        write(documents, digest, "Cards", "readme", "Readme.", pointing("peek", "README.md"))
      val other = write(documents, digest, "Cards", "other", "Other.", pointing("peek", "OTHER.md"))
      val turn = say(entries, "what does the readme say?")
      val durable = new InMemoryDurable
      val output = durable.run(turn.workflowId)(
        followingBody(
          entries,
          documents,
          Vector(followed, other),
          calling("peek", "README.md"),
          succeeding
        )
      )
      assert(output.startsWith("replied:"))
      TurnRecord.followed(durable.history(turn.workflowId).map(FlowFixtures.recorded)) ==>
        Right(Some(Vector(followed)))
    }

    test("a heard-rooted turn whose draft's call followed a document's pointer counts it") {
      val entries = new InMemoryEntryStore
      val documents = new InMemoryDocuments
      documents.declare(Vector(digest -> terms("Cards")))(using TestTx.fake)
      val followed =
        write(documents, digest, "Cards", "readme", "Readme.", pointing("peek", "README.md"))
      val turn = hear(entries, "someone should read the readme")
      new InMemoryDurable().run(turn.workflowId)(
        followingBody(
          entries,
          documents,
          Vector(followed),
          calling("peek", "README.md"),
          succeeding
        )
      )
      (
        ok(entries.get(turn.draftId)(using TestTx.fake)).isDefined,
        following(documents, Vector(followed))
      ) ==> (true, Vector(Following.of(1, appendedAt)))
    }

    test(
      "a call of a tool the turn was not offered is refused at bind and follows nothing"
    ) {
      val entries = new InMemoryEntryStore
      val documents = new InMemoryDocuments
      documents.declare(Vector(digest -> terms("Cards")))(using TestTx.fake)
      val pointed =
        write(documents, digest, "Cards", "readme", "Readme.", pointing("fetch", "README.md"))
      val turn = say(entries, "what does the readme say?")
      val output = new InMemoryDurable().run(turn.workflowId)(
        followingBody(
          entries,
          documents,
          Vector(pointed),
          calling("fetch", "README.md"),
          succeeding
        )
      )
      (output.startsWith("replied:"), following(documents, Vector(pointed))) ==> (
        true,
        Vector(None)
      )
    }
  }
}
