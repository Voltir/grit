package grit.app.main

import java.time.Instant

import scala.annotation.unused
import scala.concurrent.duration.*
import scala.util.Using

import grit.core.claim.Claims
import grit.core.clock.{Clock, SetClock}
import grit.core.document.{DocLabel, DocText, DocWeight, DocumentKeeper, DocumentTerms}
import grit.core.durable.Durable
import grit.core.edge.CommandError
import grit.core.flow.TestTainted
import grit.core.id.{
  CloseRef,
  DocKey,
  PeriodRef,
  PeriodSeq,
  PluginName,
  SourceId,
  TestCallSlots,
  ToolCallId,
  WorkflowId
}
import grit.core.identity.Account
import grit.core.job.{InMemorySchedules, OwnJobs, ScheduleDesk}
import grit.core.message.{AssistantBlock, Message}
import grit.core.need.{Needs, PluginReads}
import grit.core.period.{CloseOrdinal, CloseReason, Probability, TestClosings}
import grit.core.place.{Namespace, Place}
import grit.core.plugin.{
  CacheDocs,
  CachePosting,
  CommandBy,
  CommandOutcome,
  DocumentPosting,
  Documents,
  KeepingCommand,
  Plugin,
  PluginCommand,
  PostRef,
  ReadingCommand
}
import grit.core.route.Routed
import grit.core.store.{ClosedPeriod, Db, Origin, Sealed, StoreError, Tx}
import grit.core.tool.{Args, Bound, Field, Outcome, Repairs, ToolName, ToolSpec, Toolbox}
import grit.core.visibility.Integrity
import grit.core.visibility.Subject
import grit.dbos.engine.{Engine, LiveEngine}
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}
import grit.digest.Digest
import grit.lifecycle.post.{PostEnv, Posting}

import utest.*

/** Plugins over DBOS against a real Postgres: posting from closing entries alone, the
  * cursor and the plugin's writes committing together, Digest's backfill and surface, and a
  * plugin's commands as an edge runs them for a person in a room.
  */
object PluginLiveTests extends TestSuite {

  private def name(s: String): PluginName = PluginName.of(s).getOrElse(sys.error(s))

  private def nothing(id: WorkflowId)(using @unused d: Durable^): String = WorkflowId.value(id)

  /** Writes a document, then refuses: nothing it wrote may outlive the refusal. */
  private final class Refusing(val name: PluginName) extends Plugin {
    val version = 1
    override val cache: Option[CachePosting] = Some(new CachePosting {
      def post(closed: ClosedPeriod, docs: CacheDocs)(using Tx^): Either[StoreError, Unit] =
        docs
          .put("half", ujson.Str("written before the refusal"))
          .flatMap(_ => Left(StoreError.Invalid("refused")))
    })
  }

  /** Keeps one document per closed period, under its close ordinal, holding its closing's
    * time.
    */
  private final class Noting(val name: PluginName) extends Plugin {
    val version = 1
    override val documents: Option[Documents] = Some(new Documents with DocumentPosting {
      val posting: Option[DocumentPosting] = Some(this)
      val terms: DocumentTerms =
        (for {
          label <- DocLabel.of("notes")
          terms <- DocumentTerms.of(label, DocWeight.Unscaled, 1.day, 10)
        } yield terms).fold(e => sys.error(e), identity)
      def post(closed: ClosedPeriod, keeper: DocumentKeeper)(using Tx^): Either[StoreError, Unit] =
        (for {
          key <- DocKey.of(CloseOrdinal.value(closed.order).toString)
          text <- DocText.of(s"closed at ${closed.at}")
        } yield (key, text)) match {
          case Left(why) => Left(StoreError.Invalid(why))
          case Right((key, text)) =>
            keeper
              .write(
                key,
                grit.core.visibility.Label.Public,
                Integrity.Member,
                Place.under(Namespace.Task, Vector("notes")),
                text,
                ujson.Obj(),
                None,
                closed.at
              )
              .map(_ => ())
        }
    })
  }

  /** `n` periods of one conversation, each one message, closed in turn: as a database
    * holds them before any plugin is enabled.
    */
  private def closed(engine: Engine^, n: Int): Unit =
    for (i <- 1 to n) {
      val t = engine.inbox
        .ingest(
          Origin.Tui(
            grit.core.place.Directory.of("/plugins").fold(e => sys.error(e), identity),
            "plugins"
          ),
          SourceId(s"m$i"),
          Message.User(s"message $i"),
          Account.Local,
          None
        )
        .fold(e => sys.error(s"$e"), identity)
      val closing = TestClosings.prose(s"Period $i. More.")
      val ref = PeriodRef(t.conversationId, PeriodSeq.of(i.toLong).getOrElse(sys.error("seq")))
      engine.jot.write(Subject.Public)(
        engine.periods.seal(
          CloseRef(ref, t.turnSeq, Instant.EPOCH),
          CloseReason.Resolved(Probability.One),
          closing,
          Instant.parse(s"2026-09-2${i}T10:00:00Z")
        )
      ) ==>
        Right(Sealed.Closed(ref.closingId))
    }

  /** Waits up to 30 s for workflow `id` to finish, and says whether it did. */
  private def finished(config: DbConfig, id: WorkflowId): Boolean = {
    val until = System.nanoTime() + 30.seconds.toNanos
    def done = LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(
        conn.prepareStatement("SELECT status FROM dbos.workflow_status WHERE workflow_uuid = ?")
      ) { ps =>
        ps.setString(1, WorkflowId.value(id))
        Using.resource(ps.executeQuery())(rs => rs.next() && rs.getString(1) == "SUCCESS")
      }
    }
    var held = done
    while (!held && System.nanoTime() < until) { Thread.sleep(50); held = done }
    held
  }

  private def launch(engine: Engine^, plugins: Vector[Plugin]): Unit =
    engine.launch(
      nothing,
      nothing,
      nothing,
      Posting.body(
        plugins,
        PostEnv(
          engine.periods,
          engine.cursors,
          engine.cache,
          engine.keeper,
          engine.tombstones,
          engine.jot,
          Clock.system()
        )
      ),
      nothing,
      LiveEngine.Unplaced,
      plugins
    )

  /** What the commands below read: a key, and a text to keep under it. */
  private final case class Note(key: String, text: String) extends caps.Pure

  private def noting(called: ToolName): ToolSpec[Note] =
    ToolSpec(
      called,
      "Keeps a note.",
      Args
        .of((key = Field.text("The key."), text = Field.text("The text.")))
        .map(a => Note(a.key, a.text))
    )

  /** Writes `text` under `key` at public, believed at the most, kept at `notes`. */
  private def write(own: DocumentKeeper, key: String, text: String, at: Instant)(using
      Tx^
  ): Either[StoreError, Unit] =
    (for {
      k <- DocKey.of(key)
      t <- DocText.of(text)
    } yield (k, t)) match {
      case Left(why) => Left(StoreError.Invalid(why))
      case Right((k, t)) =>
        own
          .write(
            k,
            grit.core.visibility.Label.Public,
            Integrity.Authority,
            Place.under(Namespace.Task, Vector("notes")),
            t,
            ujson.Obj(),
            None,
            at
          )
          .map(_ => ())
    }

  /** `note`: keeps the note, answering Done; a text of `refuse` is kept first, then refused. */
  private object Keeps extends KeepingCommand[Note] {
    val spec: ToolSpec[Note] = noting(ToolName("note"))
    def run(args: Note, by: CommandBy, own: DocumentKeeper, claims: Claims, at: Instant)(using
        Tx^
    ): Either[StoreError, CommandOutcome] =
      write(own, args.key, args.text, at).map(_ =>
        if (args.text == "refuse") CommandOutcome.Refused("refused")
        else CommandOutcome.Done(s"kept ${args.key}")
      )
  }

  /** `twice`: keeps the note under its key and under its key with `-2`. */
  private object Twice extends KeepingCommand[Note] {
    val spec: ToolSpec[Note] = noting(ToolName("twice"))
    def run(args: Note, by: CommandBy, own: DocumentKeeper, claims: Claims, at: Instant)(using
        Tx^
    ): Either[StoreError, CommandOutcome] =
      for {
        _ <- write(own, args.key, args.text, at)
        _ <- write(own, s"${args.key}-2", args.text, at)
      } yield CommandOutcome.Done("kept twice")
  }

  /** `list`: the texts of the notes its transaction reads, sorted, joined by commas. */
  private object Lists extends ReadingCommand[Note] {
    val spec: ToolSpec[Note] = noting(ToolName("list"))
    def run(args: Note, by: CommandBy, own: PluginReads, at: Instant)(using
        Tx^
    ): Either[StoreError, CommandOutcome] =
      own.documents
        .newest(10)
        .map(ds => CommandOutcome.Done(ds.map(d => DocText.value(d.text)).sorted.mkString(",")))
  }

  /** `sneak`: a reading command writing through a keeper it was built with. */
  private final class Sneaks(keeper: DocumentKeeper) extends ReadingCommand[Note] {
    val spec: ToolSpec[Note] = noting(ToolName("sneak"))
    def run(args: Note, by: CommandBy, own: PluginReads, at: Instant)(using
        Tx^
    ): Either[StoreError, CommandOutcome] =
      write(keeper, args.key, args.text, at).map(_ => CommandOutcome.Done("wrote"))
  }

  private def terms(label: String, bound: Int, full: DocumentTerms.Full): DocumentTerms =
    (for {
      l <- DocLabel.of(label)
      t <- DocumentTerms.of(l, DocWeight.Unscaled, 1.day, bound, full = full)
    } yield t).fold(e => sys.error(e), identity)

  /** Notes, bound at 10; its commands are `note`, `list` and `sneak` over `sneaking`. */
  private final class Notebook(sneaking: DocumentKeeper) extends Plugin {
    val name: PluginName = PluginLiveTests.name("notebook")
    val version = 1
    override val documents: Option[Documents] = Some(new Documents {
      val terms: DocumentTerms = notebookTerms
      val posting: Option[DocumentPosting] = None
    })
    override val commands: Vector[PluginCommand[?]] = Vector(Keeps, Lists, new Sneaks(sneaking))
  }
  private val notebookTerms = terms("notes", 10, DocumentTerms.Full.Withdraw)

  /** A ledger of one record, refusing a second; its command is `twice`. */
  private object Ledger extends Plugin {
    val name: PluginName = PluginLiveTests.name("ledger")
    val version = 1
    override val documents: Option[Documents] = Some(new Documents {
      val terms: DocumentTerms = PluginLiveTests.terms("records", 1, DocumentTerms.Full.Refuse)
      val posting: Option[DocumentPosting] = None
    })
    override val commands: Vector[PluginCommand[?]] = Vector(Twice)
  }

  import CommandRooms.{ann, bea, board, desk, nobody, team}

  /** [[CommandRooms.commanding]] over the notebook and the ledger. */
  private def commanding[A](suite: String)(
      body: (DbConfig, Engine^, grit.core.edge.PluginCommands^) => A
  ): A =
    CommandRooms.commanding(
      suite,
      engine => Vector(new Notebook(engine.keeper(name("notebook"), notebookTerms)), Ledger)
    )(body)

  private def kept(
      config: DbConfig,
      engine: Engine^,
      plugin: String
  ): Vector[(String, String, String)] =
    CommandRooms.kept(config, engine, name(plugin))

  private def note(key: String, text: String): ujson.Value = ujson.Obj("key" -> key, "text" -> text)

  private val at = Instant.parse("2026-10-27T10:00:00Z")

  val tests = Tests {
    test(
      "a keeping command keeps at the room's label, believed at no more than the edge gave, answering Done"
    ) {
      commanding("command_keeps") { (config, engine, commands) =>
        commands.run(ann, desk, Integrity.Member, ToolName("note"), note("k", "hello"), at) ==>
          Right(CommandOutcome.Done("kept k"))
        kept(config, engine, "notebook") ==> Vector(("k", "internal", "member"))
      }
    }

    test("an edge's integrity above the room's cap is cut to the cap") {
      commanding("command_capped") { (config, engine, commands) =>
        commands.run(ann, team, Integrity.Member, ToolName("note"), note("k", "hello"), at) ==>
          Right(CommandOutcome.Done("kept k"))
        kept(config, engine, "notebook") ==> Vector(("k", "internal", "outside"))
      }
    }

    test("a refused command keeps nothing it wrote first") {
      commanding("command_refused") { (config, engine, commands) =>
        commands.run(ann, desk, Integrity.Member, ToolName("note"), note("k", "refuse"), at) ==>
          Right(CommandOutcome.Refused("refused"))
        kept(config, engine, "notebook") ==> Vector()
      }
    }

    test("a reading command that writes is Left(Store(DatabaseError))") {
      commanding("command_sneaks") { (config, engine, commands) =>
        val ran = commands.run(ann, desk, Integrity.Member, ToolName("sneak"), note("k", "x"), at)
        assertMatch(ran) {
          case Left(CommandError.Store(_, StoreError.DatabaseError(cause)))
              if cause.contains("read-only") =>
        }
        kept(config, engine, "notebook") ==> Vector()
      }
    }

    test("a write past the bound is Left(Store(Full)), keeping nothing") {
      commanding("command_full") { (config, engine, commands) =>
        commands.run(ann, desk, Integrity.Member, ToolName("twice"), note("k", "x"), at) ==>
          Left(CommandError.Store(ToolName("twice"), StoreError.Full(1)))
        kept(config, engine, "ledger") ==> Vector()
      }
    }

    test("a command reads nothing above its asker's clearance") {
      commanding("command_cleared") { (_, _, commands) =>
        commands.run(bea, board, Integrity.Member, ToolName("note"), note("b", "board"), at) ==>
          Right(CommandOutcome.Done("kept b"))
        commands.run(ann, board, Integrity.Member, ToolName("note"), note("a", "staff"), at) ==>
          Right(CommandOutcome.Done("kept a"))
        (
          commands.run(ann, board, Integrity.Member, ToolName("list"), note("", ""), at),
          commands.run(bea, board, Integrity.Member, ToolName("list"), note("", ""), at)
        ) ==> (Right(CommandOutcome.Done("staff")), Right(CommandOutcome.Done("board,staff")))
      }
    }

    test("unknown names, unread arguments and unlinked accounts are Left, keeping nothing") {
      commanding("command_unread") { (config, engine, commands) =>
        (
          commands.run(ann, desk, Integrity.Member, ToolName("forget"), note("k", "x"), at),
          commands.run(ann, desk, Integrity.Member, ToolName("note"), ujson.Obj("key" -> "k"), at),
          commands.run(nobody, desk, Integrity.Member, ToolName("note"), note("k", "x"), at)
        ) ==> (
          Left(CommandError.Unknown(ToolName("forget"))),
          Left(CommandError.Unread(ToolName("note"), "`text` is missing: it takes text.")),
          Left(CommandError.NotVouched(nobody))
        )
        kept(config, engine, "notebook") ==> Vector()
      }
    }

    test(
      "Digest enabled on a database with closed periods posts them all into their room's document, and lists them"
    ) {
      val config = TestPostgres.freshDatabase("plugin_backfill")
      val engine = LiveEngine.open(config, "test")
      try {
        val digest = new Digest(name("digest"))
        launch(engine, Vector(digest))
        closed(engine, 2)
        val run = PostRef(digest.name, digest.version, CloseOrdinal.Start, 0)
        engine.sweep(Instant.now()).map(_.posted) ==> Right(Vector(run))
        assert(finished(config, run.workflowId))
        engine.jot.write(Subject.Public)(
          engine.cursors.start(digest.name, digest.version, java.time.Instant.now())
        ) ==> Right(CloseOrdinal.of(2).getOrElse(sys.error("o")))
        engine.sweep(Instant.now()).map(_.posted) ==> Right(Vector())
        // Both closed in one room: one document there, written as of the newer.
        engine.db
          .read(Subject.Public)(engine.reads(digest.name).documents.newest(10))
          .map(_.map(d => (d.place.written, d.written))) ==> Right(
          Vector(("fs:/plugins", Instant.parse("2026-09-22T10:00:00Z")))
        )
        val store: Db^ = engine.db
        val desk: ScheduleDesk^ = new InMemorySchedules()
          .desk(digest.name, Vector.empty, new SetClock(Instant.parse("2026-09-23T00:00:00Z")))
        val recent = Digest.RecentActivity
          .bind(
            engine.reads(digest.name),
            Needs.over(digest.name, Vector.empty),
            OwnJobs.over(digest.name, Vector.empty)
          )
          .fold(u => sys.error(s"$u"), identity)
        val tools = Toolbox
          .of[caps.CapSet^{desk}](
            Digest.RecentActivity.described
              .reading((n, at, reads) => recent.run(n, at, reads, desk))
          )
          .fold(d => sys.error(s"$d"), identity)
        tools.bind(
          TestTainted.authority(
            AssistantBlock.ToolCall(ToolCallId("c"), "recent_activity", ujson.Obj())
          ),
          Repairs.All
        ) match {
          // Run as the turn runs it: through reads for its turn, bounded by its routing.
          case Right(reading: Bound.Reading) =>
            Routed.reading(store, Subject.Turn(TestCallSlots.First.turn))((_, reads) =>
              reading.through(reads) match {
                case free: Bound.Free => free(TestCallSlots.First)
                case gated => Outcome.Failed(s"not free: $gated")
              }
            ) ==> Right(
              Outcome.Done(
                "2026-09-22 10:00 UTC · tui plugins · resolved · Period 2.\n" +
                  "2026-09-21 10:00 UTC · tui plugins · resolved · Period 1.",
                // Believed as the least of the documents read: closings of the local
                // operator's sessions.
                Some(Integrity.Authority)
              )
            )
          case other => sys.error(s"not reading: $other")
        }
      } finally engine.close()
    }

    test("a plugin's documents are posted through the engine, and its shelf reads them") {
      val config = TestPostgres.freshDatabase("plugin_documents")
      val engine = LiveEngine.open(config, "test")
      try {
        val noting = new Noting(name("noting"))
        launch(engine, Vector(noting))
        closed(engine, 2)
        val run = PostRef(noting.name, 1, CloseOrdinal.Start, 0)
        engine.sweep(Instant.now()).map(_.posted) ==> Right(Vector(run))
        assert(finished(config, run.workflowId))
        engine.db
          .read(Subject.Public)(engine.reads(noting.name).documents.newest(10))
          .map(_.map(d => (DocKey.value(d.key), DocText.value(d.text)))) ==> Right(
          Vector("2" -> "closed at 2026-09-22T10:00:00Z", "1" -> "closed at 2026-09-21T10:00:00Z")
        )
      } finally engine.close()
    }

    test(
      "a plugin that refuses leaves its cursor and nothing it wrote, and is run again a bounded number of times"
    ) {
      val config = TestPostgres.freshDatabase("plugin_refused")
      val engine = LiveEngine.open(config, "test")
      try {
        val refusing = new Refusing(name("refusing"))
        launch(engine, Vector(refusing))
        closed(engine, 1)
        val runs =
          (0 until PostRef.Attempts).toVector.map(PostRef(refusing.name, 1, CloseOrdinal.Start, _))
        // Still behind after each: the next run from the same cursor, under the next id.
        runs.map { run =>
          val posted = engine.sweep(Instant.now()).map(_.posted)
          assert(finished(config, run.workflowId))
          posted
        } ==> runs.map(run => Right(Vector(run)))
        engine.db.read(Subject.Public)(engine.reads(refusing.name).cache.get("half")) ==> Right(
          None
        )
        engine.jot.write(Subject.Public)(
          engine.cursors.start(refusing.name, 1, java.time.Instant.now())
        ) ==> Right(
          CloseOrdinal.Start
        )
        // Then the cursor is left for a person, its last run named.
        engine.sweep(Instant.now()).map(s => (s.posted, s.stuck)) ==>
          Right((Vector(), runs.lastOption.map(_.workflowId).toVector))
      } finally engine.close()
    }
  }
}
