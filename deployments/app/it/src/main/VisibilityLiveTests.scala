package grit.app.main

import java.time.Instant

import scala.annotation.unused
import scala.concurrent.duration.*
import scala.util.Using

import grit.assembly.estimate.CharEstimate
import grit.assembly.retrieval.RetrievalAssembler
import grit.core.clock.{Clock, SetClock}
import grit.core.context.{AssemblyRequest, Window}
import grit.core.document.{DocLabel, DocText, DocWeight, DocumentKeeper, DocumentTerms}
import grit.core.durable.Durable
import grit.core.id.{
  CallSlot,
  CloseRef,
  ConversationId,
  DocKey,
  EntryId,
  EntrySeq,
  PeriodRef,
  PeriodSeq,
  PluginName,
  PrincipalId,
  SourceId,
  ToolCallId,
  TurnRef,
  WorkflowId
}
import grit.core.job.{InMemorySchedules, OwnJobs, ScheduleDesk}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.period.{CloseReason, Probability, TestClosings}
import grit.core.place.{Namespace, Place}
import grit.core.plugin.{Documents, Needs, Plugin}
import grit.core.provider.{ModelRequest, Provider, ProviderError}
import grit.core.store.{ClosedPeriod, Db, Entry, Nearby, Origin, Payload, Sealed, StoreError, Tx}
import grit.core.tool.{Bound, Outcome, Repairs, Toolbox}
import grit.core.visibility.{
  Compartments,
  Grant,
  Group,
  Label,
  Labelled,
  RoomLabels,
  Subject,
  TestLabels,
  Visibility
}
import grit.dbos.engine.LiveEngine
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}
import grit.digest.Digest
import grit.lifecycle.post.{PostEnv, Posting}

import utest.*

/** Visibility labels end to end, against Postgres and DBOS (ADR 0030): two Slack rooms of one
  * team, `#a` labelled `{trial}` and `#b` public, a group `trial` holding one person, the
  * Digest posted through the engine, and each asker's retrieval window and `recent_activity`
  * read as the turn reads them. The same scenario under [[Visibility.Shipped]] gives the
  * windows grit gave before labels.
  */
object VisibilityLiveTests extends TestSuite {

  private val Team = "T1"

  private def thread(channel: String, ts: String): Origin = Origin.Slack(Team, channel, ts)

  private val RoomA: Place = thread("a", "0").room

  /** Cleared for `{trial}`, by the group `trial`. */
  private val Cleared: PrincipalId = TestLabels.Trialist

  /** In no group. */
  private val Uncleared: PrincipalId = PrincipalId("slack:T1/U-uncleared")

  /** `trial` declared, `#a` at `{trial}` and every other room public, and [[Cleared]] alone in
    * the group `trial`, cleared for `{trial}`.
    */
  private val Declared: Visibility =
    (for {
      compartments <- Compartments.of(Vector(TestLabels.trial)).left.map(_.toString)
      rooms <- RoomLabels
        .of(Vector(RoomA -> TestLabels.Trial), Labelled.Mapped(Label.Public))
        .left
        .map(_.written)
      v <- Visibility
        .of(
          compartments,
          rooms,
          Vector(Group(TestLabels.group("trial"), Set(Cleared))),
          Vector(Grant(TestLabels.group("trial"), TestLabels.Trial))
        )
        .left
        .map(_.toString)
    } yield v).fold(e => throw new java.lang.AssertionError(e), identity)

  /** Writes every query as `zephyr`, the word every thread and document below holds. */
  private object Query extends Provider {
    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] =
      Right(
        Message.Assistant(
          Vector(AssistantBlock.Text("zephyr")),
          StopReason.EndTurn,
          Usage.Zero,
          "q"
        )
      )
  }

  /** Keeps documents the scenario writes itself, and posts nothing. */
  private final class Notes(val name: PluginName) extends Plugin {
    val version = 1
    override val documents: Option[Documents] = Some(new Documents {
      val terms: DocumentTerms = Notes.Terms
      def post(closed: ClosedPeriod, keeper: DocumentKeeper)(using Tx^): Either[StoreError, Unit] =
        Right(())
    })
  }

  private object Notes {
    val Terms: DocumentTerms =
      (for {
        label <- DocLabel.of("notes")
        terms <- DocumentTerms.of(label, DocWeight.Unscaled, 1.day, 10)
      } yield terms).fold(e => throw new java.lang.AssertionError(e), identity)
  }

  private def plugin(s: String): PluginName =
    PluginName.of(s).fold(e => throw new java.lang.AssertionError(e), identity)

  private def ok[A](what: String)(e: Either[?, A]): A =
    e.fold(why => throw new java.lang.AssertionError(s"$what: $why"), identity)

  private def nothing(id: WorkflowId)(using @unused d: Durable^): String = WorkflowId.value(id)

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

  /** What one asker was given: its window, each item as the model would be shown it, and what
    * `recent_activity` answered it.
    */
  private final case class Asked(window: Vector[String], recent: Outcome)

  /** The scenario's askers, each a fresh thread asking `zephyr?`: an uncleared and a cleared
    * person in `#a`, then a cleared person in `#b`.
    */
  private final case class Seen(
      unclearedInA: Asked,
      clearedInA: Asked,
      clearedInB: Asked,
      resultRead: Vector[String]
  )

  /** The scenario in a fresh database `name` under `visibility`:
    *   - in `#a`, a closed thread whose closing says when the launch is, and an open one where a
    *     cleared person's tool call found the budget;
    *   - in `#b`, a closed thread and an open one, an uncleared person's;
    *   - the Digest posted through the engine: one document per room;
    *   - three of Notes' documents: a public one kept in no room, and two at `#a`'s label, one
    *     kept in no room and one in `#b`.
    * Then each asker's window and `recent_activity`, in turn, and what the uncleared asker in
    * `#a` reads of the cleared person's open thread.
    */
  private def scenario(name: String, visibility: Visibility): Seen = {
    val config = TestPostgres.freshDatabase(name)
    val engine = LiveEngine.open(config, "test", visibility = visibility)
    try {
      val digest = new Digest(plugin("digest"))
      val notes = new Notes(plugin("notes"))
      val plugins: Vector[Plugin] = Vector(digest, notes)
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
      ok("enrolling")(engine.jot.write(Subject.Public) {
        for {
          _ <- engine.principals.enroll(Cleared, "Cleared")
          _ <- engine.principals.enroll(Uncleared, "Uncleared")
        } yield ()
      })
      val names = scala.collection.mutable.Map.empty[ConversationId, String]

      def ask(label: String, origin: Origin, text: String, by: PrincipalId): TurnRef = {
        val turn = ok("ingesting")(
          engine.inbox.ingest(origin, SourceId(label), Message.User(text), by)
        )
        names.update(turn.conversationId, label)
        turn
      }

      def append(turn: TurnRef, n: Int, payload: Payload): Unit =
        ok("appending")(engine.jot.write(Subject.Turn(turn)) {
          for {
            next <- engine.entries.lockNext(turn.conversationId)
            _ <- engine.entries.insert(
              Entry(
                EntryId(s"${ConversationId.value(turn.conversationId)}:${turn.turnSeq}:$n"),
                turn.conversationId,
                turn.turnSeq,
                None,
                next.seq,
                payload,
                Instant.now()
              )
            )
          } yield ()
        })

      def reply(turn: TurnRef, text: String): Unit =
        append(
          turn,
          9,
          Payload.Message(
            Message.Assistant(
              Vector(AssistantBlock.Text(text)),
              StopReason.EndTurn,
              Usage.Zero,
              "m"
            )
          )
        )

      def close(turn: TurnRef, outcome: String, at: String): Unit = {
        val ref = PeriodRef(turn.conversationId, PeriodSeq.First)
        ok("sealing")(
          engine.jot.write(Subject.Conversation(turn.conversationId))(
            engine.periods.seal(
              CloseRef(ref, turn.turnSeq, Instant.EPOCH),
              CloseReason.Resolved(Probability.One),
              TestClosings.prose(s"$outcome. Settled.", Some(outcome)),
              Instant.parse(at)
            )
          )
        ) ==> Sealed.Closed(ref.closingId)
      }

      val launch = ask("a-closed", thread("a", "1"), "When is the zephyr launch?", Cleared)
      reply(launch, "The zephyr launch is on Tuesday.")
      close(launch, "zephyr launches Tuesday", "2026-09-21T10:00:00Z")

      val budget = ask("a-open", thread("a", "2"), "What is the zephyr budget?", Cleared)
      val call = ToolCallId("budget")
      append(
        budget,
        1,
        Payload.Exchange(
          Message.Assistant(
            Vector(AssistantBlock.ToolCall(call, "read", ujson.Obj("path" -> "budget.md"))),
            StopReason.ToolUse,
            Usage.Zero,
            "m"
          )
        )
      )
      append(
        budget,
        2,
        Payload.Result(Message.ToolResult(call, "zephyr budget: 40k", isError = false), "read")
      )
      reply(budget, "The zephyr budget is 40k.")

      val agenda = ask("b-closed", thread("b", "1"), "Is zephyr on the agenda?", Uncleared)
      reply(agenda, "zephyr is on Thursday's agenda.")
      close(agenda, "zephyr is on Thursday's agenda", "2026-09-22T10:00:00Z")

      val docs = ask("b-open", thread("b", "2"), "Who owns the zephyr docs?", Uncleared)
      reply(docs, "Nobody owns the zephyr docs yet.")

      // Posting runs one workflow per plugin from its cursor's start: every closing so far.
      val posted = ok("sweeping")(engine.sweep(Instant.now())).posted
      posted.size ==> plugins.size
      posted.foreach(run => assert(finished(config, run.workflowId)))

      val keeper = engine.keeper(notes.name, Notes.Terms)
      val high = visibility.roomLabel(RoomA)
      def note(subject: Subject, key: String, label: Label, text: String): Unit =
        ok("noting")(engine.jot.write(subject) {
          for {
            k <- DocKey.of(key).left.map(StoreError.Invalid(_))
            t <- DocText.of(text).left.map(StoreError.Invalid(_))
            _ <- keeper.write(
              k,
              label,
              Place.under(Namespace.Task, Vector("notes")),
              t,
              ujson.Obj(),
              Instant.parse("2026-09-01T00:00:00Z")
            )
          } yield ()
        })
      note(Subject.Public, "public", Label.Public, "A public zephyr note.")
      note(Subject.Public, "nowhere", high, "A zephyr note kept in no room.")
      note(Subject.Conversation(docs.conversationId), "in-b", high, "A zephyr note kept in #b.")

      val assembler = new RetrievalAssembler(
        engine.entries,
        engine.conversations,
        engine.periods,
        engine.principals,
        engine.lifecycle,
        engine.search,
        engine.documents,
        engine.stitches,
        Query,
        CharEstimate,
        Tokens(24_000),
        RetrievalAssembler.DefaultTail,
        grit.core.stitch.Tuning.Default
      )
      val store: Db^ = engine.db
      val desk: ScheduleDesk^ = new InMemorySchedules()
        .desk(digest.name, Vector.empty, new SetClock(Instant.parse("2026-10-01T00:00:00Z")))
      val recent = ok("binding")(
        Digest.RecentActivity
          .bind(
            engine.reads(digest.name),
            Needs.over(digest.name, Vector.empty),
            OwnJobs.over(digest.name, Vector.empty)
          )
      )
      val tools = ok("tools")(
        Toolbox.of[caps.CapSet^{store, desk}](
          Digest.RecentActivity.described
            .calling((n, at) => recent.run(n, at, store.as(Subject.Turn(at.turn)), desk))
        )
      )

      def text(e: Entry): String = e.payload match {
        case Payload.Message(Message.User(t)) => t
        case Payload.Message(Message.Assistant(blocks, _, _, _, _)) =>
          blocks.collect { case AssistantBlock.Text(t) => t }.mkString
        case Payload.Closed(_, _, closing) => closing.headline
        case Payload.Result(r, _) => r.content
        case other => other.productPrefix
      }

      def shown(turn: TurnRef, window: Window): Vector[String] =
        LiveDb.transaction(config) {
          def at(c: ConversationId, seqs: Vector[EntrySeq], kind: String): Vector[String] =
            ok("reading the window's entries")(engine.entries.at(c, seqs))
              .map(e => s"$kind ${names.getOrElse(c, "?")}: ${text(e)}")
          val own: Vector[String] = at(turn.conversationId, window.entries, "own")
          val near: Vector[String] = window.nearby.flatMap {
            case Nearby.Open(c, _, seqs) => at(c, seqs, "open")
            case Nearby.Closed(c, _, seq) => at(c, Vector(seq), "closed")
            case Nearby.Along(c, _, seqs) => at(c, seqs, "along")
            case Nearby.Asked(c, _, seqs) => at(c, seqs, "asked")
          }
          val documents: Vector[String] =
            ok("reading the window's documents")(engine.documents.read(window.documents))
              .map(d => s"document ${PluginName.value(d.plugin)}: ${DocText.value(d.text)}")
          own ++ near ++ documents
        }

      def asked(label: String, origin: Origin, by: PrincipalId): (TurnRef, Asked) = {
        val turn = ask(label, origin, "zephyr?", by)
        val window = ok("assembling")(assembler.assemble(AssemblyRequest(turn))(using store))
        val slot = CallSlot.of(turn, 0, 0).getOrElse(throw new java.lang.AssertionError("slot"))
        val outcome = tools.bind(
          AssistantBlock.ToolCall(ToolCallId("recent"), "recent_activity", ujson.Obj()),
          Repairs.All
        ) match {
          case Right(free: Bound.Free) => free(slot)
          case other => throw new java.lang.AssertionError(s"not free: $other")
        }
        (turn, Asked(shown(turn, window), outcome))
      }

      val (unclearedTurn, unclearedInA) = asked("a-uncleared", thread("a", "3"), Uncleared)
      val (_, clearedInA) = asked("a-cleared", thread("a", "4"), Cleared)
      val (_, clearedInB) = asked("b-cleared", thread("b", "3"), Cleared)
      val resultRead = ok("reading as the uncleared asker")(
        store.read(Subject.Turn(unclearedTurn))(engine.entries.list(budget.conversationId))
      ).collect { case Entry(_, _, _, _, _, Payload.Result(r, _), _) => r.content }
      Seen(unclearedInA, clearedInA, clearedInB, resultRead)
    } finally engine.close()
  }

  /** What the scenario's rows are shown as. */
  private object Shown {
    val aOpen: Vector[String] =
      Vector("open a-open: What is the zephyr budget?", "open a-open: The zephyr budget is 40k.")
    val aClosed = "closed a-closed: zephyr launches Tuesday"
    val bOpen: Vector[String] =
      Vector(
        "open b-open: Who owns the zephyr docs?",
        "open b-open: Nobody owns the zephyr docs yet."
      )
    val bClosed = "closed b-closed: zephyr is on Thursday's agenda"
    val publicNote = "document notes: A public zephyr note."
    val noteInB = "document notes: A zephyr note kept in #b."
    val noteNowhere = "document notes: A zephyr note kept in no room."
    val aLine = "2026-09-21 10:00 UTC · slack #a · resolved · zephyr launches Tuesday"
    val bLine = "2026-09-22 10:00 UTC · slack #b · resolved · zephyr is on Thursday's agenda"
    val aDigest = s"document digest: Closed here, newest first:\n$aLine"
    val bDigest = s"document digest: Closed here, newest first:\n$bLine"
  }

  private lazy val labelled: Seen = scenario("visibility_labelled", Declared)

  val tests = Tests {
    test(
      "#b's window and recent_activity hold nothing of #a's, its closing and digest line included, though its asker is cleared"
    ) {
      import Shown.*
      (labelled.clearedInB.window, labelled.clearedInB.recent) ==> (
        Vector(bClosed) ++ bOpen ++ Vector(publicNote, bDigest),
        Outcome.Done(bLine)
      )
    }

    test(
      "an uncleared asker in #a is shown #a's earlier threads, their closing, a cleared asker's tool call's reply, and #a's digest document"
    ) {
      import Shown.*
      (labelled.unclearedInA.window, labelled.unclearedInA.recent) ==> (
        aOpen ++ Vector(bClosed, aClosed) ++ bOpen ++ Vector(publicNote, aDigest, bDigest),
        Outcome.Done(s"$bLine\n$aLine")
      )
      // A tool's result is never in a window; the asker reads it in its room as it reads the
      // reply drawn from it.
      labelled.resultRead ==> Vector("zephyr budget: 40k")
    }

    test("a {trial} document kept in no room, or in #b, reaches the cleared asker in #a only") {
      import Shown.*
      Vector(labelled.unclearedInA, labelled.clearedInA, labelled.clearedInB)
        .map(_.window.filter(_.startsWith("document notes"))) ==> Vector(
        Vector(publicNote),
        Vector(publicNote, noteInB, noteNowhere),
        Vector(publicNote)
      )
    }

    // Under the shipped visibility every label is public: each conversation, entry and
    // document is kept at public and every clearance reads public everywhere, so each filter
    // passes every row. These are the windows grit gave before labels: every asker is shown
    // every thread, closing and document, whatever its room.
    test(
      "under the shipped visibility each asker's window and recent_activity are as before labels"
    ) {
      import Shown.*
      val shipped = scenario("visibility_shipped", Visibility.Shipped)
      val all = Vector(bClosed, aClosed) ++ bOpen ++
        Vector(publicNote, noteInB, noteNowhere, aDigest, bDigest)
      val recent = Outcome.Done(s"$bLine\n$aLine")
      shipped ==> Seen(
        Asked(aOpen ++ all, recent),
        Asked(Vector("open a-uncleared: zephyr?") ++ aOpen ++ all, recent),
        Asked(
          Vector("open a-cleared: zephyr?", "open a-uncleared: zephyr?") ++ aOpen ++ all,
          recent
        ),
        Vector("zephyr budget: 40k")
      )
    }
  }
}
