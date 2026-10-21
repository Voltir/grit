package grit.eval.harness.capture

import java.time.Instant

import scala.concurrent.duration.*

import grit.assembly.estimate.CharEstimate
import grit.assembly.linear.LinearAssembler
import grit.core.clock.{Clock, Fresh}
import grit.core.context.{
  AssemblyError,
  AssemblyRequest,
  Briefs,
  ContextAssembler,
  Packing,
  Prepared,
  Querying,
  Results,
  Retrieved,
  Width,
  Window
}
import grit.core.document.{DocLabel, DocText, DocWeight, DocumentTerms, Pointer, Written}
import grit.core.durable.Durable
import grit.core.id.{
  CloseRef,
  DocKey,
  DocumentVersionId,
  PluginName,
  SourceId,
  TurnRef,
  TurnSeq,
  WorkflowId
}
import grit.core.identity.Account
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{Catalog, Pinned}
import grit.core.period.{CloseReason, Probability, TestClosings}
import grit.core.place.Directory
import grit.core.provider.{ModelRequest, Models, Provider, ProviderError, ToolUse}
import grit.core.speech.{Limits, Reach, Speaking}
import grit.core.spend.DailyCap
import grit.core.stitch.{StitchReads, Tuning}
import grit.core.store.{Db, Focus, Origin}
import grit.core.tool.{Args, Field, Gate, Outcome, Tool, ToolName, ToolSpec, Toolbox}
import grit.core.triage.Corpora
import grit.core.visibility.Integrity
import grit.core.visibility.Subject
import grit.dbos.engine.{Engine, LiveEngine}
import grit.dbos.internal.Reader
import grit.dbos.sql.TestPostgres
import grit.eval.harness.label.Verdicts
import grit.lifecycle.stitch.{Stitch, StitchEnv}
import grit.lifecycle.triage.{Triage, TriageEnv, TriageRecords, TriageSpeech}
import grit.models.{StubClassifier, StubModels, StubProvider}
import grit.turn.{
  Turn,
  TurnEnv,
  TurnHosting,
  TurnLoop,
  TurnOffer,
  TurnRecord,
  TurnRecords,
  TurnSpeech,
  TurnStitching,
  TurnTooling,
  TurnWeighing
}

import utest.*

/** Turns captured from a database a live engine ran them on: the real turn, a scripted model,
  * the stub classifier. Every message, draft, tool result, closing and model error holds
  * [[Marker]], which no captured file may.
  */
object TurnCaptureTests extends TestSuite {

  private val Marker = "zqxmarker"

  private val nothing = (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id)

  /** The system clock's now. */
  private def now(): Instant = Clock.system().now()

  private def eventually(done: => Boolean): Boolean = {
    val clock = Clock.system()
    val until = clock.millis() + 30.seconds.toMillis
    var held = done
    while (!held && clock.millis() < until) { clock.sleep(50.millis); held = done }
    held
  }

  private def right[E, A](e: Either[E, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why.toString), identity)

  /** The result `echo` gives every call. */
  private val Echoed = s"$Marker echoed"

  /** `echo`: a free tool run in the turn's own step, answering [[Echoed]]. */
  private val echo: Tool[String] =
    Tool(
      ToolSpec(
        ToolName("echo"),
        "Echoes.",
        Args.of((text = Field.text("What to echo."))).map(_.text)
      ),
      Gate.Free,
      t => t,
      _ => Outcome.Done(Echoed)
    )

  /** A model scripted by the turn's own message: `~fail` refuses; `~loop` calls `echo` until
    * two of its results are in, each with its count of results so far, then replies with the
    * results' words; `~pass` passes; "what was it" answers from the
    * record; anything else replies.
    */
  private final class Scripted extends Provider {
    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
      val said = request.messages.collect { case Message.User(t) => t }.lastOption.getOrElse("")
      val results = request.messages.count(_.isInstanceOf[Message.ToolResult])
      def reply(blocks: AssistantBlock*): Either[ProviderError, Message.Assistant] = Right(
        Message.Assistant(
          blocks.toVector,
          if (blocks.exists(_.isInstanceOf[AssistantBlock.ToolCall])) StopReason.ToolUse
          else StopReason.EndTurn,
          Usage(Tokens(100), Tokens(10), Tokens.Zero, Some(BigDecimal("0.001"))),
          "test/scripted"
        )
      )
      if (said.contains("~fail")) Left(ProviderError.Refused(s"$Marker refused"))
      else if (said.contains("~loop") && results < 2 && request.use == ToolUse.Auto)
        reply(
          AssistantBlock.Text(s"$Marker calling"),
          AssistantBlock.ToolCall(
            grit.core.id.ToolCallId(s"call-$results"),
            "echo",
            ujson.Obj("text" -> s"$Marker argument $results")
          )
        )
      else if (said.contains("~loop")) reply(AssistantBlock.Text(s"$Marker echoed back"))
      else if (said.contains("~pass")) reply(AssistantBlock.Text("pass"))
      else if (said.contains("what was it")) reply(AssistantBlock.Text(s"$Marker Falcon 4200"))
      else reply(AssistantBlock.Text(s"$Marker replied"))
    }
  }

  /** The stub's catalog: the turn's calls to `turn`, every other role's to the stub. */
  private final class ScriptedModels(turn: Provider^) extends Models {
    private val rest = new StubProvider()
    def catalog(): Either[String, Catalog] = Right(StubModels.Catalog)
    def provider(pinned: Pinned): Provider^ =
      if (pinned.assignment == StubModels.Catalog.policy.turn.first.assignment) turn else rest
  }

  /** V2's `gap` choosing `asks`, which the stub classifier answers from `~back:asks`. */
  private val asks = grit.core.triage.Tags.V2.asks(Probability.clamped(0.5))

  /** `under`'s windows, each holding `held` too, as a retrieval assembler's would. */
  private final class Holding(under: LinearAssembler, held: Vector[DocumentVersionId])
      extends ContextAssembler {
    def prepare(request: AssemblyRequest)(using Db^): Either[AssemblyError, Prepared] =
      under.prepare(request)
    def search(request: AssemblyRequest, written: Querying.Written)(using
        Db^
    ): Either[AssemblyError, Results] = under.search(request, written)
    def briefs(request: AssemblyRequest, retrieved: Vector[Retrieved], chars: Int)(using
        Db^
    ): Either[AssemblyError, Briefs] = under.briefs(request, retrieved, chars)
    def assemble(request: AssemblyRequest, packing: Packing)(using
        Db^
    ): Either[AssemblyError, Window] =
      under.assemble(request, packing).map(_.copy(documents = held))
  }

  /** The plugin every document here is kept by. */
  private val notes = right(PluginName.of("notes"))
  private val terms =
    right(DocumentTerms.of(right(DocLabel.of("Notes")), DocWeight.Unscaled, 30.days, 10))

  /** A document of `notes` under `key`, pointing at `pointer`, written now. */
  private def keep(engine: Engine^, key: String, pointer: Option[Pointer]): DocumentVersionId =
    right(
      engine.jot.write(Subject.Public)(
        engine
          .keeper(notes, terms)
          .write(
            right(DocKey.of(key)),
            grit.core.visibility.Label.Public,
            Integrity.Member,
            Origin.Task("notes", key).room,
            right(DocText.of(s"$Marker the $key note")),
            ujson.Obj(),
            pointer,
            now()
          )
      )
    ) match {
      case Written.Versioned(v, _, _) => v
      case other => throw new java.lang.AssertionError(s"not written: $other")
    }

  private def launch(engine: Engine^, held: Vector[DocumentVersionId]): Unit = {
    val speaking = Speaking.Within(Limits.suggested(right(DailyCap.of("0.25")), asks))
    val classifier = new StubClassifier
    val tooling = TurnTooling[{}](
      right(Toolbox.of[{}](echo)),
      Toolbox.Empty,
      Vector.empty,
      engine.jot,
      right(TurnLoop.Rounds.of(4))
    )
    // One Mentions weighs a said root before its offer and asks its task questions in route.
    val mentions =
      new grit.lifecycle.triage.Mentions(
        grit.core.stitch.StitchReads(
          engine.entries,
          engine.conversations,
          engine.stitches,
          engine.search,
          engine.principals
        ),
        engine.rooms,
        engine.documents,
        Corpora.Empty,
        grit.lifecycle.triage.TriageQuestions.shipped(grit.core.persona.Persona.Grit),
        grit.core.classify.ClearedClassifier.none("no classifier"),
        engine.placements,
        engine.db,
        Clock.system(),
        CharEstimate,
        Tuning.Default
      )
    engine.launch(
      Turn.body(
        TurnEnv(
          TurnRecords(
            engine.entries,
            engine.ledger,
            CharEstimate,
            engine.profiles,
            engine.principals,
            engine.documents
          ),
          TurnHosting(
            engine.conversations,
            engine.prompts,
            engine.toolSets,
            engine.requests,
            engine.edgeDirectory,
            engine.voices,
            engine.askers
          ),
          grit.kit.run.Launch.moveRecords(engine),
          new Holding(
            new LinearAssembler(
              engine.entries,
              engine.periods,
              engine.principals,
              CharEstimate,
              LinearAssembler.DefaultBudget
            ),
            held
          ),
          grit.core.classify.ClearedClassifier.none("no classifier"),
          new ScriptedModels(new Scripted),
          engine.db,
          Clock.system(),
          Fresh.random(),
          TurnSpeech(speaking, engine.speech, engine.deliveries),
          TurnStitching(
            engine.stitches,
            engine.search,
            Tuning.Default,
            grit.act.phase.Placing
              .Env(
                engine.entries,
                engine.conversations,
                engine.db,
                engine.placements,
                grit.core.clock.Clock.system()
              )
          ),
          grit.turn.TurnWeighing(engine.triage, mentions),
          grit.turn.TurnRouting(mentions, engine.budget, engine.spending)
        ),
        tooling
      ),
      nothing,
      nothing,
      nothing,
      Triage.body(
        TriageEnv(
          TriageRecords(
            engine.entries,
            engine.triage,
            engine.principals,
            engine.conversations,
            engine.speech,
            engine.spending,
            engine.acknowledgements,
            engine.deliveries,
            engine.stitches,
            engine.search,
            engine.rooms,
            engine.periods,
            engine.documents
          ),
          grit.kit.run.Launch.moves(
            engine,
            engine.db,
            new grit.models.StubModels,
            grit.core.classify.ClearedClassifier.of(grit.models.StubClassifier.Service, classifier),
            Clock.system()
          ),
          engine.budget,
          engine.db,
          Clock.system(),
          TriageSpeech(
            speaking,
            engine.budget,
            t => engine.inbox.startTurn(t).left.map(_.toString)
          ),
          Tuning.Default,
          grit.act.phase.Placing
            .Env(
              engine.entries,
              engine.conversations,
              engine.db,
              engine.placements,
              grit.core.clock.Clock.system()
            ),
          Corpora.Empty,
          grit.lifecycle.triage.TriageQuestions.shipped(grit.core.persona.Persona.Grit)
        )
      ),
      Stitch.body(
        StitchEnv(
          StitchReads(
            engine.entries,
            engine.conversations,
            engine.stitches,
            engine.search,
            engine.principals
          ),
          grit.kit.run.Launch.moves(
            engine,
            engine.db,
            new grit.models.StubModels,
            grit.core.classify.ClearedClassifier.of(grit.models.StubClassifier.Service, classifier),
            Clock.system()
          ),
          engine.budget,
          engine.db,
          Clock.system(),
          Tuning.Default
        )
      ),
      Vector.empty
    )
  }

  /** `text` said to grit at `origin` as `source`, and its turn run to its end. */
  private def ask(engine: Engine^, origin: Origin, source: String, text: String): TurnRef = {
    val turn = right(
      engine.inbox.ingest(origin, SourceId(source), Message.User(text), Account.Local)
    )
    right(engine.inbox.startTurn(turn))
    val _ = engine.awaitTurn(turn)
    turn
  }

  private val dump = Dump(Digest.text("synthetic dump"), Instant.parse("2100-01-01T00:00:00Z"))

  val tests = Tests {
    test(
      "every turn is captured, TUI and Slack, addressed and heard, under the root its offer recorded, as it ran, with the documents its window held, the same on recapture, and no text reaches its files"
    ) {
      val config = TestPostgres.freshDatabase("harness_turns")
      val engine = LiveEngine.open(config, Turn.Epoch)
      try {
        // Every window holds two documents: a card pointing at the loop's first echo, and a note
        // with no pointer, forgotten before the capture. Their plugin is enabled once the engine
        // has launched, which declares its own plugins.
        val card = keep(
          engine,
          "card",
          Some(right(Pointer.of(ToolName("echo"), "text" -> ujson.Str(s"$Marker argument 0"))))
        )
        val note = keep(engine, "note", None)
        // Current, but held by no window: a card pointing at the loop's second echo.
        val _ = keep(
          engine,
          "index",
          Some(right(Pointer.of(ToolName("echo"), "text" -> ujson.Str(s"$Marker argument 1"))))
        )
        launch(engine, Vector(card, note))
        right(engine.jot.write(Subject.Public)(engine.documents.declare(Vector(notes -> terms))))
        val tui = Origin.Tui(right(Directory.of("/tmp/harness-turns")), "s")
        // A TUI session: a remark, its period closed with a record, then a question it answers.
        val remark = ask(engine, tui, "t0", s"$Marker remember the Falcon budget is 4200")
        right(engine.jot.write(Subject.Public)(engine.periods.of(remark))).foreach { p =>
          right(
            engine.jot.write(Subject.Public)(
              engine.periods.seal(
                CloseRef(p.ref, remark.turnSeq, now()),
                CloseReason.Lapsed,
                TestClosings.prose(s"$Marker the Falcon budget is 4200, agreed."),
                now()
              )
            )
          )
        }
        val question = ask(engine, tui, "t1", s"$Marker what was it?")
        val failed = ask(engine, tui, "t2", s"$Marker ~fail")
        // A Slack thread asked directly, the model looping through two rounds of tools.
        val looped = ask(engine, Origin.Slack("T1", "C1", "2000.1"), "2000.1", s"$Marker ~loop")
        // A Slack message heard in thread `ts`, drafted or answered by triage's gate, passed by
        // the model.
        def overheard(ts: String, text: String): TurnRef = {
          val origin = Origin.Slack("T1", "C1", ts)
          engine.inbox.hear(
            origin,
            SourceId(ts),
            text,
            Account.Local,
            now(),
            Reach(Some(s"C1/$ts/$ts"), Set.empty)
          ) ==> Right(())
          assert(
            eventually(
              right(engine.db.read(Subject.Public)(engine.conversations.find(origin))).nonEmpty
            )
          )
          val t = TurnRef(
            right(engine.db.read(Subject.Public)(engine.conversations.find(origin)))
              .fold(sys.error(ts))(_.id),
            TurnSeq.First
          )
          // A draft, or, for a message answered as said to grit, a reply.
          assert(
            eventually(
              right(engine.db.read(Subject.Public)(engine.entries.ofTurn(t))).exists(e =>
                e.id == t.draftId || e.id == t.replyId
              )
            )
          )
          val _ = engine.awaitTurn(t)
          t
        }
        // The stub answers every yes/no at 0.9, to-grit too: answered as said to grit. At 0.1
        // it reads open and to-grit low, so the same gate (asks alone) drafts it as heard.
        val heard = overheard("3000.1", s"$Marker ~pass is the deploy on friday? ~back:asks")
        val unnamed = overheard("4000.1", s"$Marker ~pass ~0.1 is the freeze on monday? ~back:asks")
        right(engine.jot.write(Subject.Public)(engine.documents.forget(note)))

        def captured(): (Turns, Verdicts) = {
          val reader = Reader.open(config)
          try {
            (
              right(TurnCapture(reader, dump)),
              right(grit.eval.harness.pull.Pull.verdicts(reader, Instant.EPOCH)).verdicts
            )
          } finally reader.close()
        }
        val (turns, verdicts) = captured()
        val byWorkflow = turns.cases.map(t => t.workflow -> t).toMap
        def at(t: TurnRef): TurnCase =
          byWorkflow.getOrElse(t.workflowId, throw new java.lang.AssertionError(s"no case for $t"))

        turns.cases.map(_.workflow) ==>
          Vector(remark, question, failed, looped, heard, unnamed).map(_.workflowId)
        turns.skipped ==> 0

        // Where each was said, its root and focus, and how it ended.
        turns.cases.map(t =>
          (
            t.said match {
              case Said.Slack(id) => s"slack ${id.written}"
              case Said.Tui(_) => "tui"
              case Said.Task(_) => "task"
            },
            t.root,
            t.focus,
            t.ended
          )
        ) ==> Vector(
          (
            "tui",
            TurnOffer.Root.Addressed,
            Focus.Focused,
            Ended.Replied(s"$Marker replied".length, false)
          ),
          (
            "tui",
            TurnOffer.Root.Addressed,
            Focus.Focused,
            Ended.Replied(s"$Marker Falcon 4200".length, false)
          ),
          (
            "tui",
            TurnOffer.Root.Addressed,
            Focus.Focused,
            Ended.Failed(Turn.Step.Ask, Ended.Why.Model)
          ),
          (
            "slack C1/2000.1",
            TurnOffer.Root.Addressed,
            Focus.Open,
            Ended.Replied(s"$Marker echoed back".length, false)
          ),
          // The stub reads every yes/no at 0.9, to-grit too: its offer recorded it by name, its
          // reply read as an addressed one's, and the case keeps the root the offer recorded,
          // not one derived from the message.
          (
            "slack C1/3000.1",
            TurnOffer.Root.ByName,
            Focus.Open,
            Ended.Replied("pass".length, true)
          ),
          ("slack C1/4000.1", TurnOffer.Root.Heard, Focus.Open, Ended.Replied("pass".length, true))
        )

        // The offer: echo alone, its schema costed; the TUI session's directory as workspace.
        turns.cases.map(
          _.offered.map(o => (o.tools.map(ToolName.value), Tokens.value(o.schema) > 0))
        ) ==>
          Vector.fill(6)(Some((Vector("echo"), true)))
        at(question).offered.flatMap(_.workspace) ==> Some(tui.place)
        at(looped).offered.flatMap(_.workspace) ==> None

        // The shape each offer recorded: the deployed width, its whole set the one offered
        // (nothing withheld), and no service, none being linked. Its weighing: the tags live
        // triage had kept for the heard root, nothing for a message said to grit.
        turns.cases.map(
          _.offered.map(o => o.shape.map(s => (s.width, s.whole == o.set, s.services)))
        ) ==> Vector.fill(6)(Some(Some((Width.Deployed, true, Vector.empty))))
        Vector(remark, question, failed, looped).map(at(_).weighed) ==>
          Vector.fill(4)(TurnRecord.Weigh.Recorded(None))
        for (t <- Vector(heard, unnamed)) {
          (at(t).weighed match {
            case TurnRecord.Weigh.Recorded(Some(TurnWeighing.Weighed.Kept(tags))) =>
              Some(Live.of(tags))
            case _ => None
          }) ==> at(t).triage
          assert(at(t).triage.nonEmpty)
        }

        // The loop: two rounds of one echo each, both ran, each read through a current card,
        // the first's held by its window, and each result carrying the whole reply.
        at(looped).rounds ==> Vector(true, false).map(followed =>
          Round(
            Vector(
              Call(
                Called.Tool(ToolName("echo")),
                Settled.Ok(Echoed.length),
                Reading.Indexed(followed, Support.read(1.0))
              )
            )
          )
        )
        Vector(remark, question, failed, heard, unnamed).map(at(_).rounds) ==>
          Vector.fill(5)(Vector.empty)

        // What each call was for.
        turns.cases.map(_.spend.map(_.role)) ==> Vector(
          Vector(Some(TurnRecord.Role.Reply), Some(TurnRecord.Role.Summary)),
          Vector(Some(TurnRecord.Role.Reply), Some(TurnRecord.Role.Summary)),
          Vector.empty,
          Vector(
            Some(TurnRecord.Role.Round(0)),
            Some(TurnRecord.Role.Round(1)),
            Some(TurnRecord.Role.Reply),
            Some(TurnRecord.Role.Summary)
          ),
          // Answered as said to grit, its reply summarised as an addressed one's is; the
          // unprompted draft that passed is not.
          Vector(Some(TurnRecord.Role.Reply), Some(TurnRecord.Role.Summary)),
          Vector(Some(TurnRecord.Role.Reply))
        )
        at(looped).spend.filter(_.model == "test/scripted").map(_.usage.input) ==> Vector.fill(3)(
          Tokens(100)
        )

        // The question's window is the record alone, which carries the whole reply.
        at(question).window.map(_.parts.map(p => (p.kind, p.support.map(_.value)))) ==>
          Some(Vector((Part.Kind.Record, Some(1.0))))
        at(question).window.exists(w =>
          Tokens.value(w.parts.foldLeft(Tokens.Zero)(_ + _.tokens)) > 0 &&
            Tokens.value(w.own) > 0
        ) ==> true
        // A reply that passes supports nothing, nor does a turn that failed after its window: the
        // record, then the question's turn, shown by recency.
        at(heard).window.map(_.parts.flatMap(_.support)) ==> Some(Vector.empty)
        at(failed).window.map(_.parts.map(p => (p.kind, p.support))) ==>
          Some(Vector((Part.Kind.Record, None), (Part.Kind.Recent, None)))
        at(failed).window.flatMap(_.parts.lift(1)).map(_.seqs.size) ==> Some(2)

        // Each window's documents in the order held: the card, kept by its plugin and pointing at
        // echo, shown at a cost; the note, forgotten since, by its version alone. The question's
        // reply shares no distinctive word with the card; the failed turn has no reply.
        def documents(t: TurnRef) = at(t).window.map(_.documents.map {
          case Held.Kept(v, plugin, pointer, tokens, support) =>
            (v, Some((plugin, pointer, Tokens.value(tokens) > 0, support.map(_.value))))
          case Held.Gone(v) => (v, None)
        })
        Vector(question, failed).map(documents) ==> Vector(
          Some(
            Vector((card, Some((notes, Some(ToolName("echo")), true, Some(0.0)))), (note, None))
          ),
          Some(Vector((card, Some((notes, Some(ToolName("echo")), true, None))), (note, None)))
        )

        // Live triage's answers on the heard roots alone; the unprompted draft passed, and the
        // one answered as said to grit, like an addressed turn, keeps no draft's outcome.
        turns.cases.map(_.triage.isDefined) ==> Vector(false, false, false, false, true, true)
        turns.cases.map(_.speech.map(_.outcome)) ==> Vector(
          None,
          None,
          None,
          None,
          None,
          Some(Drafted.Kind.Passed)
        )
        verdicts ==> Verdicts.Empty

        // Text-free, and the same bytes again.
        def lines(t: Turns, v: Verdicts) =
          t.cases.map(TurnJson.write(_).render()).mkString("\n") + Verdicts.written(v)
        val written = lines(turns, verdicts)
        assert(!written.contains(Marker))
        assert(!written.contains("Falcon"))
        val (again, verdictsAgain) = captured()
        lines(again, verdictsAgain) ==> written
      } finally engine.close()
    }
  }
}
