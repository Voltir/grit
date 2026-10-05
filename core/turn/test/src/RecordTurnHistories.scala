package grit.turn

import grit.core.context.AssemblyNote
import grit.core.durable.{History, InMemoryDurable}
import grit.core.id.{EntrySeq, TurnRef}
import grit.core.store.{InMemoryEntryStore, InMemoryUsageLedger, Payload}

/** Writes this epoch's recorded turn histories, one per shape a turn can leave behind,
  * into `GRIT_HISTORIES/{Turn.Epoch}`. Never overwrites: a history, once written, is what
  * builds of this epoch must keep replaying. Run it when an epoch starts or a new shape
  * appears:
  *
  * {{{./mill grit.turn.test.runMain grit.turn.RecordTurnHistories}}}
  */
object RecordTurnHistories {
  import TurnFixtures.*

  def main(args: Array[String]): Unit = {
    val dir = os.Path(sys.env("GRIT_HISTORIES")) / Turn.Epoch
    os.makeDir.all(dir)
    for ((name, history) <- shapes) {
      val file = dir / s"$name.json"
      if (os.exists(file)) println(s"kept    $file")
      else {
        os.write(file, ujson.write(History.write(history), indent = 2) + "\n")
        println(s"wrote   $file")
      }
    }
  }

  private def recorded(durable: InMemoryDurable, turn: TurnRef): History = {
    val steps = durable.history(turn.workflowId)
    History("turn", turn.workflowId, Turn.Epoch, "recorded", steps, keptBy(steps))
  }

  /** `history` as it stood once its first `n` steps were recorded: a turn in flight. */
  private def inFlight(history: History, n: Int): History = {
    val steps = history.steps.take(n)
    history.copy(steps = steps, kept = keptBy(steps))
  }

  /** Each shape, by name. A name whose file was written before a later step existed keeps
    * that shorter history, so a new step that changes a shape gets a new name.
    *
    * Builders that now run the same turn (`replied`, `summarised` and `windowFirst`;
    * `crashedBeforeAppend` and `crashedBeforeAppendWindowFirst`) are kept apart because each
    * name's file was written by an earlier build, and is replayed as it was written.
    */
  private def shapes: Vector[(String, History)] = {

    /** A turn rooted on a heard message, its draft judged at 0.9, under `speaking`. */
    def heard(speaking: grit.core.speech.Speaking): History = {
      val w = speechWorld()
      val durable = new InMemoryDurable
      durable.run(w.turn.workflowId)(
        turnBodyWith(
          w.entries,
          new RecordingProvider,
          new Before(w.entries),
          w.ledger,
          new Judge(Some((0.9, 0.9))),
          TurnSpeech(speaking, w.store, w.deliveries)
        )
      )
      recorded(durable, w.turn)
    }

    /** A turn rooted on a heard message triage read as put to grit by name, under Within;
      * any question it asks answered yes at 0.9.
      */
    def named: History = {
      val w = speechWorld()
      val durable = new InMemoryDurable
      durable.run(w.turn.workflowId)(
        turnBodyWith(
          w.entries,
          new RecordingProvider,
          new Before(w.entries),
          w.ledger,
          new AnswersYes(0.9),
          TurnSpeech(grit.core.speech.Speaking.Within(speechLimits), w.store, w.deliveries),
          weighing = TurnWeighing(
            directed(w),
            new Weighs(Left(grit.core.triage.Weighing.Unweighed.Unavailable))
          )
        )
      )
      recorded(durable, w.turn)
    }

    /** A turn rooted on a heard message triage decided to answer as said to grit, under
      * Within; its model failing when `fail`.
      */
    def byName(fail: Boolean): History = {
      val w = speechWorld(answering = true)
      val durable = new InMemoryDurable
      durable.run(w.turn.workflowId)(
        turnBodyWith(
          w.entries,
          new RecordingProvider(fail = fail),
          new Before(w.entries),
          w.ledger,
          new AnswersYes(0.9),
          TurnSpeech(grit.core.speech.Speaking.Within(speechLimits), w.store, w.deliveries),
          weighing = TurnWeighing(
            directed(w),
            new Weighs(Left(grit.core.triage.Weighing.Unweighed.Unavailable))
          )
        )
      )
      recorded(durable, w.turn)
    }

    /** A turn rooted on a heard message whose offer fails, its conversation gone. */
    def heardFailedAtOffer: History = {
      val w = speechWorld()
      val edges = new grit.core.edge.InMemoryEdges
      val durable = new InMemoryDurable
      durable.run(w.turn.workflowId)(
        turnBodyWith(
          w.entries,
          new RecordingProvider,
          new Before(w.entries),
          w.ledger,
          new AnswersYes(0.9),
          TurnSpeech(grit.core.speech.Speaking.Within(speechLimits), w.store, w.deliveries),
          hosted = TurnHosting(
            new grit.core.store.InMemoryConversationStore,
            Prompts,
            ToolSets,
            edges,
            edges,
            new grit.core.store.InMemoryVoiceStore
          )
        )
      )
      recorded(durable, w.turn)
    }

    /** [[named]] cut just after its `judge` step, as recorded before named drafts went
      * unjudged: in flight across that change.
      */
    def namedBeforeSpeech: History = {
      val h = named
      val judged = h.steps.indexWhere(_.name == Turn.Step.Judge)
      inFlight(h, if (judged < 0) h.steps.size else judged + 1)
    }
    val replied = {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val turn = say(entries, "hello")
      durable.run(turn.workflowId)(turnBody(entries, new RecordingProvider))
      recorded(durable, turn)
    }
    val laterTurn = {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      durable.run(say(entries, "one").workflowId)(turnBody(entries, new RecordingProvider))
      val turn = say(entries, "two")
      durable.run(turn.workflowId)(turnBody(entries, new RecordingProvider))
      recorded(durable, turn)
    }
    val modelFailed = {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val turn = say(entries, "hello")
      durable.run(turn.workflowId)(turnBody(entries, new RecordingProvider(fail = true)))
      recorded(durable, turn)
    }
    val crashedBeforeAppend = {
      val store = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val turn = say(store, "hello")
      try
        durable.run(turn.workflowId)(
          turnBody(new CrashOnInsert(store, isReply), new RecordingProvider)
        )
      catch { case _: InMemoryDurable.Crash => "" }
      recorded(durable, turn)
    }
    val summarised = {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val turn = say(entries, "hello")
      durable.run(turn.workflowId)(turnBody(entries, new RecordingProvider))
      recorded(durable, turn)
    }
    val summaryFailed = {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val turn = say(entries, "hello")
      durable.run(turn.workflowId)(
        turnBody(entries, new RecordingProvider, summarizer = new RecordingProvider(fail = true))
      )
      recorded(durable, turn)
    }
    val crashedBeforeSummaryAppend = {
      val store = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val turn = say(store, "hello")
      val entries = new CrashOnInsert(store, _.payload.isInstanceOf[Payload.Summary])
      try durable.run(turn.workflowId)(turnBody(entries, new RecordingProvider))
      catch { case _: InMemoryDurable.Crash => "" }
      recorded(durable, turn)
    }
    val queried = {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val turn = say(entries, "hello")
      durable.run(turn.workflowId)(
        turnBodyWith(
          entries,
          new RecordingProvider,
          new Noting(entries, TurnFixtures.queried),
          new InMemoryUsageLedger
        )
      )
      recorded(durable, turn)
    }
    val recalled = {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val one = say(entries, "one")
      durable.run(one.workflowId)(turnBody(entries, new RecordingProvider))
      val turn = say(entries, "two")
      durable.run(turn.workflowId)(
        turnBodyWith(
          entries,
          new RecordingProvider,
          new Noting(entries, AssemblyNote.Recalled(Vector(one.turnSeq))),
          new InMemoryUsageLedger
        )
      )
      recorded(durable, turn)
    }
    val nearby = {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val api = grit.core.id.ConversationId("api")
      entries.insert(
        grit.core.store.Entry(
          grit.core.id.EntryId("api:u"),
          api,
          grit.core.id.TurnSeq(0),
          None,
          EntrySeq(0),
          Payload.Message(grit.core.message.Message.User("the invoice test is flaky")),
          java.time.Instant.EPOCH
        )
      )(using grit.dbos.sql.TestTx.fake)
      val turn = say(entries, "which fix did I settle on?")
      val place = grit.core.place.Place.read("fs:/home/nick/api").fold(e => sys.error(e), identity)
      val section = grit.core.store.Nearby.Open(api, place, Vector(EntrySeq(0)))
      val near = new grit.core.context.ContextAssembler {
        def assemble(request: grit.core.context.AssemblyRequest)(using
            grit.core.store.Db^
        ): Either[grit.core.context.AssemblyError, grit.core.context.Window] =
          Right(
            grit.core.context.Window(Vector.empty, Vector(TurnFixtures.queried), Vector(section))
          )
      }
      durable.run(turn.workflowId)(
        turnBodyWith(entries, new RecordingProvider, near, new InMemoryUsageLedger)
      )
      recorded(durable, turn)
    }
    // A window holding a plugin's document, and one collected before the model call: the
    // assemble step records both versions, and record-window counts them placed.
    val documented = {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val documents = new grit.core.document.InMemoryDocuments
      def got[A](e: Either[String, A]): A = e.fold(sys.error, identity)
      val digest = got(grit.core.id.PluginName.of("digest"))
      val terms = got(
        grit.core.document.DocumentTerms.of(
          got(grit.core.document.DocLabel.of("Weekly digest")),
          grit.core.document.DocWeight.Unscaled,
          scala.concurrent.duration.Duration(30, "days"),
          10
        )
      )
      given grit.core.store.Tx = grit.dbos.sql.TestTx.fake
      documents.declare(Vector(digest -> terms))
      val board = got(grit.core.place.Place.read("slack:T1/C1"))
      def write(key: String, text: String): grit.core.id.DocumentVersion =
        documents
          .keeper(digest, terms)
          .write(
            got(grit.core.id.DocKey.of(key)),
            board,
            got(grit.core.document.DocText.of(text)),
            ujson.Obj(),
            java.time.Instant.EPOCH
          ) match {
          case Right(grit.core.document.Written.Versioned(v, _)) => v
          case other => sys.error(s"not written: $other")
        }
      val week = write("week", "Deploys frozen until Friday.")
      val gone = write("old", "Deploys open.")
      val _ = documents.forget(gone)
      val turn = say(entries, "when does the freeze end?")
      val held = new grit.core.context.ContextAssembler {
        def assemble(request: grit.core.context.AssemblyRequest)(using
            grit.core.store.Db^
        ): Either[grit.core.context.AssemblyError, grit.core.context.Window] =
          Right(
            grit.core.context.Window(Vector.empty, Vector.empty, Vector.empty, Vector(week, gone))
          )
      }
      durable.run(turn.workflowId)(
        turnBodyWith(
          entries,
          new RecordingProvider,
          held,
          new InMemoryUsageLedger,
          documents = documents
        )
      )
      recorded(durable, turn)
    }
    // Another conversation's record, and one whose closing was collected before the model
    // call: its section is dropped from the request, and the window keeps naming it.
    val nearbyClosed = {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val ops = grit.core.id.ConversationId("ops")
      entries.insert(
        grit.core.store.Entry(
          grit.core.id.EntryId("ops:closing"),
          ops,
          grit.core.id.TurnSeq(3),
          None,
          EntrySeq(4),
          Payload.Closed(
            grit.core.id.PeriodSeq.First,
            grit.core.period.CloseReason.Lapsed,
            grit.core.period.TestClosings.prose("Froze deploys until Friday.")
          ),
          java.time.Instant.EPOCH
        )
      )(using grit.dbos.sql.TestTx.fake)
      val turn = say(entries, "when does the freeze end?")
      val thread = (ts: String) =>
        grit.core.place.Place.read(s"slack:T1/C1/$ts").fold(e => sys.error(e), identity)
      val sections = Vector(
        grit.core.store.Nearby.Closed(ops, thread("2.0"), EntrySeq(4)),
        grit.core.store.Nearby.Closed(
          grit.core.id.ConversationId("old"),
          thread("1.0"),
          EntrySeq(0)
        )
      )
      val near = new grit.core.context.ContextAssembler {
        def assemble(request: grit.core.context.AssemblyRequest)(using
            grit.core.store.Db^
        ): Either[grit.core.context.AssemblyError, grit.core.context.Window] =
          Right(grit.core.context.Window(Vector.empty, Vector(TurnFixtures.queried), sections))
      }
      durable.run(turn.workflowId)(
        turnBodyWith(entries, new RecordingProvider, near, new InMemoryUsageLedger)
      )
      recorded(durable, turn)
    }
    val windowFirst = {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val turn = say(entries, "hello")
      durable.run(turn.workflowId)(turnBody(entries, new RecordingProvider))
      recorded(durable, turn)
    }
    val crashedRecordingWindow = {
      val store = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val turn = say(store, "hello")
      val entries = new CrashOnInsert(store, _.payload.isInstanceOf[Payload.Window])
      try durable.run(turn.workflowId)(turnBody(entries, new RecordingProvider))
      catch { case _: InMemoryDurable.Crash => "" }
      recorded(durable, turn)
    }
    val crashedBeforeAppendWindowFirst = {
      val store = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val turn = say(store, "hello")
      try
        durable.run(turn.workflowId)(
          turnBody(new CrashOnInsert(store, isReply), new RecordingProvider)
        )
      catch { case _: InMemoryDurable.Crash => "" }
      recorded(durable, turn)
    }

    /** `said` answered in turn in one conversation, with the stub classifier (failing when
      * `down`), the last turn's entries crashing where `crash` says; the last turn's history.
      */
    def topical(
        said: Vector[String],
        down: Boolean = false,
        crash: Option[grit.core.store.Entry -> Boolean] = None
    ): History = answered(said, new RecordingProvider, down, crash)

    /** As [[topical]], the last turn answered by `provider`. */
    def answered(
        said: Vector[String],
        provider: grit.core.provider.Provider^,
        down: Boolean = false,
        crash: Option[grit.core.store.Entry -> Boolean] = None,
        summary: String = ""
    ): History = {
      val store = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val classifier = new CountingClassifier(fail = down)
      said.dropRight(1).foreach { text =>
        val t = say(store, text)
        durable.run(t.workflowId)(turnBody(store, new RecordingProvider, classifier = classifier))
      }
      val turn = say(store, said.lastOption.getOrElse("hello"))
      val entries = crash.fold[grit.core.store.EntryStore](store)(new CrashOnInsert(store, _))
      val summarizer = new Scripted((r, _) =>
        if (summary.isEmpty) new grit.models.StubProvider().complete(r)
        else
          Right(
            grit.core.message.Message.Assistant(
              Vector(grit.core.message.AssistantBlock.Text(summary)),
              grit.core.message.StopReason.EndTurn,
              grit.core.message.Usage(
                grit.core.message.Tokens.Zero,
                grit.core.message.Tokens.Zero,
                grit.core.message.Tokens.Zero,
                None
              ),
              "s"
            )
          )
      )
      try
        durable.run(turn.workflowId)(
          turnBody(entries, provider, summarizer = summarizer, classifier = classifier)
        )
      catch { case _: InMemoryDurable.Crash => "" }
      recorded(durable, turn)
    }

    /** `said` answered in turn in one conversation with the stub classifier, the last turn by
      * `provider` with `peek` and `poke` offered over a checkout of `a.txt`, `b.txt` and
      * `c.txt`, its entries crashing where `crash` says and `answers` (call id, approval)
      * sent once it reaches its first ask; the last turn's history.
      */
    def looping(
        said: Vector[String],
        provider: grit.core.provider.Provider^,
        crash: Option[grit.core.store.Entry -> Boolean] = None,
        answers: Vector[(String, grit.core.approval.Approval)] = Vector.empty
    ): History = {
      val store = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val classifier = new CountingClassifier
      said.dropRight(1).foreach { text =>
        val t = say(store, text)
        durable.run(t.workflowId)(turnBody(store, new RecordingProvider, classifier = classifier))
      }
      val turn = say(store, said.lastOption.getOrElse("hello"))
      val ws = new Files(Map("a.txt" -> "alpha", "b.txt" -> "beta", "c.txt" -> "gamma"))
      def run(entries: grit.core.store.EntryStore): String =
        try
          durable.run(turn.workflowId)(
            tooledBody(
              entries,
              provider,
              new InMemoryUsageLedger,
              new grit.models.StubProvider(),
              classifier,
              ws,
              tools(ws),
              5
            )
          )
        catch { case _: InMemoryDurable.Crash => "" }
      if (answers.nonEmpty) {
        // A send needs the turn started: stop it at its ask, which records nothing, and
        // answer there.
        run(crashingAtAsk(store))
        answers.foreach { (call, approval) =>
          val topic = grit.core.approval.Approval.topic(grit.core.id.ToolCallId(call))
          durable.send(turn.workflowId, topic, grit.core.approval.Approval.encode(approval))
        }
      }
      run(crash.fold[grit.core.store.EntryStore](store)(new CrashOnInsert(store, _)))
      recorded(durable, turn)
    }

    /** One turn whose model calls `first` in its first reply and answers after, its hosted
      * calls sent to an edge that treats each as `serve` says (unadvertised when `advertised`
      * is false), `answers` (call id, approval) sent once it reaches its first ask, its
      * entries crashing where `crash` says.
      */
    def hosting(
        first: Vector[(String, String, ujson.Value)],
        serve: grit.core.edge.ToolRequest -> Serve,
        advertised: Boolean = true,
        answers: Vector[(String, grit.core.approval.Approval)] = Vector.empty,
        crash: Option[grit.core.store.Entry -> Boolean] = None
    ): History = {
      val store = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val turn = say(store, "hosted")
      val edge = new Served(new grit.core.edge.InMemoryEdges, durable, serve)
      if (advertised) edge.advertise(hostedTools)
      val provider =
        new Scripted((_, n) => Right(if (n == 0) calling("", first*) else calling("done")))
      def run(entries: grit.core.store.EntryStore): String =
        try durable.run(turn.workflowId)(hostedBody(entries, provider, edge))
        catch { case _: InMemoryDurable.Crash => "" }
      if (answers.nonEmpty) {
        run(crashingAtAsk(store))
        answers.foreach { (call, approval) =>
          val topic = grit.core.approval.Approval.topic(grit.core.id.ToolCallId(call))
          durable.send(turn.workflowId, topic, grit.core.approval.Approval.encode(approval))
        }
      }
      run(crash.fold[grit.core.store.EntryStore](store)(new CrashOnInsert(store, _)))
      recorded(durable, turn)
    }

    def calling(
        text: String,
        calls: (String, String, ujson.Value)*
    ): grit.core.message.Message.Assistant =
      grit.core.message.Message.Assistant(
        Vector(grit.core.message.AssistantBlock.Text(text)).filter(_ => text.nonEmpty) ++
          calls.map((id, name, args) =>
            grit.core.message.AssistantBlock
              .ToolCall(grit.core.id.ToolCallId(id), name, args)
          ),
        grit.core.message.StopReason.ToolUse,
        grit.core.message.Usage(
          grit.core.message.Tokens(10),
          grit.core.message.Tokens(2),
          grit.core.message.Tokens.Zero,
          Some(BigDecimal("0.001"))
        ),
        "m"
      )

    def peeking(p: String): ujson.Value = ujson.Obj("path" -> p)

    /** `peek` a, then `peek` b and c, then the answer. */
    def threeRounds = new TurnFixtures.Scripted((r, n) =>
      n match {
        case 0 => Right(calling("looking", ("t1", "peek", peeking("a.txt"))))
        case 1 =>
          Right(calling("", ("t2", "peek", peeking("b.txt")), ("t3", "peek", peeking("c.txt"))))
        case _ => new grit.models.StubProvider().complete(r.copy(tools = Vector.empty))
      }
    )

    Vector(
      "loop-three-rounds" -> looping(Vector("read them"), threeRounds),
      "loop-approved" -> looping(
        Vector("poke a"),
        new TurnFixtures.Scripted((r, n) =>
          if (n == 0) Right(calling("", ("t1", "poke", peeking("a.txt"))))
          else new grit.models.StubProvider().complete(r.copy(tools = Vector.empty))
        ),
        answers = Vector("t1" -> grit.core.approval.Approval.Approved)
      ),
      "loop-crashed-in-tool" -> looping(
        Vector("read them"),
        threeRounds,
        crash = Some(e => grit.core.id.EntryId.value(e.id).startsWith("result:"))
      ),
      "loop-verdict" -> looping(
        Vector("hello", "knots? ~0.1", "back ~0.5"),
        new TurnFixtures.Scripted((r, n) =>
          if (n == 0)
            Right(
              calling(
                "",
                ("v", "topic", ujson.Obj("about" -> "earlier", "earlier" -> "new topic (2)")),
                ("t1", "peek", peeking("a.txt"))
              )
            )
          else new grit.models.StubProvider().complete(r.copy(tools = Vector.empty))
        )
      ),
      "topical-first" -> topical(Vector("hello")),
      "topical-same" -> topical(Vector("hello", "more ~0.9")),
      "topical-uncertain" -> topical(Vector("hello", "hm ~0.5")),
      "topical-changed-new" -> topical(Vector("hello", "knots? ~0.1")),
      "topical-changed-back" ->
        topical(Vector("hello", "knots? ~0.1", "back ~0.1 ~back:new topic (2)")),
      "topical-unclassified" -> topical(Vector("hello", "more"), down = true),
      "topical-crashed-placing" ->
        topical(Vector("hello", "more"), crash = Some(_.payload.isInstanceOf[Payload.Topic])),
      "topical-verdict" -> topical(
        Vector(
          "hello",
          "knots? ~0.1",
          """back ~0.5 #call:{"about":"earlier","name":"new topic (2)"}"""
        )
      ),
      "topical-verdict-no-call" -> answered(
        Vector("hello", "hm ~0.5"),
        new Scripted((r, _) =>
          new grit.models.StubProvider().complete(r.copy(tools = Vector.empty))
        )
      ),
      "topical-verdict-plain" -> answered(
        Vector("hello", "hm ~0.5"),
        new Scripted((r, n) =>
          if (n == 0) new grit.models.StubProvider().complete(r)
          else if (r.tools.isEmpty) new grit.models.StubProvider().complete(r)
          else Left(grit.core.provider.ProviderError.Unavailable("HTTP 529"))
        )
      ),
      "topical-verdict-crashed-between-rounds" -> answered(
        Vector("hello", "hm ~0.5"),
        new Scripted((r, n) =>
          if (n == 1) throw new InMemoryDurable.Crash
          else new grit.models.StubProvider().complete(r)
        )
      ),
      "topical-described" -> answered(
        Vector("knots?"),
        new RecordingProvider,
        summary = "Summary: Asked about knots.\nTopic: Knots\nAbout: which knot holds."
      ),
      "window-first" -> windowFirst,
      "crashed-recording-window" -> crashedRecordingWindow,
      "crashed-before-append-window-first" -> crashedBeforeAppendWindowFirst,
      "nearby" -> nearby,
      "nearby-closed" -> nearbyClosed,
      "documented" -> documented,
      "recalled" -> recalled,
      "queried" -> queried,
      "summarised" -> summarised,
      "summary-failed" -> summaryFailed,
      "crashed-before-summary-append" -> crashedBeforeSummaryAppend,
      "heard-posted" -> heard(grit.core.speech.Speaking.Within(speechLimits)),
      "heard-shadowed" -> heard(grit.core.speech.Speaking.Shadow(speechLimits)),
      // A named turn, judged on whether its draft answers (recorded before named drafts went
      // unjudged), and the same in flight between its judge and its record-speech.
      "named-judged" -> named,
      "named-judged-before-speech" -> namedBeforeSpeech,
      // A named turn since its drafts went unjudged: its patch's marker, no judge step.
      "named-posted" -> named,
      // A heard message triage answered as said to grit: an addressed turn's steps, and, its
      // model failing, its failure kept in record-speech.
      "by-name-replied" -> byName(fail = false),
      "by-name-failed" -> byName(fail = true),
      // A heard turn whose offer failed: its failure kept in record-failure.
      "heard-failed-at-offer" -> heardFailedAtOffer,
      "replied" -> replied,
      "stitched-first" -> {
        val ch = new StitchChannel(
          Payload.Message(grit.core.message.Message.User("@grit is this a real question?"))
        )
        val (durable, _) = ch.run(new FirstOption)
        recorded(durable, ch.turn)
      },
      "stitched-first-in-order" -> {
        // Its placement waited for, a workflow of its own.
        val ch = new StitchChannel(
          Payload.Message(grit.core.message.Message.User("@grit is this a real question?"))
        )
        val (durable, _) = ch.run(new FirstOption)
        recorded(durable, ch.turn)
      },
      // A heard turn whose workspace's only source triage answered below the recipe's
      // threshold: weighed, and its workspace's tools withheld.
      "heard-withheld" -> {
        val t = new Sourced.Thread(heard = true, Some(Sourced.repoReads(0.1)))
        t.run()
        recorded(t.durable, t.turn)
      },
      // A message said to grit, its workspace's service offered by its source: asked, the
      // call's cost in the ledger with the offer, and the tools withheld.
      "addressed-weighed" -> {
        val t = new Sourced.Thread(
          heard = false,
          None,
          addressed = grit.core.recipe.Offering.BySource(grit.core.period.Probability.clamped(0.2))
        )
        t.run()
        recorded(t.durable, t.turn)
      },
      // The same, its weighing failed: why kept by kind, everything offered.
      "addressed-failed" -> {
        val t = new Sourced.Thread(
          heard = false,
          None,
          addressed = grit.core.recipe.Offering.BySource(grit.core.period.Probability.clamped(0.2)),
          asked = Left(grit.core.triage.Weighing.Unweighed.PlacementLate)
        )
        t.run()
        recorded(t.durable, t.turn)
      },
      // As recorded before a failed weighing kept why: its weigh step recorded null.
      "addressed-unweighed" -> {
        val t = new Sourced.Thread(
          heard = false,
          None,
          addressed = grit.core.recipe.Offering.BySource(grit.core.period.Probability.clamped(0.2)),
          asked = Left(grit.core.triage.Weighing.Unweighed.PlacementLate)
        )
        t.run()
        recorded(t.durable, t.turn)
      },
      // In flight across the weigh step's patch: it had offered (unweighed), or only pinned.
      "offered-before-weigh" -> {
        val t = new Sourced.Thread(
          heard = true,
          Some(Sourced.repoReads(0.1)),
          unpatched = Set(Turn.Patches.Weigh)
        )
        t.run()
        inFlight(recorded(t.durable, t.turn), 2)
      },
      "pinned-before-weigh" -> {
        val t = new Sourced.Thread(
          heard = true,
          Some(Sourced.repoReads(0.1)),
          unpatched = Set(Turn.Patches.Weigh)
        )
        t.run()
        inFlight(recorded(t.durable, t.turn), 1)
      },
      "later-turn" -> laterTurn,
      "model-failed" -> modelFailed,
      "crashed-before-append" -> crashedBeforeAppend,
      "hosted-read" -> hosting(
        Vector(("h1", "fetch", ujson.Obj("path" -> "a.txt"))),
        _ => Serve.Now(grit.core.tool.Outcome.Done("alpha"))
      ),
      "hosted-round" -> hosting(
        Vector("a.txt", "b.txt", "c.txt").zipWithIndex.map((p, i) =>
          (s"h$i", "fetch", ujson.Obj("path" -> p))
        ),
        q => Serve.Now(grit.core.tool.Outcome.Done(q.arguments("path").str))
      ),
      "hosted-approved" -> hosting(
        Vector(("h1", "prod", ujson.Obj("path" -> "a.txt"))),
        _ => Serve.Now(grit.core.tool.Outcome.Done("prodded")),
        answers = Vector("h1" -> grit.core.approval.Approval.Approved)
      ),
      "hosted-unserved" -> hosting(
        Vector(("h1", "fetch", ujson.Obj("path" -> "a.txt"))),
        _ => Serve.Never,
        advertised = false
      ),
      "hosted-expired" -> hosting(
        Vector(("h1", "fetch", ujson.Obj("path" -> "a.txt"))),
        _ => Serve.Never
      ),
      "hosted-slow" -> hosting(
        Vector(("h1", "fetch", ujson.Obj("path" -> "a.txt"))),
        _ => Serve.Later(grit.core.tool.Outcome.Done("late alpha"))
      ),
      "hosted-orphaned" -> hosting(
        Vector(("h1", "fetch", ujson.Obj("path" -> "a.txt"))),
        _ => Serve.Claimed
      ),
      // Rung, its `DBOS.recv` recorded, and crashed keeping the answer in `tool:0:0`.
      "hosted-rung-crashed-before-tool" -> hosting(
        Vector(("h1", "fetch", ujson.Obj("path" -> "a.txt"))),
        _ => Serve.Now(grit.core.tool.Outcome.Done("alpha")),
        crash = Some(_.payload.isInstanceOf[Payload.Result])
      )
    )
  }
}
