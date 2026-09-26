package grit.turn

import grit.core.context.AssemblyNote
import grit.core.durable.{History, InMemoryDurable}
import grit.core.id.TurnRef
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

  private def recorded(durable: InMemoryDurable, turn: TurnRef): History =
    History("turn", turn.workflowId, Turn.Epoch, "recorded", durable.history(turn.workflowId))

  /** Each shape, by name. A name whose file was written before a later step existed keeps
    * that shorter history, so a new step that changes a shape gets a new name.
    *
    * Builders that now run the same turn (`replied`, `summarised` and `windowFirst`;
    * `crashedBeforeAppend` and `crashedBeforeAppendWindowFirst`) are kept apart because each
    * name's file was written by an earlier build, and is replayed as it was written.
    */
  private def shapes: Vector[(String, History)] = {
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
      "recalled" -> recalled,
      "queried" -> queried,
      "summarised" -> summarised,
      "summary-failed" -> summaryFailed,
      "crashed-before-summary-append" -> crashedBeforeSummaryAppend,
      "replied" -> replied,
      "later-turn" -> laterTurn,
      "model-failed" -> modelFailed,
      "crashed-before-append" -> crashedBeforeAppend
    )
  }
}
