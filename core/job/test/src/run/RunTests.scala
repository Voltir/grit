package grit.job.run

import grit.act.moves.MoveSteps
import grit.core.act.MovesFixtures.{Judged, PerChar, replied, shown}
import grit.core.act.{MoveError, Moves, Posed}
import grit.core.classify.{
  Answer as Judgment,
  Answers,
  Ask,
  ClassifierError,
  Criterion,
  Decision,
  Level as Rung,
  Request,
  Scored,
  StateJson
}
import grit.core.durable.InMemoryDurable
import grit.core.edge.{Pending, RequestState}
import grit.core.flow.{TestTainted, Trusted}
import grit.core.id.{EntryId, PrincipalId, SourceId, WorkflowId}
import grit.core.identity.Account
import grit.core.job.JobTests.Count
import grit.core.job.{Ending, JobRun, PlainJob}
import grit.core.message.{Message, Tokens, Usage}
import grit.core.schema.Typed
import grit.core.store.Origin
import grit.core.tool.{Outcome, Retry, ToolName}
import grit.core.visibility.{Compartments, Integrity, Label, Level}
import grit.dbos.sql.TestTx

import utest.*

object RunTests extends TestSuite {
  import RunFixtures.*

  val tests = Tests {
    test(
      "a run replies its job's reply as its turn's reply, awaited nowhere for a kept schedule, and its once schedule ends ran"
    ) {
      val w = new World
      val id = w.declared("standup", 3)
      val turn = w.started(id)
      val durable = new InMemoryDurable
      val said = durable.run(turn.workflowId)(Run.body(w.env(), jobs(1)))
      (said, w.reply(turn), w.pending, w.schedule(id).flatMap(_.ended)) ==>
        (s"replied: reply:${turn.conversationId}:0", Some("3"), Vector(), Some(Ending.Ran))
      durable.recordedSteps(turn.workflowId) ==> Vector(Run.Step.ReadSlot, Run.Step.Reply)
    }

    test(
      "a run's job is given its run believed at its schedule's integrity: a declared one's grit's own, an asked one's its asking turn's"
    ) {
      val w = new World
      val declared = w.started(w.declared("standup"))
      val asked = w.started(w.asked("thread", "C1/1.0", believed = Integrity.Outside))
      Vector(declared, asked).foreach(turn =>
        new InMemoryDurable().run(turn.workflowId)(Run.body(w.env(), jobsOf(Believing)))
      )
      (w.reply(declared), w.reply(asked)) ==> (
        Some(Integrity.written(Integrity.top(Compartments.Shipped))),
        Some(Integrity.written(Integrity.Outside))
      )
    }

    test("an asked schedule's run is awaited at the address its asking turn's reply was posted") {
      val w = new World
      val id = w.asked("thread", "C1/1.0", 2)
      val turn = w.started(id)
      new InMemoryDurable().run(turn.workflowId)(Run.body(w.env(), jobs(1)))
      (w.reply(turn), w.pending, w.schedule(id).flatMap(_.ended)) ==>
        (Some("2"), Vector(Pending(turn, "C1/1.0", Map.empty)), Some(Ending.Ran))
    }

    test(
      "a run started at another version than its job's now is superseded: no reply, no await, its slot still running"
    ) {
      val w = new World
      val id = w.asked("thread", "C1/1.0")
      val turn = w.started(id)
      val said = new InMemoryDurable().run(turn.workflowId)(Run.body(w.env(), jobs(2)))
      (said, w.reply(turn), w.pending, w.running(id)) ==>
        ("superseded: started at v1, its job at v2", None, Vector(), Some((Due, Some(1))))
    }

    test("a run whose job the deployment lacks writes nothing: jobless") {
      val w = new World
      val id = w.declared("standup")
      val turn = w.started(id)
      val said = new InMemoryDurable().run(turn.workflowId)(Run.body(w.env(), jobsOf()))
      (said, w.entries(turn), w.running(id)) ==> ("jobless: no job remind", 1, Some((Due, Some(1))))
    }

    test("a run whose job cannot read its schedule's parameters replies saying so") {
      val w = new World
      val id = w.declared("standup", 3)
      val turn = w.started(id)
      new InMemoryDurable().run(turn.workflowId)(Run.body(w.env(), jobsOf(new Texting)))
      (w.reply(turn), w.schedule(id).flatMap(_.ended)) ==>
        (Some(Run.unreadParams(remind.name, "not text: 3")), Some(Ending.Ran))
    }

    test(
      "a run of a conversation no slot's, or of a slot whose schedule is gone, is unreadable and takes no reply step"
    ) {
      val w = new World
      val main = w.inbox
        .ingest(Origin.Task("remind", "main"), SourceId("v1"), Message.User("go"), Account.Grit)
        .fold(e => sys.error(s"$e"), identity)
      val standup = w.declared("standup")
      val gone = w.started(standup)
      ok(w.inbox.schedules.declare(Vector(), Due)(using grit.dbos.sql.TestTx.fake))
      w.inbox.schedules.forget(standup) ==> true
      val durable = new InMemoryDurable
      Vector(main, gone).map(t => durable.run(t.workflowId)(Run.body(w.env(), jobs(1)))) ==>
        Vector("unreadable: its conversation is no slot's", "unreadable: its schedule is gone")
      Vector(main, gone).map(t => durable.recordedSteps(t.workflowId)) ==>
        Vector(Vector(Run.Step.ReadSlot), Vector(Run.Step.ReadSlot))
    }

    test(
      "a reply step run again after its reply committed, under a later version, finds the reply by its id and returns it, writing nothing more"
    ) {
      val w = new World
      val id = w.asked("thread", "C1/1.0", 4)
      val turn = w.started(id)
      val durable = new InMemoryDurable
      val crashed =
        try durable.run(turn.workflowId)(Run.body(w.env(new FakeJot(crashAfter = true)), jobs(1)))
        catch { case _: InMemoryDurable.Crash => "crashed" }
      val again = durable.run(turn.workflowId)(Run.body(w.env(), jobs(2)))
      (crashed, again, w.reply(turn), w.entries(turn), w.pending.size) ==>
        ("crashed", s"replied: reply:${turn.conversationId}:0", Some("4"), 2, 1)
    }

    test(
      "an ask's cost is recorded once under the run's conversation, across a crash between its model call and its record and across a replay"
    ) {
      val w = new World(crashRecord = true)
      val turn = w.started(w.declared("standup", 3))
      val job =
        new Moving(
          1,
          limits(1, 0),
          (n, m) => said(m.ask(move("a"), n.map(c => Posed.Text(request(c.n)))))
        )
      val first =
        try w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(job)))
        catch { case _: InMemoryDurable.Crash => "crashed" }
      val again = w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(job)))
      val replayed = new InMemoryDurable()
        .replay(turn.workflowId, w.durable.history(turn.workflowId))(Run.body(w.env(), jobsOf(job)))
      (
        first,
        again,
        replayed,
        w.reply(turn),
        w.models.calls,
        w.ledger.rows.map(r => (EntryId.value(r._1), r._6.conversationId))
      ) ==> (
        "crashed",
        s"replied: reply:${turn.conversationId}:0",
        Right(s"replied: reply:${turn.conversationId}:0"),
        Some(Answer),
        1,
        Vector((s"move:${WorkflowId.value(turn.workflowId)}:a", turn.conversationId))
      )
      w.durable.recordedSteps(turn.workflowId) ==>
        Vector(
          Run.Step.ReadSlot,
          MoveSteps.ask(move("a")),
          MoveSteps.record(move("a")),
          Run.Step.Reply
        )
    }

    test(
      "an ask made from what no model is cleared for is Uncleared at its label, and asks no model"
    ) {
      val w = new World(floor = Label.at(Level.Internal))
      val turn = w.started(w.declared("standup"))
      val job =
        new Moving(
          1,
          limits(1, 0),
          (n, m) => uncleared(m.ask(move("a"), n.map(c => Posed.Text(request(c.n)))))
        )
      w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(job)))
      (w.reply(turn), w.models.calls, w.durable.recordedSteps(turn.workflowId)) ==>
        (
          Some("uncleared at internal"),
          0,
          Vector(Run.Step.ReadSlot, MoveSteps.ask(move("a")), Run.Step.Reply)
        )
    }

    test(
      "a judgment made from what the classifier's service is not cleared for is Uncleared at its label, at once: no classifier call, no wait to ask again"
    ) {
      val w = new World(floor = Label.at(Level.Internal))
      val turn = w.started(w.declared("standup"))
      val started = w.clock.now()
      val job = new Moving(
        1,
        limits(1, 0),
        (n, m) => {
          import Triaged.given
          uncleared(
            m.ask(move("j"), n.map(_ => Posed.judge(Triaged.State, Triaged.questions)))
          )
        }
      )
      w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(job)))
      (w.reply(turn), w.classifier.calls, w.clock.now()) ==>
        (Some("uncleared at internal"), 0, started)
    }

    test("an ask's and a judgment's ledger rows record who served them") {
      val w = new World(upstream = Some("acme"))
      val turn = w.started(w.declared("standup"))
      val job = new Moving(
        1,
        limits(2, 0),
        (n, m) => {
          import Triaged.given
          val _ = m.ask(move("a"), n.map(c => Posed.Text(request(c.n))))
          m.ask(move("j"), n.map(_ => Posed.judge(Triaged.State, Triaged.questions)))
            .fold(_.toString, _ => "")
        }
      )
      w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(job)))
      w.ledger.of(turn.workflowId)(using TestTx.fake).map(_.map(_.upstream)) ==>
        Right(Vector(Some("acme"), Some("service:test")))
    }

    test("an ask once the day's spend reached its cap is Capped and asks no model") {
      val w = new World(cap = Some("0.5"))
      ok(
        w.ledger.record(
          EntryId("earlier"),
          w.started(w.declared("earlier")),
          WorkflowId("earlier"),
          "m",
          None,
          None,
          Usage(Tokens(1), Tokens(1), Tokens.Zero, Some(BigDecimal("1"))),
          Tokens(1)
        )(using TestTx.fake)
      )
      val turn = w.started(w.declared("standup"))
      val job =
        new Moving(
          1,
          limits(1, 0),
          (n, m) => said(m.ask(move("a"), n.map(c => Posed.Text(request(c.n)))))
        )
      w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(job)))
      (w.reply(turn), w.models.calls, w.durable.recordedSteps(turn.workflowId)) ==>
        (Some("Capped"), 0, Vector(Run.Step.ReadSlot, MoveSteps.ask(move("a")), Run.Step.Reply))
    }

    test(
      "a call sends one request to the service's place for the schedule's principal, with the advertised retry, and returns the edge's answer at the run's floor"
    ) {
      val internal = Label.at(Level.Internal)
      val w = new World(floor = internal, trust = internal)
      w.serve()
      val turn = w.started(w.declared("standup"))
      val job = new Moving(
        1,
        limits(0, 1),
        (n, m) =>
          m.call(move("c"), Probe, Read, n.map(_ => ujson.Obj("path" -> "a")))
            .fold(_.toString, shown)
      )
      w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(job)))
      (
        w.reply(turn),
        w.sent.map(q => (q.workspace, q.principal, q.tool, q.retry, q.arguments)),
        w.durable.recordedSteps(turn.workflowId).filterNot(_.startsWith("DBOS."))
      ) ==> (
        Some("Done(read,internal)"),
        Vector((Probe.place, PrincipalId.Grit, Read, Retry.Interrupt, ujson.Obj("path" -> "a"))),
        Vector(
          Run.Step.ReadSlot,
          MoveSteps.call(move("c")),
          MoveSteps.answer(move("c")),
          Run.Step.Reply
        )
      )
    }

    test(
      "a call of a tool its service does not advertise, or of one that asks first, sends nothing"
    ) {
      val w = new World
      w.serve()
      val turn = w.started(w.declared("standup"))
      val job = new Moving(
        1,
        limits(0, 2),
        (n, m) =>
          Vector(
            m.call(move("nope"), Probe, ToolName("probe_nope"), n.map(_ => ujson.Obj())),
            m.call(move("asks"), Probe, Asks, n.map(_ => ujson.Obj("path" -> "a")))
          ).map(_.fold(_.toString, shown)).mkString("\n")
      )
      w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(job)))
      (w.reply(turn), w.sent) ==> (
        Some(
          "Failed(probe does not offer probe_nope, so this call did not run.)\n" +
            "Failed(probe_ask asks a person first, and nobody waits on this call to approve it, so it did not run.)"
        ),
        Vector()
      )
    }

    test(
      "a call whose arguments are believed less than its tool requires fails in words a person follows, and sends nothing"
    ) {
      val w = new World
      w.serve(guarded = true)
      val turn = w.started(w.declared("standup"))
      val job = new Moving(
        1,
        limits(0, 2),
        (_, m) =>
          Vector(Integrity.Outside, Integrity.Member).zipWithIndex
            .map((i, n) =>
              m.call(move(s"c$n"), Probe, Guarded, TestTainted.at(ujson.Obj("path" -> "a"), i))
            )
            .map(_.fold(_.toString, shown))
            .mkString("\n")
      )
      w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(job)))
      (w.reply(turn), w.sent.map(_.tool)) ==> (
        Some(
          "Failed(You can't read guarded paths right now, because of something you read; " +
            "nothing was done, so answer without it.)\nDone(read,public)"
        ),
        Vector(Guarded)
      )
    }

    test(
      "a run asked at Outside whose job calls, from its run, a tool requiring Member is withheld, and sends nothing"
    ) {
      val w = new World
      w.serve(guarded = true)
      val turn = w.started(w.asked("thread", "C1/1.0", believed = Integrity.Outside))
      val job = new Moving(
        1,
        limits(0, 1),
        (n, m) =>
          m.call(move("c"), Probe, Guarded, n.map(_ => ujson.Obj("path" -> "a")))
            .fold(_.toString, shown)
      )
      w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(job)))
      (w.reply(turn), w.sent) ==> (
        Some(
          "Failed(You can't read guarded paths right now, because of something you read; " +
            "nothing was done, so answer without it.)"
        ),
        Vector()
      )
    }

    test(
      "a call the run may not send to is written answered with its refusal, and fails unwaited"
    ) {
      val w = new World(floor = Label.at(Level.Internal))
      w.serve()
      val turn = w.started(w.declared("standup"))
      val job = new Moving(
        1,
        limits(0, 1),
        (n, m) =>
          m.call(move("c"), Probe, Read, n.map(_ => ujson.Obj("path" -> "a")))
            .fold(_.toString, shown)
      )
      w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(job)))
      val refusal = "Nothing was sent: this conversation may not send to service:probe."
      (w.reply(turn), w.standing, w.durable.recordedSteps(turn.workflowId)) ==> (
        Some(s"Failed($refusal)"),
        Vector(RequestState.Answered(Outcome.Failed(refusal))),
        Vector(Run.Step.ReadSlot, MoveSteps.call(move("c")), Run.Step.Reply)
      )
    }

    test("a call whose schedule was forgotten while its run was down sends nothing") {
      val w = new World(crashServing = true)
      w.serve()
      val id = w.declared("standup")
      val turn = w.started(id)
      val job = new Moving(
        1,
        limits(0, 1),
        (n, m) =>
          m.call(move("c"), Probe, Read, n.map(_ => ujson.Obj("path" -> "a")))
            .fold(_.toString, shown)
      )
      val first =
        try w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(job)))
        catch { case _: InMemoryDurable.Crash => "crashed" }
      // A deploy that no longer declares it.
      ok(w.inbox.schedules.declare(Vector(), Due)(using TestTx.fake))
      w.inbox.schedules.forget(id) ==> true
      w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(job)))
      (first, w.reply(turn), w.sent) ==> (
        "crashed",
        Some("Failed(There is nobody to make this call for, so it did not run.)"),
        Vector()
      )
    }

    test("a call no edge claims within ServeWithin fails naming its service") {
      val w = new World
      w.serve(answers = false)
      val turn = w.started(w.declared("standup"))
      val job = new Moving(
        1,
        limits(0, 1),
        (n, m) =>
          m.call(move("c"), Probe, Read, n.map(_ => ujson.Obj("path" -> "a")))
            .fold(_.toString, shown)
      )
      w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(job)))
      (
        w.reply(turn),
        w.standing,
        w.durable.recordedSteps(turn.workflowId).filterNot(_.startsWith("DBOS."))
      ) ==> (
        Some("Failed(No edge is serving probe right now, so this call did not run.)"),
        Vector(RequestState.Expired),
        Vector(
          Run.Step.ReadSlot,
          MoveSteps.call(move("c")),
          MoveSteps.Expire(move("c")),
          Run.Step.Reply
        )
      )
    }

    test(
      "a name made again is Repeated, also after its store failed"
    ) {
      val w = new World(failRecord = true)
      val turn = w.started(w.declared("standup"))
      val job = new Moving(
        1,
        limits(2, 0),
        (n, m) =>
          Vector(
            m.ask(move("a"), n.map(c => Posed.Text(request(c.n)))),
            m.ask(move("a"), n.map(c => Posed.Text(request(c.n))))
          )
            .map(said)
            .mkString("\n")
      )
      w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(job)))
      w.reply(turn) ==> Some("Store(the ledger is down)\nRepeated(a)")
    }

    test(
      "a rerun whose move's input differs from what the run recorded is Diverged, every later move too, and its reply says which"
    ) {
      val w = new World(crashRecord = true)
      val turn = w.started(w.declared("standup", 3))
      def asking(first: Int) = new Moving(
        1,
        limits(2, 0),
        (n, m) =>
          Vector(
            m.ask(move("a"), n.map(_ => Posed.Text(request(first)))),
            m.ask(move("b"), n.map(_ => Posed.Text(request(9))))
          )
            .map(said)
            .mkString("\n")
      )
      val first =
        try w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(asking(1))))
        catch { case _: InMemoryDurable.Crash => "crashed" }
      w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(asking(2))))
      (first, w.reply(turn), w.models.calls) ==> ("crashed", Some("Diverged(a)\nDiverged(a)"), 1)
    }

    test(
      "a diverged ask's cost is estimated from the request the model was sent, not the rerun's"
    ) {
      val w = new World(crashRecord = true)
      val turn = w.started(w.declared("standup", 3))
      def asking(n: Int) =
        new Moving(
          1,
          limits(1, 0),
          (t, m) => said(m.ask(move("a"), t.map(_ => Posed.Text(request(n)))))
        )
      try w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(asking(1))))
      catch { case _: InMemoryDurable.Crash => "crashed" }
      w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(asking(1000))))
      w.ledger.rows.map(_._5) ==> Vector(PerChar.request(request(1)))
    }

    test(
      "a plugin's keeping job keeps its documents at the run's floor, in its keep's step, then replies"
    ) {
      val w = new World(floor = Label.at(Level.Internal))
      val turn = w.started(w.declared("standup", 3))
      w.durable.run(turn.workflowId)(Run.body(w.env(), keepingJobs(new Noting(1))))
      val kept = w.documents
        .keeper(Notes, NotesTerms)
        .newest(10)(using
          TestTx.fake(grit.core.visibility.Clearance.of(Label.at(Level.Restricted)))
        )
        .fold(
          e => sys.error(s"$e"),
          _.map(d => (grit.core.document.DocText.value(d.text), Label.written(d.label)))
        )
      (w.reply(turn), kept, w.durable.recordedSteps(turn.workflowId)) ==> (
        Some("internal"),
        Vector(("counted 3", "internal")),
        Vector(Run.Step.ReadSlot, MoveSteps.keep(move("note")), Run.Step.Reply)
      )
    }

    test(
      "a JSON ask's reply is read as its type on the first run, and on replay with the model not called again"
    ) {
      val w = new World(shapes = Vector(ujson.Obj("count" -> 3)))
      val turn = w.started(w.declared("standup", 3))
      val seen = new Seen
      val first = w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(counting(seen))))
      val again = new Seen
      val replayed = new InMemoryDurable()
        .replay(turn.workflowId, w.durable.history(turn.workflowId))(
          Run.body(w.env(), jobsOf(counting(again)))
        )
      (first, seen.reply, replayed, again.reply, w.reply(turn), w.models.calls) ==> (
        s"replied: reply:${turn.conversationId}:0",
        Some("6"),
        Right(s"replied: reply:${turn.conversationId}:0"),
        Some("6"),
        Some("6"),
        1
      )
    }

    test(
      "a JSON reply's quoted number is read as its number, in one call, its model repairing it"
    ) {
      val w = new World(shapes = Vector(ujson.Obj("count" -> "3")))
      val turn = w.started(w.declared("standup", 3))
      w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(counting(new Seen))))
      (w.reply(turn), w.models.calls) ==> (Some("6"), 1)
    }

    test(
      "a model required to reply that answers in text, calling nothing, is asked once more, then Model"
    ) {
      val w = new World(calling = 0)
      val turn = w.started(w.declared("standup", 3))
      w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(counting(new Seen))))
      (w.reply(turn), w.models.calls) ==> (Some("Model(no call of `reply`)"), 2)
    }

    test("a model required to reply that calls it twice is read from its first call") {
      val w =
        new World(calling = 2, shapes = Vector(ujson.Obj("count" -> 3), ujson.Obj("count" -> 4)))
      val turn = w.started(w.declared("standup", 3))
      w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(counting(new Seen))))
      (w.reply(turn), w.models.calls) ==> (Some("6"), 1)
    }

    test("a JSON ask once the day's spend reached its cap is Capped and asks no model") {
      val w = new World(cap = Some("0.5"), shapes = Vector(ujson.Obj("count" -> 3)))
      ok(
        w.ledger.record(
          EntryId("earlier"),
          w.started(w.declared("earlier")),
          WorkflowId("earlier"),
          "m",
          None,
          None,
          Usage(Tokens(1), Tokens(1), Tokens.Zero, Some(BigDecimal("1"))),
          Tokens(1)
        )(using TestTx.fake)
      )
      val turn = w.started(w.declared("standup", 3))
      w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(counting(new Seen))))
      (w.reply(turn), w.models.calls) ==> (Some("Capped"), 0)
    }

    test(
      "a repaired JSON reply records two ledger rows, the repair's estimated from the request that carried its error, once each across a crash between its model calls and its record"
    ) {
      val w = new World(
        crashRecord = true,
        shapes = Vector(ujson.Obj("count" -> "many"), ujson.Obj("count" -> 3))
      )
      val turn = w.started(w.declared("standup", 3))
      val seen = new Seen
      val first =
        try w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(counting(seen))))
        catch { case _: InMemoryDurable.Crash => "crashed" }
      val again = w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(counting(seen))))
      val base = s"move:${WorkflowId.value(turn.workflowId)}:a"
      (
        first,
        again,
        w.reply(turn),
        w.models.requests.map(_.messages.lastOption.collect {
          case Message.ToolResult(_, _, isError) => isError
        }),
        w.ledger.rows.map(r => (EntryId.value(r._1), r._5))
      ) ==> (
        "crashed",
        s"replied: reply:${turn.conversationId}:0",
        Some("6"),
        Vector(None, Some(true)),
        Vector(base, s"$base:repair").zip(w.models.requests.map(PerChar.request))
      )
      w.durable.recordedSteps(turn.workflowId) ==>
        Vector(
          Run.Step.ReadSlot,
          MoveSteps.ask(move("a")),
          MoveSteps.record(move("a")),
          Run.Step.Reply
        )
    }

    test("a JSON reply that does not read after its repair is Model, both calls' costs recorded") {
      val w = new World(shapes = Vector(ujson.Obj("count" -> "many")))
      val turn = w.started(w.declared("standup", 3))
      val seen = new Seen
      w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(counting(seen))))
      val base = s"move:${WorkflowId.value(turn.workflowId)}:a"
      (w.reply(turn), w.models.calls, w.ledger.rows.map(r => EntryId.value(r._1))) ==> (
        Some(
          "Model(its arguments do not match its schema: count: expected an integer, got \"many\")"
        ),
        2,
        Vector(base, s"$base:repair")
      )
    }

    test(
      "a JSON reply that does not read, whose calls' record fails, is Store: the record's failure outranks the reply's"
    ) {
      val w = new World(failRecord = true, shapes = Vector(ujson.Obj("count" -> "many")))
      val turn = w.started(w.declared("standup", 3))
      val seen = new Seen
      w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(counting(seen))))
      (w.reply(turn), w.models.calls) ==> (Some("Store(the ledger is down)"), 2)
    }

    test(
      "a JSON ask whose schema changed on a rerun is Diverged, its recorded call's cost recorded"
    ) {
      val w = new World(crashRecord = true, shapes = Vector(ujson.Obj("count" -> 3)))
      val turn = w.started(w.declared("standup", 3))
      val seen = new Seen
      val first =
        try w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(counting(seen))))
        catch { case _: InMemoryDurable.Crash => "crashed" }
      val bounded = Typed(
        schema(ujson.Obj("count" -> ujson.Obj("type" -> "integer", "maximum" -> 9))),
        Counted.read
      )
      w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(counting(seen, bounded))))
      (first, w.reply(turn), w.models.calls, w.ledger.rows.size) ==>
        ("crashed", Some("Diverged(a)"), 1, 1)
    }

    test(
      "a run that recorded a text ask and its record, resumed posing JSON under its name, replays the record step and is Diverged, recording no second row"
    ) {
      val w = new World
      val turn = w.started(w.declared("standup", 3))
      val text =
        new Moving(
          1,
          limits(1, 0),
          (n, m) => said(m.ask(move("a"), n.map(c => Posed.Text(request(c.n)))))
        )
      val crashed =
        try
          w.durable.run(turn.workflowId)(
            Run.body(w.env(new FakeJot(crashAfter = true)), jobsOf(text))
          )
        catch { case _: InMemoryDurable.Crash => "crashed" }
      val recorded = w.durable.recordedSteps(turn.workflowId)
      val seen = new Seen
      val resumed = w.durable.replay(turn.workflowId, w.durable.history(turn.workflowId))(
        Run.body(w.env(), jobsOf(counting(seen)))
      )
      (crashed, recorded, resumed, seen.reply, w.models.calls, w.ledger.rows.size) ==> (
        "crashed",
        Vector(Run.Step.ReadSlot, MoveSteps.ask(move("a")), MoveSteps.record(move("a"))),
        Right(s"replied: reply:${turn.conversationId}:0"),
        Some("Diverged(a)"),
        1,
        1
      )
    }

    test(
      "a run that recorded a repaired JSON ask and its record, resumed posing text under its name, replays the record step and is Diverged, recording no more rows"
    ) {
      val w = new World(shapes = Vector(ujson.Obj("count" -> "many"), ujson.Obj("count" -> 3)))
      val turn = w.started(w.declared("standup", 3))
      val seen = new Seen
      val crashed =
        try
          w.durable.run(turn.workflowId)(
            Run.body(w.env(new FakeJot(crashAfter = true)), jobsOf(counting(seen)))
          )
        catch { case _: InMemoryDurable.Crash => "crashed" }
      val text = new Moving(
        1,
        limits(1, 0),
        (n, m) => {
          val r = said(m.ask(move("a"), n.map(c => Posed.Text(request(c.n)))))
          seen.reply = Some(r)
          r
        }
      )
      val resumed = w.durable.replay(turn.workflowId, w.durable.history(turn.workflowId))(
        Run.body(w.env(), jobsOf(text))
      )
      (crashed, resumed, seen.reply, w.models.calls, w.ledger.rows.size) ==> (
        "crashed",
        Right(s"replied: reply:${turn.conversationId}:0"),
        Some("Diverged(a)"),
        2,
        2
      )
    }

    test(
      "a judgment's answers are read on its first run and on replay from what it recorded, a choice's confidence as its classifier reported it, the classifier asked once"
    ) {
      val w = new World(judged = Triaged.judged("docs"))
      val turn = w.started(w.declared("standup"))
      val seen = new Seen
      val first = w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(judging(seen))))
      val again = new Seen
      val replayed = new InMemoryDurable()
        .replay(turn.workflowId, w.durable.history(turn.workflowId))(
          Run.body(w.env(), jobsOf(judging(again)))
        )
      (first, seen.reply, replayed, again.reply, w.classifier.calls) ==> (
        s"replied: reply:${turn.conversationId}:0",
        Some("docs 0.99 1"),
        Right(s"replied: reply:${turn.conversationId}:0"),
        Some("docs 0.99 1"),
        1
      )
    }

    test(
      "a judgment's cost is recorded once under the run's conversation, priced as its classifier reported, its estimate the classifier's request's"
    ) {
      val w = new World(crashRecord = true)
      val turn = w.started(w.declared("standup"))
      val seen = new Seen
      val first =
        try w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(judging(seen))))
        catch { case _: InMemoryDurable.Crash => "crashed" }
      val again = w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(judging(seen))))
      (
        first,
        again,
        w.classifier.calls,
        w.ledger.rows.map(r => (EntryId.value(r._1), r._3, r._4, r._5, r._6.conversationId))
      ) ==> (
        "crashed",
        s"replied: reply:${turn.conversationId}:0",
        1,
        Vector(
          (
            s"move:${WorkflowId.value(turn.workflowId)}:j",
            "test/judge",
            Judged,
            PerChar.system(Request.json(Triaged.request(Triaged.State)).render()),
            turn.conversationId
          )
        )
      )
    }

    test("a judgment once the day's spend reached its cap is Capped and asks no classifier") {
      val w = new World(cap = Some("0.5"))
      ok(
        w.ledger.record(
          EntryId("earlier"),
          w.started(w.declared("earlier")),
          WorkflowId("earlier"),
          "m",
          None,
          None,
          Usage(Tokens(1), Tokens(1), Tokens.Zero, Some(BigDecimal("1"))),
          Tokens(1)
        )(using TestTx.fake)
      )
      val turn = w.started(w.declared("standup"))
      w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(judging(new Seen))))
      (w.reply(turn), w.classifier.calls) ==> (Some("Capped"), 0)
    }

    test("a judgment whose answers do not read is Model, the classifier's cost recorded") {
      val w = new World(judged = Triaged.judged("ops"))
      val turn = w.started(w.declared("standup"))
      w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(judging(new Seen))))
      (w.reply(turn), w.ledger.rows.map(r => EntryId.value(r._1))) ==> (
        Some(
          "Model(the classifier's answers do not read: \"Which team owns `text`?\": chose ops, not an option)"
        ),
        Vector(s"move:${WorkflowId.value(turn.workflowId)}:j")
      )
    }

    test(
      "a judgment whose state changed on a rerun is Diverged, its recorded call's cost recorded once"
    ) {
      val w = new World(crashRecord = true)
      val turn = w.started(w.declared("standup"))
      val seen = new Seen
      val first =
        try w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(judging(seen))))
        catch { case _: InMemoryDurable.Crash => "crashed" }
      w.durable.run(turn.workflowId)(
        Run.body(w.env(), jobsOf(judging(seen, "the build is green")))
      )
      (first, w.reply(turn), w.classifier.calls, w.ledger.rows.size) ==>
        ("crashed", Some("Diverged(j)"), 1, 1)
    }

    test("a run superseded before its job runs makes no move") {
      val w = new World
      w.serve()
      val turn = w.started(w.declared("standup"))
      val job = new Moving(
        2,
        limits(1, 1),
        (n, m) =>
          said(m.ask(move("a"), n.map(c => Posed.Text(request(c.n))))) + m.call(
            move("c"),
            Probe,
            Read,
            n.map(_ => ujson.Obj())
          )
      )
      val said2 = w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(job)))
      (said2, w.models.calls, w.sent, w.durable.recordedSteps(turn.workflowId)) ==> (
        "superseded: started at v1, its job at v2",
        0,
        Vector(),
        Vector(Run.Step.ReadSlot, Run.Step.Reply)
      )
    }
  }

  /** `remind`, replying what its run is believed at, written. */
  private object Believing extends PlainJob[Count] {
    val name = remind.name
    val version = 1
    def write(params: Count): ujson.Value = remind.write(params)
    def read(params: ujson.Value): Either[String, Count] = remind.read(params)
    def run(run: Trusted[JobRun[Count]], moves: Moves^): String = Integrity.written(run.integrity)
  }

  /** `{"count": integer}`, read as the count. */
  private object Counted {
    val read: grit.core.schema.Conforming -> Either[String, Int] =
      c => c.json.objOpt.flatMap(_.get("count")).flatMap(_.numOpt).map(_.toInt).toRight("no count")
    val typed: Typed[Int] =
      Typed(schema(ujson.Obj("count" -> ujson.Obj("type" -> "integer"))), read)
  }

  /** `remind` at version 1, within an ask: it asks `a` for `reply` as JSON and replies twice
    * the count, or the error, telling `seen` what it replied.
    */
  private def counting(seen: Seen, reply: Typed[Int] = Counted.typed): Moving =
    new Moving(
      1,
      limits(1, 0),
      (n, m) => {
        val r = m
          .ask(
            move("a"),
            n.map(c => Posed.Json("Count.", Vector(Message.User(s"to ${c.n}")), reply))
          )
          .fold(_.toString, a => (replied(a) * 2).toString)
        seen.reply = Some(r)
        r
      }
    )

  /** Which team owns a text, and how urgent it is, asked together. */
  private object Triaged {
    given StateJson[String] = StateJson.instance(s => ujson.Obj("text" -> s))

    val State = "the build is red"

    val questions: Ask[String, (Decision[String], Scored[Int])] = (for {
      owner <- Ask
        .choice[String, String](
          "Which team owns `text`?",
          Criterion("build", "build", None),
          Criterion("docs", "docs", None)
        )
        .left
        .map(_.toString)
      urgency <- Ask
        .score[String, Int]("How urgent is `text`?", Rung(0, "not"), Rung(1, "very"))
        .left
        .map(_.toString)
    } yield owner.zip(urgency)).fold(sys.error, identity)

    def request(state: String): Request = Request.of(state, questions)

    /** A choice of `owner`, weighing docs 0.75, its confidence reported as 0.99; and a score at
      * the second level.
      */
    def answers(owner: String): Vector[Judgment] = Vector(
      Judgment.Choice(
        owner,
        Vector(Judgment.Weight("build", 0.25), Judgment.Weight("docs", 0.75)),
        0.99
      ),
      Judgment.Score(1, Vector(0.2, 0.8), 0.6)
    )

    /** A classifier's script: [[answers]] of `owner`, each time. */
    def judged(owner: String): Vector[Either[ClassifierError, Answers]] =
      Vector(Right(Answers(answers(owner), Judged, "test/judge")))
  }

  /** What a move came to, as a job's reply says it, an uncleared one as the label it was made
    * at.
    */
  private def uncleared[R](asked: Either[MoveError, R]): String = asked match {
    case Left(MoveError.Uncleared(at)) => s"uncleared at ${Label.written(at)}"
    case other => other.toString
  }

  /** `remind` at version 1, within an ask: it judges `j`, [[Triaged]]'s questions about
    * `state`, and replies the owner chosen, its confidence and the likeliest urgency, or the
    * error, telling `seen` what it replied.
    */
  private def judging(seen: Seen, state: String = Triaged.State): Moving =
    new Moving(
      1,
      limits(1, 0),
      (n, m) => {
        import Triaged.given
        val r = m
          .ask(move("j"), n.map(_ => Posed.judge(state, Triaged.questions)))
          .fold(
            _.toString,
            a => s"${replied(a)._1.choice} ${replied(a)._1.confidence} ${replied(a)._2.likeliest}"
          )
        seen.reply = Some(r)
        r
      }
    )
}
