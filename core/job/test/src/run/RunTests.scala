package grit.job.run

import grit.act.moves.MoveSteps
import grit.core.durable.InMemoryDurable
import grit.core.edge.{InMemoryEdges, Pending, RequestState}
import grit.core.id.{EntryId, PrincipalId, SourceId, WorkflowId}
import grit.core.identity.Account
import grit.core.job.Ending
import grit.core.message.{Message, Tokens, Usage}
import grit.core.store.Origin
import grit.core.tool.{Outcome, Retry, ToolName}
import grit.core.visibility.{Label, Level}
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
      val job = new Moving(1, limits(1, 0), (n, m) => said(m.ask(move("a"), request(n.n))))
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

    test("an ask once the day's spend reached its cap is Capped and asks no model") {
      val w = new World(cap = Some("0.5"))
      ok(
        w.ledger.record(
          EntryId("earlier"),
          w.started(w.declared("earlier")),
          WorkflowId("earlier"),
          "m",
          Usage(Tokens(1), Tokens(1), Tokens.Zero, Some(BigDecimal("1"))),
          Tokens(1)
        )(using TestTx.fake)
      )
      val turn = w.started(w.declared("standup"))
      val job = new Moving(1, limits(1, 0), (n, m) => said(m.ask(move("a"), request(n.n))))
      w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(job)))
      (w.reply(turn), w.models.calls, w.durable.recordedSteps(turn.workflowId)) ==>
        (Some("Capped"), 0, Vector(Run.Step.ReadSlot, MoveSteps.ask(move("a")), Run.Step.Reply))
    }

    test(
      "a call sends one request to the service's place for the schedule's principal, with the advertised retry, and returns the edge's answer at the run's floor"
    ) {
      val w = new World
      w.serve()
      val turn = w.started(w.declared("standup"))
      val job = new Moving(
        1,
        limits(0, 1),
        (_, m) =>
          m.call(move("c"), Probe, Read, ujson.Obj("path" -> "a")).fold(_.toString, _.toString)
      )
      w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(job)))
      (
        w.reply(turn),
        w.sent.map(q => (q.workspace, q.principal, q.tool, q.retry, q.arguments)),
        w.durable.recordedSteps(turn.workflowId).filterNot(_.startsWith("DBOS."))
      ) ==> (
        Some("Done(read,public)"),
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
        (_, m) =>
          Vector(
            m.call(move("nope"), Probe, ToolName("probe_nope"), ujson.Obj()),
            m.call(move("asks"), Probe, Asks, ujson.Obj("path" -> "a"))
          ).map(_.fold(_.toString, _.toString)).mkString("\n")
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
      "a call the run may not send to is written answered with its refusal, and fails unwaited"
    ) {
      val w = new World(floor = Label.at(Level.Internal))
      w.serve()
      val turn = w.started(w.declared("standup"))
      val job = new Moving(
        1,
        limits(0, 1),
        (_, m) =>
          m.call(move("c"), Probe, Read, ujson.Obj("path" -> "a")).fold(_.toString, _.toString)
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
        (_, m) =>
          m.call(move("c"), Probe, Read, ujson.Obj("path" -> "a")).fold(_.toString, _.toString)
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
        (_, m) =>
          m.call(move("c"), Probe, Read, ujson.Obj("path" -> "a")).fold(_.toString, _.toString)
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
          MoveSteps.expire(move("c")),
          Run.Step.Reply
        )
      )
    }

    test(
      "a name made again is Repeated, also after its store failed, and a move past its limit OverLimit"
    ) {
      val w = new World(failRecord = true)
      val turn = w.started(w.declared("standup"))
      val job = new Moving(
        1,
        limits(1, 0),
        (n, m) =>
          Vector(
            m.ask(move("a"), request(n.n)),
            m.ask(move("a"), request(n.n)),
            m.ask(move("b"), request(n.n))
          ).map(said).mkString("\n")
      )
      w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(job)))
      w.reply(turn) ==> Some("Store(the ledger is down)\nRepeated(a)\nOverLimit(Ask,1)")
    }

    test(
      "a rerun whose move's input differs from what the run recorded is Diverged, every later move too, and its reply says which"
    ) {
      val w = new World(crashRecord = true)
      val turn = w.started(w.declared("standup", 3))
      def asking(first: Int) = new Moving(
        1,
        limits(2, 0),
        (_, m) =>
          Vector(m.ask(move("a"), request(first)), m.ask(move("b"), request(9)))
            .map(said)
            .mkString("\n")
      )
      val first =
        try w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(asking(1))))
        catch { case _: InMemoryDurable.Crash => "crashed" }
      w.durable.run(turn.workflowId)(Run.body(w.env(), jobsOf(asking(2))))
      (first, w.reply(turn), w.models.calls) ==> ("crashed", Some("Diverged(a)\nDiverged(a)"), 1)
    }

    test("a run superseded before its job runs makes no move") {
      val w = new World
      w.serve()
      val turn = w.started(w.declared("standup"))
      val job = new Moving(
        2,
        limits(1, 1),
        (n, m) => said(m.ask(move("a"), request(n.n))) + m.call(move("c"), Probe, Read, ujson.Obj())
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
}
