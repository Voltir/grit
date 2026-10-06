package grit.job.run

import grit.core.durable.InMemoryDurable
import grit.core.edge.Pending
import grit.core.id.{PrincipalId, SourceId}
import grit.core.job.Ending
import grit.core.message.Message
import grit.core.store.Origin

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
        .ingest(Origin.Task("remind", "main"), SourceId("v1"), Message.User("go"), PrincipalId.Grit)
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
  }
}
