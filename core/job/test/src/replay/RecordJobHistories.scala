package grit.job.replay

import grit.core.durable.{History, InMemoryDurable}
import grit.core.id.{SourceId, WorkflowId}
import grit.core.identity.Account
import grit.core.message.Message
import grit.core.store.Origin
import grit.job.run.Run
import grit.job.run.RunFixtures.*
import grit.turn.Turn

/** Writes this epoch's recorded run histories, one per shape a run can leave behind, into
  * `GRIT_HISTORIES/{Turn.Epoch}`. Never overwrites: a history, once written, is what builds of
  * this epoch must keep replaying. Run it when an epoch starts or a new shape appears:
  *
  * {{{./mill grit.job.test.runMain grit.job.replay.RecordJobHistories}}}
  */
object RecordJobHistories {

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

  /** Each shape, by name, recorded by running a run over a fresh world. */
  private def shapes: Vector[(String, History)] = {
    def record(name: String)(run: (World, InMemoryDurable) => WorkflowId): (String, History) = {
      val durable = new InMemoryDurable
      val id = run(new World, durable)
      name -> History("run", id, Turn.Epoch, "recorded", durable.history(id))
    }
    Vector(
      record("run-replied") { (w, d) =>
        val turn = w.started(w.declared("standup", 3))
        d.run(turn.workflowId)(Run.body(w.env(), jobs(1)))
        turn.workflowId
      },
      record("run-posted") { (w, d) =>
        val turn = w.started(w.asked("thread", "C1/1.0", 2))
        d.run(turn.workflowId)(Run.body(w.env(), jobs(1)))
        turn.workflowId
      },
      record("run-superseded") { (w, d) =>
        val turn = w.started(w.asked("thread", "C1/1.0"))
        d.run(turn.workflowId)(Run.body(w.env(), jobs(2)))
        turn.workflowId
      },
      record("run-jobless") { (w, d) =>
        val turn = w.started(w.declared("standup"))
        d.run(turn.workflowId)(Run.body(w.env(), jobsOf()))
        turn.workflowId
      },
      record("run-unreadable") { (w, d) =>
        val turn = w.inbox
          .ingest(
            Origin.Task("remind", "main"),
            SourceId("v1"),
            Message.User("go"),
            Account.Grit
          )
          .fold(e => sys.error(s"$e"), identity)
        d.run(turn.workflowId)(Run.body(w.env(), jobs(1)))
        turn.workflowId
      },
      // The reply committed at v1 and the process died before the step was recorded; grit
      // came back at v2, and the step, run again, found the reply.
      record("run-replied-then-redeployed") { (w, d) =>
        val turn = w.started(w.asked("thread", "C1/1.0", 4))
        try d.run(turn.workflowId)(Run.body(w.env(new FakeJot(crashAfter = true)), jobs(1)))
        catch { case _: InMemoryDurable.Crash => "crashed" }
        d.run(turn.workflowId)(Run.body(w.env(), jobs(2)))
        turn.workflowId
      }
    )
  }
}
