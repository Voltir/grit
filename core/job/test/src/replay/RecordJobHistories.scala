package grit.job.replay

import grit.core.durable.{History, InMemoryDurable}
import grit.core.id.{SourceId, WorkflowId}
import grit.core.identity.Account
import grit.core.message.Message
import grit.core.store.Origin
import grit.core.visibility.{Label, Level}
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
    def record(
        name: String,
        cap: Option[String] = None,
        floor: Label = Label.Public,
        crashRecord: Boolean = false,
        crashModel: Boolean = false
    )(run: (World, InMemoryDurable) => WorkflowId): (String, History) = {
      val w = new World(cap, floor, crashRecord = crashRecord, crashModel = crashModel)
      val id = run(w, w.durable)
      name -> History("run", id, Turn.Epoch, "recorded", w.durable.history(id))
    }

    /** A run of `probing(1)` over `w`'s schedule `key` with the count `n`. */
    def probed(w: World, d: InMemoryDurable, n: Int, key: String = "probe"): WorkflowId = {
      val turn = w.started(w.declared(key, n))
      d.run(turn.workflowId)(Run.body(w.env(), jobsOf(probing(1))))
      turn.workflowId
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
      },
      // A job's moves (ADR 0034), each a shape its steps can leave.
      record("run-asked")((w, d) => probed(w, d, 1)),
      record("run-called") { (w, d) => w.serve(); probed(w, d, 2) },
      record("run-called-then-asked") { (w, d) => w.serve(); probed(w, d, 3) },
      record("run-call-unserved")((w, d) => probed(w, d, 2)),
      record("run-call-unclaimed") { (w, d) => w.serve(answers = false); probed(w, d, 2) },
      record("run-call-refused", floor = Label.at(Level.Internal)) { (w, d) =>
        w.serve()
        probed(w, d, 2)
      },
      record("run-capped", cap = Some("0.0001")) { (w, d) =>
        // The day's spend is past the cap before the run asks.
        val _ = probed(w, d, 1, key = "earlier")
        probed(w, d, 1)
      },
      // The process died inside the first model call, and grit came back at v2: no move was
      // recorded, so the run is superseded.
      record("run-redeployed-before-moves", crashModel = true) { (w, d) =>
        val turn = w.started(w.declared("probe", 1))
        try d.run(turn.workflowId)(Run.body(w.env(), jobsOf(probing(1))))
        catch { case _: InMemoryDurable.Crash => "crashed" }
        d.run(turn.workflowId)(Run.body(w.env(), jobsOf(probing(2))))
        turn.workflowId
      },
      // A plugin's keeping job's keep, at an internal floor.
      record("run-kept", floor = Label.at(Level.Internal)) { (w, d) =>
        val turn = w.started(w.declared("tally", 2))
        d.run(turn.workflowId)(Run.body(w.env(), keepingJobs(new Noting(1))))
        turn.workflowId
      },
      // The process died recording the ask's cost: its model call is recorded, its record not.
      // Coming back at another version, the run's reply step meets that move.
      record("run-moved-then-redeployed", crashRecord = true) { (w, d) =>
        val turn = w.started(w.declared("probe", 1))
        try d.run(turn.workflowId)(Run.body(w.env(), jobsOf(probing(1))))
        catch { case _: InMemoryDurable.Crash => "crashed" }
        turn.workflowId
      }
    )
  }
}
