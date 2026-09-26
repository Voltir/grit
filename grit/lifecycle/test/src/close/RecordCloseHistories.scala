package grit.lifecycle.close

import grit.core.durable.{History, InMemoryDurable}
import grit.core.provider.ProviderError
import grit.turn.Turn

/** Writes this epoch's recorded close histories, one per shape a close can leave behind,
  * into `GRIT_HISTORIES/{Turn.Epoch}`. Never overwrites: a history, once written, is what
  * builds of this epoch must keep replaying. Run it when an epoch starts or a new shape
  * appears:
  *
  * {{{./mill grit.lifecycle.test.runMain grit.lifecycle.close.RecordCloseHistories}}}
  */
object RecordCloseHistories {
  import CloseFixtures.*

  private val Lapsed = 24 * 60 + 1L

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

  private def written = new Summariser(_ =>
    Right(
      replyOf(
        "Summary: We chose staging.\nOutcome: staging\nDecisions:\n- staging first\nOpen:\n- none"
      )
    )
  )

  /** Each shape, by name, recorded by running the close over a fresh world. */
  private def shapes: Vector[(String, History)] = {
    def record(
        name: String
    )(run: (World, InMemoryDurable) => grit.core.id.WorkflowId): (String, History) = {
      val durable = new InMemoryDurable
      val id = run(new World, durable)
      name -> History("close", id, Turn.Epoch, "recorded", durable.history(id))
    }
    Vector(
      record("close-sealed") { (w, d) =>
        w.turn("where do we deploy?", "staging", "Chose staging.", 0)
        val id = attempt(w.say("and prod?", 1)).workflowId
        d.run(id)(
          w.body(new Gate(Some(Vector(0.9, 0.9, 0.1, 0.8))), written, new SetClock(at(Lapsed)))
        )
        id
      },
      record("close-not-due") { (w, d) =>
        val id = attempt(w.say("hello", 0)).workflowId
        d.run(id)(w.body(new Gate(None), written, new SetClock(at(1))))
        id
      },
      record("close-abandoned-at-seal") { (w, d) =>
        val id = attempt(w.say("hello", 0)).workflowId
        val interrupting =
          new Summariser(_ => Right(replyOf("Summary: x")), () => { w.say("one more", 30); () })
        d.run(id)(w.body(new Gate(None), interrupting, new SetClock(at(Lapsed))))
        id
      },
      record("close-no-summary") { (w, d) =>
        val id = attempt(w.turn("hi", "hello", "Greetings.", 0)).workflowId
        val failing = new Summariser(_ => Left(ProviderError.Refused("HTTP 400")))
        d.run(id)(w.body(new Gate(None), failing, new SetClock(at(Lapsed))))
        id
      }
    )
  }
}
