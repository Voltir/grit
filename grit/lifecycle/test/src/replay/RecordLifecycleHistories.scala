package grit.lifecycle.replay

import java.time.Instant

import grit.core.durable.{History, InMemoryDurable}
import grit.core.id.{CloseRef, EntryId, PeriodRef, PeriodSeq}
import grit.core.message.Message
import grit.core.period.{CloseOrdinal, CloseReason, Closing}
import grit.core.plugin.{InMemoryPlugins, Plugin, PluginDocs, PluginName, PostRef}
import grit.core.provider.ProviderError
import grit.core.store.{
  ClosedPeriod,
  Entry,
  InMemoryEntryStore,
  InMemoryPeriodStore,
  Payload,
  StoreError,
  Tx
}
import grit.dbos.sql.TestTx
import grit.lifecycle.close.CloseFixtures
import grit.lifecycle.post.{PostEnv, Posting}
import grit.turn.Turn

/** Writes this epoch's recorded close and posting histories, one per shape each can leave
  * behind, into `GRIT_HISTORIES/{Turn.Epoch}`. Never overwrites: a history, once written, is
  * what builds of this epoch must keep replaying. Run it when an epoch starts or a new shape
  * appears:
  *
  * {{{./mill grit.lifecycle.test.runMain grit.lifecycle.replay.RecordLifecycleHistories}}}
  */
object RecordLifecycleHistories {
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
    val posted = {
      val w = postWorld(2)
      val durable = new InMemoryDurable
      val id = PostRef(Posted.name, Posted.version, CloseOrdinal.Start, 0).workflowId
      durable.run(id)(
        Posting.body(
          Vector(Posted),
          PostEnv(w.periods, w.plugins.cursors, w.plugins.docs, new FakeJot)
        )
      )
      "post-two" -> History("post", id, Turn.Epoch, "recorded", durable.history(id))
    }
    posted +: Vector(
      record("close-sealed") { (w, d) =>
        w.turn("where do we deploy?", "staging", "Chose staging.", 0)
        w.say("and prod?", 1)
        val id = w.attempt.workflowId
        d.run(id)(
          w.body(new Gate(Some(Vector(0.9, 0.9, 0.1, 0.8))), written, new SetClock(at(Lapsed)))
        )
        id
      },
      record("close-deadline-moved") { (w, d) =>
        val t = w.say("hello", 0)
        val id = w.attempt.workflowId
        w.add(t, Payload.Summary("said hello"), 30, "late")
        d.run(id)(w.body(new Gate(None), written, new SetClock(at(3 * Lapsed))))
        id
      },
      record("close-abandoned-at-seal") { (w, d) =>
        w.say("hello", 0)
        val id = w.attempt.workflowId
        val interrupting =
          new Summariser(_ => Right(replyOf("Summary: x")), () => { w.say("one more", 30); () })
        d.run(id)(w.body(new Gate(None), interrupting, new SetClock(at(Lapsed))))
        id
      },
      record("close-no-summary") { (w, d) =>
        w.turn("hi", "hello", "Greetings.", 0)
        val id = w.attempt.workflowId
        val failing = new Summariser(_ => Left(ProviderError.Refused("HTTP 400")))
        d.run(id)(w.body(new Gate(None), failing, new SetClock(at(Lapsed))))
        id
      }
    )
  }

  /** The plugin posting histories are recorded with: each closed period's prose, under its
    * close ordinal.
    */
  object Posted extends Plugin {
    val name: PluginName = PluginName.of("recorded").fold(sys.error, identity)
    val version: Int = 1
    def post(closed: ClosedPeriod, docs: PluginDocs)(using Tx^): Either[StoreError, Unit] =
      docs.put(CloseOrdinal.value(closed.order).toString, ujson.Str(closed.closing.prose))
  }

  /** `n` closed periods of one conversation, and an empty plugin store. */
  final class PostWorld(n: Int) {
    val entries = new InMemoryEntryStore
    val periods = new InMemoryPeriodStore(entries)
    val plugins = new InMemoryPlugins
    locally {
      given Tx = TestTx.fake
      for (i <- 1 to n) {
        val next = entries.lockNext(CloseFixtures.c).getOrElse(sys.error("in-memory"))
        periods.openFor(CloseFixtures.c, next.turnSeq, Instant.EPOCH)
        entries.insert(
          Entry(
            EntryId(s"m$i"),
            CloseFixtures.c,
            next.turnSeq,
            None,
            next.seq,
            Payload.Message(Message.User("hi")),
            Instant.EPOCH
          )
        )
        val closing = Closing
          .of(s"p$i", None, Vector(), Vector(), Vector(), Vector())
          .getOrElse(sys.error("closing"))
        periods.seal(
          CloseRef(
            PeriodRef(CloseFixtures.c, PeriodSeq.of(i.toLong).getOrElse(sys.error("seq"))),
            next.turnSeq,
            Instant.EPOCH
          ),
          CloseReason.Lapsed,
          closing,
          Instant.EPOCH
        )
      }
    }
  }

  def postWorld(n: Int): PostWorld = new PostWorld(n)
}
