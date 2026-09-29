package grit.lifecycle.replay

import java.time.Instant

import grit.core.durable.{History, InMemoryDurable}
import grit.core.id.{CloseRef, EntryId, PeriodRef, PeriodSeq, PluginName}
import grit.core.message.Message
import grit.core.period.{CloseOrdinal, CloseReason, TestClosings}
import grit.core.plugin.{CacheDocs, InMemoryPlugins, Plugin, PostRef}
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
import grit.lifecycle.settle.SettleFixtures
import grit.lifecycle.triage.TriageFixtures
import grit.turn.Turn

/** Writes this epoch's recorded close, settle, posting and triage histories, one per shape each can leave
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
        "Summary: We chose staging.\nOutcome: staging\nStanding:\n- staging first\nOpen:\n- none\nTouched:\n- s1"
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
          PostEnv(
            w.periods,
            w.plugins.cursors,
            w.plugins.posting,
            new grit.core.store.InMemoryTombstones,
            new FakeJot,
            new CloseFixtures.SetClock(java.time.Instant.EPOCH)
          )
        )
      )
      "post-two" -> History("post", id, Turn.Epoch, "recorded", durable.history(id))
    }
    def settled(
        name: String
    )(
        run: (SettleFixtures.World, InMemoryDurable) => grit.core.id.WorkflowId
    ): (String, History) = {
      val durable = new InMemoryDurable
      val id = run(new SettleFixtures.World, durable)
      name -> History("settle", id, Turn.Epoch, "recorded", durable.history(id))
    }
    val settles = Vector(
      settled("settle-judged") { (w, d) =>
        w.turn("deploy staging?", 0)
        val id = w.question.workflowId
        d.run(id)(w.body(new SettleFixtures.Weigher(Some(Vector(0.85, 0.1, 0.05))), 90))
        id
      },
      settled("settle-unanswered") { (w, d) =>
        w.turn("hello", 0)
        val id = w.question.workflowId
        d.run(id)(w.body(new SettleFixtures.Weigher(None), 90))
        id
      },
      settled("settle-abandoned") { (w, d) =>
        w.turn("one", 0)
        val id = w.question.workflowId
        w.turn("two", 5)
        d.run(id)(w.body(new SettleFixtures.Weigher(None), 90))
        id
      },
      settled("settle-ignored") { (w, d) =>
        w.turn("hello", 0)
        val id = w.question.workflowId
        val interrupted = new SettleFixtures.Weigher(
          Some(Vector(0.9, 0.1, 0.0)),
          () => { w.turn("wait", 80); () }
        )
        d.run(id)(w.body(interrupted, 90))
        id
      }
    )
    def triaged(
        name: String
    )(
        run: (TriageFixtures.World, InMemoryDurable) => grit.core.id.WorkflowId
    ): (String, History) = {
      val durable = new InMemoryDurable
      val id = run(new TriageFixtures.World, durable)
      name -> History("triage", id, Turn.Epoch, "recorded", durable.history(id))
    }
    val triages = Vector(
      triaged("triage-tagged") { (w, d) =>
        val t = w.hear("standup moves to 10:00 from Monday", "Ana", 0)
        val decided =
          new TriageFixtures.Scripted(
            Vector(0.125, 0.0, 0.75, 0.125, 0.0),
            Vector(0.125, 0.875, 0.25)
          )
        d.run(t.workflowId)(w.body(decided, 5))
        t.workflowId
      },
      triaged("triage-unanswered") { (w, d) =>
        val t = w.hear("lunch?", "Ana", 0)
        d.run(t.workflowId)(w.body(new TriageFixtures.Scripted(Vector.empty, Vector.empty), 5))
        t.workflowId
      },
      triaged("triage-ignored") { (w, d) =>
        val t = w.hear("lunch?", "Ana", 0)
        val purging = new TriageFixtures.Scripted(
          Vector(0, 0, 0, 0, 1),
          Vector(0.1, 0.1, 0.1),
          () => w.purge(grit.core.id.TurnRef(TriageFixtures.c, t.turn))
        )
        d.run(t.workflowId)(w.body(purging, 5))
        t.workflowId
      },
      triaged("triage-no-message") { (w, d) =>
        val said = w.say("hello", 0)
        val id = grit.core.id.TriageRef(TriageFixtures.p1, said.turnSeq).workflowId
        d.run(id)(w.body(new TriageFixtures.Scripted(Vector.empty, Vector.empty), 5))
        id
      }
    )
    (posted +: settles) ++ triages ++ Vector(
      record("close-overheard") { (w, d) =>
        // A period grit only heard: written by the heard pin though the gate found nothing new.
        w.hear("The freeze moves to Friday.", "Ana", 0)
        val id = w.attempt.workflowId
        val heard =
          new Summariser(_ => Right(replyOf("Summary: Ana said the freeze moves to Friday.")))
        d.run(id)(
          w.body(new Gate(Some(Vector(0.9, 0.1, 0.1, 0.1))), heard, new SetClock(at(Lapsed)))
        )
        id
      },
      record("close-grounded") { (w, d) =>
        // A closing written as version 3: a person's, a tool's and a claimed Standing line.
        val t = w.say("what port does the api use?", 0)
        w.add(
          t,
          Payload.Result(
            Message.ToolResult(grit.core.id.ToolCallId("c1"), "port: 3000", false),
            "read config.yml"
          ),
          0,
          "result:0"
        )
        w.add(t, Payload.Message(replyOf("It uses 3000.")), 0, "reply:0")
        w.say("we keep 3000.", 1)
        val cites = new Summariser(_ =>
          Right(
            replyOf(
              "Summary: We checked the port.\nStanding:\n- config.yml sets port 3000 [t2] by tool\n" +
                "- The api stays on 3000 [u4] by person\n- Port 3000 is the usual choice [a3] by assistant"
            )
          )
        )
        val id = w.attempt.workflowId
        d.run(id)(
          w.body(new Gate(Some(Vector(0.9, 0.9, 0.1, 0.1))), cites, new SetClock(at(Lapsed)))
        )
        id
      },
      record("close-sealed") { (w, d) =>
        w.turn("where do we deploy?", "staging", "Chose staging.", 0)
        w.say("and prod?", 1)
        val id = w.attempt.workflowId
        d.run(id)(
          w.body(new Gate(Some(Vector(0.9, 0.9, 0.1, 0.8))), written, new SetClock(at(Lapsed)))
        )
        id
      },
      record("close-carried") { (w, d) =>
        // Period 2's close, opening with period 1's balance: a standing line and a topic,
        // the topic spoken in again.
        val t0 = w.turn("where do we deploy?", "staging", "Chose staging.", 0)
        val deploy = grit.core.topic.TopicId("topic:c1:0")
        w.add(
          t0,
          Payload.Topic(
            Vector(
              grit.core.topic.TopicEvent.Opened(deploy),
              grit.core.topic.TopicEvent.Placed(
                t0.turnSeq,
                grit.core.topic.Weights.whole(deploy),
                grit.core.topic.Placement.First
              ),
              grit.core.topic.TopicEvent.Described(deploy, "Deploy Target", "where to deploy")
            )
          ),
          0,
          "topic:0"
        )
        new InMemoryDurable().run(w.attempt.workflowId)(
          w.body(new Gate(Some(Vector(0.9, 0.9, 0.1, 0.8))), written, new SetClock(at(Lapsed)))
        )
        val t1 = w.turn("staging again?", "yes", "Staging again.", Lapsed + 1)
        w.add(
          t1,
          Payload.Topic(
            Vector(
              grit.core.topic.TopicEvent.Placed(
                t1.turnSeq,
                grit.core.topic.Weights.whole(
                  grit.core.topic.TopicId.carried(
                    grit.core.period.TestClosings
                      .line(grit.core.period.Section.Topics, "Deploy Target", 1, 1)
                      .id
                  )
                ),
                grit.core.topic.Placement.First
              )
            )
          ),
          Lapsed + 1,
          "topic:1"
        )
        val id = w.attemptOn(PeriodRef(c, PeriodSeq.First.next)).workflowId
        d.run(id)(
          w.body(new Gate(Some(Vector(0.9, 0.9, 0.1, 0.8))), written, new SetClock(at(3 * Lapsed)))
        )
        id
      },
      record("close-nothing-new") { (w, d) =>
        w.turn("where do we deploy?", "staging", "Chose staging.", 0)
        val id = w.attempt.workflowId
        d.run(id)(
          w.body(new Gate(Some(Vector(0.9, 0.1, 0.1, 0.1))), written, new SetClock(at(Lapsed)))
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
    def post(closed: ClosedPeriod, docs: CacheDocs)(using Tx^): Either[StoreError, Unit] =
      docs.put(CloseOrdinal.value(closed.order).toString, ujson.Str(closed.closing.flows.prose))
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
        val closing = TestClosings.prose(s"p$i")
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
