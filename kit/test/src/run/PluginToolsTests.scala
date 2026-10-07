package grit.kit.run

import java.time.Instant

import grit.core.clock.SetClock
import grit.core.document.InMemoryDocuments
import grit.core.id.{JobName, PluginName, TestCallSlots, ToolCallId}
import grit.core.job.{InMemorySchedules, ScheduleDesk}
import grit.core.message.AssistantBlock
import grit.core.model.{ModelSetting, ModelSettings}
import grit.core.period.CloseOrdinal
import grit.core.persona.Persona
import grit.core.plugin.{InMemoryPlugins, PluginReads}
import grit.core.store.{Db, StoreError, Tx}
import grit.core.tool.{Bound, Outcome, Repairs, ToolName, Toolbox}
import grit.core.visibility.Subject
import grit.dbos.sql.TestTx
import grit.kit.deployment.{Desks, PluginBinding, TestPlugins}
import grit.models.StubModels
import grit.tools.{About, Coding, Names, Probes, Tuning}

import utest.*

/** How the kit binds a deployment's plugins' tools, and which names grit's own tools hold. */
object PluginToolsTests extends TestSuite {
  import TestPlugins.name

  /** Reads in the one fake transaction. */
  private object FakeDb extends Db {
    def read[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake)
  }

  /** Keeps nothing; only its tool's name is read. */
  private object Unkept extends ModelSettings {
    def keep(setting: ModelSetting): Either[String, Unit] = Right(())
  }

  val tests = Tests {
    test("a plugin's tool reads the plugin it needs through its service, not its own documents") {
      val plugins = new InMemoryPlugins
      val documents = new InMemoryDocuments
      val keys = new TestPlugins.Keys(name("keys"))
      val reading = new TestPlugins.Reading(name("reading"), keys, declared = true)
      locally {
        given Tx = TestTx.fake
        val at = CloseOrdinal.of(1).getOrElse(sys.error("ordinal"))
        for (p <- Vector(keys.name, reading.name)) {
          val _ = plugins.cursors.start(p, 1, Instant.EPOCH)
        }
        val _ = plugins.cache(keys.name, at).put("theirs-1", ujson.Str("x"))
        val _ = plugins.cache(reading.name, at).put("mine-1", ujson.Str("x"))
      }
      val bound = PluginBinding
        .bound(Vector(keys, reading), n => PluginReads(plugins.docs(n), documents.shelf(n)))
        .fold(u => sys.error(u.toString), identity)
      val desk =
        new InMemorySchedules().desk(name("keys"), Vector.empty, new SetClock(Instant.EPOCH))
      bound.map(b =>
        b.described.name -> b.described.spec.args
          .read(ujson.Obj("n" -> 1), Set.empty)
          .map(args =>
            b.run.run(
              args,
              TestCallSlots.First,
              FakeDb.as(Subject.Turn(TestCallSlots.First.turn)),
              desk
            )
          )
      ) ==> Vector(
        ToolName("keys") -> Right(Outcome.Done("theirs-1")),
        ToolName("theirs") -> Right(Outcome.Done("theirs-1"))
      )
    }

    test("each plugin's tool writes through its own plugin's desk, of that plugin's jobs alone") {
      val (nudge, standup, other) =
        (
          new TestPlugins.Named("nudge"),
          new TestPlugins.Named("standup"),
          new TestPlugins.Named("other")
        )
      val plugins = Vector(
        new TestPlugins.Booking(name("books"), nudge, Vector(nudge, standup)),
        new TestPlugins.Booking(name("more"), other, Vector(other), ToolName("book_more"))
      )
      val asked = scala.collection.mutable.ArrayBuffer.empty[(PluginName, Vector[JobName])]
      val schedules = new InMemorySchedules()
      val clock = new SetClock(Instant.EPOCH)
      val desks: Desks^{clock} = new Desks {
        def of(plugin: PluginName, jobs: Vector[JobName]): ScheduleDesk^ = {
          asked += plugin -> jobs
          schedules.desk(plugin, jobs, clock)
        }
      }
      val reads = new InMemoryPlugins
      val documents = new InMemoryDocuments
      val bound = PluginBinding
        .bound(plugins, n => PluginReads(reads.docs(n), documents.shelf(n)))
        .fold(u => sys.error(u.toString), identity)
      val box = Toolbox
        .of[caps.CapSet^{FakeDb, desks}](bound.map(_.over(FakeDb, desks))*)
        .fold(d => sys.error(d.toString), identity)
      val outcomes = Vector("book", "book_more").map(tool =>
        box.bind(
          AssistantBlock.ToolCall(ToolCallId(tool), tool, ujson.Obj("n" -> 1)),
          Repairs.All
        ) match {
          case Right(free: Bound.Free) => free(TestCallSlots.First)
          case other => sys.error(s"not free: $other")
        }
      )
      (outcomes, asked.toVector) ==> (
        Vector(Outcome.Done("booked"), Outcome.Done("booked")),
        Vector(
          name("books") -> Vector(nudge.name, standup.name),
          name("more") -> Vector(other.name)
        )
      )
    }

    test("grit's own tool names are those of every tool the kit offers besides the plugins'") {
      val about = About.load(Persona.Grit).fold(why => sys.error(why), _.name)
      val offered =
        Vector(about, Tuning.propose(Unkept).name, Probes.probe(new StubModels()).name) ++
          Coding.hosted.map(_.name)
      Names.all.toSet ==> offered.toSet
      Names.all.size ==> offered.size
    }
  }
}
