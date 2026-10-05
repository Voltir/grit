package grit.kit.run

import java.time.Instant

import grit.core.model.{ModelSetting, ModelSettings}
import grit.core.period.CloseOrdinal
import grit.core.persona.Persona
import grit.core.plugin.{InMemoryPlugins, PluginReads}
import grit.core.store.{Db, StoreError, Tx}
import grit.core.tool.{Outcome, ToolName}
import grit.dbos.sql.TestTx
import grit.kit.deployment.{PluginBinding, TestPlugins}
import grit.models.StubModels
import grit.tools.{About, Coding, Names, Probes, Tuning}

import utest.*

/** How the kit binds a deployment's plugins' tools, and which names grit's own tools hold. */
object PluginToolsTests extends TestSuite {
  import TestPlugins.name

  /** Reads in the one fake transaction. */
  private object FakeDb extends Db {
    def read[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake)
  }

  /** Keeps nothing; only its tool's name is read. */
  private object Unkept extends ModelSettings {
    def keep(setting: ModelSetting): Either[String, Unit] = Right(())
  }

  val tests = Tests {
    test("a plugin's tool reads the plugin it needs through its service, not its own documents") {
      val plugins = new InMemoryPlugins
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
        .bound(Vector(keys, reading), n => PluginReads(plugins.docs(n)))
        .fold(u => sys.error(u.toString), identity)
      bound.map(b =>
        b.described.name -> b.described.spec.args
          .read(ujson.Obj("n" -> 1), Set.empty)
          .map(args => b.run.run(args, FakeDb))
      ) ==> Vector(
        ToolName("keys") -> Right(Outcome.Done("theirs-1")),
        ToolName("theirs") -> Right(Outcome.Done("theirs-1"))
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
