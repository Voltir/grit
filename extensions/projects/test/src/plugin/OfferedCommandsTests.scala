package grit.projects.plugin

import grit.core.claim.InMemoryClaims
import grit.core.clock.SetClock
import grit.core.document.InMemoryDocuments
import grit.core.edge.PluginCommands
import grit.core.need.PluginReads
import grit.core.plugin.{CommandTools, Commanded, InMemoryPlugins}
import grit.core.store.{Askers, StoreError, Tx}
import grit.core.tool.{ToolSet, Toolbox}
import grit.projects.method.MethodFixtures

import utest.*

/** The projects plugin's commands as a turn is offered them: built into a turn's tool set as
  * the kit's binding builds them ([[Commanded.of]], then [[CommandTools.of]]), so a turn can
  * record that set and read it back.
  */
object OfferedCommandsTests extends TestSuite {

  private val nobody: Askers = new Askers {
    def of(turn: grit.core.id.TurnRef)(using Tx^): Either[StoreError, Option[Nothing]] =
      Right(None)
  }

  val tests = Tests {
    test("every command a turn is offered builds a tool set that reads back from its stored form") {
      val docs = new InMemoryDocuments
      val clock = new SetClock(CommandFixtures.at)
      val offered = Commanded
        .of(Vector(CommandFixtures.plugin()))
        .filter(_.command.offeredToTurns)
        .map(
          CommandTools.of(
            _,
            p => PluginReads(new InMemoryPlugins().docs(p), docs.keeper(p, Projects.Terms)),
            docs.keeper,
            p => new InMemoryClaims().of(p, _ => None),
            nobody,
            clock
          )
        )
      val names = offered.map(_.name)
      Toolbox
        .of[caps.CapSet^{clock}](offered*)
        .map(box => ToolSet.read(ToolSet.write(box.set)).map(_.tools.map(_.name))) ==>
        Right(Right(names))
    }

    test("an edge's tool list marks each reading command as reading and each keeping one not") {
      val proposing = Projects
        .of(
          Vector(CommandFixtures.method() -> Vector.empty),
          CommandFixtures.methodName,
          TurnCommands.Proposing(MethodFixtures.change.name)
        )
        .fold(e => throw new java.lang.AssertionError(e.toString), identity)
      val listed = Commanded
        .of(Vector(proposing))
        .map(PluginCommands.Listed.of)
        .map(l => l.schema.name -> l.reads)
        .distinct
      val reading = Set(
        "project_show",
        "project_history",
        "project_list",
        "project_find",
        "project_export",
        "project_stale",
        "project_brief"
      )
      val keeping = Set(
        "project_new",
        "project_keep",
        "project_move",
        "project_propose",
        "project_batch",
        "project_claim",
        "project_release",
        "project_withdraw",
        "project_sweep",
        "project_migrate",
        "project_claims"
      )
      listed.toSet ==> (reading.map(_ -> true) ++ keeping.map(_ -> false))
    }
  }
}
