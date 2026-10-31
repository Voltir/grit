package grit.projects.plugin

import grit.core.claim.InMemoryClaims
import grit.core.clock.SetClock
import grit.core.document.InMemoryDocuments
import grit.core.need.PluginReads
import grit.core.plugin.{CommandTools, Commanded, InMemoryPlugins}
import grit.core.store.{Askers, StoreError, Tx}
import grit.core.tool.{ToolSet, Toolbox}

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
  }
}
