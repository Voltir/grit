package grit.kit.run

import java.time.Instant

import grit.core.claim.{Claims, InMemoryClaims}
import grit.core.clock.SetClock
import grit.core.document.{DocumentKeeper, InMemoryDocuments}
import grit.core.id.TurnRef
import grit.core.identity.Principal
import grit.core.need.PluginReads
import grit.core.plugin.{
  CommandBy,
  CommandOutcome,
  InMemoryPlugins,
  KeepingCommand,
  PluginCommand,
  ReadingCommand
}
import grit.core.store.{Askers, StoreError, Tx}
import grit.core.tool.{Args, Field, Tool, ToolName, ToolSpec}
import grit.kit.deployment.{PluginBinding, TestPlugins}

import utest.*

/** Which plugin commands the kit offers a turn as tools. */
object HeldPluginCommandToolsTests extends TestSuite {
  import TestPlugins.name

  final case class Said(word: String) extends caps.Pure

  private def spec(called: String): ToolSpec[Said] =
    ToolSpec(
      ToolName.of(called).fold(e => throw new java.lang.AssertionError(e), identity),
      "Says a word.",
      Args.of((word = Field.text("The word."))).map(a => Said(a.word))
    )

  /** A reading command named `called`, offered to turns when `offered`. */
  private final class Saying(called: String, offered: Boolean) extends ReadingCommand[Said] {
    val spec: ToolSpec[Said] = HeldPluginCommandToolsTests.spec(called)
    override def offeredToTurns: Boolean = offered
    def run(args: Said, by: CommandBy, own: PluginReads, at: Instant)(using
        Tx^
    ): Either[StoreError, CommandOutcome] = Right(CommandOutcome.Done(args.word))
  }

  /** A keeping command named `called`, keeping nothing, offered to turns when `offered`. */
  private final class Noting(called: String, offered: Boolean) extends KeepingCommand[Said] {
    val spec: ToolSpec[Said] = HeldPluginCommandToolsTests.spec(called)
    override def offeredToTurns: Boolean = offered
    def run(args: Said, by: CommandBy, own: DocumentKeeper, claims: Claims, at: Instant)(using
        Tx^
    ): Either[StoreError, CommandOutcome] = Right(CommandOutcome.Done(args.word))
  }

  /** A plugin named `p` whose commands are `commands`, keeping tallies. */
  private def plugin(p: String, commands: PluginCommand[?]*) =
    new TestPlugins.Commanding(name(p), commands.toVector, documents = Some(TestPlugins.Tallies))

  val tests = Tests {
    test("a turn is offered exactly the commands that say so, in the plugins' order") {
      val docs = new InMemoryDocuments
      val clock = new SetClock(Instant.EPOCH)
      val offered: Vector[Tool.Offered^{clock}] = PluginBinding.commands(
        Vector(
          plugin("b", new Saying("b_read", true), new Noting("b_hush", false)),
          plugin("c", new Saying("c_hush", false), new Noting("c_keep", true)),
          plugin("a", new Noting("a_keep", true), new Saying("a_read", true))
        ),
        p => PluginReads(new InMemoryPlugins().docs(p), docs.keeper(p, TestPlugins.Tallies.terms)),
        docs.keeper,
        p => new InMemoryClaims().of(p, _ => None),
        new Askers {
          def of(turn: TurnRef)(using Tx^): Either[StoreError, Option[Principal]] = Right(None)
        },
        clock
      )
      offered.map(t => ToolName.value(t.name)) ==> Vector("b_read", "c_keep", "a_keep", "a_read")
    }
  }
}
