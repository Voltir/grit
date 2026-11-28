package grit.projects.plugin

import grit.core.id.{ConversationId, TurnRef, TurnSeq}
import grit.core.plugin.{CommandBy, CommandOutcome, KeepingCommand}
import grit.core.tool.ToolName

import utest.*

/** The projects plugin's commands as a turn is offered and runs them. */
object HeldProjectCommandToolsTests extends TestSuite {

  import CommandFixtures.*

  /** `project_keep` with `args`, run for `who` as a turn's tool runs it. */
  private def keepFor(who: CommandBy, projects: Projects, store: Store)(
      args: (String, ujson.Value)*
  ): CommandOutcome =
    projects.commands.find(c => ToolName.value(c.spec.name) == "project_keep") match {
      case Some(c: KeepingCommand[a]) =>
        c.spec.args
          .read(ujson.Obj.from(args))
          .fold(
            e => throw new java.lang.AssertionError(e.message),
            a =>
              inTx(c.run(a, who, store.keeper, store.claims, at))
                .fold(e => throw new java.lang.AssertionError(e.toString), identity)
          )
      case _ => throw new java.lang.AssertionError("no keeping command project_keep")
    }

  private def described(name: String): String =
    plugin().commands
      .find(c => ToolName.value(c.spec.name) == name)
      .fold(throw new java.lang.AssertionError(s"no command $name"))(_.spec.does)

  val tests: Tests = Tests {
    test("a keep through a turn names the person and the turn as its writer") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      val turn = TurnRef(ConversationId("c7"), TurnSeq(5))
      done(
        keepFor(CommandBy.Turn(person, turn), projects, store)(
          "key" -> "p/a",
          "kind" -> "change",
          "title" -> "Title a",
          "body" -> "## Why\n\nBecause."
        )
      )
      val text = shown(projects, store, "p/a")
      assert(text.contains(s"by: \"$by, through grit (turn 5 of conversation c7)\""))
    }

    test("a turn is offered exactly the seven reads, new, keep and move") {
      plugin().commands.filter(_.offeredToTurns).map(c => ToolName.value(c.spec.name)).sorted ==>
        Vector(
          "project_brief",
          "project_export",
          "project_find",
          "project_history",
          "project_keep",
          "project_list",
          "project_move",
          "project_new",
          "project_show",
          "project_stale"
        )
    }

    test("project_stale's description does not point at project_withdraw") {
      described("project_stale").contains("project_withdraw") ==> false
    }
  }
}
