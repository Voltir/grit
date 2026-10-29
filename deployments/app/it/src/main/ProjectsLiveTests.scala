package grit.app.main

import java.time.Instant

import grit.core.identity.Account
import grit.core.place.Place
import grit.core.plugin.CommandOutcome
import grit.core.tool.ToolName
import grit.core.visibility.Integrity
import grit.projects.plugin.{CommandFixtures, Projects}

import utest.*

/** The projects plugin's commands run as an edge runs them for a person in a labelled room, over
  * a real Postgres: what they keep is raised to the room's label and believed at no more than the
  * edge gave.
  */
object ProjectsLiveTests extends TestSuite {

  import CommandRooms.{ann, bea, board, desk, kept, team}

  private val at = Instant.parse("2026-10-27T10:00:00Z")

  private def ran(
      commands: grit.core.edge.PluginCommands^,
      by: Account,
      room: Place,
      name: ToolName,
      args: (String, ujson.Value)*
  ): Either[grit.core.edge.CommandError, CommandOutcome] =
    commands.run(by, room, Integrity.Member, name, ujson.Obj.from(args), at)

  private def keep(
      commands: grit.core.edge.PluginCommands^,
      room: Place,
      local: String,
      body: String,
      more: (String, ujson.Value)*
  ) =
    ran(
      commands,
      ann,
      room,
      ToolName("project_keep"),
      (Vector[(String, ujson.Value)](
        "key" -> s"p/$local",
        "kind" -> "note",
        "title" -> s"Title $local",
        "body" -> body
      ) ++ more)*
    ).map(_.getClass)

  val tests: Tests = Tests {

    test(
      "a projects command keeps at the room's label, believed at no more than the edge gave"
    ) {
      CommandRooms.commanding("projects_keeps", _ => Vector(CommandFixtures.plugin())) {
        (config, engine, commands) =>
          (
            ran(commands, ann, desk, ToolName("project_new"), "project" -> "p").map(_.getClass),
            keep(commands, desk, "a", "In the desk."),
            keep(commands, team, "b", "In the team.")
          ) ==> ((
            Right(classOf[CommandOutcome.Done]),
            Right(classOf[CommandOutcome.Done]),
            Right(classOf[CommandOutcome.Done])
          ))
          kept(config, engine, Projects.Name) ==> Vector(
            ("p", "internal", "member"),
            ("p/a", "internal", "member"),
            ("p/b", "internal", "outside")
          )
      }
    }

    test(
      "a cascade is refused, withdrawing nothing, naming a citer kept below what the asker's room writes"
    ) {
      CommandRooms.commanding("projects_cascade_floor", _ => Vector(CommandFixtures.plugin())) {
        (config, engine, commands) =>
          ran(commands, ann, desk, ToolName("project_new"), "project" -> "p").map(_.getClass) ==>
            Right(classOf[CommandOutcome.Done])
          keep(commands, desk, "a", "Pruned.") ==> Right(classOf[CommandOutcome.Done])
          keep(commands, desk, "c", "See [a](artifact:p/a).") ==> Right(
            classOf[CommandOutcome.Done]
          )
          ran(commands, ann, desk, ToolName("project_move"), "key" -> "p/c", "to" -> "closed")
            .map(_.getClass) ==> Right(classOf[CommandOutcome.Done])
          def cascade(by: Account, room: Place) =
            ran(commands, by, room, ToolName("project_withdraw"), "key" -> "p/a", "cascade" -> true)
          cascade(bea, board) ==> Right(
            CommandOutcome.Refused(
              "p/c cites what would be withdrawn, but is kept labelled [level: internal], below " +
                "what this room writes ([level: confidential]): withdraw from a room at its label"
            )
          )
          kept(config, engine, Projects.Name).map(_._1) ==> Vector("p", "p/a", "p/c")
          cascade(ann, desk).map(_.getClass) ==> Right(classOf[CommandOutcome.Done])
          kept(config, engine, Projects.Name).map(_._1) ==> Vector("p", "p/c")
      }
    }
  }
}
