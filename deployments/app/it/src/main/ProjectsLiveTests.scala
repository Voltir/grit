package grit.app.main

import java.time.Instant
import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Using

import grit.core.identity.Account
import grit.core.place.Place
import grit.core.plugin.CommandOutcome
import grit.core.store.Tx
import grit.core.tool.ToolName
import grit.core.visibility.Integrity
import grit.dbos.sql.LiveDb
import grit.projects.method.{ArtifactKind, Method, MethodFixtures, TransitionChecks}
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

  /** `project_sweep` of the notes of `p` last changed before the day after [[at]], two days on. */
  private def sweep(
      commands: grit.core.edge.PluginCommands^,
      by: Account,
      room: Place,
      dry: Boolean
  ): Either[grit.core.edge.CommandError, CommandOutcome] =
    commands.run(
      by,
      room,
      Integrity.Member,
      ToolName("project_sweep"),
      ujson.Obj(
        "project" -> "p",
        "kinds" -> ujson.Arr("note"),
        "before" -> "2026-10-28",
        "dry" -> dry
      ),
      at.plusSeconds(2L * 86_400)
    )

  /** The text of each current version of `p/m`'s brief, as the database holds it. */
  private def currentBrief(config: grit.dbos.sql.DbConfig): Vector[String] =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(
        conn.prepareStatement(
          "SELECT body FROM grit.documents WHERE plugin = 'projects' AND key = 'p/m#brief' " +
            "AND superseded_at IS NULL"
        )
      )(ps =>
        Using.resource(ps.executeQuery())(rs =>
          Iterator.continually(rs).takeWhile(_.next()).map(_.getString(1)).toVector
        )
      )
    }

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

    test("project_claims, run read-only as an edge runs it, lists a claim made through the edge") {
      CommandRooms.commanding("projects_claims", _ => Vector(CommandFixtures.plugin())) {
        (_, _, commands) =>
          val claims = for {
            _ <- ran(commands, ann, desk, ToolName("project_new"), "project" -> "p")
            _ <- keep(commands, desk, "a", "Claimed.")
            _ <- ran(
              commands,
              ann,
              desk,
              ToolName("project_claim"),
              "key" -> "p/a",
              "role" -> "builder",
              "holder" -> "coder/abc"
            )
            listed <- ran(commands, ann, desk, ToolName("project_claims"))
          } yield listed
          assertMatch(claims) {
            case Right(CommandOutcome.Done(said))
                if ujson.read(said)("claims").arr.map(c => (c("key").str, c("holder").str)) ==
                  Vector(("p/a", "coder/abc")) =>
          }
      }
    }

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
          ran(commands, ann, desk, ToolName("project_move"), "key" -> "p/c", "state" -> "closed")
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

    test(
      "a sweep is refused, dry or not, changing nothing, naming an artifact it would withdraw kept below what the asker's room writes"
    ) {
      CommandRooms.commanding("projects_sweep_floor", _ => Vector(CommandFixtures.plugin())) {
        (config, engine, commands) =>
          ran(commands, ann, desk, ToolName("project_new"), "project" -> "p").map(_.getClass) ==>
            Right(classOf[CommandOutcome.Done])
          keep(commands, desk, "a", "Pruned.") ==> Right(classOf[CommandOutcome.Done])
          ran(commands, ann, desk, ToolName("project_move"), "key" -> "p/a", "state" -> "closed")
            .map(_.getClass) ==> Right(classOf[CommandOutcome.Done])
          val refused = Right(
            CommandOutcome.Refused(
              "p/a would be withdrawn, but is kept labelled [level: internal], below what this " +
                "room writes ([level: confidential]): withdraw from a room at its label"
            )
          )
          (sweep(commands, bea, board, dry = true), sweep(commands, bea, board, dry = false)) ==>
            (refused, refused)
          kept(config, engine, Projects.Name).map(_._1) ==> Vector("p", "p/a")
          sweep(commands, ann, desk, dry = false).map(_.getClass) ==>
            Right(classOf[CommandOutcome.Done])
          kept(config, engine, Projects.Name).map(_._1) ==> Vector("p")
      }
    }

    test(
      "keep and move from a room above an artifact's label are refused, keeping no second copy"
    ) {
      CommandRooms.commanding("projects_keep_floor", _ => Vector(CommandFixtures.plugin())) {
        (config, engine, commands) =>
          ran(commands, ann, desk, ToolName("project_new"), "project" -> "p").map(_.getClass) ==>
            Right(classOf[CommandOutcome.Done])
          keep(commands, desk, "a", "In the desk.") ==> Right(classOf[CommandOutcome.Done])
          def below(verb: String) = Right(
            CommandOutcome.Refused(
              "p/a is kept labelled [level: internal], below what this room writes " +
                s"([level: confidential]): $verb from a room at its label"
            )
          )
          ran(
            commands,
            bea,
            board,
            ToolName("project_keep"),
            "key" -> "p/a",
            "title" -> "Title a",
            "body" -> "From the board."
          ) ==> below("keep")
          ran(
            commands,
            bea,
            board,
            ToolName("project_move"),
            "key" -> "p/a",
            "state" -> "closed"
          ) ==>
            below("move")
          kept(config, engine, Projects.Name) ==> Vector(
            ("p", "internal", "member"),
            ("p/a", "internal", "member")
          )
      }
    }

    test(
      "a migration is refused from a room above its project's label, and from the project's own " +
        "room renames in place"
    ) {
      import MethodFixtures.{change, note, transitions, views}
      val memo = ArtifactKind(
        MethodFixtures.kind("memo"),
        MethodFixtures.state("open"),
        Vector(),
        Set(),
        Vector()
      )
      val withMemo = Method
        .of(
          CommandFixtures.methodName,
          1,
          Vector(change, note, memo),
          transitions,
          views,
          TransitionChecks.all
        )
        .fold(why => sys.error(why.toString), identity)
      CommandRooms.commanding("projects_migrate", _ => Vector(CommandFixtures.plugin(withMemo))) {
        (config, engine, commands) =>
          ran(commands, ann, desk, ToolName("project_new"), "project" -> "p").map(_.getClass) ==>
            Right(classOf[CommandOutcome.Done])
          keep(commands, desk, "a", "In the desk.") ==> Right(classOf[CommandOutcome.Done])
          def migrate(by: Account, room: Place) =
            ran(
              commands,
              by,
              room,
              ToolName("project_migrate"),
              "project" -> "p",
              "version" -> 1,
              "kinds" -> ujson.Arr(ujson.Obj("from" -> "note", "to" -> "memo"))
            )
          migrate(bea, board) ==> Right(
            CommandOutcome.Refused(
              Vector("project p", "p/a")
                .map(subject =>
                  s"$subject is kept labelled [level: internal], below what this room writes " +
                    "([level: confidential]): migrate from a room at its label"
                )
                .mkString("; ")
            )
          )
          kept(config, engine, Projects.Name) ==> Vector(
            ("p", "internal", "member"),
            ("p/a", "internal", "member")
          )
          migrate(ann, desk) ==> Right(
            CommandOutcome.Done(
              "Moved project p to method grit version 1; 1 artifact renamed and kept again: p/a."
            )
          )
          ran(commands, ann, desk, ToolName("project_show"), "key" -> "p/a").map {
            case CommandOutcome.Done(text) => text.linesIterator.find(_.startsWith("kind:"))
            case refused => Some(refused.toString)
          } ==> Right(Some("kind: memo"))
          kept(config, engine, Projects.Name) ==> Vector(
            ("p", "internal", "member"),
            ("p/a", "internal", "member")
          )
      }
    }

    test(
      "of two keeps that each leave a claimed brief within its budget and together past it, the second is refused"
    ) {
      CommandRooms.commanding(
        "projects_brief_budget",
        _ => Vector(CommandFixtures.plugin(CommandFixtures.managing))
      ) { (config, _, commands) =>
        ran(commands, ann, desk, ToolName("project_new"), "project" -> "p")
        keep(commands, desk, "c", "Cited.")
        keep(commands, desk, "process", "How work is run.")
        keep(commands, desk, "m", "The manager's note.", "cites" -> ujson.Arr("p/c"))
        ran(
          commands,
          ann,
          desk,
          ToolName("project_claim"),
          "key" -> "p/m",
          "role" -> "manager",
          "holder" -> "coder-opus/abc"
        )
        val pool = Executors.newFixedThreadPool(3)
        given ExecutionContext = ExecutionContext.fromExecutorService(pool)
        val (holding, go) = (new CountDownLatch(1), new CountDownLatch(1))
        try {
          // Holds the claimed brief's current row, so the first keep stops at its brief, its
          // project's change made and not yet committed.
          val brief = Future {
            LiveDb.transaction(config) { (tx: Tx^) ?=>
              val conn: java.sql.Connection^{tx} = Tx.connection(tx)
              Using.resource(
                conn.prepareStatement(
                  "SELECT version FROM grit.documents WHERE plugin = 'projects' " +
                    "AND key = 'p/m#brief' AND superseded_at IS NULL FOR UPDATE"
                )
              )(ps => Using.resource(ps.executeQuery())(rs => rs.next()))
              holding.countDown()
              go.await(30, TimeUnit.SECONDS)
            }
          }
          holding.await(30, TimeUnit.SECONDS)
          val first = Future(
            keep(commands, desk, "m", "The note, grown. " * 300, "cites" -> ujson.Arr("p/c"))
          )
          val stopped = scala.util.Try(Await.ready(first, 1.second)).isFailure
          val second = Future(
            ran(
              commands,
              ann,
              desk,
              ToolName("project_keep"),
              "key" -> "p/process",
              "kind" -> "note",
              "title" -> "Title process",
              "body" -> "How work is run, grown. " * 210
            )
          )
          val waited = scala.util.Try(Await.ready(second, 1.second)).isFailure
          go.countDown()
          val (held, kept, refused) = (
            Await.result(brief, 30.seconds),
            Await.result(first, 30.seconds),
            Await.result(second, 30.seconds)
          )
          val rendered = ran(
            commands,
            ann,
            desk,
            ToolName("project_brief"),
            "key" -> "p/m",
            "role" -> "manager",
            "chars" -> 9500
          ).map { case CommandOutcome.Done(text) => text; case other => other.toString }
          (
            stopped,
            waited,
            held,
            kept,
            refused.map {
              case CommandOutcome.Refused(why) => why.contains("over the 9,500 a brief holds")
              case done => done
            },
            rendered.map(r => (r.contains("The note, grown."), r.contains("run, grown."))),
            currentBrief(config).map(Right(_))
          ) ==> (
            true,
            true,
            true,
            Right(classOf[CommandOutcome.Done]),
            Right(true),
            Right((true, false)),
            Vector(rendered)
          )
        } finally pool.shutdownNow()
      }
    }

    test(
      "a keep changing a claimed brief while another command keeps it waits for that one, then keeps it again"
    ) {
      CommandRooms.commanding(
        "projects_briefs",
        _ => Vector(CommandFixtures.plugin(CommandFixtures.managing))
      ) { (config, _, commands) =>
        ran(commands, ann, desk, ToolName("project_new"), "project" -> "p")
        keep(commands, desk, "c", "Cited.")
        keep(commands, desk, "process", "How work is run.")
        keep(commands, desk, "m", "The manager's note.", "cites" -> ujson.Arr("p/c"))
        ran(
          commands,
          ann,
          desk,
          ToolName("project_claim"),
          "key" -> "p/m",
          "role" -> "manager",
          "holder" -> "coder-opus/abc"
        )
        val pool = Executors.newFixedThreadPool(2)
        given ExecutionContext = ExecutionContext.fromExecutorService(pool)
        val (holding, go) = (new CountDownLatch(1), new CountDownLatch(1))
        try {
          // Another command's transaction, mid-way: it has renewed the claim and kept its brief
          // again, and has not yet ended.
          val other = Future {
            LiveDb.transaction(config) { (tx: Tx^) ?=>
              val conn: java.sql.Connection^{tx} = Tx.connection(tx)
              def run(sql: String, args: Any*): Vector[Long] =
                Using.resource(conn.prepareStatement(sql)) { ps =>
                  args.zipWithIndex.foreach((a, i) => ps.setObject(i + 1, a))
                  if (sql.contains("RETURNING")) Using.resource(ps.executeQuery()) { rs =>
                    Iterator.continually(rs).takeWhile(_.next()).map(_.getLong(1)).toVector
                  }
                  else Vector(ps.executeUpdate().toLong)
                }
              run(
                "UPDATE grit.claims SET role = role WHERE plugin = 'projects' AND subject = 'p/m'"
              )
              val was = run(
                "UPDATE grit.documents SET superseded_at = now() WHERE plugin = 'projects' " +
                  "AND key = 'p/m#brief' AND superseded_at IS NULL RETURNING version"
              )
              val now = run(
                "INSERT INTO grit.documents (plugin, key, place, body, data, pointer, written_at, " +
                  "last_placed, classification_id, room_id, integrity_id) SELECT plugin, key, " +
                  "place, 'A brief another command kept.', data, pointer, now(), now(), " +
                  "classification_id, room_id, integrity_id FROM grit.documents WHERE version = ? " +
                  "RETURNING version",
                was*
              )
              run(
                "INSERT INTO grit.document_refs (version, kind, ref) SELECT ?, kind, ref " +
                  "FROM grit.document_refs WHERE version = ?",
                (now ++ was)*
              )
              holding.countDown()
              go.await(30, TimeUnit.SECONDS)
              Right(())
            }
          }
          holding.await(30, TimeUnit.SECONDS)
          val keeping = Future(keep(commands, desk, "c", "Cited, rewritten."))
          val waited = scala.util.Try(Await.ready(keeping, 1.second)).isFailure
          go.countDown()
          val (ended, kept) = (Await.result(other, 30.seconds), Await.result(keeping, 30.seconds))
          val rendered = ran(
            commands,
            ann,
            desk,
            ToolName("project_brief"),
            "key" -> "p/m",
            "role" -> "manager",
            "chars" -> 9500
          ).map { case CommandOutcome.Done(text) => text; case refused => refused.toString }
          val current = currentBrief(config)
          (waited, ended, kept, current.map(Right(_)), rendered.map(_.contains("rewritten"))) ==>
            (true, Right(()), Right(classOf[CommandOutcome.Done]), Vector(rendered), Right(true))
        } finally pool.shutdownNow()
      }
    }
  }
}
