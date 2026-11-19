package grit.app.main

import java.time.Instant
import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Using

import grit.core.document.{DocRef, DocText}
import grit.core.id.DocKey
import grit.core.identity.Account
import grit.core.place.Place
import grit.core.plugin.CommandOutcome
import grit.core.store.StoreError
import grit.core.store.Tx
import grit.core.tool.ToolName
import grit.core.visibility.Integrity
import grit.core.visibility.{Clearance, Label, Level}
import grit.dbos.sql.LiveDb
import grit.projects.artifact.{Artifact, ArtifactJson}
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

  /** The plugin, its methods kept at internal: what [[desk]], where they are declared, writes. */
  private def atInternal: Projects =
    Projects
      .of(
        TransitionChecks.all,
        CommandFixtures.methodName,
        methodsAt = grit.core.visibility.Label.at(grit.core.visibility.Level.Internal)
      )
      .fold(e => throw new java.lang.AssertionError(e.toString), identity)

  private def ran(
      commands: grit.core.edge.PluginCommands^,
      by: Account,
      room: Place,
      name: ToolName,
      args: (String, ujson.Value)*
  ): Either[grit.core.edge.CommandError, CommandOutcome] =
    commands.run(by, room, Integrity.Member, name, ujson.Obj.from(args), at)

  /** `m` declared through the edge from [[desk]], by [[ann]] believed at Authority. */
  private def declared(
      commands: grit.core.edge.PluginCommands^,
      m: Method = CommandFixtures.method()
  ): Unit =
    commands
      .run(
        ann,
        desk,
        Integrity.Authority,
        ToolName("project_declare"),
        ujson.Obj(CommandFixtures.declaring(m)),
        at
      )
      .map(_.getClass) ==> Right(classOf[CommandOutcome.Done])

  /** Every current document of the plugin but its methods', as [[CommandRooms.kept]] gives them. */
  private def artifactsKept(
      config: grit.dbos.sql.DbConfig,
      engine: grit.dbos.engine.Engine^
  ): Vector[(String, String, String)] =
    kept(config, engine, Projects.Name).filterNot(_._1.contains('@'))

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

  /** The text of each current version of `key`'s brief, as the database holds it. */
  private def currentBrief(config: grit.dbos.sql.DbConfig, key: String = "p/3"): Vector[String] =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(
        conn.prepareStatement(
          "SELECT body FROM grit.documents WHERE plugin = 'projects' AND key = ? " +
            "AND superseded_at IS NULL"
        )
      ) { ps =>
        ps.setString(1, s"$key#brief")
        Using.resource(ps.executeQuery())(rs =>
          Iterator.continually(rs).takeWhile(_.next()).map(_.getString(1)).toVector
        )
      }
    }

  private def keep(
      commands: grit.core.edge.PluginCommands^,
      room: Place,
      local: String,
      body: String,
      more: (String, ujson.Value)*
  ) = {
    // A new version of the one of that handle when it is kept, else a new artifact.
    val naming: Vector[(String, ujson.Value)] =
      ran(commands, ann, room, ToolName("project_show"), "key" -> s"p/$local") match {
        case Right(CommandOutcome.Done(_)) => Vector("key" -> s"p/$local")
        case _ => Vector("project" -> "p", "handle" -> local)
      }
    ran(
      commands,
      ann,
      room,
      ToolName("project_keep"),
      (naming ++ Vector[(String, ujson.Value)](
        "kind" -> "note",
        "title" -> s"Title $local",
        "body" -> body
      ) ++ more)*
    ).map(_.getClass)
  }

  val tests: Tests = Tests {

    test("project_claims, run read-only as an edge runs it, lists a claim made through the edge") {
      CommandRooms.commanding("projects_claims", _ => Vector(atInternal)) { (_, _, commands) =>
        declared(commands)
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
                Vector(("p/1", "coder/abc")) =>
        }
      }
    }

    test(
      "a method is declared through the edge only by a caller believed at Authority, and a move reads it"
    ) {
      CommandRooms.commanding("projects_declare", _ => Vector(atInternal)) {
        (config, engine, commands) =>
          def declare(room: Place, believed: Integrity) = commands.run(
            ann,
            room,
            believed,
            ToolName("project_declare"),
            ujson.Obj(CommandFixtures.declaring(CommandFixtures.method())),
            at
          )
          (declare(desk, Integrity.Member), kept(config, engine, Projects.Name)) ==> (
            Right(CommandOutcome.Refused(CommandFixtures.declareRefused)),
            Vector()
          )
          declared(commands)
          (
            ran(commands, ann, desk, ToolName("project_new"), "project" -> "p").map(_.getClass),
            ran(
              commands,
              ann,
              desk,
              ToolName("project_keep"),
              "project" -> "p",
              "handle" -> "a",
              "kind" -> "change",
              "title" -> "A",
              "body" -> "## Why\n\nBecause."
            ).map(_.getClass),
            ran(
              commands,
              ann,
              desk,
              ToolName("project_move"),
              "key" -> "p/a",
              "state" -> "building"
            )
          ) ==> (
            Right(classOf[CommandOutcome.Done]),
            Right(classOf[CommandOutcome.Done]),
            Right(CommandOutcome.Done("Moved p/1 a (change, building)."))
          )
          kept(config, engine, Projects.Name).take(1) ==> Vector(
            ("grit@1", "internal", "authority")
          )
      }
    }

    test(
      "a move only people make is refused for a session believed below Authority, and made at it"
    ) {
      CommandRooms.commanding("projects_people", _ => Vector(atInternal)) { (_, _, commands) =>
        declared(commands, CommandFixtures.method(MethodFixtures.peopleFinish))
        def move(believed: Integrity, to: String) = commands.run(
          ann,
          desk,
          believed,
          ToolName("project_move"),
          ujson.Obj("key" -> "p/a", "state" -> to),
          at
        )
        (
          ran(commands, ann, desk, ToolName("project_new"), "project" -> "p").map(_.getClass),
          ran(
            commands,
            ann,
            desk,
            ToolName("project_keep"),
            "project" -> "p",
            "handle" -> "a",
            "kind" -> "change",
            "title" -> "A",
            "body" -> "## Why\n\nBecause."
          ).map(_.getClass)
        ) ==> (Right(classOf[CommandOutcome.Done]), Right(classOf[CommandOutcome.Done]))
        (
          move(Integrity.Member, "building"),
          move(Integrity.Member, "done"),
          move(Integrity.Authority, "done")
        ) ==> (
          Right(CommandOutcome.Done("Moved p/1 a (change, building).")),
          Right(CommandOutcome.Refused(CommandFixtures.personOnlyMove)),
          Right(CommandOutcome.Done("Moved p/1 a (change, done)."))
        )
      }
    }

    test(
      "a person-only move or a declaration is refused for a caller believed at Authority in a room capped below it, and made in an uncapped one"
    ) {
      CommandRooms.commanding("projects_capped", _ => Vector(atInternal)) { (_, _, commands) =>
        val people = CommandFixtures.method(MethodFixtures.peopleFinish)
        def declare(room: Place) = commands.run(
          ann,
          room,
          Integrity.Authority,
          ToolName("project_declare"),
          ujson.Obj(CommandFixtures.declaring(people)),
          at
        )
        def move(room: Place) = commands.run(
          ann,
          room,
          Integrity.Authority,
          ToolName("project_move"),
          ujson.Obj("key" -> "p/a", "state" -> "done"),
          at
        )
        val declareRefused = CommandOutcome.Refused(CommandFixtures.declareRefused)
        val moveRefused = CommandOutcome.Refused(CommandFixtures.personOnlyMove)
        declare(team) ==> Right(declareRefused)
        declared(commands, people)
        (
          ran(commands, ann, desk, ToolName("project_new"), "project" -> "p").map(_.getClass),
          ran(
            commands,
            ann,
            desk,
            ToolName("project_keep"),
            "project" -> "p",
            "handle" -> "a",
            "kind" -> "change",
            "title" -> "A",
            "body" -> "## Why\n\nBecause."
          ).map(_.getClass)
        ) ==> (Right(classOf[CommandOutcome.Done]), Right(classOf[CommandOutcome.Done]))
        ran(commands, ann, desk, ToolName("project_move"), "key" -> "p/a", "state" -> "building")
          .map(_.getClass) ==> Right(classOf[CommandOutcome.Done])
        (move(team), move(desk)) ==> (
          Right(moveRefused),
          Right(CommandOutcome.Done("Moved p/1 a (change, done)."))
        )
      }
    }

    test(
      "a person's move of what a session kept, and the brief showing it, are believed at the session's"
    ) {
      CommandRooms.commanding("projects_carried", _ => Vector(atInternal)) {
        (config, engine, commands) =>
          declared(commands)
          val moved = for {
            _ <- ran(commands, ann, desk, ToolName("project_new"), "project" -> "p")
            _ <- keep(commands, desk, "a", "A session's note.")
            _ <- ran(
              commands,
              ann,
              desk,
              ToolName("project_claim"),
              "key" -> "p/a",
              "role" -> "builder",
              "holder" -> "coder/abc"
            )
            moved <- commands.run(
              ann,
              desk,
              Integrity.Authority,
              ToolName("project_move"),
              ujson.Obj("key" -> "p/a", "state" -> "closed"),
              at
            )
          } yield moved
          // The claim was made at the session's too, so the brief's own header shows it re-rendered.
          (
            moved,
            artifactsKept(config, engine),
            currentBrief(config, "p/1").map(_.contains("| closed |"))
          ) ==> (
            Right(CommandOutcome.Done("Moved p/1 a (note, closed).")),
            Vector(
              ("p", "internal", "member"),
              ("p/1", "internal", "member"),
              ("p/1#brief", "internal", "member")
            ),
            Vector(true)
          )
      }
    }

    test(
      "a projects command keeps at the room's label, believed at no more than the edge gave, and a create lowers the index so"
    ) {
      CommandRooms.commanding("projects_keeps", _ => Vector(atInternal)) {
        (config, engine, commands) =>
          declared(commands)
          (
            ran(commands, ann, desk, ToolName("project_new"), "project" -> "p").map(_.getClass),
            keep(commands, desk, "a", "In the desk."),
            keep(commands, team, "b", "In the team.")
          ) ==> ((
            Right(classOf[CommandOutcome.Done]),
            Right(classOf[CommandOutcome.Done]),
            Right(classOf[CommandOutcome.Done])
          ))
          // A create from the team moves the index's `next`, so the index is believed no higher
          // than the team; a member's create after it does not raise it back.
          val lowered = artifactsKept(config, engine)
          keep(commands, desk, "c", "In the desk again.")
          (lowered, artifactsKept(config, engine).take(1)) ==> (
            Vector(
              ("p", "internal", "outside"),
              ("p/1", "internal", "member"),
              ("p/2", "internal", "outside")
            ),
            Vector(("p", "internal", "outside"))
          )
      }
    }

    test("an edit through the edge keeps the body changed, answering the version it is kept at") {
      CommandRooms.commanding("projects_edit", _ => Vector(atInternal)) { (_, _, commands) =>
        declared(commands)
        ran(commands, ann, desk, ToolName("project_new"), "project" -> "p")
        keep(commands, desk, "a", "Lead.\n\n## Goal\n\nOne.")
        val show = () =>
          ran(commands, ann, desk, ToolName("project_show"), "key" -> "p/a") match {
            case Right(CommandOutcome.Done(text)) => text
            case other => other.toString
          }
        val version = (text: String) =>
          text.linesIterator.collectFirst {
            case l if l.startsWith("version: ") => l.drop(9)
          }
        val read = version(show()).getOrElse("")
        val answer = ran(
          commands,
          ann,
          desk,
          ToolName("project_edit"),
          "key" -> "p/a",
          "version" -> read.toLong,
          "section" -> "Goal",
          "body" -> "Two."
        )
        val after = show()
        (answer, after.endsWith("\n## Goal\n\nTwo.\n"), version(after) != Some(read)) ==> (
          Right(
            CommandOutcome.Done(
              s"Kept p/1 a (note, open). Now at version ${version(after).getOrElse("")}."
            )
          ),
          true,
          true
        )
      }
    }

    test(
      "a cascade is refused, withdrawing nothing, naming a citer kept below what the asker's room writes"
    ) {
      CommandRooms.commanding("projects_cascade_floor", _ => Vector(atInternal)) {
        (config, engine, commands) =>
          declared(commands)
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
              "p/2 cites what would be withdrawn, but is kept labelled [level: internal], below " +
                "what this room writes ([level: confidential]): withdraw from a room at its label"
            )
          )
          artifactsKept(config, engine).map(_._1) ==> Vector("p", "p/1", "p/2")
          cascade(ann, desk).map(_.getClass) ==> Right(classOf[CommandOutcome.Done])
          artifactsKept(config, engine).map(_._1) ==> Vector("p", "p/2")
      }
    }

    test(
      "a sweep is refused, dry or not, changing nothing, naming an artifact it would withdraw kept below what the asker's room writes"
    ) {
      CommandRooms.commanding("projects_sweep_floor", _ => Vector(atInternal)) {
        (config, engine, commands) =>
          declared(commands)
          ran(commands, ann, desk, ToolName("project_new"), "project" -> "p").map(_.getClass) ==>
            Right(classOf[CommandOutcome.Done])
          keep(commands, desk, "a", "Pruned.") ==> Right(classOf[CommandOutcome.Done])
          ran(commands, ann, desk, ToolName("project_move"), "key" -> "p/a", "state" -> "closed")
            .map(_.getClass) ==> Right(classOf[CommandOutcome.Done])
          val refused = Right(
            CommandOutcome.Refused(
              "p/1 would be withdrawn, but is kept labelled [level: internal], below what this " +
                "room writes ([level: confidential]): withdraw from a room at its label"
            )
          )
          (sweep(commands, bea, board, dry = true), sweep(commands, bea, board, dry = false)) ==>
            (refused, refused)
          artifactsKept(config, engine).map(_._1) ==> Vector("p", "p/1")
          sweep(commands, ann, desk, dry = false).map(_.getClass) ==>
            Right(classOf[CommandOutcome.Done])
          artifactsKept(config, engine).map(_._1) ==> Vector("p")
      }
    }

    test(
      "keep and move from a room above an artifact's label are refused, keeping no second copy"
    ) {
      CommandRooms.commanding("projects_keep_floor", _ => Vector(atInternal)) {
        (config, engine, commands) =>
          declared(commands)
          ran(commands, ann, desk, ToolName("project_new"), "project" -> "p").map(_.getClass) ==>
            Right(classOf[CommandOutcome.Done])
          keep(commands, desk, "a", "In the desk.") ==> Right(classOf[CommandOutcome.Done])
          def below(verb: String) = Right(
            CommandOutcome.Refused(
              "p/1 is kept labelled [level: internal], below what this room writes " +
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
          artifactsKept(config, engine) ==> Vector(
            ("p", "internal", "member"),
            ("p/1", "internal", "member")
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
      CommandRooms.commanding("projects_migrate", _ => Vector(atInternal)) {
        (config, engine, commands) =>
          declared(commands, withMemo)
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
              Vector("project p", "p/1")
                .map(subject =>
                  s"$subject is kept labelled [level: internal], below what this room writes " +
                    "([level: confidential]): migrate from a room at its label"
                )
                .mkString("; ")
            )
          )
          artifactsKept(config, engine) ==> Vector(
            ("p", "internal", "member"),
            ("p/1", "internal", "member")
          )
          migrate(ann, desk) ==> Right(
            CommandOutcome.Done(
              "Moved project p to method grit version 1; 1 artifact renamed and kept again: p/1."
            )
          )
          ran(commands, ann, desk, ToolName("project_show"), "key" -> "p/a").map {
            case CommandOutcome.Done(text) => text.linesIterator.find(_.startsWith("kind:"))
            case refused => Some(refused.toString)
          } ==> Right(Some("kind: memo"))
          artifactsKept(config, engine) ==> Vector(
            ("p", "internal", "member"),
            ("p/1", "internal", "member")
          )
      }
    }

    test(
      "of two keeps that each leave a claimed brief within its budget and together past it, the second is refused"
    ) {
      CommandRooms.commanding(
        "projects_brief_budget",
        _ => Vector(atInternal)
      ) { (config, _, commands) =>
        declared(commands, CommandFixtures.managing)
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
                    "AND key = 'p/3#brief' AND superseded_at IS NULL FOR UPDATE"
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
        _ => Vector(atInternal)
      ) { (config, engine, commands) =>
        declared(commands, CommandFixtures.managing)
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
          // Another command's transaction, mid-way: it holds the claim and has kept its brief
          // again, through the plugin's own claims and keeper, and has not yet ended.
          val internal = Label.at(Level.Internal)
          val other = Future {
            LiveDb.transaction(config, Clearance.inRoom(desk, internal, internal)) {
              val (claims, keeper) =
                (engine.claims(Projects.Name), engine.keeper(Projects.Name, Projects.Terms))
              for {
                live <- claims.live(at)
                claim <- live.headOption.toRight(StoreError.Invalid("no live claim"))
                held <- claims.held(claim).flatMap(_.toRight(StoreError.Invalid("not held")))
                key <- DocKey.of("p/3#brief").left.map(StoreError.Invalid(_))
                was <- keeper
                  .current(key, held.label)
                  .flatMap(_.toRight(StoreError.Invalid("no brief")))
                text <- DocText.of("A brief another command kept.").left.map(StoreError.Invalid(_))
                _ <- keeper.write(
                  key,
                  held.label,
                  Integrity.Authority,
                  was.place,
                  text,
                  was.data,
                  None,
                  at,
                  Set(DocRef.brief(held.holder))
                )
              } yield {
                holding.countDown()
                val _ = go.await(30, TimeUnit.SECONDS)
              }
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
    test(
      "a claim made while a batch holds its project waits for it, and its brief holds its writes"
    ) {
      CommandRooms.commanding(
        "projects_locked",
        _ => Vector(atInternal)
      ) { (config, engine, commands) =>
        declared(commands, CommandFixtures.managing)
        ran(commands, ann, desk, ToolName("project_new"), "project" -> "p")
        keep(commands, desk, "process", "How work is run.")
        keep(commands, desk, "m", "The manager's note.")
        val pool = Executors.newFixedThreadPool(2)
        given ExecutionContext = ExecutionContext.fromExecutorService(pool)
        val (holding, go) = (new CountDownLatch(1), new CountDownLatch(1))
        try {
          // A batch's transaction, mid-way: it holds the project's lock and has kept p/m
          // rewritten, through the plugin's own claims and keeper, and has not yet ended.
          val internal = Label.at(Level.Internal)
          val batch = Future {
            LiveDb.transaction(config, Clearance.inRoom(desk, internal, internal)) {
              val (claims, keeper) =
                (engine.claims(Projects.Name), engine.keeper(Projects.Name, Projects.Terms))
              for {
                lock <- DocKey.of("p#lock").left.map(StoreError.Invalid(_))
                _ <- claims.lock(lock)
                key <- DocKey.of("p/2").left.map(StoreError.Invalid(_))
                was <- keeper
                  .current(key, internal)
                  .flatMap(_.toRight(StoreError.Invalid("no p/m")))
                data = ujson.read(
                  was.data.render().replace("The manager's note.", "The manager's note, rewritten.")
                )
                rewritten <- Artifact
                  .of(was.copy(data = data))
                  .flatMap(ArtifactJson.write)
                  .left
                  .map(StoreError.Invalid(_))
                (text, written, refs) = rewritten
                _ <- keeper.write(
                  key,
                  was.label,
                  Integrity.Member,
                  was.place,
                  text,
                  written,
                  None,
                  at,
                  refs
                )
              } yield {
                holding.countDown()
                val _ = go.await(30, TimeUnit.SECONDS)
              }
            }
          }
          holding.await(30, TimeUnit.SECONDS)
          val claiming = Future(
            ran(
              commands,
              ann,
              desk,
              ToolName("project_claim"),
              "key" -> "p/m",
              "role" -> "manager",
              "holder" -> "coder-opus/abc"
            ).map(_.getClass)
          )
          val waited = scala.util.Try(Await.ready(claiming, 1.second)).isFailure
          go.countDown()
          val (ended, claimed) =
            (Await.result(batch, 30.seconds), Await.result(claiming, 30.seconds))
          (waited, ended, claimed, currentBrief(config, "p/2").map(_.contains("rewritten"))) ==>
            (true, Right(()), Right(classOf[CommandOutcome.Done]), Vector(true))
        } finally pool.shutdownNow()
      }
    }
    test(
      "a re-key keeps how far each artifact was believed, and the next create mints above it"
    ) {
      CommandRooms.commanding("projects_rekey", _ => Vector(atInternal)) {
        (config, engine, commands) =>
          declared(commands)
          ran(commands, ann, desk, ToolName("project_new"), "project" -> "p")
          val internal = Label.at(Level.Internal)
          // Project p as the old form left it: its index with no next, and two artifacts keyed by
          // name, p/b kept by a turn, believed lower than a person's own word.
          val old = Vector(
            (
              "p/a",
              Integrity.Authority,
              """{"head":{"kind":"note","state":"open","title":"A",""" +
                """"parent":null,"cites":["p/b"],"by":"ann"},"body":{"v":1,"notes":["p/b"],""" +
                """"blocks":[{"type":"para","text":[{"t":"See "},{"t":"b","n":[0]}]}]}}"""
            ),
            (
              "p/b",
              Integrity.Member,
              """{"head":{"kind":"note","state":"open","title":"B",""" +
                """"parent":null,"cites":[],"by":null},"body":{"v":1,"notes":[],""" +
                """"blocks":[{"type":"para","text":[{"t":"Plain."}]}]}}"""
            )
          )
          def written[A](e: Either[String, A]): Either[StoreError, A] =
            e.left.map(StoreError.Invalid(_))
          LiveDb.transaction(config, Clearance.inRoom(desk, internal, internal)) {
            val keeper = engine.keeper(Projects.Name, Projects.Terms)
            for {
              key <- written(DocKey.of("p"))
              index <- keeper
                .current(key, internal)
                .flatMap(_.toRight(StoreError.Invalid("no index")))
              _ <- keeper.write(
                key,
                internal,
                index.integrity,
                index.place,
                index.text,
                ujson.Obj.from(index.data.obj.toSeq.filterNot(_._1 == "next")),
                None,
                at,
                Set(DocRef.of("pinned", "p").fold(sys.error, identity))
              )
              _ <- old.foldLeft[Either[StoreError, Unit]](Right(())) {
                case (so, (k, believed, data)) =>
                  for {
                    _ <- so
                    under <- written(DocKey.of(k))
                    text <- written(DocText.of("Old."))
                    ref <- written(DocRef.of("project", "p"))
                    _ <- keeper.write(
                      under,
                      internal,
                      believed,
                      Place.Everywhere,
                      text,
                      ujson.read(data),
                      None,
                      at,
                      Set(ref)
                    )
                  } yield ()
              }
            } yield ()
          } ==> Right(())
          val rekeyed = commands.run(
            ann,
            desk,
            Integrity.Authority,
            ToolName("project_rekey"),
            ujson.Obj("project" -> "p"),
            at
          )
          (
            rekeyed,
            artifactsKept(config, engine).filter(k => k._1 == "p/a" || k._1 == "p/b"),
            ran(
              commands,
              ann,
              desk,
              ToolName("project_keep"),
              "project" -> "p",
              "kind" -> "note",
              "title" -> "C",
              "body" -> "c"
            ).map {
              case CommandOutcome.Done(t) => t.linesIterator.next().split(" Now").head;
              case r => r.toString
            }
          ) ==> (
            Right(
              CommandOutcome.Done(
                "p: 2 artifacts, 1 body links, 1 head cites, 0 handles carried\n" +
                  "It gave ids p/1 to p/2; the next new artifact is p/3."
              )
            ),
            Vector(("p/a", "internal", "authority"), ("p/b", "internal", "member")),
            Right("Kept p/3 (note, open).")
          )
      }
    }
  }
}
