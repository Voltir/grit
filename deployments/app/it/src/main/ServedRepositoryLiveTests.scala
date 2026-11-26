package grit.app.main

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import grit.core.agent.{Subagent, Subagents}
import grit.core.clock.Clock
import grit.core.files.PagedText
import grit.core.host.{Allowlist, Argv}
import grit.core.id.{AgentType, SourceId}
import grit.core.identity.Account
import grit.core.message.{Message, SpendCap, Tokens}
import grit.core.model.{Assignment, Ladder, ModelId, ModelRef, Policy}
import grit.core.period.LifecycleSettings
import grit.core.persona.Persona
import grit.core.speech.Speaking
import grit.core.spend.Budget
import grit.core.store.Origin
import grit.core.visibility.Integrity
import grit.dbos.engine.LiveEngine
import grit.dbos.sql.{DbConfig, TestPostgres}
import grit.host.git.GitFixture
import grit.kit.deployment.{Assembly, Deployment, Offer, Offered, Placer, RepositoryClone}
import grit.kit.environment.Secrets
import grit.kit.run.Kit
import grit.models.StubProvider
import grit.turn.{Turn, TurnLoop}

import utest.*

/** Against Postgres and DBOS, over a real git clone: a served deployment ([[Kit.opened]]) hosts the
  * repository its coding subagent works in, so a child a task's turn delegates to runs its tools
  * in a working copy of its own, on its own branch. The stub model calls what each message names.
  */
object ServedRepositoryLiveTests extends TestSuite {

  private def ok[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val assigned =
    Assignment(ModelRef(ok(ModelId.of("openai/gpt-oss-120b").toRight("id")), None), 100, None)

  /** A coder's allowlist as a deployment declares one: `git add` and `git commit` alone among
    * git's commands, and a build.
    */
  private val allowlist: Allowlist = ok(
    Allowlist.of(
      Vector(
        Allowlist.Rule.Leading(ok(Argv.of("git", "add"))),
        Allowlist.Rule.Leading(ok(Argv.of("git", "commit"))),
        Allowlist.Rule.Leading(ok(Argv.of("./mill")))
      ),
      10.minutes
    )
  )

  private val coder: AgentType = ok(AgentType.of("coder"))

  /** Hosting `clone`, its one subagent a coder working there. */
  private def deployment(clone: RepositoryClone): Deployment = {
    val coding = ok(
      Subagent.of(
        coder,
        ok(Persona.of("Coder")),
        Ladder.one(assigned),
        Subagent.Tools.Coding(Vector.empty, clone.repository, allowlist),
        ok(SpendCap.of(BigDecimal("0.05"))),
        Integrity.WritesOut
      )
    )
    Deployment
      .of(
        plugins = Vector.empty,
        policy = Policy.one(assigned, assigned, assigned, assigned),
        offer = Offer(Offered.Read, TurnLoop.Rounds.of(3).getOrElse(sys.error("rounds"))),
        assembly = Assembly.Linear(Tokens(4000)),
        placer = Placer.Off("no classifier here"),
        lifecycle = LifecycleSettings.Default,
        budget = Budget(java.time.ZoneOffset.UTC, None),
        speaking = Speaking.Off,
        sweep = 30.seconds,
        persona = grit.core.persona.Persona.Grit,
        formats = PagedText.formats,
        subagents = ok(Subagents.of(Vector(coding))),
        repositories = Vector(clone)
      )
      .fold(r => sys.error(r.message), identity)
  }

  /** The copies under `copies` for the repository, each a directory, its lock beside it. */
  private def made(copies: Path): Vector[Path] =
    if (!Files.isDirectory(copies)) Vector.empty
    else {
      val listing = Files.list(copies)
      try listing.iterator.asScala.filter(Files.isDirectory(_)).toVector
      finally listing.close()
    }

  val tests = Tests {
    test(
      "a served deployment's coding child, delegated to from a task's turn, writes in a working copy of its own on its own branch"
    ) {
      GitFixture.inRepository { repo =>
        val config = TestPostgres.freshDatabase("served_repository")
        val engine = LiveEngine.open(config, Turn.Epoch)
        try {
          val d = deployment(RepositoryClone(GitFixture.repository, repo.source, repo.root))
          val env = Map(
            DbConfig.UrlVar -> config.jdbcUrl,
            DbConfig.UserVar -> config.user,
            DbConfig.PasswordVar -> config.password
          )
          val secrets = Secrets.of(env, d).fold(r => sys.error(r.message), identity)
          val write = ujson.Obj("path" -> "hello.txt", "content" -> "hello\n")
          val task = s"Write the greeting.\n${StubProvider.calling("write")}${write.render()}\n"
          val delegate = ujson.Obj("agent" -> "coder", "task" -> task)
          val turn = (for {
            turn <- engine.inbox.ingest(
              Origin.Task("served", "parent"),
              SourceId("served-1"),
              Message.User(s"hand it on ${StubProvider.calling("delegate")}${delegate.render()}"),
              Account.Local,
              None
            )
            _ <- engine.inbox.startTurn(turn)
          } yield turn).fold(e => sys.error(s"inbox: $e"), identity)
          val log = org.slf4j.LoggerFactory.getLogger("grit.serve")
          Kit.opened(engine, d, secrets, env, Clock.system(), _ => (), log) match {
            case Left(failure) => sys.error(failure.toString)
            case Right(opened) =>
              try {
                val _ = engine.awaitTurn(turn)
                val copies = made(repo.root.resolve(GitFixture.repository.name))
                val seen = copies.map { copy =>
                  val name = copy.getFileName.toString
                  val branch = GitFixture
                    .git(repo, copy, "rev-parse", "--abbrev-ref", "HEAD")
                    .output
                    .text
                    .trim
                  (
                    branch == s"grit/$name",
                    name.startsWith("s"),
                    Option.when(Files.exists(copy.resolve("hello.txt")))(
                      Files.readString(copy.resolve("hello.txt"))
                    )
                  )
                }
                (seen, Files.exists(repo.source.resolve("hello.txt"))) ==> (
                  Vector((true, true, Some("hello\n"))),
                  false
                )
              } finally opened.close()
          }
        } finally engine.close()
      }
    }
  }
}
