package grit.app.main

import java.nio.file.{Files, Path}

import grit.core.clock.{Clock, Fresh}
import grit.core.id.{ToolCallId, TurnRef}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.place.Repository
import grit.core.provider.{ModelRequest, Provider, ProviderError}
import grit.core.store.{Entry, Payload, WentOn}
import grit.core.visibility.{Subject, Visibility}
import grit.dbos.engine.{Engine, LiveEngine}
import grit.dbos.sql.TestPostgres
import grit.host.checkout.{LocalRuns, Redaction}
import grit.host.git.{GitFixture, WorkingCopies}
import grit.kit.deployment.Offered
import grit.kit.work.Work
import grit.turn.Turn

import utest.*

/** A run of work in a declared repository, against Postgres and DBOS, with the real coding tools
  * served by this process's edge in the run's own working copy of a real git clone: its model
  * reads a file, changes it and commits it through the allowlisted `git commit`, and its check
  * passes there; the clone is left as it was.
  */
object RepositoryLiveTests extends TestSuite {
  import LiveTurn.*

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  /** Reads `README.md`, writes it, then `git add` and `git commit` it, one call per model call,
    * then answers; it notes whether the call after the read was shown what the read read.
    */
  private final class Committing extends Provider {
    // Written by the turn's model calls, one at a time; read only by the test once it ends.
    @volatile @caps.unsafe.untrackedCaptures
    var shownRead: Option[Boolean] = None

    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
      val usage = Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, Some(BigDecimal(0)))
      def calling(id: String, tool: String, args: ujson.Obj): Message.Assistant =
        Message.Assistant(
          Vector(AssistantBlock.ToolCall(ToolCallId(id), tool, args)),
          StopReason.ToolUse,
          usage,
          "m"
        )
      def git(args: String*) =
        ujson.Obj("program" -> "git", "args" -> ujson.Arr(args.map(ujson.Str(_))*))
      val results = request.messages.collect { case Message.ToolResult(_, content, _) => content }
      if (results.size == 1) shownRead = Some(results.exists(_.contains("first")))
      Right(results.size match {
        case 0 => calling("r", "read", ujson.Obj("path" -> "README.md"))
        case 1 => calling("w", "write", ujson.Obj("path" -> "README.md", "content" -> "changed\n"))
        case 2 => calling("a", "run", git("add", "README.md"))
        case 3 => calling("c", "run", git("commit", "--quiet", "-m", "change the readme"))
        case _ =>
          Message.Assistant(Vector(AssistantBlock.Text("done")), StopReason.EndTurn, usage, "m")
      })
    }
  }

  /** The reference deployment's visibility, `declared` declared as the operator's repositories. */
  private def reference(declared: Vector[Repository]): Visibility =
    right(
      Clearances
        .caps(Map.empty, declared)
        .flatMap(Clearances.visibility(Vector.empty, None, Vector.empty, _, Vector.empty))
    )

  private def own(engine: Engine^, turn: TurnRef): Vector[Entry] =
    engine.db
      .read(Subject.Public)(engine.entries.list(turn.conversationId))
      .getOrElse(Vector.empty)
      .filter(_.turnSeq == turn.turnSeq)

  private def out(repo: GitFixture.Repo^, in: Path, args: String*): String =
    GitFixture.git(repo, in, args*).output.text.trim

  /** What a run of `GitFixture.repository` came to, its repository declared when `declared`:
    * whether the model was shown what it read, and whether each of its tool calls failed; in its copy, the branch, the last commit's subject,
    * its check's verdict and what git shows uncommitted after it; in the clone, whether its
    * HEAD, branch, status, file and refs are as they were.
    */
  private def ran(declared: Boolean) =
    GitFixture.inRepository { repo =>
      val clone = repo.source
      Files.writeString(clone.resolve("README.md"), "first\n")
      // The clone's own config, which its worktrees share, so the model's `git commit` runs
      // under no person's.
      for (
        (k, v) <- Seq(
          "user.name" -> "grit",
          "user.email" -> "grit@example.invalid",
          "commit.gpgsign" -> "false"
        )
      )
        GitFixture.git(repo, clone, "config", k, v)
      GitFixture.git(repo, clone, "add", "README.md")
      GitFixture.git(repo, clone, "commit", "--quiet", "-m", "readme")
      def standing = (
        out(repo, clone, "rev-parse", "HEAD"),
        out(repo, clone, "rev-parse", "--abbrev-ref", "HEAD"),
        out(repo, clone, "status", "--porcelain"),
        Files.readString(clone.resolve("README.md"))
      )
      val before = standing
      val copies = new WorkingCopies(
        GitFixture.repository,
        clone,
        repo.root,
        repo.processes,
        Clock.system()
      )
      val run = GitFixture.work("w1")
      val visibility = reference(if (declared) Vector(GitFixture.repository) else Vector.empty)
      val engine = LiveEngine.open(
        TestPostgres.freshDatabase(if (declared) "repository_run" else "repository_uncapped"),
        Turn.Epoch,
        visibility = visibility
      )
      try {
        val inCopy = right(
          copies
            .held(run) { copy =>
              val model = new Committing
              launchIn(
                engine,
                engine.entries,
                model,
                clone,
                offered = Offered.Allowlisted(Offered.ReferenceAllowlist),
                copies = Some(copies),
                run = Some(run)
              )
              val turn = say(engine, "change the readme")
              val _ = engine.awaitTurn(turn)
              val failed = own(engine, turn).collect {
                case Entry(
                      _,
                      _,
                      _,
                      _,
                      _,
                      Payload.Result(Message.ToolResult(_, _, failed), _),
                      _,
                      _
                    ) =>
                  failed
              }
              val checks = new LocalRuns(
                copy,
                Map.empty,
                Redaction.of(Map.empty),
                Clock.system(),
                Fresh.random()
              )
              val verdict =
                try Work.checked(checks, right(WentOn.Command.of("grep -q changed README.md")))
                finally checks.close()
              (
                model.shownRead,
                failed,
                out(repo, copy, "rev-parse", "--abbrev-ref", "HEAD"),
                out(repo, copy, "log", "-1", "--format=%s"),
                verdict,
                out(repo, copy, "status", "--porcelain")
              )
            }
            .left
            .map(_.message)
        )
        (
          inCopy,
          standing == before,
          out(repo, clone, "for-each-ref", "--format=%(refname)").linesIterator.toVector
        )
      } finally engine.close()
    }

  val tests = Tests {
    test(
      "a run of a declared repository is shown what it reads in its own copy, changes and commits it on its branch, its check passes there, and the clone is as it was"
    ) {
      ran(declared = true) ==> (
        (
          Some(true),
          Vector(false, false, false, false),
          "grit/w1",
          "change the readme",
          Work.Verdict.Passed,
          ""
        ),
        true,
        Vector("refs/heads/grit/w1", "refs/heads/main")
      )
    }
  }
}
