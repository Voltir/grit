package grit.eval.harness.main

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.util.Try

import grit.core.clock.Clock
import grit.core.host.{RelPath, Variables}
import grit.core.id.{ConversationId, TurnRef, WorkflowId}
import grit.core.message.{Message, Tokens}
import grit.core.secret.Secret
import grit.core.visibility.{Label, Subject}
import grit.dbos.internal.TrialRows
import grit.dbos.sql.DbConfig
import grit.eval.harness.bench.{
  Arm,
  Bench,
  BenchTask,
  Caps,
  CoderTurns,
  GritTurn,
  MemoryCap,
  PerPass,
  Rig,
  Standing,
  Tried
}
import grit.eval.harness.evidence.{Proposal, Shown}
import grit.eval.harness.sent.{ConversationSent, Cut, SentMarkdown}
import grit.host.blobs.LocalBlobs
import grit.host.checkout.{LocalHanding, LocalMemory, LocalProcesses, Redaction}
import grit.kit.environment.DotEnv
import grit.models.AnthropicProvider
import grit.turn.{TurnFailure, TurnRecord}

import Cli.*
import Engines.*

/** The coder bench's runs: `bench`. */
private[main] object BenchCommands {

  /** `bench --tasks <dir> --task <name> --repository <path> --work <dir>`, then either `--known
    * true`, or `--model <id> --max-budget-usd <usd> --max-turns <n> --scout <program>` and a
    * harness: `--harness claude-code` (the default) with `--claude <program> --agent <file>`, or
    * `--harness grit` with `--grit <launcher> --java-home <dir> --standing <file> --coder-url
    * <jdbc> --max-tokens <n> [--max-memory-gb <n>] [--window-tokens <n>] [--work-turns <n>]` (8
    * GB when not given, [[MemoryCap.of]]; the last two grit's [[Arm.Settings]]; a standing file
    * [[Standing.read]] refuses is refused before anything runs): the task
    * `<name>` of `<dir>` ([[BenchTask.of]], its `task.json` and `brief.md`), in a checkout under `<work>` named for it and `clock`'s now, of the repository
    * at `<path>`. With `--known`, its own commits through its checks ([[Rig.known]]). With `--dry
    * true` and grit, what grit's first call would send ([[Rig.dry]]), printed whole, nothing
    * kept, no key read. Otherwise a coder run under the caps, the key `ANTHROPIC_API_KEY` from
    * the environment ([[DotEnv.loadDefault]]): Claude Code's ([[Rig.trial]], the agent's prompt
    * the file `<file>`), or grit's ([[Rig.gritTrial]]: its standing text the file `<file>`, its turn kept in the database at `<jdbc>` and read back from there), kept with its
    * transcript and its diff in the database at `--url <jdbc>` and the blobs at `--blobs <dir>`
    * ([[Bench.keep]]), its log at `<work>/<checkout>.log`. Before any run, `<work>` is cleared
    * ([[Rig.cleared]]: every checkout a killed run left, and every log but the newest
    * [[TrialRows.MostTrials]]). Prints what [[Tried.lines]] gives, or each check; never the key.
    */
  private[main] def bench(f: Map[String, String], clock: Clock^): Either[String, Unit] =
    if (f.get("summary").contains("true")) summary(f) else run(f, clock)

  /** `bench --summary true --url <jdbc>`: cents per passing trial over every trial the
    * database at `<jdbc>` keeps ([[PerPass.lines]]), read through a read-only [[Reader]], the
    * login from `GRIT_DATABASE_USER` and `_PASSWORD`.
    */
  private def summary(f: Map[String, String]): Either[String, Unit] =
    for {
      url <- need(f, "url")
      config <- DbConfig.fromEnv(sys.env.updated(DbConfig.UrlVar, url)).left.map(_.message)
      trials <- opened(config)(Bench.trials(_)(Subject.Public))
    } yield PerPass.lines(trials).foreach(println)

  private def run(f: Map[String, String], clock: Clock^): Either[String, Unit] =
    for {
      name <- need(f, "task")
      taskName <- TrialRows.TaskName.of(name)
      at <- need(f, "tasks").map(Path.of(_).resolve(name))
      task <- read(at.resolve("task.json")).flatMap(json =>
        read(at.resolve("brief.md")).flatMap(BenchTask.of(taskName, json, _))
      )
      repository <- need(f, "repository")
      root <- need(f, "work").map(Path.of(_))
      _ <- Try(Files.createDirectories(root)).toEither.left.map(e => s"no work directory: $e")
      cleared <- Rig.cleared(root)(TrialRows.MostTrials)
      _ = if (cleared > 0)
        System.err.println(s"[bench] removed $cleared old runs' entries from $root")
      env <- DotEnv.loadDefault(sys.env)
      redaction = Redaction.of(env)
      _ = System.err.println(s"[bench] ${redaction.described}")
      work = new LocalProcesses(root, sys.env, redaction)
      handing = new LocalHanding(root, sys.env, redaction)
      dir <- RelPath.of(s"$name-${clock.now().getEpochSecond}").left.map(_.toString)
      _ <-
        if (f.get("known").contains("true"))
          Rig.known(work)(repository, task, dir).map { c =>
            c.lines.foreach(println)
            println(s"known: ${if (c.passed) "passed" else "failed"}")
          }
        else
          for {
            model <- need(f, "model")
            budget <- need(f, "max-budget-usd")
            turns <- need(f, "max-turns")
            cap <- Caps.of(model, budget, turns)
            scout <- need(f, "scout")
            output = root.resolve(s"${RelPath.value(dir)}.log")
            _ <- f.getOrElse("harness", "claude-code") match {
              case "claude-code" =>
                for {
                  claude <- need(f, "claude")
                  agent <- need(f, "agent").flatMap(a => read(Path.of(a)))
                  key <- Secret.read(env, AnthropicProvider.KeyVar).left.map(_.message)
                  _ = println(s"the run's log: $output")
                  _ <- kept(f, env, clock)(() =>
                    Rig.trial(work, handing, clock)(
                      repository,
                      task,
                      Arm.ClaudeCode(claude, scout, agent),
                      cap,
                      key,
                      dir,
                      Rig.Quiet,
                      output
                    )
                  )
                } yield ()
              case "grit" =>
                for {
                  launcher <- need(f, "grit")
                  javaHome <- need(f, "java-home")
                  standing <- need(f, "standing")
                    .flatMap(s => Standing.read(Path.of(s)))
                    .flatMap(s =>
                      Shown
                        .scanned(s)
                        .left
                        .map(Proposal.graded(s"--standing ${f.getOrElse("standing", "")}", _))
                    )
                  url <- need(f, "coder-url")
                  maxTokens <- need(f, "max-tokens").flatMap(positive("max-tokens"))
                  memoryCap <- f.get("max-memory-gb").fold(Right(MemoryCap.Default))(MemoryCap.of)
                  window <- f.get("window-tokens") match {
                    case None => Right(None)
                    case Some(_) =>
                      need(f, "window-tokens")
                        .flatMap(positive("window-tokens"))
                        .map(n => Some(Tokens(n.toLong)))
                  }
                  workTurns <- f.get("work-turns") match {
                    case None => Right(None)
                    case Some(_) =>
                      need(f, "work-turns").flatMap(positive("work-turns")).map(Some(_))
                  }
                  passed <- Variables.of(
                    Map(
                      "PATH" -> sys.env.getOrElse("PATH", "/usr/bin:/bin"),
                      "HOME" -> sys.env.getOrElse("HOME", "/"),
                      "JAVA_HOME" -> javaHome
                    )
                  )
                  arm <- Arm.Grit.of(
                    launcher,
                    scout,
                    standing,
                    url,
                    maxTokens,
                    passed,
                    Arm.Settings(window, workTurns)
                  )
                  _ <-
                    if (f.get("dry").contains("true"))
                      Rig.dry(work)(repository, task, arm, cap, dir).map(println)
                    else
                      for {
                        key <- Secret.read(env, AnthropicProvider.KeyVar).left.map(_.message)
                        coderDb <- DbConfig.fromEnv(Map(DbConfig.UrlVar -> url)).left.map(_.message)
                        _ = println(s"the run's log: $output")
                        _ <- kept(f, env, clock)(() =>
                          Rig.gritTrial(
                            work,
                            handing,
                            clock,
                            coderTurns(coderDb, clock),
                            new LocalMemory(root)
                          )(
                            repository,
                            task,
                            arm,
                            cap,
                            memoryCap,
                            key,
                            dir,
                            Rig.Quiet,
                            output
                          )
                        )
                      } yield ()
                } yield ()
              case other => Left(s"--harness $other: not claude-code or grit")
            }
          } yield ()
    } yield ()

  /** The trial `run` makes, printed ([[Tried.lines]]) and kept with its transcript and diff in the
    * database at `--url` and the blobs at `--blobs` ([[Bench.keep]]), through an engine opened on
    * that database for the purpose; the database's login from `env`.
    */
  private def kept(f: Map[String, String], env: Map[String, String], clock: Clock^)(
      run: () => Either[String, Tried]
  ): Either[String, Unit] =
    for {
      url <- need(f, "url")
      config <- DbConfig.fromEnv(env.updated(DbConfig.UrlVar, url)).left.map(_.message)
      blobsAt <- need(f, "blobs").map(Path.of(_))
      _ <- LocalBlobs.open(blobsAt) match {
        case Left(e) => Left(s"the bench's blobs did not open: $e")
        case Right(blobs) =>
          withEngine(config, clock, "eval-bench", "the trial was not kept") { engine =>
            for {
              tried <- run()
              _ = tried.lines.foreach(println)
              kept <- Bench.keep(engine.jot, blobs, clock)(
                Subject.Public,
                tried.trial,
                Standing.ofArm(tried.arm).map(_.text),
                new ByteArrayInputStream(
                  tried.transcript.mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8)
                ),
                new ByteArrayInputStream(tried.diff.getBytes(StandardCharsets.UTF_8)),
                Label.Public
              )
            } yield println(s"kept: ${kept._1}; pruned: ${kept._2}")
          }
      }
      trials <- opened(config)(Bench.trials(_)(Subject.Public))
    } yield PerPass.lines(trials).foreach(println)

  /** The grit coder's turns in the database `config` names, logged in as `docker-compose.yml`'s
    * user: the conversation of the newest created before `clock`'s now, every turn of it
    * ([[ConversationSent.read]]) rendered whole ([[SentMarkdown.render]]), with the messages its
    * last turn's last call was sent; or, when its turns cannot be read back, a turn failed for
    * why ([[GritTurn.unread]]).
    */
  private def coderTurns(config: DbConfig, clock: Clock^): CoderTurns^ = new CoderTurns {
    def task(): Either[String, GritTurn] =
      opened(config) { reader =>
        val now = clock.now()
        for {
          turns <- reader
            .turns(now)
            .left
            .map(e => s"the grit coder's turns could not be read: ${e.described}")
          newest <- turns
            .flatMap((id, _) => TurnRef.fromWorkflowId(id))
            .lastOption
            .toRight("the grit coder's database records no turn")
          status <- reader
            .workflow(newest.workflowId)
            .toRight(s"DBOS does not know ${WorkflowId.value(newest.workflowId)}")
          session = ConversationId.value(newest.conversationId)
          read =
            for {
              task <- ConversationSent.read(reader, newest.conversationId, now)
              last <- task.turns.lastOption.toRight("the grit coder's database records no turn")
              steps <- reader
                .steps(last.workflow)
                .left
                .map(e => s"the grit coder's turn's steps could not be read: ${e.kind}")
            } yield GritTurn(
              session,
              last.requests.lastOption.fold(Vector.empty[Message])(_._3.messages),
              status.status,
              task.turns
                .map(SentMarkdown.render(_, config.endpoint.toString, Cut.Whole))
                .mkString("\n"),
              if (last.answer.isDefined) None
              else
                TurnRecord.failure(steps).map((step, f) => s"$step: ${TurnFailure.reason(f)}")
            )
        } yield read.fold(GritTurn.unread(session, status.status, _), identity)
      }
  }
}
