package grit.eval.harness.main

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.concurrent.duration.DurationInt
import scala.util.Try

import grit.core.clock.Clock
import grit.core.host.{RelPath, Variables}
import grit.core.id.ConversationId
import grit.core.message.Message
import grit.core.secret.Secret
import grit.core.stitch.Tuning as StitchTuning
import grit.core.visibility.{Label, Subject}
import grit.dbos.internal.TrialRows
import grit.dbos.sql.DbConfig
import grit.eval.Room
import grit.eval.harness.bench.{Arm, Bench, Caps, CoderTurns, GritTurn, Rig, Task, Tried}
import grit.eval.harness.jev.Budget
import grit.eval.harness.lab.StitchLab
import grit.eval.harness.log.{Cache, Weights}
import grit.eval.harness.report.{LabRun, Report}
import grit.eval.harness.score.{Against, RoomScore}
import grit.eval.harness.sent.{Cut, SentMarkdown, TurnSent}
import grit.host.blobs.LocalBlobs
import grit.host.checkout.{LocalHanding, LocalProcesses, Redaction}
import grit.kit.environment.DotEnv
import grit.models.JevConfig
import grit.models.{AnthropicProvider, JevClassifier}

import Cli.*
import Engines.*

/** The lab's runs: `bench` and `lab-stitch`. */
private[main] object LabCommands {

  /** `bench --tasks <dir> --task <name> --repository <path> --work <dir>`, then either `--known
    * true`, or `--model <id> --max-budget-usd <usd> --max-turns <n> --scout <program>` and a
    * harness: `--harness claude-code` (the default) with `--claude <program> --agent <file>`, or
    * `--harness grit` with `--grit <launcher> --java-home <dir> --coder <file> --coder-url <jdbc>
    * --max-tokens <n>`: the task `<name>` of `<dir>` ([[Task.of]], its `task.json` and
    * `brief.md`), in a checkout under `<work>` named for it and `clock`'s now, of the repository
    * at `<path>`. With `--known`, its own commits through its checks ([[Rig.known]]). With `--dry
    * true` and grit, what grit's first call would send ([[Rig.dry]]), printed whole, nothing
    * kept, no key read. Otherwise a coder run under the caps, the key `ANTHROPIC_API_KEY` from
    * the environment ([[DotEnv.loadDefault]]): Claude Code's ([[Rig.trial]], the agent's prompt
    * the file `<file>`), or grit's ([[Rig.gritTrial]]: its coder's instructions the file
    * `<file>`, its turn kept in the database at `<jdbc>` and read back from there), kept with its
    * transcript in the database at `--url <jdbc>` and the blobs at `--blobs <dir>`
    * ([[Bench.keep]]). Before any run, `<work>` is cleared of old runs ([[Rig.cleared]]: the
    * newest [[Rig.KeptCheckouts]] keep their checkouts, the newest [[TrialRows.Kept]] grit's
    * output). Prints what [[Tried.lines]] gives, or each check; never the key.
    */
  private[main] def bench(f: Map[String, String], clock: Clock^): Either[String, Unit] =
    for {
      name <- need(f, "task")
      taskName <- TrialRows.TaskName.of(name)
      at <- need(f, "tasks").map(Path.of(_).resolve(name))
      task <- read(at.resolve("task.json")).flatMap(json =>
        read(at.resolve("brief.md")).flatMap(Task.of(taskName, json, _))
      )
      repository <- need(f, "repository")
      root <- need(f, "work").map(Path.of(_))
      _ <- Try(Files.createDirectories(root)).toEither.left.map(e => s"no work directory: $e")
      cleared <- Rig.cleared(root)(TrialRows.Kept, Rig.KeptCheckouts)
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
            c.runs.foreach((run, ok) => println(s"check: $run ${if (ok) "passed" else "failed"}"))
            println(s"known: ${if (c.passed) "passed" else "failed"}")
          }
        else
          for {
            model <- need(f, "model")
            budget <- need(f, "max-budget-usd")
            turns <- need(f, "max-turns")
            cap <- Caps.of(model, budget, turns)
            scout <- need(f, "scout")
            _ <- f.getOrElse("harness", "claude-code") match {
              case "claude-code" =>
                for {
                  claude <- need(f, "claude")
                  agent <- need(f, "agent").flatMap(a => read(Path.of(a)))
                  key <- Secret.read(env, AnthropicProvider.KeyVar).left.map(_.message)
                  _ <- kept(f, env, clock)(() =>
                    Rig.trial(work, handing, clock)(
                      repository,
                      task,
                      Arm.ClaudeCode(claude, scout, agent),
                      cap,
                      key,
                      dir,
                      Rig.Quiet
                    )
                  )
                } yield ()
              case "grit" =>
                for {
                  launcher <- need(f, "grit")
                  javaHome <- need(f, "java-home")
                  coder <- need(f, "coder").flatMap(c => read(Path.of(c)))
                  url <- need(f, "coder-url")
                  maxTokens <- need(f, "max-tokens").flatMap(positive("max-tokens"))
                  passed <- Variables.of(
                    Map(
                      "PATH" -> sys.env.getOrElse("PATH", "/usr/bin:/bin"),
                      "HOME" -> sys.env.getOrElse("HOME", "/"),
                      "JAVA_HOME" -> javaHome
                    )
                  )
                  arm <- Arm.Grit.of(launcher, scout, coder, url, maxTokens, passed)
                  _ <-
                    if (f.get("dry").contains("true"))
                      Rig.dry(work)(repository, task, arm, cap, dir).map(println)
                    else
                      for {
                        key <- Secret.read(env, AnthropicProvider.KeyVar).left.map(_.message)
                        coderDb <- DbConfig.fromEnv(Map(DbConfig.UrlVar -> url)).left.map(_.message)
                        output = root.resolve(s"${RelPath.value(dir)}.grit.log")
                        _ = println(s"grit's output: $output")
                        _ <- kept(f, env, clock)(() =>
                          Rig.gritTrial(work, handing, clock, coderTurns(coderDb, clock))(
                            repository,
                            task,
                            arm,
                            cap,
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

  /** The trial `run` makes, printed ([[Tried.lines]]) and kept with its transcript in the
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
                new ByteArrayInputStream(
                  tried.transcript.mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8)
                ),
                Label.Public
              )
            } yield println(s"kept: ${kept._1}; pruned: ${kept._2}")
          }
      }
    } yield ()

  /** The grit coder's turns in the database `config` names, logged in as `docker-compose.yml`'s
    * user: the newest created before `clock`'s now, with the messages its last call was sent and
    * the turn rendered whole ([[SentMarkdown.render]]).
    */
  private def coderTurns(config: DbConfig, clock: Clock^): CoderTurns^ = new CoderTurns {
    def newest(): Either[String, GritTurn] =
      opened(config) { reader =>
        for {
          turns <- reader
            .turns(clock.now())
            .left
            .map(e => s"the grit coder's turns could not be read: ${e.described}")
          last <- turns.lastOption.toRight("the grit coder's database records no turn")
          sent <- TurnSent.read(reader, last._1)
        } yield GritTurn(
          ConversationId.value(sent.turn.conversationId),
          sent.requests.lastOption.fold(Vector.empty[Message])(_._3.messages),
          last._2.status,
          SentMarkdown.render(sent, config.endpoint.toString, Cut.Whole)
        )
      }
  }

  /** `lab-stitch --room <file> --url <jdbc> --cache <dir> --spend <usd> --out <dir>
    * [--follows-at <p>] [--lexical <n>] [--recent <n>] [--repeats <n>] [--burst-prior <p>
    * --burst-gap <seconds> | --burst none]`: the room written at
    * `<file>` ([[Room.read]], named for its file less `.json`) placed by the stitch lab
    * ([[StitchLab.run]]) `--repeats` times (1 when not given) against the strands its placings
    * make and as often against the truth's ([[Against]]), repeat `r` as Jev's repeat `r`, each
    * run in a channel of its own, in the database at `<jdbc>`, which must
    * be `grit_eval_lab` and hold nothing of the room, through an engine opened on it for the
    * purpose; under shipped tuning but for the knobs named (a burst prior, [[StitchTuning.burst]],
    * needs both of its flags; `--burst none` is the no-prior baseline, and the shipped run keeps
    * [[StitchTuning.Default]]'s prior); Jev asked through the cache at
    * `<dir>` within `--spend` USD over all the runs. Each run scored ([[RoomScore.of]]) and all
    * written to `<out>/lab-stitch-<room>.md` ([[Report.lab]]); printed up to the first run's
    * section, before any of the room's text. Jev's key and the database's login
    * come from the environment ([[DotEnv.loadDefault]]); each call is timed by
    * `clock`.
    */
  private[main] def labStitch(f: Map[String, String], clock: Clock^): Either[String, Unit] =
    for {
      path <- need(f, "room").map(Path.of(_))
      url <- need(f, "url")
      _ <- Either.cond(
        url.endsWith("/grit_eval_lab"),
        (),
        s"the lab runs on grit_eval_lab alone, not $url"
      )
      cap <- need(f, "spend").flatMap(decimal("spend"))
      cacheDir <- need(f, "cache").map(Path.of(_))
      out <- need(f, "out").map(Path.of(_))
      text <- read(path)
      room <- Room.read(path.getFileName.toString.stripSuffix(".json"), text)
      followsAt <- f
        .get("follows-at")
        .fold(Right(StitchTuning.Default.followsAt))(chance("follows-at"))
      burst <- (f.get("burst"), f.get("burst-prior"), f.get("burst-gap")) match {
        case (None, None, None) => Right(StitchTuning.Default.burst)
        case (Some("none"), None, None) => Right(None)
        case (None, Some(p), Some(gap)) =>
          for {
            at <- chance("burst-prior")(p)
            seconds <- count("burst-gap")(gap)
          } yield Some(StitchTuning.Burst(at, seconds.seconds))
        case (Some(other), None, None) => Left(s"--burst takes none, not $other")
        case _ => Left("--burst-prior and --burst-gap go together, and not with --burst")
      }
      lexical <- f.get("lexical").fold(Right(StitchTuning.Default.lexical))(count("lexical"))
      recent <- f.get("recent").fold(Right(StitchTuning.Default.recent))(count("recent"))
      repeats <- f.get("repeats").fold(Right(1))(count("repeats"))
      tuning = StitchTuning.Default.copy(
        followsAt = followsAt,
        lexical = lexical,
        recent = recent,
        burst = burst
      )
      env <- DotEnv.loadDefault(sys.env)
      jev <- JevConfig.fromEnv(env).left.map(_.message)
      config <- DbConfig.fromEnv(env.updated(DbConfig.UrlVar, url)).left.map(_.message)
      budget <- Budget.of(cap, BigDecimal(0)).left.map(refused)
      runs <- withEngine(config, clock, "eval-synthetic", "the stitch lab's room was not run") {
        engine =>
          val classifier = JevClassifier(jev)
          val cache = Cache.at[Vector[Weights]](cacheDir)
          val each: Vector[(Against, Int)] =
            Vector(Against.Placings, Against.Truth).flatMap(a => (0 until repeats).map(a -> _))
          each.foldLeft[Either[String, (Vector[LabRun], Budget)]](
            Right((Vector.empty, budget))
          ) { (acc: Either[String, (Vector[LabRun], Budget)], ar: (Against, Int)) =>
            val (against, r) = ar
            acc.flatMap { (done: Vector[LabRun], left: Budget) =>
              StitchLab
                .run(
                  room,
                  engine,
                  classifier,
                  jev.model,
                  cache,
                  left,
                  tuning,
                  clock,
                  r,
                  s"${room.channel}-${against.ordinal}r$r",
                  against
                )
                .map { lab =>
                  val run = LabRun(
                    against,
                    s"${against.label}, repeat $r",
                    lab.placings,
                    RoomScore.of(room, lab.placings, lab.strands, against),
                    lab.rows.size,
                    lab.rows.count(_.cached),
                    lab.budget.spent - left.spent
                  )
                  (done :+ run, lab.budget)
                }
            }
          }
      }
      report = Report.lab(room, jev.model, tuning, runs._1)
      _ <- Try(Files.createDirectories(out)).toEither.left.map(e => s"$out: ${e.getClass.getName}")
      file = out.resolve(s"lab-stitch-${room.name}.md")
      _ <- write(file, report)
    } yield {
      report.linesIterator
        .takeWhile(l => !runs._1.exists(r => l == s"## ${r.label}"))
        .foreach(println)
      println(s"report: $file")
    }
}
