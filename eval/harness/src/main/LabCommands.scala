package grit.eval.harness.main

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.concurrent.duration.DurationInt
import scala.util.Try

import grit.core.clock.Clock
import grit.core.host.RelPath
import grit.core.stitch.Tuning as StitchTuning
import grit.core.visibility.{Label, Subject}
import grit.dbos.internal.BenchRows
import grit.dbos.sql.DbConfig
import grit.eval.Room
import grit.eval.harness.bench.{ApiKey, Arm, Bench, Caps, Rig, Task}
import grit.eval.harness.jev.Budget
import grit.eval.harness.lab.StitchLab
import grit.eval.harness.log.{Cache, Weights}
import grit.eval.harness.report.{LabRun, Report}
import grit.eval.harness.score.{Against, RoomScore}
import grit.host.blobs.LocalBlobs
import grit.host.checkout.LocalProcesses
import grit.kit.environment.DotEnv
import grit.models.JevClassifier
import grit.models.JevConfig

import Cli.*
import Engines.*

/** The lab's runs: `bench` and `lab-stitch`. */
private[main] object LabCommands {

  /** `bench --tasks <dir> --task <name> --repository <path> --work <dir>`, then either `--known
    * true`, or `--model <id> --max-budget-usd <usd> --max-turns <n> --claude <program> --scout
    * <program> --agent <file> --url <jdbc> --blobs <dir>`: the task `<name>` of `<dir>`
    * ([[Task.of]], its `task.json` and `brief.md`), in a checkout under `<work>` named for it and
    * `clock`'s now, of the repository at `<path>`. With `--known`, its own commits through its
    * checks ([[Rig.known]]); otherwise a coder run under the caps ([[Rig.trial]]: the agent's
    * prompt the file `<file>`, the key `ANTHROPIC_API_KEY` from the environment over
    * `GRIT_ENV_FILE`), kept with its transcript in the database at `<jdbc>` and the blobs at
    * `<blobs>` ([[Bench.keep]]). Prints what [[Tried.lines]] gives, or each check; never the key.
    */
  private[main] def bench(f: Map[String, String], clock: Clock^): Either[String, Unit] =
    for {
      name <- need(f, "task")
      taskName <- BenchRows.TaskName.of(name)
      at <- need(f, "tasks").map(Path.of(_).resolve(name))
      task <- read(at.resolve("task.json")).flatMap(json =>
        read(at.resolve("brief.md")).flatMap(Task.of(taskName, json, _))
      )
      repository <- need(f, "repository")
      root <- need(f, "work").map(Path.of(_))
      _ <- Try(Files.createDirectories(root)).toEither.left.map(e => s"no work directory: $e")
      work = new LocalProcesses(root, sys.env)
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
            env <- DotEnv.load(Path.of(sys.env.getOrElse("GRIT_ENV_FILE", ".env")), sys.env)
            key <- ApiKey.of(env.getOrElse("ANTHROPIC_API_KEY", ""))
            arm <-
              for {
                claude <- need(f, "claude")
                scout <- need(f, "scout")
                agent <- need(f, "agent").flatMap(a => read(Path.of(a)))
              } yield Arm(claude, scout, agent)
            url <- need(f, "url")
            config <- DbConfig.fromEnv(env.updated(DbConfig.UrlVar, url)).left.map(_.message)
            blobsAt <- need(f, "blobs").map(Path.of(_))
            _ <- LocalBlobs.open(blobsAt) match {
              case Left(e) => Left(s"the bench's blobs did not open: $e")
              case Right(blobs) =>
                withEngine(config, clock, "eval-bench", "the trial was not kept") { engine =>
                  for {
                    tried <- Rig.trial(work, clock)(repository, task, arm, cap, key, dir, Rig.Quiet)
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
    } yield ()

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
    * come from the environment over `GRIT_ENV_FILE` (`.env` when unset); each call is timed by
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
      env <- DotEnv.load(Path.of(sys.env.getOrElse("GRIT_ENV_FILE", ".env")), sys.env)
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
