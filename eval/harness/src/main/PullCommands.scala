package grit.eval.harness.main

import java.nio.file.{Files, Path}
import java.time.{Instant, ZoneOffset}

import scala.util.Try

import grit.core.clock.Clock
import grit.core.either.Traversal
import grit.core.id.{QuestionName, ShadowName}
import grit.core.message.Cost
import grit.core.speech.Limits
import grit.core.spend.DailyCap
import grit.core.triage.Gate
import grit.dbos.sql.DbConfig
import grit.eval.harness.capture.Digest
import grit.eval.harness.jev.Sets
import grit.eval.harness.label.Verdicts
import grit.eval.harness.log.{Footer, Log, LogJson, Weights}
import grit.eval.harness.pull.{Pull, ShadowLog}
import grit.eval.harness.report.Report
import grit.eval.harness.score.GateSweep

import Cli.*
import CaseFiles.*

/** Live logs read back: `pull` and `bounds`. */
private[main] object PullCommands {

  /** `bounds --eval <dir> --capture <yyyymmdd> --url <jdbc> --since <instant> [--grid <file>]`:
    * the gate sweep ([[GateSweep.swept]]) over the capture's cases tagged since `<instant>`, each
    * as the database at `<url>` kept it ([[Pull.Pulled.heard]]), against the verdicts standing
    * on the messages a review considered since then ([[Pull.verdicts]]). The grid is the one
    * `<file>` holds ([[GateSweep.Grid.read]]), else each question set's draft gate under its name
    * ([[Sets.all]]); either under [[Limits.suggested]] at a $1 cap. Written to
    * `<dir>/reports/bounds-<yyyymmdd>.md` and printed; the database's login from
    * `GRIT_DATABASE_USER` and `_PASSWORD`. Asks no model.
    */
  private[main] def bounds(f: Map[String, String], clock: Clock^): Either[String, Unit] =
    for {
      dir <- need(f, "eval").map(Path.of(_))
      capture <- need(f, "capture").filterOrElse(_.matches("[0-9]{8}"), "--capture is yyyymmdd")
      at = dir.resolve("capture").resolve(capture)
      url <- need(f, "url")
      since <- need(f, "since").flatMap(a =>
        Try(Instant.parse(a)).toOption.toRight(s"--since $a: not an instant")
      )
      limits <- DailyCap.of("1").map(Limits.suggested(_, Gate.Open))
      grid <- f
        .get("grid")
        .fold(Right(GateSweep.Grid(Sets.all.map(s => s.name -> s.speak), limits)))(g =>
          read(Path.of(g)).flatMap(GateSweep.Grid.read(_, limits))
        )
      manifest <- read(at.resolve("capture.json"))
      casesText <- read(at.resolve("cases.jsonl"))
      cases <- readCases(at)
      config <- DbConfig.fromEnv(sys.env.updated(DbConfig.UrlVar, url)).left.map(_.message)
      got <- opened(config)(reader =>
        for {
          pulled <- Pull(
            reader,
            capture,
            Digest.text(manifest + casesText),
            cases,
            Vector.empty,
            since,
            clock.now()
          )
          standing <- Pull.verdicts(reader, since)
        } yield (pulled, standing)
      )
      (pulled, standing) = got
      swept = pulled.heard.toVector.map(GateSweep.Case(_, _))
      _ <- publish(
        dir,
        s"bounds-$capture",
        Report.bounds(
          capture,
          since,
          swept,
          swept.count(c => standing.verdicts.cases.contains(c.id)),
          GateSweep.swept(grid, swept, standing.verdicts)
        )
      )
    } yield ()

  /** `pull --capture <dir> --url <jdbc> --runs <dir> --shadows <name,…> [--since <instant>]`:
    * what the database kept of the heard messages tagged since `<instant>` (default: ever) and
    * the capture in `<dir>` holds, as run logs ([[Pull]]): `<runs>/live-<yyyymmdd>.jsonl`, live
    * triage's tags, and `<runs>/shadow-<name>-<yyyymmdd>.jsonl` for each shadow named, with
    * the database's login from `GRIT_DATABASE_USER` and `_PASSWORD`; and
    * `<runs>/verdicts-<yyyymmdd>.json`, the verdicts standing on the messages a review
    * considered since `<instant>` ([[Pull.verdicts]], [[Verdicts.written]]). Prints counts, and
    * the ids of the messages no capture holds yet, for the next capture. `<yyyymmdd>` is
    * `clock`'s day in UTC.
    */
  private[main] def pull(f: Map[String, String], clock: Clock^): Either[String, Unit] =
    for {
      dir <- need(f, "capture").map(Path.of(_))
      url <- need(f, "url")
      runs <- need(f, "runs").map(Path.of(_))
      names <- need(f, "shadows").flatMap(n =>
        Traversal.traverse(n.split(',').toVector.filter(_.nonEmpty))(x =>
          ShadowName.of(x).left.map(w => s"--shadows $x: $w")
        )
      )
      since <- f
        .get("since")
        .fold(Right(Instant.EPOCH))(a =>
          Try(Instant.parse(a)).toOption.toRight(s"--since $a: not an instant")
        )
      manifest <- read(dir.resolve("capture.json"))
      casesText <- read(dir.resolve("cases.jsonl"))
      cases <- readCases(dir)
      config <- DbConfig.fromEnv(sys.env.updated(DbConfig.UrlVar, url)).left.map(_.message)
      at = clock.now()
      got <- opened(config)(reader =>
        for {
          pulled <- Pull(
            reader,
            dir.getFileName.toString,
            Digest.text(manifest + casesText),
            cases,
            names,
            since,
            at
          )
          standing <- Pull.verdicts(reader, since)
        } yield (pulled, standing)
      )
      (pulled, standing) = got
      day = Day.format(at.atOffset(ZoneOffset.UTC))
      verdicts = s"verdicts-$day.json"
      logs = Pulled(s"live-$day.jsonl", lines(pulled.live)(using LogJson.named)) +:
        pulled.shadows.flatMap { (s: Pull.Pulled.Shadow) =>
          val name = s"shadow-${ShadowName.value(s.name)}-$day.jsonl"
          s.log match {
            case ShadowLog.Worded(log) => Some(Pulled(name, lines(log)))
            case ShadowLog.Named(log) => Some(Pulled(name, lines(log)(using LogJson.named)))
            case ShadowLog.Mixed(_, _) => None
          }
        }
      _ <- Try(Files.createDirectories(runs)).toEither.left.map(e =>
        s"$runs: ${e.getClass.getName}"
      )
      _ <- logs.foldLeft[Either[String, Unit]](Right(()))((done, p: Pulled) =>
        done.flatMap(_ => write(runs.resolve(p.name), p.lines))
      )
      _ <- write(runs.resolve(verdicts), Verdicts.written(standing.verdicts))
    } yield {
      def counted[A](log: Log[A]): String = {
        val footer = log.footer.getOrElse(Footer.of(BigDecimal(0), log.rows))
        s"${log.rows.size} rows: answered ${footer.answered}, failed ${footer.failed}; " +
          s"spent ${Cost.cents(footer.spent)}"
      }
      println(
        s"live (${Pull.Kept}): questions " +
          pulled.live.header.questions
            .fold("none answered")(_.map(QuestionName.value).mkString(", ")) +
          s"; ${counted(pulled.live)}; answered under other questions, left out: " +
          s"${pulled.renamed} (pull --since when live triage's question set changed); capture " +
          s"cases with no rebuilt question, left out: ${pulled.unbuilt}"
      )
      pulled.shadows.foreach { s =>
        val kept = s.log match {
          case ShadowLog.Worded(log) => counted(log)
          case ShadowLog.Named(log) =>
            s"a question set's, ${log.header.questions.fold(0)(_.size)} questions; ${counted(log)}"
          case ShadowLog.Mixed(worded, named) =>
            s"refused, no log: $worded of its rows are a wording's and $named a question " +
              "set's; a shadow's name asks one question for its life, so a changed one is " +
              "declared under a new name"
        }
        println(
          s"shadow ${ShadowName.value(s.name)}: $kept; tagged since with no row: ended " +
            s"keeping nothing ${s.ended}, not yet shadowed ${s.waiting}"
        )
      }
      println(s"not in the capture, for the next capture: ${pulled.uncaptured.size}")
      pulled.uncaptured.foreach(id => println(s"  ${id.written}"))
      println(
        s"verdicts standing: ${standing.verdicts.cases.size}; on a message no case id names, " +
          s"left out: ${standing.unnamed}"
      )
      println(s"written: ${(logs.map((p: Pulled) => p.name) :+ verdicts).mkString(", ")}")
    }

  /** A pulled log's file `name`, and its `lines`. */
  private[main] final case class Pulled(name: String, lines: String)
}
