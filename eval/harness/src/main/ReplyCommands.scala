package grit.eval.harness.main

import java.nio.file.{Files, Path}
import java.time.{Instant, ZoneOffset}

import scala.util.Try

import grit.core.clock.{Clock, Fresh}
import grit.core.either.Traversal
import grit.core.id.WorkflowId
import grit.dbos.sql.DbConfig
import grit.eval.harness.capture.CaptureJson
import grit.eval.harness.label.{ChurnQuestions, ReplyLabels, Verdicts}
import grit.eval.harness.reply.{Assembled, Refound, ReplyReview}
import grit.eval.harness.report.Report
import grit.eval.harness.score.Churn
import grit.kit.environment.DotEnv

import Cli.*
import CaseFiles.*

/** The commands over a capture's recorded turns: `replies`, `reply-labels`, `turns`, `churn`,
  * `recall`.
  */
private[main] object ReplyCommands {

  /** `replies --capture <dir> --url <jdbc> --out <dir> --labels <file> (--cases <id,...> |
    * --pick <n> [--by <order>, default random])`, with `records` for `--records` and `show`
    * for `--show`: a review of the capture's turns ([[ReplyReview]]), its text read from the
    * restored database, written to `<out>/replies-<capture>-<stamp>.md`, the stamp `clock`'s
    * now; a random order is seeded from `fresh`. A pick leaves out the turns `<file>`
    * (`reply-labels.json`; none when it does not exist) labels. Prints the file's path, and
    * each case's workflow and record count; the file's text only with `show`.
    */
  private[main] def replies(
      f: Map[String, String],
      records: Boolean,
      show: Boolean,
      clock: Clock^,
      fresh: Fresh^
  ): Either[String, Unit] =
    for {
      dir <- need(f, "capture").map(Path.of(_))
      url <- need(f, "url")
      out <- need(f, "out").map(Path.of(_))
      labelled <- need(f, "labels").map(Path.of(_)).flatMap(replyLabelsAt)
      turns <- readTurns(dir)
      ask <- (f.get("cases"), f.get("pick")) match {
        case (Some(ids), None) if !f.contains("by") =>
          Right(ReplyReview.Ask.Named(ids.split(",").toVector.filter(_.nonEmpty)))
        case (None, Some(n)) =>
          for {
            count <- n.toIntOption.toRight(s"--pick $n: not a whole number")
            by <- ReplyReview.By.named(f.getOrElse("by", "random"), fresh)
          } yield ReplyReview.Ask.Pick(count, by)
        case _ => Left("give --cases, or --pick with or without --by")
      }
      picked <- ReplyReview.pick(turns, labelled, ask)
      at = clock.now()
      capture = Option(dir.getFileName).fold("")(_.toString)
      config <- DbConfig.fromEnv(sys.env.updated(DbConfig.UrlVar, url)).left.map(_.message)
      review <- opened(config)(reader =>
        ReplyReview.review(capture, picked, at)(t => ReplyReview.read(reader, t, records))
      )
      path = out.resolve(s"replies-$capture-${Stamp.format(at.atOffset(ZoneOffset.UTC))}.md")
      _ <- Try(Files.createDirectories(out)).toEither.left.map(e => s"$out: ${e.getClass.getName}")
      text = ReplyReview.render(review)
      _ <- write(path, text)
    } yield {
      println(s"written: $path, ${review.cases.size} cases")
      review.cases.foreach(c =>
        println(s"  ${WorkflowId.value(c.turn.workflow)}: ${c.records.size} records")
      )
      if (show) print(text)
    }

  /** `reply-labels --file <review file> --labels <file>`: the labels the filled review file
    * holds ([[ReplyReview.labels]]) merged into `<file>` (`reply-labels.json`, created when it
    * does not exist), each in place of an earlier label of the same turn. Prints counts.
    */
  private[main] def replyLabels(f: Map[String, String]): Either[String, Unit] =
    for {
      file <- need(f, "file").map(Path.of(_))
      at <- need(f, "labels").map(Path.of(_))
      text <- read(file)
      filled <- ReplyReview.labels(text)
      before <- replyLabelsAt(at)
      after = before.merged(filled)
      _ <- write(at, ReplyLabels.written(after))
    } yield println(
      s"labels read: ${filled.size}, replacing ${filled.count((w, _) => before.turns.contains(w))}; " +
        s"turns labelled: ${after.turns.size}"
    )

  /** The reply labels at `path`; none when it does not exist. */
  private[main] def replyLabelsAt(path: Path): Either[String, ReplyLabels] =
    if (!Files.exists(path)) Right(ReplyLabels.Empty)
    else read(path).flatMap(ReplyLabels.read)

  /** `turns --eval <dir> --capture <yyyymmdd>`: the capture's recorded turns read structurally,
    * beside the verdicts standing in its `verdicts.json` (none when it has no such file),
    * written to `<dir>/reports/turns-<yyyymmdd>.md` and printed.
    */
  private[main] def turns(f: Map[String, String]): Either[String, Unit] =
    for {
      dir <- need(f, "eval").map(Path.of(_))
      capture <- need(f, "capture").filterOrElse(_.matches("[0-9]{8}"), "--capture is yyyymmdd")
      at = dir.resolve("capture").resolve(capture)
      captured <- readTurns(at)
      standing <- {
        val file = at.resolve("verdicts.json")
        if (!Files.exists(file)) Right(Verdicts.Empty) else read(file).flatMap(Verdicts.read)
      }
      _ <- publish(dir, s"turns-$capture", Report.turns(capture, captured, standing))
    } yield ()

  /** `churn --eval <dir> --capture <yyyymmdd> --questions <file> --before <round> --after
    * <round> [--url <jdbc>]`: the capture's recorded turns' churn over the questions `<file>`
    * holds ([[ChurnQuestions]]) asked in both rounds ([[Churn.of]]), written to
    * `<dir>/reports/churn-<yyyymmdd>-<before>-<after>.md` and printed. With `--url`, the restored database there,
    * each paired turn's reply is read from it and matched against its question's gold, only the
    * counts kept ([[Gold.matched]]); the database's login from `GRIT_DATABASE_USER` and
    * `_PASSWORD`. Refused as [[Churn.of]] refuses, or naming what could not be read.
    */
  private[main] def churn(f: Map[String, String]): Either[String, Unit] =
    for {
      dir <- need(f, "eval").map(Path.of(_))
      capture <- need(f, "capture").filterOrElse(_.matches("[0-9]{8}"), "--capture is yyyymmdd")
      captured <- readTurns(dir.resolve("capture").resolve(capture))
      questions <- need(f, "questions").map(Path.of(_)).flatMap(read).flatMap(ChurnQuestions.read)
      before <- need(f, "before")
      after <- need(f, "after")
      graded <- f.get("url").fold(Right(None)) { url =>
        // Every asking's turn with a reply, by its question's gold.
        val replied = questions.questions.toVector.flatMap((_, c) =>
          c.asked.values.toVector
            .flatMap(Churn.answering(captured, _))
            .filter(ReplyReview.reviewable)
            .map(c.gold -> _)
        )
        for {
          env <- DotEnv.loadDefault(sys.env)
          config <- DbConfig.fromEnv(env.updated(DbConfig.UrlVar, url)).left.map(_.message)
          matched <- opened(config) { reader =>
            Traversal.traverse(replied) { (gold, t) =>
              ReplyReview.read(reader, t, records = false).map { r =>
                // The reply's text is matched here and dropped: only the counts leave.
                val text = r.reply match {
                  case ReplyReview.Reply.Replied(text) => text
                  case ReplyReview.Reply.Draft(text, _) => text
                }
                t.workflow -> gold.matched(text)
              }
            }
          }
        } yield Some(matched.toMap)
      }
      measured <- Churn.of(captured, questions, before, after, graded)
      _ <- publish(dir, s"churn-$capture-$before-$after", Report.churn(capture, measured))
    } yield ()

  /** `recall --eval <dir> --capture <yyyymmdd> --url <jdbc> [--until <instant>]`: every turn the
    * restored database at `<jdbc>` recorded before its dump, and before `--until` when given,
    * its followed documents found again ([[Refound.of]]) over the database as it stood at the
    * dump, every current document read once ([[Refound.Current]]); written to
    * `<dir>/reports/recall-<yyyymmdd>.md` and printed ([[Report.recall]]). A turn that recorded no
    * search or followed nothing is left out. The database's login from `GRIT_DATABASE_USER` and
    * `_PASSWORD`. No model is asked.
    */
  private[main] def recall(f: Map[String, String]): Either[String, Unit] =
    for {
      dir <- need(f, "eval").map(Path.of(_))
      capture <- need(f, "capture").filterOrElse(_.matches("[0-9]{8}"), "--capture is yyyymmdd")
      url <- need(f, "url")
      manifest <- read(dir.resolve("capture").resolve(capture).resolve("capture.json")).flatMap(t =>
        Try(ujson.read(t)).toOption
          .toRight("capture.json: not JSON")
          .flatMap(CaptureJson.readManifest)
      )
      dumped = manifest.dump.at
      until <- f
        .get("until")
        .fold(Right(dumped))(v =>
          Try(Instant.parse(v)).toOption.toRight(s"--until $v: not an instant")
        )
      env <- DotEnv.loadDefault(sys.env)
      config <- DbConfig.fromEnv(env.updated(DbConfig.UrlVar, url)).left.map(_.message)
      replayed <- opened(config) { reader =>
        for {
          workflows <- reader.turns(dumped).left.map(_ => "turn workflows unread")
          current <- reader.all
            .read(reader.current())
            .left
            .map(e => s"the current documents unread: ${e.kind}")
            .map(Refound.Current.of)
        } yield workflows
          .filter(_._2.created.isBefore(until))
          .flatMap((w, _) =>
            Refound.of(reader, w, Assembled.Shipped, dumped, current) match {
              case Right(None) => None
              case Right(Some(fs)) => Some(w -> Right(fs))
              case Left(why) => Some(w -> Left(why))
            }
          )
      }
      _ <- publish(dir, s"recall-$capture", Report.recall(capture, replayed))
    } yield ()
}
