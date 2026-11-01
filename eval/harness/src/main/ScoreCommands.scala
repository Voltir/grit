package grit.eval.harness.main

import java.nio.file.{Files, Path}
import java.time.ZoneOffset

import scala.collection.immutable.VectorMap
import scala.util.Try
import scala.util.chaining.*

import grit.core.classify.Answer
import grit.core.clock.Clock
import grit.core.id.QuestionName
import grit.core.triage.Corpora
import grit.eval.harness.capture.{CaseId, Digest}
import grit.eval.harness.jev.{QuestionSet, Sets}
import grit.eval.harness.label.{Labels, Verdicts}
import grit.eval.harness.log.{Log, LogJson, Weights}
import grit.eval.harness.report.{Report, Scored}
import grit.eval.harness.score.{
  Decision,
  Drafting,
  Drafts,
  Guarded,
  Judgement,
  MovedOn,
  Order,
  Paired,
  Rule,
  Scoring,
  Target
}
import grit.lifecycle.triage.TriageQuestions

import Cli.*
import CaseFiles.*

/** Scoring Jev's logs: `score`, `compare` (and its drafts gate), `order`. */
private[main] object ScoreCommands {

  /** `score --eval <dir> --run <name> [--labels <file>]`: the run `<dir>/runs/<name>` scored
    * against its capture (`<dir>/capture/<its capture>`) and the labels in `<file>` (default
    * `<dir>/labels.json`; none when it does not exist), written to `<dir>/reports/<name less
    * .jsonl>.md` and printed.
    */
  private[main] def score(f: Map[String, String]): Either[String, Unit] =
    for {
      dir <- need(f, "eval").map(Path.of(_))
      labels <- labelsAt(dir, f)
      run <- need(f, "run").flatMap(scored(dir, _, labels))
      _ <- publish(dir, run.name.stripSuffix(".jsonl"), Report.score(run))
    } yield ()

  /** `compare --eval <dir> --a <name> --b <name> [--labels <file>] [--decide <rule file>]
    * [--replica <name> --spread <name>]`: run B against run A, both in `<dir>/runs/`, written
    * to `<dir>/reports/<A>-vs-<B>.md` (each name less `.jsonl`) and printed; the labels as for
    * `score`. With `--replica` (a pulled replica shadow's log) and `--spread` (a run with
    * repeats), A being live triage's kept log: the cases whose state moved on ([[MovedOn]])
    * left out of both, under the noise measured in the `--spread` run. With `--decide`, what
    * the rule makes of them too, and the exit code says it: 0 adopt B, 3 refused, 4 keep A.
    */
  private[main] def compare(f: Map[String, String]): Either[String, Int] =
    for {
      dir <- need(f, "eval").map(Path.of(_))
      labels <- labelsAt(dir, f)
      runA <- need(f, "a").flatMap(scored(dir, _, labels))
      runB <- need(f, "b").flatMap(scored(dir, _, labels))
      replica <- f.get("replica").fold(Right(None))(n => scored(dir, n, labels).map(Some(_)))
      noise <- f
        .get("spread")
        .fold(Right(None))(n =>
          scored(dir, n, labels).flatMap(s =>
            MovedOn.noise(s.log.rows).map(Some(_)).toRight(s"--spread $n: no case answered twice")
          )
        )
      movedOn <- (replica, noise) match {
        case (Some(r), Some(n)) => Right(Some(MovedOn.of(r.answers, runA.answers, n)))
        case (None, None) => Right(None)
        case _ => Left("--replica and --spread are given together")
      }
      gone = movedOn.fold(Set.empty[CaseId])(_.cases.toSet)
      (a, b) = (runA.without(gone), runB.without(gone))
      rule <- f
        .get("decide")
        .fold(Right(None))(r => read(Path.of(r)).flatMap(Rule.read).map(Some(_)))
      decided = rule.map { r =>
        val (sa, sb) =
          (Scoring(a.cases, a.answers, labels), Scoring(b.cases, b.answers, labels))
        r -> Decision.of(r, b.log.header, Paired.of(r, sa, sb), Guarded.of(r, sa, sb))
      }
      _ <- publish(
        dir,
        s"${a.name.stripSuffix(".jsonl")}-vs-${b.name.stripSuffix(".jsonl")}",
        Report.compare(runA, runB, decided, movedOn)
      )
    } yield decided.fold(0)(_._2 match {
      case Decision.Adopted(_) => 0
      case Decision.Refused(_) => 3
      case Decision.Kept(_) => 4
    })

  /** `compare --eval <dir> --a <name> --b <name> --live <set> --gate <set> [--verdicts <name>]`:
    * run B, a pulled question set's shadow log asking the `--gate` set ([[Sets]]), against A,
    * live triage's pulled log asking the `--live` set, both in `<dir>/runs/` (B may be A
    * itself, to read live's answers under another set's gate): each one's draft
    * by its own set's gate, by focus ([[Drafts]]), written to `<dir>/reports/<A>-vs-<B>.md`
    * (each name less `.jsonl`) and printed; the ids of the cases each decided alone, one a
    * line, to `<dir>/order/<yyyymmdd>-draft-<set>-live-only.txt` and `…-set-only.txt`, for
    * labelling. A live log an earlier build pulled by position is read under v1's names
    * ([[Log.named]]). With `--verdicts <name>`, a verdicts file `pull` wrote in `<dir>/runs/`
    * ([[Verdicts]]), each pick reason's verdicts against both drafts and each side's `to`
    * ([[Judgement]]); without it, the report says no verdicts were given. `--helps-at`,
    * `--decide`, `--replica` and `--spread` are refused beside it. `<yyyymmdd>` is `clock`'s
    * day in UTC.
    */
  private[main] def drafts(f: Map[String, String], clock: Clock^): Either[String, Unit] =
    for {
      _ <- Vector("decide", "replica", "spread")
        .find(f.contains)
        .map(k => s"--$k is not taken with --gate")
        .toLeft(())
      _ <- Either.cond(
        !f.contains("helps-at"),
        (),
        "--helps-at is gone: name the set live triage asked with --live (its gate is the set's)"
      )
      dir <- need(f, "eval").map(Path.of(_))
      liveSet <- need(f, "live").flatMap(setNamed)
      set <- need(f, "gate").flatMap(setNamed)
      aName <- need(f, "a")
      a <- liveLog(dir, aName)
      bName <- need(f, "b")
      bText <- read(dir.resolve("runs").resolve(bName))
      b <- LogJson
        .read[VectorMap[QuestionName, Answer]](bText.linesIterator.filter(_.nonEmpty).toVector)(
          using LogJson.named
        )
        .left
        .map(e => s"$bName: not a question set's log: $e")
      (live, shadow) = (drafting(a, liveSet), drafting(b, set))
      found = Drafts.of(live, shadow)
      verdicts <- f
        .get("verdicts")
        .fold[Either[String, Option[Verdicts]]](Right(None))(n =>
          read(dir.resolve("runs").resolve(n)).flatMap(Verdicts.read).map(Some(_))
        )
      judged = verdicts.map(Judgement.of(live, shadow, _))
      day = Day.format(clock.now().atOffset(ZoneOffset.UTC))
      alone = Vector(
        s"$day-draft-${set.name}-live-only.txt" -> found.gate.values.toVector.flatMap(_.aOnly),
        s"$day-draft-${set.name}-set-only.txt" -> found.gate.values.toVector
          .flatMap(_.bOnly)
      )
      order = dir.resolve("order")
      _ <- Try(Files.createDirectories(order)).toEither.left.map(e =>
        s"$order: ${e.getClass.getName}"
      )
      _ <- alone.foldLeft[Either[String, Unit]](Right(()))((done, named) =>
        done.flatMap(_ =>
          write(order.resolve(named._1), named._2.sorted.map(_.written + "\n").mkString)
        )
      )
      _ <- publish(
        dir,
        s"${aName.stripSuffix(".jsonl")}-vs-${bName.stripSuffix(".jsonl")}",
        Report.drafts(
          Report.Side(aName, a, liveSet.name, live.gate),
          Report.Side(bName, b, set.name, shadow.gate),
          found,
          judged
        )
      )
    } yield alone.foreach((name, ids) => println(s"order: ${ids.size} cases to order/$name"))

  /** The question set named `n`; why not, naming those there are. */
  private[main] def setNamed(n: String): Either[String, QuestionSet] =
    Sets.named(n).toRight(s"no set $n: ${Sets.all.map(_.name).mkString(", ")}")

  /** `log`'s rows as a side of a draft comparison asking `set`. */
  private[main] def drafting(
      log: Log[VectorMap[QuestionName, Answer]],
      set: QuestionSet
  ): Drafting =
    Drafting(log.rows, set.speak, set.durable, set.to)

  /** Live triage's pulled log `<dir>/runs/<name>`, under its questions' names; one an earlier
    * build pulled by position, under v1's.
    */
  private[main] def liveLog(
      dir: Path,
      name: String
  ): Either[String, Log[VectorMap[QuestionName, Answer]]] =
    read(dir.resolve("runs").resolve(name)).flatMap { text =>
      val lines = text.linesIterator.filter(_.nonEmpty).toVector
      LogJson
        .read[VectorMap[QuestionName, Answer]](lines)(using LogJson.named)
        .orElse(
          LogJson
            .read[Vector[Weights]](lines)
            .map(Log.named(_, TriageQuestions.V1.questions(Corpora.Empty)))
        )
        .left
        .map(e => s"$name: not live triage's log, under names or by position: $e")
    }

  /** `order --eval <dir> --a <name> [--b <name>] [--tag <question>] [--by spread|difference]`:
    * the case ids in labelling order ([[Order]]) on `<question>` (`kind`, a tag's name, or
    * `place`; default `durable`): by the difference between runs A and B (the default when `--b`
    * is given), or by run A's repeat spread (the default without), written one a line to
    * `<dir>/order/<yyyymmdd>-<question>-<by>.txt`, `<yyyymmdd>` `clock`'s day in UTC. Prints
    * how many, and how many unlabelled lead.
    */
  private[main] def order(f: Map[String, String], clock: Clock^): Either[String, Unit] =
    for {
      dir <- need(f, "eval").map(Path.of(_))
      labels <- labelsAt(dir, f)
      q <- f
        .getOrElse("tag", "durable")
        .pipe(t => Target.read(t).toRight(s"--tag $t: not kind, place or a tag"))
      a <- need(f, "a").flatMap(scored(dir, _, labels))
      b <- f.get("b").fold(Right(None))(n => scored(dir, n, labels).map(Some(_)))
      how = f.getOrElse("by", if (b.isDefined) "difference" else "spread")
      by <- how match {
        case "spread" => Right(Order.spread(q, a.cases, a.answers, labels))
        case "difference" =>
          b.toRight("--by difference needs --b")
            .map(b => Order.difference(q, a.answers, b.answers, labels))
        case other => Left(s"--by $other: not spread or difference")
      }
      name = s"${Day.format(clock.now().atOffset(ZoneOffset.UTC))}-" +
        s"${Target.written(q)}-$how.txt"
      out = dir.resolve("order").resolve(name)
      _ <- Try(Files.createDirectories(out.getParent)).toEither.left.map(e =>
        s"${out.getParent}: ${e.getClass.getName}"
      )
      _ <- write(out, by.map(_.written + "\n").mkString)
    } yield println(
      s"order: ${by.size} cases on ${Target.written(q)}, " +
        s"${by.count(id => !Target.labelled(q, labels.of(id)))} not yet labelled on it first; " +
        s"written to order/$name"
    )

  /** The labels `--labels` names, else `<dir>/labels.json`; none when that file does not exist. */
  private[main] def labelsAt(dir: Path, f: Map[String, String]): Either[String, Labels] = {
    val file = f.get("labels").fold(dir.resolve("labels.json"))(Path.of(_))
    if (Files.isRegularFile(file)) read(file).flatMap(Labels.read) else Right(Labels.Empty)
  }

  /** The run `<dir>/runs/<name>`, with its capture's cases and `labels`; a capture whose files
    * changed since the run is reported, not refused. A log of answers under their names, as
    * pull writes live triage's and a question set's shadow's, is refused, naming `--live`.
    */
  private[main] def scored(dir: Path, name: String, labels: Labels): Either[String, Scored] =
    for {
      text <- read(dir.resolve("runs").resolve(name))
      lines = text.linesIterator.filter(_.nonEmpty).toVector
      log <- LogJson
        .read[Vector[Weights]](lines)
        .left
        .map(why =>
          LogJson
            .read[VectorMap[QuestionName, Answer]](lines)(using LogJson.named)
            .fold(
              _ => s"$name: $why",
              _ =>
                s"$name: a question set's answers under their names (live triage's or a " +
                  "shadow's, as pull writes them): compare it with --live and --gate"
            )
        )
      capture = dir.resolve("capture").resolve(log.header.capture)
      manifest <- read(capture.resolve("capture.json"))
      casesText <- read(capture.resolve("cases.jsonl"))
      cases <- readCases(capture)
    } yield {
      if (Digest.text(manifest + casesText) != log.header.captureDigest)
        println(s"$name: its capture ${log.header.capture} changed since it ran")
      Scored(name, log, cases, labels)
    }
}
