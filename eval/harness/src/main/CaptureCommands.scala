package grit.eval.harness.main

import java.nio.file.Path
import java.time.Instant

import scala.util.Try

import grit.dbos.engine.Build
import grit.dbos.sql.DbConfig
import grit.eval.harness.capture.{
  Capture,
  CaptureJson,
  Case,
  Digest,
  Dump,
  Ended,
  Live,
  Manifest,
  Said,
  SeenCheck,
  Stitched,
  TurnCapture,
  TurnCase,
  TurnJson,
  Turns
}
import grit.eval.harness.label.Verdicts
import grit.eval.harness.pull.Pull

import Cli.*

/** `capture`: a database's turns and heard messages written out as an eval's cases. */
private[main] object CaptureCommands {

  /** `capture --url <jdbc> --source <db> --restored <db> --sha256 <hex> --at <instant> --out
    * <dir>`: the capture of the restored database, written to `capture.json` and `cases.jsonl` in
    * `<dir>`, its recorded turns to `turns.jsonl` ([[TurnCapture]]) and the verdicts standing
    * on reviewed messages to `verdicts.json` ([[Pull.verdicts]]), with the database's login
    * from `GRIT_DATABASE_USER` and `_PASSWORD`.
    */
  private[main] def capture(f: Map[String, String]): Either[String, Unit] =
    for {
      url <- need(f, "url")
      source <- need(f, "source")
      restored <- need(f, "restored")
      sha <- need(f, "sha256").flatMap(Digest.read)
      at <- need(f, "at").flatMap(a => Try(Instant.parse(a)).toOption.toRight(s"--at $a"))
      out <- need(f, "out").map(Path.of(_))
      config <- DbConfig
        .fromEnv(sys.env.updated(DbConfig.UrlVar, url))
        .left
        .map(_.message)
      read <- opened(config)(reader =>
        for {
          captured <- Capture(reader, source, restored, Dump(sha, at), Build.current)
          turns <- TurnCapture(reader, Dump(sha, at))
          standing <- Pull.verdicts(reader, Instant.EPOCH)
        } yield (captured, turns, standing)
      )
      (captured, turns, standing) = read
      _ <- write(
        out.resolve("cases.jsonl"),
        captured.cases.map(CaptureJson.writeCase(_).render() + "\n").mkString
      )
      _ <- write(
        out.resolve("turns.jsonl"),
        turns.cases.map(TurnJson.write(_).render() + "\n").mkString
      )
      _ <- write(out.resolve("verdicts.json"), Verdicts.written(standing.verdicts))
      _ <- write(
        out.resolve("capture.json"),
        CaptureJson.writeManifest(captured.manifest).render(2) + "\n"
      )
    } yield {
      report(captured.manifest, captured.cases)
      reportTurns(turns)
      println(s"verdicts standing: ${standing.verdicts.cases.size}")
    }

  /** The turns captured, as counts: by where their message was said and their root, by how
    * they ended, and how many were skipped.
    */
  private[main] def reportTurns(turns: Turns): Unit = {
    def said(t: TurnCase) = t.said match {
      case Said.Slack(_) => "slack"
      case Said.Tui(_) => "tui"
      case Said.Task(_) => "task"
    }
    def ended(t: TurnCase) = t.ended match {
      case Ended.Replied(_, true) => "passed"
      case Ended.Replied(_, false) => "replied"
      case Ended.Failed(step, why) => s"failed ($step, ${why.toString.toLowerCase})"
      case Ended.Unfinished(status) => s"unfinished (${status.toLowerCase})"
    }
    def counts(f: TurnCase => String) = {
      val keys = turns.cases.map(f)
      keys.distinct.sorted.map(k => s"$k ${keys.count(_ == k)}").mkString(", ")
    }
    println(s"turns: ${turns.cases.size}, skipped ${turns.skipped}")
    println(s"  by root: ${counts(t => s"${said(t)} ${t.root.toString.toLowerCase}")}")
    println(s"  ended: ${counts(ended)}")
    println(s"  with a tool loop: ${turns.cases.count(_.rounds.nonEmpty)}")
  }

  private[main] def report(m: Manifest, cases: Vector[Case]): Unit = {
    val stitches = cases.flatMap(_.stitch)
    val seen = stitches.map(_.seen)
    def count(kind: String)(f: SeenCheck => Boolean) = s"$kind ${seen.count(f)}"
    println(
      "seen check: " + Vector(
        count("match")(_ == SeenCheck.Match),
        count("lexical only")(_.isInstanceOf[SeenCheck.LexicalOnly]),
        count("recent differs")(_.isInstanceOf[SeenCheck.RecentDiffers]),
        count("same root differs")(_.isInstanceOf[SeenCheck.SameRootDiffers]),
        count("message differs")(_.isInstanceOf[SeenCheck.MessageDiffers]),
        count("unbuilt")(_ == SeenCheck.Unbuilt)
      ).mkString(", ")
    )
    // A difference the rebuild should never show, by case id and slot: ids and times only.
    cases.foreach { c =>
      c.stitch.map(_.seen).foreach {
        case SeenCheck.RecentDiffers(ranks) =>
          println(
            s"  recent differs: ${c.id.written} tagged ${c.tagged}, ranks ${ranks.mkString(" ")}"
          )
        case SeenCheck.SameRootDiffers(roots, fields) =>
          println(
            s"  same root differs: ${c.id.written} tagged ${c.tagged}, roots " +
              s"${roots.map(_.written).mkString(" ")} in ${fields.map(SeenCheck.Field.written).mkString(" ")}"
          )
        case SeenCheck.MessageDiffers(fields) =>
          println(
            s"  message differs: ${c.id.written} in ${fields.map(SeenCheck.Field.written).mkString(" ")}"
          )
        case _ => ()
      }
    }
    println(
      s"lexical drift: ${stitches.count(_.drift > 0)} cases, " +
        s"${stitches.map(_.drift).sum} exchanges over ${Stitched.Tolerance}"
    )
    println(
      s"build known: ${cases.count(_.triage.build != Build.Unknown)}, " +
        s"unknown: ${cases.count(_.triage.build == Build.Unknown)}"
    )
    println(
      s"tags: v1's ${cases.count(_.tags.isInstanceOf[Live.Weighed])}, a question set's " +
        s"${cases.count(_.tags.isInstanceOf[Live.Named])}, unanswered " +
        s"${cases.count(_.tags.isInstanceOf[Live.Unanswered])}"
    )
    println(s"tunings: ${m.tunings}")
  }
}
