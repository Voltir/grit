package grit.eval.harness.main

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*
import scala.util.Try

import grit.core.clock.Clock
import grit.core.host.{Blobs, RelPath}
import grit.core.visibility.Subject
import grit.dbos.internal.TrialRows
import grit.dbos.sql.DbConfig
import grit.eval.harness.bench.{Bench, BenchTask, Standing}
import grit.eval.harness.evidence.{BlindTask, EvidenceCheckout, Proposal, Trajectory}
import grit.host.blobs.LocalBlobs
import grit.host.checkout.{LocalProcesses, Redaction}

import Cli.*

/** The proposer's evidence: `propose`. */
private[main] object ProposeCommands {

  /** The most checkouts kept under `--out`: each `propose` removes all but the newest. */
  val Kept: Int = 3

  /** `propose --trials <id>,… --standing <file> --against <n> --brief <file> --amendments <dir> --tasks <dir>
    * --repository <path> --url <jdbc> --blobs <dir> --out <dir>`: the evidence checkout of the
    * bench's trials whose sessions' local ids are `<id>`s ([[Proposal.chosen]]: one task's, each
    * begun with the standing text in `<file>`), their transcripts read for the public from the
    * database at `<jdbc>` and the blobs at `<dir>` ([[Bench.trajectory]]), with the brief in
    * `--brief`'s file and each `*.md` in `--amendments`, in order of name, its key its name without
    * `.md` ([[Proposal.of]]), written under `<out>` named for `clock`'s now
    * ([[EvidenceCheckout.write]]), then every checkout there but the newest [[Kept]] removed.
    * Prints each trajectory's size as shown and how many turns it renders ([[Trajectory.turns]]):
    * one with none holds its run's own lines alone. Refused, writing nothing, when a file holds a grade shape, naming the amendment by its key
    * or the file by its path; or when a trial has no transcript. Prints the checkout's path and
    * its said file's.
    */
  private[main] def propose(f: Map[String, String], clock: Clock^): Either[String, Unit] =
    for {
      named <- need(f, "trials").map(_.split(",").toVector.map(_.trim).filter(_.nonEmpty))
      standing <- need(f, "standing").flatMap(s => Standing.read(Path.of(s)))
      against <- need(f, "against").flatMap(Proposal.Against.of)
      brief <- need(f, "brief").flatMap(b => read(Path.of(b)))
      amendments <- need(f, "amendments").flatMap(d => amendmentsIn(Path.of(d)))
      url <- need(f, "url")
      config <- DbConfig.fromEnv(sys.env.updated(DbConfig.UrlVar, url)).left.map(_.message)
      blobsAt <- need(f, "blobs").map(Path.of(_))
      _ <- LocalBlobs.open(blobsAt) match {
        case Left(e) => Left(s"the bench's blobs did not open: $e")
        case Right(blobs) =>
          checkout(f, clock, config, blobs)(named, standing, against, brief, amendments)
      }
    } yield ()

  /** [[propose]]'s checkout, its trials' transcripts read from `blobs`. */
  private def checkout(f: Map[String, String], clock: Clock^, config: DbConfig, blobs: Blobs^)(
      named: Vector[String],
      standing: Standing,
      against: Proposal.Against,
      brief: String,
      amendments: Vector[(String, String)]
  ): Either[String, Unit] =
    for {
      read <- opened[(TrialRows.TaskName, Vector[Proposal.Run])](config) { reader =>
        for {
          kept <- Bench.trials(reader)(Subject.Public)
          trials <- Proposal.chosen(kept, named, standing)
          runs <- trials.foldLeft[Either[String, Vector[Proposal.Run]]](Right(Vector.empty)) {
            (done, t) =>
              done.flatMap(runs =>
                Bench
                  .trajectory(reader, blobs)(Subject.Public, t.session)
                  .flatMap(_.toRight(s"trial ${t.session.local} has no transcript the bench keeps"))
                  .map(kept => runs :+ Proposal.Run(t.session, kept))
              )
          }
          task <- trials.headOption.map(_.task).toRight("no trial is named")
        } yield (task, runs)
      }
      task <- blind(f, read._1)
      shown <- Proposal.of(task, read._2, standing, against, amendments.map(_._2), brief).left.map {
        found =>
          val keys = amendments.zipWithIndex.map((a, i) => Proposal.amendment(i + 1) -> a._1).toMap
          found
            .map(g =>
              Proposal.graded(
                keys.get(g.path).fold(s"the file ${RelPath.value(g.path)}")(k => s"amendment $k"),
                g
              )
            )
            .mkString("; ")
      }
      repository <- need(f, "repository")
      out <- need(f, "out").map(Path.of(_))
      _ <- mkdirs(out)
      dir = EvidenceCheckout.named(clock.now())
      work = new LocalProcesses(out, sys.env, Redaction.of(sys.env))
      _ <- EvidenceCheckout.write(work)(repository, task, shown, dir)
      _ <- EvidenceCheckout.pruned(out)(Kept)
    } yield {
      val at = out.resolve(RelPath.value(dir))
      println(s"the evidence checkout: $at")
      read._2.zipWithIndex.foreach { (run, i) =>
        val file = Proposal.trajectory(i + 1)
        val bytes = shown.files
          .collectFirst {
            case (p, text) if p == file => text.getBytes(StandardCharsets.UTF_8).length
          }
          .getOrElse(0)
        println(
          f"${RelPath.value(file)}: session ${run.session.local}, $bytes%,d bytes shown, " +
            s"${Trajectory.turns(run.kept.text)} turns rendered"
        )
      }
      println(
        s"its said file: ${at.resolve(EvidenceCheckout.Evidence).resolve(RelPath.value(Proposal.Said))}"
      )
    }

  /** Each `*.md` file in `dir`, in order of name: its name without `.md`, and its text. */
  private def amendmentsIn(dir: Path): Either[String, Vector[(String, String)]] =
    Try(Files.list(dir)).toEither.left
      .map(e => s"the amendments' directory $dir could not be read: ${e.getClass.getName}")
      .flatMap { listing =>
        val files =
          try listing.iterator.asScala.toVector
          finally listing.close()
        files
          .map(_.getFileName.toString)
          .filter(_.endsWith(".md"))
          .sorted
          .foldLeft[Either[String, Vector[(String, String)]]](Right(Vector.empty))((done, n) =>
            done.flatMap(got =>
              read(dir.resolve(n)).map(text => got :+ (n.stripSuffix(".md") -> text))
            )
          )
      }

  /** The frozen task `name` under `--tasks`, blind ([[BenchTask.of]], [[BlindTask.of]]). */
  private def blind(f: Map[String, String], name: TrialRows.TaskName): Either[String, BlindTask] =
    for {
      at <- need(f, "tasks").map(Path.of(_).resolve(TrialRows.TaskName.value(name)))
      json <- read(at.resolve("task.json"))
      brief <- read(at.resolve("brief.md"))
      task <- BenchTask.of(name, json, brief)
    } yield BlindTask.of(task)
}
