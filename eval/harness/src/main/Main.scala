package grit.eval.harness.main

import grit.core.clock.{Clock, Fresh}
import grit.kit.environment.Exit

import Cli.*
import CaptureCommands.*
import ReplyCommands.*
import RebuildCommands.*
import RecipeCommands.*
import LabCommands.*
import RunCommands.*
import ScoreCommands.*
import PullCommands.*
import IntegrityCommands.*

/** The eval harness's command line, run through `scripts/eval`, which says what each command
  * takes. It prints counts, ids and field names, never a message's text.
  */
object Main {

  def main(args: Array[String]): Unit = {
    val clock: Clock^ = Clock.system() // clock-check: the eval harness's composition root
    val exit = Exit.process("eval:", 1)
    args.toList match {
      case "capture" :: rest => exit(flags(rest).flatMap(capture))
      case "run" :: rest =>
        exit(
          flags(rest.filterNot(_ == "--no-cache"))
            .flatMap(run(_, !rest.contains("--no-cache"), clock))
        )
      case "determinism" :: rest => exit(flags(rest).flatMap(determinism))
      case "inputs" :: rest =>
        exit(flags(rest.filterNot(_ == "--more")).flatMap(inputs(_, rest.contains("--more"))))
      case "context" :: rest =>
        exit(flags(rest.filterNot(_ == "--full")).flatMap(context(_, rest.contains("--full"))))
      case "score" :: rest => exit(flags(rest).flatMap(score))
      case "order" :: rest => exit(flags(rest).flatMap(order(_, clock)))
      case "pull" :: rest => exit(flags(rest).flatMap(pull(_, clock)))
      case "bounds" :: rest => exit(flags(rest).flatMap(bounds(_, clock)))
      case "replies" :: rest =>
        val switches = Set("--records", "--show")
        exit(
          flags(rest.filterNot(switches)).flatMap(
            replies(
              _,
              rest.contains("--records"),
              rest.contains("--show"),
              clock,
              Fresh.random()
            )
          )
        )
      case "reply-labels" :: rest => exit(flags(rest).flatMap(replyLabels))
      case "turns" :: rest => exit(flags(rest).flatMap(turns))
      case "churn" :: rest => exit(flags(rest).flatMap(churn))
      case "recall" :: rest => exit(flags(rest).flatMap(recall))
      case "rebuild" :: rest => exit(flags(rest).flatMap(rebuild(_, clock)))
      case "duplication" :: rest => exit(flags(rest).flatMap(duplication(_, clock)))
      case "summaries" :: rest => exit(flags(rest).flatMap(summaries(_, clock)))
      case "recipes" :: rest => exit(flags(rest).flatMap(recipes(_, clock)))
      case "lab-stitch" :: rest => exit(flags(rest).flatMap(labStitch(_, clock)))
      case "synthetic" :: rest => exit(flags(rest).flatMap(synthetic(_, clock)))
      case "integrity-loss" :: rest => exit(flags(rest).flatMap(integrityLoss(_, clock)))
      case "bench" :: rest => exit(flags(rest).flatMap(bench(_, clock)))
      case "compare" :: rest =>
        flags(rest).flatMap(f =>
          if (f.contains("gate")) drafts(f, clock).map(_ => 0) else compare(f)
        ) match {
          case Right(0) => ()
          case Right(code) => sys.exit(code)
          case Left(why) => exit.refuse(why)
        }
      case _ =>
        exit(
          Left(
            "usage: scripts/eval capture|run|determinism|inputs|score|compare|order|pull|" +
              "replies|reply-labels|turns|churn|recall|rebuild|recipes|synthetic|lab-stitch|" +
              "integrity-loss|bench (scripts/eval and scripts/integrity-loss say what each takes)"
          )
        )
    }
  }
}
