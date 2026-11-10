package grit.eval.harness.main

import grit.core.clock.{Clock, Fresh}
import grit.kit.environment.Exit

import Cli.*
import CaptureCommands.*
import ReplyCommands.*
import RebuildCommands.*
import RecipeCommands.*
import BenchCommands.*
import ProposeCommands.*
import ContextCommands.*
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
      case "context" :: rest =>
        exit(
          flags(rest.filterNot(_ == "--full")).flatMap(context(_, rest.contains("--full"), clock))
        )
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
      case "synthetic" :: rest => exit(flags(rest).flatMap(synthetic(_, clock)))
      case "integrity-loss" :: rest => exit(flags(rest).flatMap(integrityLoss(_, clock)))
      case "bench" :: rest => exit(flags(rest).flatMap(bench(_, clock)))
      case "propose" :: rest => exit(flags(rest).flatMap(propose(_, clock)))
      case _ =>
        exit(
          Left(
            "usage: scripts/eval capture|context|" +
              "replies|reply-labels|turns|churn|recall|rebuild|recipes|synthetic|" +
              "integrity-loss|bench|propose (scripts/eval and scripts/integrity-loss say what each takes)"
          )
        )
    }
  }
}
