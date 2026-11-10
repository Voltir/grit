package grit.eval.harness.main

import grit.core.clock.Clock
import grit.kit.environment.Exit

import Cli.*
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
      case "context" :: rest =>
        exit(
          flags(rest.filterNot(_ == "--full")).flatMap(context(_, rest.contains("--full"), clock))
        )
      case "integrity-loss" :: rest => exit(flags(rest).flatMap(integrityLoss(_, clock)))
      case "bench" :: rest => exit(flags(rest).flatMap(bench(_, clock)))
      case "propose" :: rest => exit(flags(rest).flatMap(propose(_, clock)))
      case _ =>
        exit(
          Left(
            "usage: scripts/eval context|bench|propose|integrity-loss " +
              "(scripts/eval and scripts/integrity-loss say what each takes)"
          )
        )
    }
  }
}
