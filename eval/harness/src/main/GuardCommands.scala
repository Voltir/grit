package grit.eval.harness.main

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

import grit.models.{JevClassifier, JevConfig}
import grit.tools.MachineGuard

/** `scripts/eval machine-guard`: Jev's answers to the machine guard's corpus, recorded for its
  * tests to replay (`MachineGuardCorpusTests`).
  */
object GuardCommands {

  /** Each state `pending` lists asked of Jev with [[MachineGuard.Questions]], one request each,
    * its answer added to `answers` (`{"state", "model", "harms"}`, the four probabilities in
    * [[MachineGuard.Harms]]'s order) and `pending` left empty; how many it recorded and their
    * input tokens. `Left` when the key is not set, a file does not read, or a request gets no
    * answer, before anything is written. It calls Jev, so it is run by hand, never by a test.
    */
  def record(f: Map[String, String], env: Map[String, String]): Either[String, String] =
    for {
      pendingAt <- Cli.need(f, "pending").map(Path.of(_))
      answersAt <- Cli.need(f, "answers").map(Path.of(_))
      config <- JevConfig.fromEnv(env).left.map(_.toString)
      pending <- read(pendingAt)
      kept <- read(answersAt)
      // The corpus is grit's own fixed text, no conversation's: there is no clearance to check.
      jev = new JevClassifier(config)
      asked <- pending.foldLeft[Either[String, Vector[(ujson.Value, ujson.Value, Long)]]](
        Right(Vector.empty)
      ) {
        case (Left(why), _) => Left(why)
        case (Right(so), state) =>
          val s =
            MachineGuard.State(state("command").str, state("checkout").str, state("outside").str)
          jev
            .ask(s, MachineGuard.Questions)
            .left
            .map(e => s"${s.command}: ${e.said}")
            .map { a =>
              val h = a.value
              val answer = ujson.Obj(
                "state" -> state,
                "model" -> a.model,
                "harms" -> ujson
                  .Arr(h.changesOutside, h.changesShared, h.stopsProcesses, h.changesRemote)
              )
              so :+ (state, answer, grit.core.message.Tokens.value(a.usage.input))
            }
      }
    } yield {
      write(answersAt, kept.filterNot(k => asked.exists(_._1 == k("state"))) ++ asked.map(_._2))
      write(pendingAt, Vector.empty)
      s"recorded ${asked.size} answers, ${asked.map(_._3).sum} input tokens"
    }

  private def read(at: Path): Either[String, Vector[ujson.Value]] =
    scala.util
      .Try(ujson.read(new String(Files.readAllBytes(at), UTF_8)).arr.toVector)
      .toEither
      .left
      .map(e => s"$at: ${e.getMessage}")

  private def write(at: Path, values: Vector[ujson.Value]): Unit = {
    val _ = Files.writeString(at, ujson.write(ujson.Arr.from(values), indent = 2) + "\n")
  }
}
