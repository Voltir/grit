package grit.kit.run

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

import grit.core.message.Tokens
import grit.kit.environment.DotEnv
import grit.models.{JevClassifier, JevConfig}
import grit.tools.MachineGuard

/** Jev's answers to the machine guard's corpus, recorded for `MachineGuardCorpusTests` to
  * replay. It calls Jev, so it is run by hand, never by a test:
  * {{{
  * at=extensions/tools/test/resources/machine-guard
  * GRIT_ENV_FILE=<a .env holding JEV_API_KEY> ./mill grit.kit.test.runMain grit.kit.run.RecordGuardAnswers \
  *   "$PWD/$at/pending.json" "$PWD/$at/answers.json"
  * }}}
  * Each state the first file lists is asked of Jev with [[MachineGuard.questions]], one request
  * of about 300 input tokens each, its answer added to the second (`{"state", "model",
  * "harms"}`, the probabilities of the questions asked, in [[MachineGuard.Harms]]'s order), and the first left empty.
  * The key is read from the environment over `GRIT_ENV_FILE` ([[DotEnv.loadDefault]]), never
  * shown. Nothing is written when the key is unset, a file does not read, or a request gets no answer.
  */
object RecordGuardAnswers {

  def main(args: Array[String]): Unit =
    args.toList match {
      case pending :: answers :: Nil =>
        DotEnv.loadDefault(sys.env).flatMap(record(Path.of(pending), Path.of(answers), _)) match {
          case Right(said) => println(said)
          case Left(why) =>
            System.err.println(s"record: $why")
            sys.exit(1)
        }
      case _ =>
        System.err.println("usage: RecordGuardAnswers <pending.json> <answers.json>")
        sys.exit(2)
    }

  private def record(
      pendingAt: Path,
      answersAt: Path,
      env: Map[String, String]
  ): Either[String, String] =
    for {
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
            MachineGuard.State(
              state("command").str,
              state("checkout").str,
              state.obj.get("outside").map(_.str)
            )
          jev
            .ask(s, MachineGuard.questions(s))
            .left
            .map(e => s"${s.command}: ${e.said}")
            .map { a =>
              val h = a.value
              val answer = ujson.Obj(
                "state" -> state,
                "model" -> a.model,
                "harms" -> ujson.Arr.from(
                  h.changesOutside.toVector ++
                    Vector(h.changesShared, h.stopsProcesses, h.changesRemote)
                )
              )
              so :+ (state, answer, Tokens.value(a.usage.input))
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
