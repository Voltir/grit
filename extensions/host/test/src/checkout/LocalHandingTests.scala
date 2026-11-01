package grit.host.checkout

import java.nio.file.{Files, Path}

import grit.core.host.*
import grit.core.secret.{Secret, Variable}

import utest.*

/** [[LocalHanding]] against a temporary root: a secret handed to a program as a file. */
object LocalHandingTests extends TestSuite {

  private val noSecrets = Redaction.of(Map.empty)

  private val value = "sk-ant-planted-handed-value"

  private val key: Secret =
    Secret.read(Map("ANTHROPIC_API_KEY" -> value), Variable("ANTHROPIC_API_KEY")) match {
      case Right(s) => s
      case Left(why) => throw new java.lang.AssertionError(why.message)
    }

  private def variables(values: (String, String)*): Variables =
    Variables.of(values.toMap) match {
      case Right(v) => v
      case Left(why) => throw new java.lang.AssertionError(why)
    }

  /** A fresh root, given to `body`, deleted after. */
  private def inRoot[A](body: Path => A): A = {
    val dir = Files.createTempDirectory("grit-handing")
    try body(dir)
    finally Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
  }

  /** The lines a started program wrote, to its end. */
  private def lines(s: Started^): Vector[String] =
    s.lines.toVector

  /** `sh -c script`, with the handed file's path as `$1`. */
  private def sh(script: String): String -> Either[String, Argv] =
    path => Argv.of("sh", "-c", script, "sh", path)

  val tests = Tests {
    test("a handed secret reaches the program only in the file whose path its argv is given") {
      inRoot { dir =>
        val seen =
          new LocalHanding(dir, sys.env, noSecrets).started(RelPath.Root, key, variables())(
            sh("cat \"$1\"; echo; stat -c %a \"$1\"; stat -c %a \"$(dirname \"$1\")\"")
          )(lines)
        seen ==> Right(Vector(value, "600", "700"))
      }
    }

    test("the secret's directory is gone once the body returns, and once it throws") {
      inRoot { dir =>
        val handing = new LocalHanding(dir, sys.env, noSecrets)
        val returned = handing
          .started(RelPath.Root, key, variables())(sh("echo \"$1\""))(lines)
          .map(_.map(p => Files.exists(Path.of(p).getParent)))
        var path = ""
        val thrown =
          try {
            val _ = handing.started(RelPath.Root, key, variables())(sh("echo \"$1\"")) { s =>
              path = lines(s).mkString
              throw new IllegalStateException("planted")
            }
            "returned"
          } catch { case e: IllegalStateException => e.getMessage }
        (returned, thrown, Files.exists(Path.of(path).getParent)) ==>
          (Right(Vector(false)), "planted", false)
      }
    }

    test("a handed program's environment holds its variables and no secret") {
      inRoot { dir =>
        val seen =
          new LocalHanding(dir, sys.env ++ Map("ANTHROPIC_API_KEY" -> "inherited"), noSecrets)
            .started(RelPath.Root, key, variables("CLAUDE_CONFIG_DIR" -> "/tmp/config"))(
              sh("echo \"${CLAUDE_CONFIG_DIR:-none} ${ANTHROPIC_API_KEY:-none}\"")
            )(lines)
        seen ==> Right(Vector("/tmp/config none"))
      }
    }

    test("a directory that is not there is refused before anything starts") {
      inRoot { dir =>
        val missing = RelPath.of("missing").fold(e => throw new java.lang.AssertionError(e), p => p)
        new LocalHanding(dir, sys.env, noSecrets).started(missing, key, variables())(sh("true"))(
          lines
        ) ==>
          Left(HostError.NotFound(missing))
      }
    }
  }
}
