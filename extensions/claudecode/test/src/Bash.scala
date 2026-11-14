package grit.claudecode

import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import grit.core.host.{Argv, HostError, RelPath, Started}
import grit.host.checkout.{LocalProcesses, Redaction}

/** bash 5, run through grit.host's processes with an environment of `env` alone, so a test reaches
  * no person's settings: `HOME` is a temporary directory, and `PATH` is `bin` (where a test puts
  * its fake `claude`) before the system's.
  */
object Bash {

  /** How a run ended: its exit code, its output's lines and its errors. */
  final case class Done(exit: Int, out: Vector[String], err: String)

  /** The system's programs a test's bash may run: bash, jq, curl, git and coreutils. */
  val SystemPath: String = "/usr/local/bin:/usr/bin:/bin"

  /** bash with `args`, in `dir`, reading `input`, given `within` to end, else failing the test.
    * `env` gets `PATH` with `bin` first, `HOME`, and `GIT_CEILING_DIRECTORIES` as `HOME`'s parent
    * (a test's temporary root), so git never finds a repository above it, when it names none.
    */
  def run(
      dir: Path,
      home: Path,
      bin: Path,
      args: Seq[String],
      input: String = "",
      env: Map[String, String] = Map.empty,
      within: FiniteDuration = 10.seconds
  ): Done = {
    val vars = (Map(
      "PATH" -> s"$bin:$SystemPath",
      "HOME" -> home.toString,
      "GIT_CEILING_DIRECTORIES" -> home.getParent.toString
    ) ++ env).toVector.sorted
      .map((k, v) => s"$k=$v")
    val argv = Argv.of("/usr/bin/env", (("-i" +: vars) ++ ("bash" +: args))*) match {
      case Right(a) => a
      case Left(why) => throw new java.lang.AssertionError(why)
    }
    val deadline = within.fromNow
    val done = new LocalProcesses(dir, Map("PATH" -> SystemPath), Redaction.of(Map.empty))
      .started(argv, RelPath.Root) { (s: Started^) =>
        try {
          s.input.write(input.getBytes(UTF_8))
          s.input.close()
        } catch { case _: IOException => () } // bash ended before reading all of it
        val out = Vector.newBuilder[String]
        var reading = true
        while (reading)
          s.next(deadline.timeLeft.max(1.milli)) match {
            case Started.Next.Line(text) => out += text
            case Started.Next.Ended | Started.Next.Quiet => reading = false
          }
        s.await(deadline.timeLeft.max(1.milli)).map(r => Done(r.exit, out.result(), r.output.text))
      }
    done.flatten match {
      case Right(d) => d
      case Left(HostError.TimedOut(after, output)) =>
        throw new java.lang.AssertionError(s"bash ran past $after: ${output.text}")
      case Left(other) => throw new java.lang.AssertionError(other.message)
    }
  }

  /** A fresh directory, given to `body`, deleted after. */
  def inTemp[A](body: Path => A): A = {
    val dir = Files.createTempDirectory("grit-claudecode")
    try body(dir)
    finally Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
  }
}
