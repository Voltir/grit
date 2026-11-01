package grit.host.checkout

import java.nio.file.Path

import scala.concurrent.duration.FiniteDuration

import grit.core.host.{Argv, Clipped, HostError, Kept, Ran, RelPath, Shell}
import grit.core.secret.Named

/** Running commands in the checkout whose root is `root`, with `sh -c`, as
  * [[LocalProcesses]] runs a program there: a command never sees an API key, a database
  * password or anything else named like a secret, and a timeout kills it and every process it
  * started that is still its descendant; one that detached itself survives. What it shows is
  * cleared by `redaction`.
  */
final class LocalShell(root: Path, environment: Map[String, String], redaction: Redaction)
    extends Shell {

  private val processes = new LocalProcesses(root, environment, redaction)

  def run(command: String, timeout: FiniteDuration): Either[HostError, Ran] =
    Argv.of("sh", "-c", command) match {
      case Left(why) => Left(HostError.Failed(why))
      case Right(argv) => processes.ran(argv, RelPath.Root, timeout, hint)
    }

  private val hint: Option[Kept] -> Option[String] = {
    case Some(Kept(_, total, true)) =>
      Some(
        s"Showing the last ${Clipped.MaxBytes / 1024} KB of line $total, the output's last. " +
          "Narrow the command's output to see the rest."
      )
    case Some(k) =>
      Some(
        s"Showing the last ${k.lines} of ${k.total} lines. Narrow the command's output, " +
          "for example with grep, head or tail, to see the rest."
      )
    case None => None
  }
}

object LocalShell {

  /** The variables a command may see, when they are set: where programs are, who and where
    * the user is, the locale and the terminal, and the toolchains a build in the checkout
    * looks for.
    */
  val Allowed: Set[String] = Set(
    "PATH",
    "HOME",
    "USER",
    "LOGNAME",
    "SHELL",
    "LANG",
    "LANGUAGE",
    "LC_ALL",
    "LC_CTYPE",
    "TERM",
    "TZ",
    "TMPDIR",
    "JAVA_HOME",
    "XDG_CACHE_HOME",
    "XDG_CONFIG_HOME",
    "COURSIER_CACHE"
  )

  /** The variables of `environment` a command sees: those [[Allowed]] names that are not named
    * like a secret ([[Named.variable]]).
    */
  def passed(environment: Map[String, String]): Map[String, String] =
    environment.filter((k, _) => Allowed.contains(k) && !Named.variable(k))
}
