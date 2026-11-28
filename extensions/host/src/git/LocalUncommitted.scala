package grit.host.git

import java.nio.file.Path

import grit.core.host.{Argv, HostError, RelPath, Uncommitted}
import grit.host.checkout.{LocalProcesses, Redaction}
import grit.std.either.Traversal.traverse

/** As [[LocalStashes]] reads the stash: git run over `environment`, cleared by `redaction`,
  * `Failed` past [[LocalStashes.Within]], or when git lists more than it can read whole.
  */
final class LocalUncommitted(root: Path, environment: Map[String, String], redaction: Redaction)
    extends Uncommitted {

  private val processes = new LocalProcesses(root, environment, redaction)

  def read: Either[HostError, Uncommitted.Files] =
    for {
      argv <- Argv.of("git", LocalUncommitted.Status*).left.map(HostError.Failed(_))
      ran <- processes.run(argv, RelPath.Root, LocalStashes.Within)
      _ <- Either.cond(
        ran.exit == 0,
        (),
        HostError.Failed(s"git status exited ${ran.exit}: ${ran.output.text.trim}")
      )
      _ <- Either.cond(
        ran.output.hint.isEmpty,
        (),
        HostError.Failed("git status lists more files than can be read whole")
      )
      files <- LocalUncommitted.files(ran.output.text)
    } yield files
}

object LocalUncommitted {

  // Optional locks off, so the read never takes the index's lock from a command the model runs.
  private val Status: Vector[String] = Vector(
    "--no-optional-locks",
    "status",
    "--porcelain=v1",
    "-z",
    "--untracked-files=all"
  )

  /** `git status --porcelain=v1 -z` read: a record `XY path`, a rename's or copy's followed by
    * the path it came from. `Left` names a path no tool could reach, a secrets file among them.
    */
  private[git] def files(status: String): Either[HostError, Uncommitted.Files] = {
    // The records, a rename's or copy's source dropped.
    def entries(records: List[String]): List[String] = records match {
      case entry :: _ :: rest if entry.length > 3 && "RC".contains(entry.charAt(0)) =>
        entry :: entries(rest)
      case entry :: rest => entry :: entries(rest)
      case Nil => Nil
    }
    // A nested repository's directory is listed with a trailing `/`.
    def path(entry: String): Either[HostError, RelPath] = {
      val written = entry.drop(3).stripSuffix("/")
      RelPath
        .of(written)
        .left
        .map(e => HostError.Failed(s"git status names `$written`: ${e.message}"))
    }
    val listed =
      entries(status.split('\u0000').toList.filter(_.nonEmpty)).filter(_.length > 3).toVector
    for {
      untracked <- traverse(listed.filter(_.startsWith("??")))(path)
      unstaged <- traverse(listed.filter(e => !e.startsWith("??") && e.charAt(1) != ' '))(path)
    } yield Uncommitted.Files(unstaged, untracked)
  }
}
