package grit.host.git

import java.nio.file.Path

import scala.concurrent.duration.*

import grit.core.host.{Argv, Clipped, HostError, Ran, RelPath, Stashes}
import grit.host.checkout.{LocalProcesses, Redaction}

/** The stash of the checkout `root`, read by git run as [[LocalProcesses]] runs it, with
  * `environment` and `redaction`: this capability's own, never the model's runs. A read is
  * three git commands; `Failed` when one runs past [[LocalStashes.Within]] or fails, or the
  * stash is too long to read whole.
  */
final class LocalStashes(root: Path, environment: Map[String, String], redaction: Redaction)
    extends Stashes {
  private val processes = new LocalProcesses(root, environment, redaction)

  def held: Either[HostError, Stashes.Held] =
    for {
      dirs <- git("rev-parse", "--path-format=absolute", "--git-common-dir", "--git-dir")
      shared <- lines(dirs) match {
        case Vector(common, own) => Right(common != own)
        case other => Left(HostError.Failed(s"git rev-parse answered ${other.size} lines, not 2"))
      }
      head <- run("symbolic-ref", "--short", "-q", "HEAD")
      list <- git("stash", "list")
      _ <- Either.cond(list.output.hint.isEmpty, (), HostError.Failed("the stash is too long"))
    } yield Stashes.Held(
      shared,
      // A non-zero exit: HEAD is on no branch.
      Option.when(head.exit == 0)(head.output.text.trim).filter(_.nonEmpty),
      lines(list).filter(_.startsWith("stash@{")).map(Stashes.branchOf)
    )

  private def lines(r: Ran): Vector[String] =
    Clipped.lines(r.output.text).map(_.trim).filter(_.nonEmpty)

  private def run(args: String*): Either[HostError, Ran] =
    Argv
      .of("git", args*)
      .left
      .map(HostError.Failed(_))
      .flatMap(processes.run(_, RelPath.Root, LocalStashes.Within))

  private def git(args: String*): Either[HostError, Ran] =
    run(args*).flatMap(r =>
      if (r.exit == 0) Right(r)
      else
        Left(
          HostError.Failed(s"git ${args.mkString(" ")} exited ${r.exit}: ${r.output.text.trim}")
        )
    )
}

object LocalStashes {

  /** 10 seconds: the longest one of its git commands may run. */
  val Within: FiniteDuration = 10.seconds
}
