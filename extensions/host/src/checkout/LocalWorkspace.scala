package grit.host.checkout

import java.nio.file.{Files, LinkOption, Path}
import java.util.regex.{Pattern, PatternSyntaxException}

import grit.core.host.{Clipped, HostError, Kept, LineNumbers, Lines, RelPath, Workspace}
import grit.core.secret.Named

/** Reading the checkout whose root is `root`, on this machine's file system. Search walks
  * the files itself, with `java.util.regex`; it does not shell out to `rg`, so it needs
  * nothing installed and behaves the same on every machine. A file's text is cleared by
  * `redaction` before it is read or searched, so a pattern never matches a secret's value.
  */
final class LocalWorkspace(root: Path, redaction: Redaction) extends Workspace {

  private val checkout = new Checkout(root)

  def read(path: RelPath, lines: Lines): Either[HostError, Clipped] =
    checkout.find(path).flatMap(checkout.text(path, _)).flatMap { text =>
      val all = Clipped.lines(redaction(text))
      val total = all.size
      val first = lines.offset
      if (first > total.max(1)) Left(HostError.PastEnd(path, first, total))
      else {
        val end = lines.limit.fold(total)(l => (first - 1 + l).min(total))
        val selected = LineNumbers.show(first, all.slice(first - 1, end)).mkString("\n")
        Right(Clipped.head(selected, hint(path, first, end, total)))
      }
    }

  def list(dir: RelPath, depth: Int): Either[HostError, Clipped] =
    checkout.find(dir).flatMap { at =>
      if (!Files.exists(at)) Left(HostError.NotFound(dir, checkout.nearest(dir)))
      else if (!Files.isDirectory(at)) Left(HostError.NotADirectory(dir))
      else
        checkout.entries(at, depth.max(1)).map { found =>
          val shown = found.map { (p, isDir) =>
            checkout.relative(at, p) + (if (isDir) "/" else "")
          }.sorted
          if (shown.isEmpty) Clipped.head("(empty)", _ => None)
          else
            Clipped.head(
              shown.mkString("\n"),
              _.map(k =>
                s"Showing ${k.lines} of ${k.total} entries. List a subdirectory, or a " +
                  "smaller depth, to see the rest."
              )
            )
        }
    }

  def search(pattern: String, under: RelPath): Either[HostError, Clipped] =
    compiled(pattern).flatMap { regex =>
      checkout.real.flatMap { top =>
        checkout.find(under).flatMap { at =>
          if (!Files.exists(at)) Left(HostError.NotFound(under, checkout.nearest(under)))
          else {
            val files =
              if (Files.isDirectory(at))
                checkout.entries(at, Int.MaxValue).map(_.collect { case (p, false) => p })
              else Right(Vector(at))
            files.map { fs =>
              Workspace.searched(
                fs
                  .map(f => (checkout.relative(top, f), f))
                  .filter((rel, f) =>
                    !secret(rel) && Files.isRegularFile(f, LinkOption.NOFOLLOW_LINKS)
                  )
                  .sortBy(_._1)
                  .map((rel, f) => (rel, matching(regex, rel, f)))
              )
            }
          }
        }
      }
    }

  /** The hint for lines `first` to `end` of a file of `total` lines, as [[read]] says. */
  private def hint(
      path: RelPath,
      first: Int,
      end: Int,
      total: Int
  ): Option[Kept] -> Option[String] = {
    def showing(last: Int): String = {
      val lines = if (first == last) s"line $first" else s"lines $first-$last"
      s"Showing $lines of $total. Use offset=${last + 1} to continue."
    }
    {
      case Some(Kept(0, _, _)) =>
        Some(
          s"Line $first is over ${Clipped.MaxBytes / 1024} KB, too long to show. Read part of " +
            s"it with a command, such as: sed -n '${first}p' ${RelPath.value(path)} | cut -c1-2000"
        )
      case Some(k) => Some(showing(first + k.lines - 1))
      case None if end < total => Some(showing(end))
      case None => None
    }
  }

  /** The lines of `file`, `rel` from the root, that `regex` finds something in, as
    * `rel:line: text`; none when the file is too large or not UTF-8.
    */
  private def matching(regex: Pattern, rel: String, file: Path): Vector[String] =
    RelPath.of(rel).toOption.flatMap(p => checkout.text(p, file).toOption) match {
      case None => Vector.empty
      case Some(text) =>
        Clipped.lines(redaction(text)).zipWithIndex.collect {
          case (line, i) if regex.matcher(line).find() =>
            s"$rel:${i + 1}: ${Workspace.cut(line)}"
        }
    }

  private def secret(rel: String): Boolean = rel.split('/').exists(Named.file)

  private def compiled(pattern: String): Either[HostError, Pattern] =
    try Right(Pattern.compile(pattern))
    catch {
      case e: PatternSyntaxException =>
        Left(HostError.BadPattern(pattern, Option(e.getDescription).getOrElse(e.toString)))
    }
}
