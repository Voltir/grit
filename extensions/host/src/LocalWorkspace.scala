package grit.host

import java.nio.file.{Files, LinkOption, Path}
import java.util.regex.{Pattern, PatternSyntaxException}

import grit.core.host.{Clipped, HostError, Kept, LineNumbers, Lines, RelPath, Workspace}

/** Reading the checkout whose root is `root`, on this machine's file system. Search walks
  * the files itself, with `java.util.regex`; it does not shell out to `rg`, so it needs
  * nothing installed and behaves the same on every machine.
  */
final class LocalWorkspace(root: Path) extends Workspace {
  import Checkout.attempt

  private val checkout = new Checkout(root)

  def read(path: RelPath, lines: Lines): Either[HostError, Clipped] =
    checkout.find(path).flatMap(checkout.text(path, _)).flatMap { text =>
      val all = Clipped.lines(text)
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
      if (!Files.exists(at)) Left(HostError.NotFound(dir))
      else if (!Files.isDirectory(at)) Left(HostError.NotADirectory(dir))
      else
        entries(at, depth.max(1)).map { found =>
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
          if (!Files.exists(at)) Left(HostError.NotFound(under))
          else {
            val files =
              if (Files.isDirectory(at))
                entries(at, Int.MaxValue).map(_.collect { case (p, false) => p })
              else Right(Vector(at))
            files.map { fs =>
              val matches = fs
                .map(f => (checkout.relative(top, f), f))
                .filter((rel, f) =>
                  !secret(rel) && Files.isRegularFile(f, LinkOption.NOFOLLOW_LINKS)
                )
                .sortBy(_._1)
                .flatMap((rel, f) => matching(regex, rel, f))
              if (matches.isEmpty) Clipped.head("No matches.", _ => None)
              else
                Clipped.head(
                  matches.mkString("\n"),
                  _.map(k =>
                    s"Showing ${k.lines} of ${k.total} matching lines. Narrow the pattern, " +
                      "or search a subdirectory, to see the rest."
                  )
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
    case Some(Kept(0, _, _)) =>
      Some(
        s"Line $first is over ${Clipped.MaxBytes / 1024} KB, too long to show. Read part of " +
          s"it with a command, such as: sed -n '${first}p' ${RelPath.value(path)} | cut -c1-2000"
      )
    case Some(k) =>
      val last = first + k.lines - 1
      Some(s"Showing lines $first-$last of $total. Use offset=${last + 1} to continue.")
    case None if end < total =>
      Some(s"${total - end} more lines in the file. Use offset=${end + 1} to continue.")
    case None => None
  }

  /** Every entry under `dir`, `depth` levels down, with whether it is a directory; a
    * directory [[Workspace.skipped]] is listed but not entered. Links are not followed.
    */
  private def entries(dir: Path, depth: Int): Either[HostError, Vector[(Path, Boolean)]] =
    attempt(children(dir)).flatMap { kids =>
      kids.foldLeft[Either[HostError, Vector[(Path, Boolean)]]](Right(Vector.empty)) { (acc, kid) =>
        acc.flatMap { so =>
          val isDir = Files.isDirectory(kid, LinkOption.NOFOLLOW_LINKS)
          val name = Option(kid.getFileName).fold("")(_.toString)
          if (isDir && depth > 1 && !Workspace.skipped(name))
            entries(kid, depth - 1).map(below => (so :+ (kid, isDir)) ++ below)
          else Right(so :+ (kid, isDir))
        }
      }
    }

  private def children(dir: Path): Vector[Path] = {
    val stream = Files.newDirectoryStream(dir)
    try {
      val out = Vector.newBuilder[Path]
      stream.forEach(p => out += p)
      out.result()
    } finally stream.close()
  }

  /** The lines of `file`, `rel` from the root, that `regex` finds something in, as
    * `rel:line: text`; none when the file is too large or not UTF-8.
    */
  private def matching(regex: Pattern, rel: String, file: Path): Vector[String] =
    RelPath.of(rel).toOption.flatMap(p => checkout.text(p, file).toOption) match {
      case None => Vector.empty
      case Some(text) =>
        Clipped.lines(text).zipWithIndex.collect {
          case (line, i) if regex.matcher(line).find() =>
            val shown =
              if (line.length <= Workspace.MaxLineChars) line
              else line.take(Workspace.MaxLineChars) + "… [line cut]"
            s"$rel:${i + 1}: $shown"
        }
    }

  private def secret(rel: String): Boolean = rel.split('/').exists(RelPath.secret)

  private def compiled(pattern: String): Either[HostError, Pattern] =
    try Right(Pattern.compile(pattern))
    catch {
      case e: PatternSyntaxException =>
        Left(HostError.BadPattern(pattern, Option(e.getDescription).getOrElse(e.toString)))
    }
}
