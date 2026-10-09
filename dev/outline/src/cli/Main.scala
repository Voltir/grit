package grit.outline.cli

import grit.outline.locate.Root
import grit.outline.query.{Answer, Loaded, Query, Status}

object Main {

  private val usage =
    "usage: show Sym[,Sym…] [--depth N] [--body m[,m…]] [--private] [--cap BYTES] [--root DIR]"

  private final case class Options(
      depth: Int,
      bodies: Set[String],
      withPrivate: Boolean,
      cap: Int,
      root: Option[String]
  )

  private def parse(rest: List[String], o: Options): Either[String, Options] = rest match {
    case Nil => Right(o)
    case "--private" :: tail => parse(tail, o.copy(withPrivate = true))
    case "--depth" :: value :: tail =>
      value.toIntOption match {
        case Some(n) => parse(tail, o.copy(depth = n))
        case None => Left(s"--depth takes a number, not $value")
      }
    case "--cap" :: value :: tail =>
      value.toIntOption match {
        case Some(n) => parse(tail, o.copy(cap = n))
        case None => Left(s"--cap takes a number of bytes, not $value")
      }
    case "--body" :: value :: tail =>
      parse(tail, o.copy(bodies = value.split(',').filter(_.nonEmpty).toSet))
    case "--root" :: value :: tail => parse(tail, o.copy(root = Some(value)))
    case flag :: _ => Left(s"unknown argument $flag")
  }

  /** The nearest directory at or above `dir` holding `build.mill`. */
  private def repoAbove(dir: os.Path): Option[os.Path] =
    if (os.exists(dir / "build.mill")) Some(dir)
    else {
      val parent = dir / os.up
      if (parent == dir) None else repoAbove(parent)
    }

  private def exitCode(status: Status): Int = status match {
    case Status.Found => 0
    case Status.NoMatch => 1
    case Status.Failed => 2
  }

  /** The exit code and the text for `args`, run from `cwd`: 0 found, 1 no match, 2 failed or a usage error.
    * A leading `--root DIR`, as the launcher script passes it, is read as if it followed the subcommand.
    */
  def run(args: Vector[String], cwd: os.Path): (Int, String) =
    args.toList match {
      case "--root" :: dir :: rest => show(rest, cwd, Some(dir))
      case rest => show(rest, cwd, None)
    }

  private def show(args: List[String], cwd: os.Path, leadingRoot: Option[String]): (Int, String) =
    args match {
      case "show" :: sym :: rest if !sym.startsWith("--") =>
        val defaults = Options(
          depth = 1,
          bodies = Set.empty,
          withPrivate = false,
          cap = 80000,
          root = leadingRoot
        )
        parse(rest, defaults) match {
          case Left(message) => (2, s"$message\n$usage")
          case Right(o) =>
            val dir = o.root.map(os.Path(_, cwd)).orElse(repoAbove(cwd))
            dir match {
              case None => (2, s"no build.mill above $cwd: pass --root DIR\n$usage")
              case Some(d) =>
                val syms = sym.split(',').toVector.filter(_.nonEmpty)
                val (answer: Answer, _) =
                  Query.show(Root(d), Loaded.empty, syms, o.depth, o.bodies, o.withPrivate, o.cap)
                (exitCode(answer.status), answer.text)
            }
        }
      case _ => (2, usage)
    }

  def main(args: Array[String]): Unit = {
    val (code, text) = run(args.toVector, os.pwd)
    println(text)
    sys.exit(code)
  }
}
