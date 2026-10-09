package grit.outline.cli

import scala.annotation.tailrec

import grit.outline.locate.{MillLayout, Root}
import grit.outline.mcp.Server
import grit.outline.query.{Answer, Config, Help, Query, Roots, Status}

object Main {

  private final case class FamilyOptions(
      member: Option[String],
      withBody: Boolean,
      cap: Int,
      root: Option[String]
  )

  private def parseFamily(rest: List[String], o: FamilyOptions): Either[String, FamilyOptions] =
    rest match {
      case Nil => Right(o)
      case "--body" :: tail => parseFamily(tail, o.copy(withBody = true))
      case "--member" :: value :: tail => parseFamily(tail, o.copy(member = Some(value)))
      case "--cap" :: value :: tail =>
        value.toIntOption match {
          case Some(n) => parseFamily(tail, o.copy(cap = n))
          case None => Left(s"--cap takes a number of bytes, not $value")
        }
      case "--root" :: value :: tail => parseFamily(tail, o.copy(root = Some(value)))
      case flag :: _ => Left(s"unknown argument $flag")
    }

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
  /** The directory `--root` names, else the repository above `cwd`. */
  private def rootDir(explicit: Option[String], cwd: os.Path): Option[os.Path] =
    explicit.map(os.Path(_, cwd)).orElse(repoAbove(cwd))

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

  private def isHelp(arg: String): Boolean = arg == "--help" || arg == "-h"

  private def show(args: List[String], cwd: os.Path, leadingRoot: Option[String]): (Int, String) =
    args match {
      case Nil => (0, Help.summary)
      case first :: _ if isHelp(first) => (0, Help.summary)
      case query :: rest if rest.exists(isHelp) =>
        Help.entry(query) match {
          case Some(e) => (0, Help.help(e))
          case None => (2, s"unknown query $query\n${Help.summary}")
        }
      case "show" :: sym :: rest if !sym.startsWith("--") =>
        val defaults = Options(
          depth = 1,
          bodies = Set.empty,
          withPrivate = false,
          cap = 80000,
          root = leadingRoot
        )
        parse(rest, defaults) match {
          case Left(message) => (2, s"$message\n${Help.usage(Help.show)}")
          case Right(o) =>
            rootDir(o.root, cwd) match {
              case None =>
                (2, s"no build.mill above $cwd: pass --root DIR\n${Help.usage(Help.show)}")
              case Some(d) =>
                val syms = sym.split(',').toVector.filter(_.nonEmpty)
                Config.read(Root(d)) match {
                  case Left(message) => (2, message)
                  case Right(config) =>
                    val (answer: Answer, _) =
                      Query.show(
                        Root(d),
                        MillLayout,
                        config,
                        Roots.empty(6000),
                        syms,
                        o.depth,
                        o.bodies,
                        o.withPrivate,
                        o.cap
                      )
                    (exitCode(answer.status), answer.text)
                }
            }
        }
      case "family" :: name :: rest if !name.startsWith("--") =>
        family(name, rest, cwd, leadingRoot)
      case "uses" :: syms :: rest if !syms.startsWith("--") =>
        uses(syms, rest, cwd, leadingRoot)
      case "tests" :: suite :: rest if !suite.startsWith("--") =>
        tests(suite, rest, cwd, leadingRoot)
      case "area" :: names :: rest if !names.startsWith("--") =>
        area(names, rest, cwd, leadingRoot)
      case other :: _ =>
        (2, Help.entry(other).fold(s"unknown query $other\n${Help.summary}")(Help.usage))
    }

  private def family(
      name: String,
      rest: List[String],
      cwd: os.Path,
      leadingRoot: Option[String]
  ): (Int, String) =
    parseFamily(rest, FamilyOptions(None, false, 80000, leadingRoot)) match {
      case Left(message) => (2, s"$message\n${Help.usage(Help.family)}")
      case Right(o) =>
        rootDir(o.root, cwd) match {
          case None => (2, s"no build.mill above $cwd: pass --root DIR\n${Help.usage(Help.family)}")
          case Some(d) =>
            Config.read(Root(d)) match {
              case Left(message) => (2, message)
              case Right(config) =>
                val (answer, _) =
                  Query.family(
                    Root(d),
                    MillLayout,
                    config,
                    Roots.empty(6000),
                    name,
                    o.member,
                    o.withBody,
                    o.cap
                  )
                (exitCode(answer.status), answer.text)
            }
        }
    }

  private final case class UsesOptions(
      in: Option[String],
      outside: Option[String],
      cap: Int,
      root: Option[String]
  )

  private def parseUses(rest: List[String], o: UsesOptions): Either[String, UsesOptions] =
    rest match {
      case Nil => Right(o)
      case "--in" :: value :: tail => parseUses(tail, o.copy(in = Some(value)))
      case "--outside" :: value :: tail => parseUses(tail, o.copy(outside = Some(value)))
      case "--cap" :: value :: tail =>
        value.toIntOption match {
          case Some(n) => parseUses(tail, o.copy(cap = n))
          case None => Left(s"--cap takes a number of bytes, not $value")
        }
      case "--root" :: value :: tail => parseUses(tail, o.copy(root = Some(value)))
      case flag :: _ => Left(s"unknown argument $flag")
    }

  private def uses(
      syms: String,
      rest: List[String],
      cwd: os.Path,
      leadingRoot: Option[String]
  ): (Int, String) =
    parseUses(rest, UsesOptions(None, None, 80000, leadingRoot)) match {
      case Left(message) => (2, s"$message\n${Help.usage(Help.uses)}")
      case Right(o) =>
        rootDir(o.root, cwd) match {
          case None => (2, s"no build.mill above $cwd: pass --root DIR\n${Help.usage(Help.uses)}")
          case Some(d) =>
            Config.read(Root(d)) match {
              case Left(message) => (2, message)
              case Right(config) =>
                val (answer, _) =
                  Query.uses(
                    Root(d),
                    MillLayout,
                    config,
                    Roots.empty(6000),
                    syms.split(',').toVector.filter(_.nonEmpty),
                    o.in,
                    o.outside,
                    o.cap
                  )
                (exitCode(answer.status), answer.text)
            }
        }
    }

  private final case class AreaOptions(
      level: Int,
      cap: Int,
      root: Option[String]
  )

  private def parseArea(rest: List[String], o: AreaOptions): Either[String, AreaOptions] =
    rest match {
      case Nil => Right(o)
      case "--level" :: value :: tail =>
        value.toIntOption match {
          case Some(n) => parseArea(tail, o.copy(level = n))
          case None => Left(s"--level takes 0 or 1, not $value")
        }
      case "--cap" :: value :: tail =>
        value.toIntOption match {
          case Some(n) => parseArea(tail, o.copy(cap = n))
          case None => Left(s"--cap takes a number of bytes, not $value")
        }
      case "--root" :: value :: tail => parseArea(tail, o.copy(root = Some(value)))
      case flag :: _ => Left(s"unknown argument $flag")
    }

  private def area(
      names: String,
      rest: List[String],
      cwd: os.Path,
      leadingRoot: Option[String]
  ): (Int, String) =
    parseArea(rest, AreaOptions(1, 80000, leadingRoot)) match {
      case Left(message) => (2, s"$message\n${Help.usage(Help.area)}")
      case Right(o) =>
        rootDir(o.root, cwd) match {
          case None => (2, s"no build.mill above $cwd: pass --root DIR\n${Help.usage(Help.area)}")
          case Some(d) =>
            Config.read(Root(d)) match {
              case Left(message) => (2, message)
              case Right(config) =>
                val (answer, _) =
                  Query.area(
                    Root(d),
                    MillLayout,
                    config,
                    Roots.empty(6000),
                    names.split(',').toVector.filter(_.nonEmpty),
                    o.level,
                    o.cap
                  )
                (exitCode(answer.status), answer.text)
            }
        }
    }

  private final case class TestsOptions(
      test: Option[String],
      cap: Int,
      root: Option[String]
  )

  private def parseTests(rest: List[String], o: TestsOptions): Either[String, TestsOptions] =
    rest match {
      case Nil => Right(o)
      case "--test" :: value :: tail => parseTests(tail, o.copy(test = Some(value)))
      case "--cap" :: value :: tail =>
        value.toIntOption match {
          case Some(n) => parseTests(tail, o.copy(cap = n))
          case None => Left(s"--cap takes a number of bytes, not $value")
        }
      case "--root" :: value :: tail => parseTests(tail, o.copy(root = Some(value)))
      case flag :: _ => Left(s"unknown argument $flag")
    }

  private def tests(
      suite: String,
      rest: List[String],
      cwd: os.Path,
      leadingRoot: Option[String]
  ): (Int, String) =
    parseTests(rest, TestsOptions(None, 80000, leadingRoot)) match {
      case Left(message) => (2, s"$message\n${Help.usage(Help.tests)}")
      case Right(o) =>
        rootDir(o.root, cwd) match {
          case None => (2, s"no build.mill above $cwd: pass --root DIR\n${Help.usage(Help.tests)}")
          case Some(d) =>
            Config.read(Root(d)) match {
              case Left(message) => (2, message)
              case Right(config) =>
                val (answer, _) = Query.tests(
                  Root(d),
                  MillLayout,
                  config,
                  Roots.empty(6000),
                  suite,
                  o.test,
                  o.cap
                )
                (exitCode(answer.status), answer.text)
            }
        }
    }

  /** The leading `--root DIR` pairs (the last one wins) and the words after them. */
  @tailrec private def leadingRoots(
      args: List[String],
      root: Option[String]
  ): (Option[String], List[String]) =
    args match {
      case "--root" :: dir :: rest => leadingRoots(rest, Some(dir))
      case rest => (root, rest)
    }

  def main(args: Array[String]): Unit =
    leadingRoots(args.toList, None) match {
      case (root, List("mcp")) =>
        root.map(os.Path(_, os.pwd)).orElse(repoAbove(os.pwd)) match {
          case Some(dir) => Server.serve(Root(dir))
          case None =>
            System.err.println(s"no build.mill above ${os.pwd}: pass --root DIR")
            sys.exit(2)
        }
      case _ =>
        val (code, text) = run(args.toVector, os.pwd)
        println(text)
        sys.exit(code)
    }
}
