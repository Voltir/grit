package grit.eval.harness.main

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}

import scala.util.Try
import scala.util.control.NonFatal

import grit.dbos.internal.Reader
import grit.dbos.sql.DbConfig

/** What the commands share: flags and their values, files read and written, how a command exits. */
private[main] object Cli {

  private[main] def read(path: Path): Either[String, String] =
    Try(Files.readString(path, StandardCharsets.UTF_8)).toEither.left.map(e =>
      s"$path unread: ${e.getClass.getName}"
    )

  private[main] def positive(what: String)(v: String): Either[String, Int] =
    v.toIntOption.filter(_ > 0).toRight(s"--$what $v: not a positive whole number")

  private[main] def opened[A](
      config: DbConfig
  )(f: Reader^ => Either[String, A]): Either[String, A] =
    try {
      val reader = Reader.open(config)
      try f(reader)
      finally reader.close()
    } catch { case NonFatal(e) => Left(s"the database could not be read: ${e.getClass.getName}") }

  /** `dir` and its parents created where missing; `Left` naming `dir` and the failure's class. */
  private[main] def mkdirs(dir: Path): Either[String, Unit] =
    Try(Files.createDirectories(dir)).toEither.left
      .map(e => s"$dir: ${e.getClass.getName}")
      .map(_ => ())

  /** Writes `text` to `path` whole: a reader never sees part of it. */
  private[main] def write(path: Path, text: String): Either[String, Unit] =
    try {
      val partial = path.resolveSibling(path.getFileName.toString + ".partial")
      Files.writeString(partial, text, StandardCharsets.UTF_8)
      Files.move(partial, path, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
      Right(())
    } catch { case NonFatal(e) => Left(s"$path not written: ${e.getClass.getName}") }

  private[main] def flags(args: List[String]): Either[String, Map[String, String]] = args match {
    case Nil => Right(Map.empty)
    case k :: v :: rest if k.startsWith("--") => flags(rest).map(_.updated(k.drop(2), v))
    case k :: _ => Left(s"not a flag and its value: $k")
  }

  private[main] def need(f: Map[String, String], k: String): Either[String, String] =
    f.get(k).toRight(s"--$k is required")
}
