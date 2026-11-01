package grit.eval.harness.main

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}
import java.time.format.DateTimeFormatter

import scala.util.Try
import scala.util.control.NonFatal

import grit.core.message.Cost
import grit.core.period.Probability
import grit.dbos.internal.Reader
import grit.dbos.sql.DbConfig
import grit.eval.harness.jev.Budget
import grit.eval.harness.log.{Codec, Footer, Log, LogJson, Row}

/** What the commands share: flags and their values, files read and written, how a command exits. */
private[main] object Cli {

  private[main] def chance(what: String)(v: String): Either[String, Probability] =
    v.toDoubleOption
      .filter(p => p >= 0 && p <= 1)
      .map(Probability.clamped)
      .toRight(s"--$what $v: not a probability")

  private[main] def count(what: String)(v: String): Either[String, Int] =
    v.toIntOption.filter(_ >= 0).toRight(s"--$what $v: not a whole number")

  /** `log` as its file's lines: header, rows, then its footer when it has one. */
  private[main] def lines[A: Codec](log: Log[A]): String =
    (Vector(LogJson.header(log.header)) ++
      log.rows.map((r: Row[A]) => LogJson.row(r)) ++
      log.footer.map(LogJson.footer).toVector).map(_ + "\n").mkString

  /** `report` written to `<dir>/reports/<name>.md`, and printed. */
  private[main] def publish(dir: Path, name: String, report: String): Either[String, Unit] = {
    val reports = dir.resolve("reports")
    for {
      _ <- Try(Files.createDirectories(reports)).toEither.left.map(e =>
        s"$reports: ${e.getClass.getName}"
      )
      _ <- write(reports.resolve(s"$name.md"), report)
    } yield print(report)
  }

  private[main] val Stamp = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss'Z'")

  private[main] val Day = DateTimeFormatter.ofPattern("yyyyMMdd")

  private[main] def read(path: Path): Either[String, String] =
    Try(Files.readString(path, StandardCharsets.UTF_8)).toEither.left.map(e =>
      s"$path unread: ${e.getClass.getName}"
    )

  private[main] def refused(r: Budget.Refused): String =
    s"refused: estimated ${Cost.cents(r.estimated)} is over the cap ${Cost.cents(r.cap)}"

  private[main] def decimal(what: String)(v: String): Either[String, BigDecimal] =
    Try(BigDecimal(v)).toOption.filter(_ >= 0).toRight(s"--$what $v: not a non-negative number")

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

  private[main] def exit(result: Either[String, Unit]): Unit = result match {
    case Right(()) => ()
    case Left(why) =>
      System.err.println(s"eval: $why")
      sys.exit(1)
  }
}
