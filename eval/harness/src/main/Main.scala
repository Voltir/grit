package grit.eval.harness.main

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}
import java.time.Instant

import scala.util.Try
import scala.util.control.NonFatal

import grit.dbos.engine.{Build, Reader}
import grit.dbos.sql.DbConfig
import grit.eval.harness.corpus.{
  Capture,
  Case,
  Corpus,
  CorpusJson,
  Digest,
  Dump,
  Manifest,
  SeenCheck,
  Stitched
}

/** The eval harness's command line, run through `scripts/eval`, which says what each command
  * takes. It prints counts, ids and field names, never a message's text.
  */
object Main {

  def main(args: Array[String]): Unit =
    args.toList match {
      case "capture" :: rest => exit(flags(rest).flatMap(capture))
      case _ => exit(Left("usage: scripts/eval capture --from <database> [--date <yyyymmdd>]"))
    }

  /** `capture --url <jdbc> --source <db> --restored <db> --sha256 <hex> --at <instant> --out
    * <dir>`: the corpus of the restored database, written to `corpus.json` and `cases.jsonl` in
    * `<dir>`, with the database's login from `GRIT_DATABASE_USER` and `_PASSWORD`.
    */
  private def capture(f: Map[String, String]): Either[String, Unit] =
    for {
      url <- need(f, "url")
      source <- need(f, "source")
      restored <- need(f, "restored")
      sha <- need(f, "sha256").flatMap(Digest.read)
      at <- need(f, "at").flatMap(a => Try(Instant.parse(a)).toOption.toRight(s"--at $a"))
      out <- need(f, "out").map(Path.of(_))
      config <- DbConfig
        .fromEnv(sys.env.updated(DbConfig.UrlVar, url))
        .left
        .map(_.message)
      captured <- opened(config)(reader =>
        Capture(reader, source, restored, Dump(sha, at), Build.current)
      )
      _ <- write(
        out.resolve("cases.jsonl"),
        captured.cases.map(CorpusJson.writeCase(_).render() + "\n").mkString
      )
      _ <- write(
        out.resolve("corpus.json"),
        CorpusJson.writeManifest(captured.manifest).render(2) + "\n"
      )
    } yield report(captured.manifest, captured.cases)

  private def opened(
      config: DbConfig
  )(f: Reader^ => Either[String, Corpus]): Either[String, Corpus] =
    try {
      val reader = Reader.open(config)
      try f(reader)
      finally reader.close()
    } catch { case NonFatal(e) => Left(s"the database could not be read: ${e.getClass.getName}") }

  private def report(m: Manifest, cases: Vector[Case]): Unit = {
    val stitches = cases.flatMap(_.stitch)
    val differs = stitches.map(_.seen).collect { case d: SeenCheck.Differs => d.fields }
    println(s"cases: ${m.cases}")
    println(s"stitched: ${m.stitched}")
    println(s"triage rebuilt: ${cases.count(_.asked.isDefined)} of ${cases.size}")
    println(
      s"seen check: match ${stitches.count(_.seen == SeenCheck.Match)}, " +
        s"differs ${differs.size}, unbuilt ${stitches.count(_.seen == SeenCheck.Unbuilt)}"
    )
    SeenCheck.Field.values.foreach { field =>
      val n = differs.count(_.contains(field))
      if (n > 0) println(s"  differs in ${SeenCheck.Field.written(field)}: $n")
    }
    println(
      s"lexical drift: ${stitches.count(_.drift > 0)} cases, " +
        s"${stitches.map(_.drift).sum} exchanges over ${Stitched.Tolerance}"
    )
    println(
      s"build known: ${cases.count(_.triage.build != Build.Unknown)}, " +
        s"unknown: ${cases.count(_.triage.build == Build.Unknown)}"
    )
    println(s"tunings: ${m.tunings}")
  }

  /** Writes `text` to `path` whole: a reader never sees part of it. */
  private def write(path: Path, text: String): Either[String, Unit] =
    try {
      val partial = path.resolveSibling(path.getFileName.toString + ".partial")
      Files.writeString(partial, text, StandardCharsets.UTF_8)
      Files.move(partial, path, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
      Right(())
    } catch { case NonFatal(e) => Left(s"$path not written: ${e.getClass.getName}") }

  private def flags(args: List[String]): Either[String, Map[String, String]] = args match {
    case Nil => Right(Map.empty)
    case k :: v :: rest if k.startsWith("--") => flags(rest).map(_.updated(k.drop(2), v))
    case k :: _ => Left(s"not a flag and its value: $k")
  }

  private def need(f: Map[String, String], k: String): Either[String, String] =
    f.get(k).toRight(s"--$k is required")

  private def exit(result: Either[String, Unit]): Unit = result match {
    case Right(()) => ()
    case Left(why) =>
      System.err.println(s"eval: $why")
      sys.exit(1)
  }
}
