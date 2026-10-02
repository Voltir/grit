package grit.eval.harness.main

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}

import scala.util.Try
import scala.util.control.NonFatal

import grit.core.clock.Clock
import grit.core.message.Tokens
import grit.dbos.engine.{Build, Reader}
import grit.dbos.sql.DbConfig
import grit.eval.harness.corpus.{
  Capture,
  Case,
  CorpusJson,
  Digest,
  Dump,
  Fields,
  Manifest,
  SeenCheck,
  Stitched
}
import grit.eval.harness.jev.{Budget, Drift, Inputs, Rebuilt, Spend, Variant, Variants}
import grit.eval.harness.log.{Cache, Cached, Footer, Header, LogJson, Outcome, Row, Suite, Weights}
import grit.eval.harness.run.{Call, Run}
import grit.kit.environment.DotEnv
import grit.models.JevClassifier
import grit.models.JevConfig

/** The eval harness's command line, run through `scripts/eval`, which says what each command
  * takes. It prints counts, ids and field names, never a message's text.
  */
object Main {

  def main(args: Array[String]): Unit =
    args.toList match {
      case "capture" :: rest => exit(flags(rest).flatMap(capture))
      case "run" :: rest =>
        exit(flags(rest.filterNot(_ == "--no-cache")).flatMap(run(_, !rest.contains("--no-cache"))))
      case _ => exit(Left("usage: scripts/eval capture|run (scripts/eval says what each takes)"))
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

  /** `run --corpus <dir> --url <jdbc> --variant <name> --spend <usd> --cache <dir> --runs
    * <dir> [--repeats n] [--first n] [--rule <file>] [--labels <file>]`, and `cache` false for
    * `--no-cache`: the corpus's cases' questions rebuilt, how they changed from capture
    * reported, then asked of Jev under the cap, and the log written to `<runs>/<stamp>-<variant>.jsonl`.
    * The key comes from the environment over `GRIT_ENV_FILE` (`.env` when unset), the
    * database's login from `GRIT_DATABASE_USER` and `_PASSWORD`.
    */
  private def run(f: Map[String, String], cache: Boolean): Either[String, Unit] =
    for {
      dir <- need(f, "corpus").map(Path.of(_))
      url <- need(f, "url")
      variant <- need(f, "variant").flatMap(n =>
        Variants
          .named(n)
          .toRight(s"no variant $n: ${Variants.all.map(Variant.name).mkString(", ")}")
      )
      cap <- need(f, "spend").flatMap(decimal("spend"))
      cacheDir <- need(f, "cache").map(Path.of(_))
      runs <- need(f, "runs").map(Path.of(_))
      repeats <- f.get("repeats").fold(Right(1))(positive("repeats"))
      first <- f.get("first").fold(Right(None))(positive("first")(_).map(Some(_)))
      rule <- f.get("rule").fold(Right(None))(r => read(Path.of(r)).map(t => Some(Digest.text(t))))
      labels <- f
        .get("labels")
        .map(Path.of(_))
        .filter(Files.isRegularFile(_))
        .fold(Right(None))(l => read(l).map(t => Some(Digest.text(t))))
      manifestText <- read(dir.resolve("corpus.json"))
      casesText <- read(dir.resolve("cases.jsonl"))
      manifest <- Try(ujson.read(manifestText)).toOption
        .toRight("corpus.json: not JSON")
        .flatMap(CorpusJson.readManifest)
      all <- Fields.each(casesText.linesIterator.filter(_.nonEmpty).toVector)(l =>
        Try(ujson.read(l)).toOption
          .toRight("cases.jsonl: a line is not JSON")
          .flatMap(CorpusJson.readCase)
      )
      env <- DotEnv.load(Path.of(sys.env.getOrElse("GRIT_ENV_FILE", ".env")), sys.env)
      jev <- JevConfig.fromEnv(env).left.map(_.message)
      config <- DbConfig.fromEnv(env.updated(DbConfig.UrlVar, url)).left.map(_.message)
      cases = first.fold(all)(n => all.sortBy(c => byTs(c)).take(n))
      rebuilt <- opened(config)(reader =>
        Fields.each(cases)(c => Inputs.rebuild(reader, c, variant, manifest.tuning).map(c -> _))
      )
      model = Variant.model(variant)
      calls = rebuilt.flatMap { (c, r) =>
        (r.triage.map(p => (Suite.Triage, p)).toVector ++ r.stitch.map(p => (Suite.Stitch, p)))
          .flatMap((suite, p) => (0 until repeats).map(Call(suite, c.id, _, p.asking, p.request)))
      }
      store = if (cache) Cache.at[Vector[Weights]](cacheDir) else Cache.off[Vector[Weights]]
      estimated = Run.estimate(
        calls,
        model,
        k => store.get(k).exists((c: Option[Cached[Vector[Weights]]]) => c.isDefined)
      )
      _ = changes(variant, rebuilt)
      _ = println(
        s"calls: ${calls.size} (${cases.size} cases × $repeats); estimated $$$estimated, cap $$$cap"
      )
      budget <- Budget
        .of(cap, estimated)
        .left
        .map(r => s"refused: estimated $$${r.estimated} is over the cap $$${r.cap}")
      started = Instant.now()
      ran = Run(calls, JevClassifier(jev.copy(model = model)), model, store, budget, Clock.system())
      header = Header(
        dir.getFileName.toString,
        Digest.text(manifestText + casesText),
        labels,
        Variant.name(variant),
        Variant.digest(Variant.wording(variant)),
        model,
        Variant.tuning(variant),
        Build.current,
        repeats,
        cache,
        cap,
        rule,
        started
      )
      footer = Footer.of(ran.budget.spent, ran.rows)
      out = runs.resolve(
        s"${Stamp.format(started.atOffset(ZoneOffset.UTC))}-${Variant.name(variant)}.jsonl"
      )
      _ <- Try(Files.createDirectories(runs)).toEither.left.map(e =>
        s"$runs: ${e.getClass.getName}"
      )
      _ <- write(
        out,
        (Vector(LogJson.header(header)) ++ ran.rows.map(LogJson.row(_)) :+ LogJson.footer(footer))
          .map(_ + "\n")
          .mkString
      )
    } yield {
      // The estimate's divisor against what Jev reported, over the calls it answered.
      val asked: Vector[(Call, Row[Vector[Weights]])] = calls.zip(ran.rows).collect {
        case (c, r @ Row(_, _, _, _, _, _, _, Outcome.Answered(_), _, _, false)) => (c, r)
      }
      val reported =
        asked.map((_: Call, r: Row[Vector[Weights]]) => Tokens.value(r.usage.input)).sum
      val estimatedTokens = asked
        .map((c: Call, _: Row[Vector[Weights]]) =>
          Run.body(model, c.request).getBytes(StandardCharsets.UTF_8).length
        )
        .map(b => (b + Spend.BytesPerToken - 1) / Spend.BytesPerToken)
        .sum
      println(
        s"rows: ${ran.rows.size}; answered ${footer.answered} (cached ${footer.cached}), " +
          s"failed ${footer.failed}, skipped ${footer.skipped}; spent $$${footer.spent}"
      )
      println(s"input tokens of the calls asked: reported $reported, estimated $estimatedTokens")
      println(s"log: ${out.getFileName}")
    }

  /** Each suite's rebuilt states against the corpus's, as counts, and the ids of every case
    * that drifted under the shipped builder: ids only.
    */
  private def changes(variant: Variant, rebuilt: Vector[(Case, Rebuilt)]): Unit = {
    val byVariant = Variant.tuning(variant).isDefined
    def report(suite: String, drifts: Vector[(Case, Drift)]): Unit = {
      def n(d: Drift) = drifts.count(_._2 == d)
      val changed =
        if (byVariant) s"changed by the variant ${n(Drift.Changed)}"
        else s"drift ${n(Drift.Changed)}"
      println(
        s"$suite inputs: same ${n(Drift.Same)}, $changed, no longer built ${n(Drift.Unbuilt)}, " +
          s"newly built ${n(Drift.Built)}"
      )
      if (!byVariant)
        drifts
          .collect { case (c, Drift.Changed | Drift.Unbuilt) => c.id.written }
          .foreach(id => println(s"  $suite drift: $id"))
    }
    report(
      "triage",
      rebuilt.map((c, r) => c -> Drift.of(c.asked.map(_.input.state), r.triage.map(_.state)))
    )
    report(
      "stitch",
      rebuilt
        .filter(_._1.stitch.isDefined)
        .map((c, r) => c -> Drift.of(c.stitch.flatMap(_.input).map(_.state), r.stitch.map(_.state)))
    )
  }

  /** A case's Slack ts as a number, for ordering by when it was said. */
  private def byTs(c: Case): BigDecimal =
    Try(BigDecimal(c.id.written.split('/').last)).getOrElse(BigDecimal(0))

  private val Stamp = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss'Z'")

  private def read(path: Path): Either[String, String] =
    Try(Files.readString(path, StandardCharsets.UTF_8)).toEither.left.map(e =>
      s"$path unread: ${e.getClass.getName}"
    )

  private def decimal(what: String)(v: String): Either[String, BigDecimal] =
    Try(BigDecimal(v)).toOption.filter(_ >= 0).toRight(s"--$what $v: not a non-negative number")

  private def positive(what: String)(v: String): Either[String, Int] =
    v.toIntOption.filter(_ > 0).toRight(s"--$what $v: not a positive whole number")

  private def opened[A](
      config: DbConfig
  )(f: Reader^ => Either[String, A]): Either[String, A] =
    try {
      val reader = Reader.open(config)
      try f(reader)
      finally reader.close()
    } catch { case NonFatal(e) => Left(s"the database could not be read: ${e.getClass.getName}") }

  private def report(m: Manifest, cases: Vector[Case]): Unit = {
    val stitches = cases.flatMap(_.stitch)
    val seen = stitches.map(_.seen)
    def count(kind: String)(f: SeenCheck => Boolean) = s"$kind ${seen.count(f)}"
    println(
      "seen check: " + Vector(
        count("match")(_ == SeenCheck.Match),
        count("lexical only")(_.isInstanceOf[SeenCheck.LexicalOnly]),
        count("recent differs")(_.isInstanceOf[SeenCheck.RecentDiffers]),
        count("same root differs")(_.isInstanceOf[SeenCheck.SameRootDiffers]),
        count("message differs")(_.isInstanceOf[SeenCheck.MessageDiffers]),
        count("unbuilt")(_ == SeenCheck.Unbuilt)
      ).mkString(", ")
    )
    // A difference the rebuild should never show, by case id and slot: ids and times only.
    cases.foreach { c =>
      c.stitch.map(_.seen).foreach {
        case SeenCheck.RecentDiffers(ranks) =>
          println(
            s"  recent differs: ${c.id.written} tagged ${c.tagged}, ranks ${ranks.mkString(" ")}"
          )
        case SeenCheck.SameRootDiffers(roots, fields) =>
          println(
            s"  same root differs: ${c.id.written} tagged ${c.tagged}, roots " +
              s"${roots.map(_.written).mkString(" ")} in ${fields.map(SeenCheck.Field.written).mkString(" ")}"
          )
        case SeenCheck.MessageDiffers(fields) =>
          println(
            s"  message differs: ${c.id.written} in ${fields.map(SeenCheck.Field.written).mkString(" ")}"
          )
        case _ => ()
      }
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
