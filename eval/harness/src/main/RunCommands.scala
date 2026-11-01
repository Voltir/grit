package grit.eval.harness.main

import java.nio.file.{Files, Path}
import java.time.ZoneOffset

import scala.util.Try

import grit.core.clock.Clock
import grit.core.either.Traversal
import grit.core.id.WorkflowId
import grit.core.message.{Cost, Tokens}
import grit.core.persona.Persona
import grit.core.triage.Corpora
import grit.dbos.engine.Build
import grit.dbos.sql.DbConfig
import grit.eval.harness.capture.{CaptureJson, Case, Digest, KnowledgeJson, PersonaJson}
import grit.eval.harness.jev.{Budget, Drift, Inputs, Rebuilt, Review, Spend, Variant, Variants}
import grit.eval.harness.log.{Cache, Cached, Footer, Header, LogJson, Outcome, Row, Suite, Weights}
import grit.eval.harness.run.{Call, Repeats, Run}
import grit.eval.harness.score.Spread
import grit.eval.harness.sent.{Cut, HeardSent, SentMarkdown, TurnSent}
import grit.kit.environment.DotEnv
import grit.lifecycle.triage.TriageRecipe
import grit.models.JevClassifier
import grit.models.JevConfig
import grit.turn.Turn

import Cli.*
import CaseFiles.*

/** Jev's run over a capture: `run`, `determinism`, `inputs`, `context`. */
private[main] object RunCommands {

  /** `run --capture <dir> --url <jdbc> --variant <name> --spend <usd> --cache <dir> --runs
    * <dir> [--repeats n (default [[Repeats.Default]])] [--first n] [--rule <file>] [--labels <file>]`, and `cache` false for
    * `--no-cache`: the capture's cases' questions rebuilt, how they changed from capture
    * reported, then asked of Jev under the cap, and the log written to `<runs>/<stamp>-<variant>.jsonl`.
    * The key comes from the environment over `GRIT_ENV_FILE` (`.env` when unset), the
    * database's login from `GRIT_DATABASE_USER` and `_PASSWORD`. The stamp and the header's
    * start are `clock`'s now, and each call's latency is measured on it.
    */
  private[main] def run(
      f: Map[String, String],
      cache: Boolean,
      clock: Clock^
  ): Either[String, Unit] =
    for {
      dir <- need(f, "capture").map(Path.of(_))
      url <- need(f, "url")
      variant <- need(f, "variant").flatMap(variantNamed)
      cap <- need(f, "spend").flatMap(decimal("spend"))
      cacheDir <- need(f, "cache").map(Path.of(_))
      runs <- need(f, "runs").map(Path.of(_))
      repeats <- f.get("repeats").fold(Right(Repeats.Default))(positive("repeats"))
      first <- f.get("first").fold(Right(None))(positive("first")(_).map(Some(_)))
      rule <- f.get("rule").fold(Right(None))(r => read(Path.of(r)).map(t => Some(Digest.text(t))))
      labels <- f
        .get("labels")
        .map(Path.of(_))
        .filter(Files.isRegularFile(_))
        .fold(Right(None))(l => read(l).map(t => Some(Digest.text(t))))
      manifestText <- read(dir.resolve("capture.json"))
      casesText <- read(dir.resolve("cases.jsonl"))
      manifest <- Try(ujson.read(manifestText)).toOption
        .toRight("capture.json: not JSON")
        .flatMap(CaptureJson.readManifest)
      all <- Traversal.traverse(casesText.linesIterator.filter(_.nonEmpty).toVector)(l =>
        Try(ujson.read(l)).toOption
          .toRight("cases.jsonl: a line is not JSON")
          .flatMap(CaptureJson.readCase)
      )
      env <- DotEnv.load(Path.of(sys.env.getOrElse("GRIT_ENV_FILE", ".env")), sys.env)
      jev <- JevConfig.fromEnv(env).left.map(_.message)
      config <- DbConfig.fromEnv(env.updated(DbConfig.UrlVar, url)).left.map(_.message)
      cases = first.fold(all)(n => all.sortBy(c => byTs(c)).take(n))
      rebuilt <- opened(config)(reader =>
        Traversal.traverse(cases)(c =>
          Inputs.rebuild(reader, c, variant, manifest.tuning).map(c -> _)
        )
      )
      model = Variant.model(variant)
      calls = rebuilt.flatMap { (c, r) =>
        (r.triage.map(p => (Suite.Triage, p, r.focus)).toVector ++
          r.stitch.map(p => (Suite.Stitch, p, None)))
          .flatMap((suite, p, focus) =>
            (0 until repeats).map(Call(suite, c.id, _, p.asking, p.request, focus))
          )
      }
      store = if (cache) Cache.at[Vector[Weights]](cacheDir) else Cache.off[Vector[Weights]]
      estimated = Run.estimate(
        calls,
        model,
        k => store.get(k).exists((c: Option[Cached[Vector[Weights]]]) => c.isDefined)
      )
      _ = changes(variant, rebuilt)
      _ = println(
        s"calls: ${calls.size} (${cases.size} cases × $repeats); estimated " +
          s"${Cost.cents(estimated)}, cap ${Cost.cents(cap)}"
      )
      budget <- Budget
        .of(cap, estimated)
        .left
        .map(refused)
      started = clock.now()
      ran = Run(calls, JevClassifier(jev.copy(model = model)), model, store, budget, clock)
      header = Header(
        dir.getFileName.toString,
        Digest.text(manifestText + casesText),
        labels,
        Variant.name(variant),
        Some(Variant.digest(Variant.wording(variant))),
        model,
        Variant.tuning(variant),
        Some(Variant.digest(Variant.recipe(variant))),
        None,
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
        case (c, r @ Row(_, _, _, _, _, _, _, Outcome.Answered(_), _, _, false, _)) => (c, r)
      }
      val reported =
        asked.map((_: Call, r: Row[Vector[Weights]]) => Tokens.value(r.usage.input)).sum
      val estimatedTokens =
        asked
          .map((c: Call, _: Row[Vector[Weights]]) => Spend.tokens(Run.body(model, c.request)))
          .sum
      println(
        s"rows: ${ran.rows.size}; answered ${footer.answered} (cached ${footer.cached}), " +
          s"failed ${footer.failed}, skipped ${footer.skipped}; spent ${Cost.cents(footer.spent)}"
      )
      println(s"input tokens of the calls asked: reported $reported, estimated $estimatedTokens")
      println(s"log: ${out.getFileName}")
    }

  /** `determinism --capture <dir> --log <file>`: how far apart the log's repeats answered,
    * and how far its answers are from what was kept live: the largest gap, and the questions
    * and cases over [[Spread.Tolerance]], by id.
    */
  private[main] def determinism(f: Map[String, String]): Either[String, Unit] =
    for {
      dir <- need(f, "capture").map(Path.of(_))
      logText <- need(f, "log").map(Path.of(_)).flatMap(read)
      log <- LogJson.read[Vector[Weights]](logText.linesIterator.filter(_.nonEmpty).toVector)
      cases <- readCases(dir)
    } yield {
      def show(what: String, s: Spread): Unit = {
        val bySuite = Suite.values.map(x => s"${Suite.written(x)} ${s.spread.count(_._1 == x)}")
        println(
          s"$what: largest ${s.largest}; questions over ${Spread.Tolerance}: ${s.spread.size} " +
            s"of ${s.compared} (${bySuite.mkString(", ")}); cases: ${s.spread.map(_._2).distinct.size}"
        )
        s.spread.foreach((suite, id) => println(s"  ${Suite.written(suite)} ${id.written}"))
      }
      println(
        s"log: ${log.header.variant}, ${log.header.model}, ${log.header.repeats} repeats, " +
          s"${log.rows.size} rows, cache ${if (log.header.cache) "on" else "off"}"
      )
      show("across repeats", Spread.repeats(log.rows))
      show("against live", Spread.live(log.rows, cases))
    }

  /** `context`: one recorded turn's request as sent, text and all, written to `--out` for a
    * person to read; prints counts only.
    */
  private[main] def context(f: Map[String, String], full: Boolean): Either[String, Unit] =
    if (f.contains("entry")) heardContext(f) else turnContext(f, full)

  /** `context` of a heard message (`--entry`): what Jev was asked of it, its triage request
    * worded for the persona and corpora in `--declared` (a capture's directory, its
    * `persona.json` and `knowledge.json`), else grit's and none.
    */
  private[main] def heardContext(f: Map[String, String]): Either[String, Unit] = {
    val declared = f.get("declared").map(Path.of(_))
    def file[A](name: String, none: A, parse: String => Either[String, A]): Either[String, A] =
      declared.map(_.resolve(name)).filter(Files.exists(_)) match {
        case Some(at) => read(at).flatMap(parse).left.map(why => s"$at: $why")
        case None => Right(none)
      }
    for {
      url <- need(f, "url")
      from <- need(f, "from")
      entry <- need(f, "entry").map(grit.core.id.EntryId(_))
      out <- need(f, "out").map(Path.of(_))
      persona <- file("persona.json", Persona.Grit, PersonaJson.read)
      sources <- file("knowledge.json", Corpora.Empty, KnowledgeJson.read)
      config <- DbConfig.fromEnv(sys.env.updated(DbConfig.UrlVar, url)).left.map(_.message)
      heard <- opened(config)(HeardSent.read(_, entry, persona, sources))
      _ <- Try(Files.createDirectories(out.getParent)).toEither.left.map(e =>
        s"${out.getParent}: ${e.getClass.getName}"
      )
      _ <- write(out, SentMarkdown.heard(heard, from))
    } yield {
      println(s"written: $out")
      println(
        s"stitch: ${if (heard.stitch.isDefined) "placement kept" else "none"}; triage: " +
          s"${heard.triage.fold(_ => "not rebuilt", _ => "rebuilt")}, ${heard.asked.size} questions, " +
          s"tags ${if (heard.tags.isDefined) "kept" else "none"}"
      )
    }
  }

  /** `context` of a turn (`--workflow`). */
  private[main] def turnContext(f: Map[String, String], full: Boolean): Either[String, Unit] =
    for {
      url <- need(f, "url")
      from <- need(f, "from")
      workflow <- need(f, "workflow").map(WorkflowId(_))
      out <- need(f, "out").map(Path.of(_))
      config <- DbConfig.fromEnv(sys.env.updated(DbConfig.UrlVar, url)).left.map(_.message)
      sent <- opened(config)(TurnSent.read(_, workflow))
      _ <- Try(Files.createDirectories(out.getParent)).toEither.left.map(e =>
        s"${out.getParent}: ${e.getClass.getName}"
      )
      _ <- write(out, SentMarkdown.render(sent, from, if (full) Cut.Whole else Cut.Default))
    } yield {
      val requests = sent.requests
      val agree = requests.count((_, reply, request) => sent.agrees(reply, request).contains(true))
      println(s"written: $out")
      println(
        s"calls: ${requests.size}, whose estimate agrees with its ledger row: $agree; " +
          s"epoch ${sent.epoch}${
              if (Turn.epochOf(sent.epoch) == Turn.Epoch) "" else s" (this build: ${Turn.Epoch})"
            }"
      )
    }

  /** `inputs --capture <dir> --url <jdbc> --out <file> [--variant <name>]`, and `more` for
    * `--more`: every case's questions rebuilt through the shipped builders ([[Review]]), triage's
    * by the variant's recipe (default `live`'s, the shipped), written to `<file>`, one line
    * a case, in the capture's order, with the database's login from `GRIT_DATABASE_USER` and
    * `_PASSWORD`. The file holds text and is never printed: this prints counts, and how each
    * request's digest compares to the capture's.
    */
  private[main] def inputs(f: Map[String, String], more: Boolean): Either[String, Unit] =
    for {
      dir <- need(f, "capture").map(Path.of(_))
      url <- need(f, "url")
      out <- need(f, "out").map(Path.of(_))
      manifest <- read(dir.resolve("capture.json")).flatMap(t =>
        Try(ujson.read(t)).toOption
          .toRight("capture.json: not JSON")
          .flatMap(CaptureJson.readManifest)
      )
      cases <- readCases(dir)
      variant <- variantNamed(f.getOrElse("variant", Variant.name(Variant.Live)))
      config <- DbConfig.fromEnv(sys.env.updated(DbConfig.UrlVar, url)).left.map(_.message)
      shown <- opened(config)(reader =>
        Traversal.traverse(cases)(c =>
          Review.of(reader, c, manifest.tuning, Variant.recipe(variant), more).map(c -> _)
        )
      )
      _ <- Try(Files.createDirectories(out.getParent)).toEither.left.map(e =>
        s"${out.getParent}: ${e.getClass.getName}"
      )
      _ <- write(out, shown.map((_, s) => Review.line(s).render() + "\n").mkString)
    } yield {
      def compare(suite: String, pairs: Vector[(Option[Digest], Option[Digest])]): Unit = {
        val matched = pairs.count((a, b) => a.isDefined && a == b)
        val differ = pairs.count((a, b) => a.isDefined && b.isDefined && a != b)
        val unbuilt = pairs.count((a, b) => a.isDefined != b.isDefined)
        println(
          s"$suite digests: match $matched, mismatch $differ, built on one side only $unbuilt"
        )
      }
      println(s"written: ${shown.size} lines${if (more) ", with cuts" else ""}")
      compare(
        "triage",
        shown.map((c, s) =>
          (c.asked.map(_.input.request), s.triage.map(t => Digest.request(t.request)))
        )
      )
      compare(
        "stitch",
        shown
          .filter(_._1.stitch.isDefined)
          .map((c, s) =>
            (c.stitch.flatMap(_.input).map(_.request), s.stitch.map(t => Digest.request(t.request)))
          )
      )
    }

  /** The variant named `n`; why not, naming those there are. */
  private[main] def variantNamed(n: String): Either[String, Variant] =
    Variants.named(n).toRight(s"no variant $n: ${Variants.all.map(Variant.name).mkString(", ")}")

  /** Each suite's rebuilt states against the capture's, as counts, and the ids of every case
    * that drifted under the shipped builder: ids only. A suite whose inputs the variant
    * changes (its tuning both, its recipe triage's) reports changes, not drift.
    */
  private[main] def changes(variant: Variant, rebuilt: Vector[(Case, Rebuilt)]): Unit = {
    val tuned = Variant.tuning(variant).isDefined
    def report(suite: String, byVariant: Boolean, drifts: Vector[(Case, Drift)]): Unit = {
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
      tuned || Variant.recipe(variant) != TriageRecipe.Shipped,
      rebuilt.map((c, r) => c -> Drift.of(c.asked.map(_.input.state), r.triage.map(_.state)))
    )
    report(
      "stitch",
      tuned,
      rebuilt
        .filter(_._1.stitch.isDefined)
        .map((c, r) => c -> Drift.of(c.stitch.flatMap(_.input).map(_.state), r.stitch.map(_.state)))
    )
  }

  /** A case's Slack ts as a number, for ordering by when it was said. */
  private[main] def byTs(c: Case): BigDecimal =
    Try(BigDecimal(c.id.written.split('/').last)).getOrElse(BigDecimal(0))
}
