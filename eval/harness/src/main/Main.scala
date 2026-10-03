package grit.eval.harness.main

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}

import scala.collection.immutable.VectorMap
import scala.util.Try
import scala.util.chaining.*
import scala.util.control.NonFatal

import grit.core.classify.Answer
import grit.core.clock.{Clock, Fresh}
import grit.core.context.Width
import grit.core.id.{QuestionName, ShadowName, WorkflowId}
import grit.core.message.Tokens
import grit.core.provider.Provider
import grit.core.triage.KnowledgeSources
import grit.dbos.engine.{Build, Reader}
import grit.dbos.sql.DbConfig
import grit.eval.harness.corpus.{
  Capture,
  Case,
  CaseId,
  CorpusJson,
  Digest,
  Dump,
  Ended,
  Fields,
  Live,
  Manifest,
  Said,
  SeenCheck,
  Stitched,
  TurnCapture,
  TurnCase,
  TurnJson,
  Turns
}
import grit.eval.harness.jev.{
  Budget,
  Drift,
  Inputs,
  QuestionSet,
  Rebuilt,
  Review,
  Sets,
  Spend,
  Variant,
  Variants
}
import grit.eval.harness.label.{Labels, ReplyLabels, Verdicts}
import grit.eval.harness.log.{
  Cache,
  Cached,
  Codec,
  Footer,
  Header,
  Log,
  LogJson,
  Outcome,
  Row,
  Suite,
  Weights
}
import grit.eval.harness.pull.{Pull, ShadowLog}
import grit.eval.harness.reply.{Assembled, Queries, Rebuild, ReplyReview, WindowOnly}
import grit.eval.harness.report.{Report, Scored}
import grit.eval.harness.run.{Call, Repeats, Run}
import grit.eval.harness.score.{
  Decision,
  Drafting,
  Drafts,
  Guarded,
  Judgement,
  MovedOn,
  Order,
  Paired,
  Rule,
  Scoring,
  Spread,
  Target
}
import grit.kit.environment.DotEnv
import grit.lifecycle.triage.TriageRecipe
import grit.models.JevClassifier
import grit.models.JevConfig
import grit.models.{OpenRouterConfig, OpenRouterProvider, Seed}

/** The eval harness's command line, run through `scripts/eval`, which says what each command
  * takes. It prints counts, ids and field names, never a message's text.
  */
object Main {

  def main(args: Array[String]): Unit =
    args.toList match {
      case "capture" :: rest => exit(flags(rest).flatMap(capture))
      case "run" :: rest =>
        exit(flags(rest.filterNot(_ == "--no-cache")).flatMap(run(_, !rest.contains("--no-cache"))))
      case "determinism" :: rest => exit(flags(rest).flatMap(determinism))
      case "inputs" :: rest =>
        exit(flags(rest.filterNot(_ == "--more")).flatMap(inputs(_, rest.contains("--more"))))
      case "score" :: rest => exit(flags(rest).flatMap(score))
      case "order" :: rest => exit(flags(rest).flatMap(order))
      case "pull" :: rest => exit(flags(rest).flatMap(pull))
      case "replies" :: rest =>
        val switches = Set("--records", "--show")
        exit(
          flags(rest.filterNot(switches)).flatMap(
            replies(
              _,
              rest.contains("--records"),
              rest.contains("--show"),
              Clock.system(),
              Fresh.random()
            )
          )
        )
      case "reply-labels" :: rest => exit(flags(rest).flatMap(replyLabels))
      case "turns" :: rest => exit(flags(rest).flatMap(turns))
      case "rebuild" :: rest => exit(flags(rest).flatMap(rebuild))
      case "compare" :: rest =>
        flags(rest).flatMap(f =>
          if (f.contains("gate")) drafts(f).map(_ => 0) else compare(f)
        ) match {
          case Right(0) => ()
          case Right(code) => sys.exit(code)
          case Left(why) => exit(Left(why))
        }
      case _ =>
        exit(
          Left(
            "usage: scripts/eval capture|run|determinism|inputs|score|compare|order|pull|" +
              "replies|reply-labels|turns|rebuild " +
              "(scripts/eval says what each takes)"
          )
        )
    }

  /** `capture --url <jdbc> --source <db> --restored <db> --sha256 <hex> --at <instant> --out
    * <dir>`: the corpus of the restored database, written to `corpus.json` and `cases.jsonl` in
    * `<dir>`, its recorded turns to `turns.jsonl` ([[TurnCapture]]) and the verdicts standing
    * on reviewed messages to `verdicts.json` ([[Pull.verdicts]]), with the database's login
    * from `GRIT_DATABASE_USER` and `_PASSWORD`.
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
      read <- opened(config)(reader =>
        for {
          captured <- Capture(reader, source, restored, Dump(sha, at), Build.current)
          turns <- TurnCapture(reader, Dump(sha, at))
          standing <- Pull.verdicts(reader, Instant.EPOCH)
        } yield (captured, turns, standing)
      )
      (captured, turns, standing) = read
      _ <- write(
        out.resolve("cases.jsonl"),
        captured.cases.map(CorpusJson.writeCase(_).render() + "\n").mkString
      )
      _ <- write(
        out.resolve("turns.jsonl"),
        turns.cases.map(TurnJson.write(_).render() + "\n").mkString
      )
      _ <- write(out.resolve("verdicts.json"), Verdicts.written(standing.verdicts))
      _ <- write(
        out.resolve("corpus.json"),
        CorpusJson.writeManifest(captured.manifest).render(2) + "\n"
      )
    } yield {
      report(captured.manifest, captured.cases)
      reportTurns(turns)
      println(s"verdicts standing: ${standing.verdicts.cases.size}")
    }

  /** The turns captured, as counts: by where their message was said and their root, by how
    * they ended, and how many were skipped.
    */
  private def reportTurns(turns: Turns): Unit = {
    def said(t: TurnCase) = t.said match {
      case Said.Slack(_) => "slack"
      case Said.Tui(_) => "tui"
      case Said.Task(_) => "task"
    }
    def ended(t: TurnCase) = t.ended match {
      case Ended.Replied(_, true) => "passed"
      case Ended.Replied(_, false) => "replied"
      case Ended.Failed(step, why) => s"failed ($step, ${why.toString.toLowerCase})"
      case Ended.Unfinished(status) => s"unfinished (${status.toLowerCase})"
    }
    def counts(f: TurnCase => String) = {
      val keys = turns.cases.map(f)
      keys.distinct.sorted.map(k => s"$k ${keys.count(_ == k)}").mkString(", ")
    }
    println(s"turns: ${turns.cases.size}, skipped ${turns.skipped}")
    println(s"  by root: ${counts(t => s"${said(t)} ${t.root.toString.toLowerCase}")}")
    println(s"  ended: ${counts(ended)}")
    println(s"  with a tool loop: ${turns.cases.count(_.rounds.nonEmpty)}")
  }

  /** `replies --corpus <dir> --url <jdbc> --out <dir> --labels <file> (--cases <id,...> |
    * --pick <n> [--by <order>, default random])`, with `records` for `--records` and `show`
    * for `--show`: a review of the corpus's turns ([[ReplyReview]]), its text read from the
    * restored database, written to `<out>/replies-<corpus>-<stamp>.md`, the stamp `clock`'s
    * now; a random order is seeded from `fresh`. A pick leaves out the turns `<file>`
    * (`reply-labels.json`; none when it does not exist) labels. Prints the file's path, and
    * each case's workflow and record count; the file's text only with `show`.
    */
  private def replies(
      f: Map[String, String],
      records: Boolean,
      show: Boolean,
      clock: Clock^,
      fresh: Fresh^
  ): Either[String, Unit] =
    for {
      dir <- need(f, "corpus").map(Path.of(_))
      url <- need(f, "url")
      out <- need(f, "out").map(Path.of(_))
      labelled <- need(f, "labels").map(Path.of(_)).flatMap(replyLabelsAt)
      turns <- readTurns(dir)
      ask <- (f.get("cases"), f.get("pick")) match {
        case (Some(ids), None) if !f.contains("by") =>
          Right(ReplyReview.Ask.Named(ids.split(",").toVector.filter(_.nonEmpty)))
        case (None, Some(n)) =>
          for {
            count <- n.toIntOption.toRight(s"--pick $n: not a whole number")
            by <- ReplyReview.By.named(f.getOrElse("by", "random"), fresh)
          } yield ReplyReview.Ask.Pick(count, by)
        case _ => Left("give --cases, or --pick with or without --by")
      }
      picked <- ReplyReview.pick(turns, labelled, ask)
      at = clock.now()
      corpus = Option(dir.getFileName).fold("")(_.toString)
      config <- DbConfig.fromEnv(sys.env.updated(DbConfig.UrlVar, url)).left.map(_.message)
      review <- opened(config)(reader =>
        ReplyReview.review(corpus, picked, at)(t => ReplyReview.read(reader, t, records))
      )
      path = out.resolve(s"replies-$corpus-${Stamp.format(at.atOffset(ZoneOffset.UTC))}.md")
      _ <- Try(Files.createDirectories(out)).toEither.left.map(e => s"$out: ${e.getClass.getName}")
      text = ReplyReview.render(review)
      _ <- write(path, text)
    } yield {
      println(s"written: $path, ${review.cases.size} cases")
      review.cases.foreach(c =>
        println(s"  ${WorkflowId.value(c.turn.workflow)}: ${c.records.size} records")
      )
      if (show) print(text)
    }

  /** `reply-labels --file <review file> --labels <file>`: the labels the filled review file
    * holds ([[ReplyReview.labels]]) merged into `<file>` (`reply-labels.json`, created when it
    * does not exist), each in place of an earlier label of the same turn. Prints counts.
    */
  private def replyLabels(f: Map[String, String]): Either[String, Unit] =
    for {
      file <- need(f, "file").map(Path.of(_))
      at <- need(f, "labels").map(Path.of(_))
      text <- read(file)
      filled <- ReplyReview.labels(text)
      before <- replyLabelsAt(at)
      after = before.merged(filled)
      _ <- write(at, ReplyLabels.written(after))
    } yield println(
      s"labels read: ${filled.size}, replacing ${filled.count((w, _) => before.turns.contains(w))}; " +
        s"turns labelled: ${after.turns.size}"
    )

  /** The reply labels at `path`; none when it does not exist. */
  private def replyLabelsAt(path: Path): Either[String, ReplyLabels] =
    if (!Files.exists(path)) Right(ReplyLabels.Empty)
    else read(path).flatMap(ReplyLabels.read)

  /** `turns --eval <dir> --corpus <yyyymmdd>`: the corpus's recorded turns read structurally,
    * beside the verdicts standing in its `verdicts.json` (none when it has no such file),
    * written to `<dir>/reports/turns-<yyyymmdd>.md` and printed.
    */
  private def turns(f: Map[String, String]): Either[String, Unit] =
    for {
      dir <- need(f, "eval").map(Path.of(_))
      corpus <- need(f, "corpus").filterOrElse(_.matches("[0-9]{8}"), "--corpus is yyyymmdd")
      at = dir.resolve("corpus").resolve(corpus)
      captured <- readTurns(at)
      standing <- {
        val file = at.resolve("verdicts.json")
        if (!Files.exists(file)) Right(Verdicts.Empty) else read(file).flatMap(Verdicts.read)
      }
      _ <- publish(dir, s"turns-$corpus", Report.turns(corpus, captured, standing))
    } yield ()

  /** `rebuild --corpus <dir> --url <jdbc> --cache <dir> --spend <usd> [--window <tokens>]
    * [--tail <tokens>]`: every turn the restored database recorded before its dump, its window
    * rebuilt as of its assembly by the shipped assembler ([[Rebuild.recorded]], its query
    * replayed) and set against the one it recorded ([[Rebuild.report]]); then every window-only
    * case ([[WindowOnly]]), its query written by the seed catalog's query model through
    * OpenRouter, each answer kept under `<cache>` so it is paid for once, under the `--spend`
    * cap in USD: refused before any call when the writes not kept are estimated over it
    * ([[Queries.Usd]] each), and each call past it skipped. The assembler is built with grit's
    * own window and tail ([[Assembled.Shipped]]) unless `--window` or `--tail` names the
    * deployment's. OpenRouter's key comes from the environment over `GRIT_ENV_FILE` (`.env`
    * when unset), read only when a write is to be asked; the database's login from
    * `GRIT_DATABASE_USER` and `_PASSWORD`. Prints counts and workflow ids, never text.
    */
  private def rebuild(f: Map[String, String]): Either[String, Unit] =
    for {
      dir <- need(f, "corpus").map(Path.of(_))
      url <- need(f, "url")
      cap <- need(f, "spend").flatMap(decimal("spend"))
      cacheDir <- need(f, "cache").map(Path.of(_))
      store = Cache.at[String](cacheDir)(using Queries.text)
      window <- f
        .get("window")
        .fold(Right(Assembled.Shipped.window))(positive("window")(_).map(Tokens(_)))
      tail <- f.get("tail").fold(Right(Assembled.Shipped.tail))(positive("tail")(_).map(Tokens(_)))
      assembled = Assembled.Shipped.copy(window = window, tail = tail)
      manifest <- read(dir.resolve("corpus.json")).flatMap(t =>
        Try(ujson.read(t)).toOption
          .toRight("corpus.json: not JSON")
          .flatMap(CorpusJson.readManifest)
      )
      until = manifest.dump.at
      env <- DotEnv.load(Path.of(sys.env.getOrElse("GRIT_ENV_FILE", ".env")), sys.env)
      config <- DbConfig.fromEnv(env.updated(DbConfig.UrlVar, url)).left.map(_.message)
      _ <- opened(config) { reader =>
        for {
          workflows <- reader.turns(until).left.map(_ => "turn workflows unread")
          rebuilt = workflows.map((w, _) =>
            w -> Rebuild.recorded(reader, w, assembled, Width.Deployed)
          )
          only <- WindowOnly.all(reader, until)
          asked <- Fields.each(only)(w =>
            WindowOnly.asked(reader, w, assembled, Width.Deployed).map(w -> _)
          )
          requests = asked.flatMap(_._2)
          pins <- Seed.catalog.map(_.pin)
          // The writer's settings name its model, so they key its answers; the key itself is
          // read only when a write is to be asked.
          keyless = OpenRouterConfig.of("", pins.query)
          estimated = Queries.estimate(requests, keyless, store)
          _ = println(
            s"window-only cases: ${only.size}; queries to write: ${requests.distinct.size}, " +
              s"estimated $$$estimated, cap $$$cap"
          )
          budget <- Budget
            .of(cap, estimated)
            .left
            .map(r => s"refused: estimated $$${r.estimated} is over the cap $$${r.cap}")
          writer <-
            if (estimated == 0) Right(None)
            else
              OpenRouterConfig
                .key(env)
                .left
                .map(_.message)
                .map(k => Some(OpenRouterConfig.of(k, pins.query)))
          answered = {
            val asking: Provider^ = writer match {
              case Some(c) => new OpenRouterProvider(c)
              case None => Queries.unasked()
            }
            Queries.answer(requests, keyless, store, asking, budget, Clock.system())
          }
          windows = asked.map((w, _) =>
            w -> WindowOnly.window(reader, w, assembled, answered.answers, Width.Deployed)
          )
        } yield {
          Rebuild.report(rebuilt).foreach(println)
          println(
            s"queries: asked ${answered.asked}, cached ${answered.cached}, skipped " +
              s"${answered.skipped}, failed ${answered.failed}; spent $$${answered.budget.spent}"
          )
          val built = windows.flatMap(_._2.toOption)
          println(
            s"window-only windows: built ${built.size}, failed ${windows.count(_._2.isLeft)}; " +
              s"with sections from elsewhere ${built.count(_.nearby.nonEmpty)}, " +
              s"own entries ${built.map(_.entries.size).sum}"
          )
          windows.foreach {
            case (w, Left(why)) => println(s"  not built: ${WindowOnly.id(w)}: $why")
            case (_, Right(_)) => ()
          }
        }
      }
    } yield ()

  /** The turns of the corpus in `dir`, from `turns.jsonl`. */
  private def readTurns(dir: Path): Either[String, Vector[TurnCase]] =
    read(dir.resolve("turns.jsonl")).flatMap(text =>
      Fields.each(text.linesIterator.filter(_.nonEmpty).toVector)(l =>
        Try(ujson.read(l)).toOption
          .toRight("turns.jsonl: a line is not JSON")
          .flatMap(TurnJson.read)
      )
    )

  /** `run --corpus <dir> --url <jdbc> --variant <name> --spend <usd> --cache <dir> --runs
    * <dir> [--repeats n (default [[Repeats.Default]])] [--first n] [--rule <file>] [--labels <file>]`, and `cache` false for
    * `--no-cache`: the corpus's cases' questions rebuilt, how they changed from capture
    * reported, then asked of Jev under the cap, and the log written to `<runs>/<stamp>-<variant>.jsonl`.
    * The key comes from the environment over `GRIT_ENV_FILE` (`.env` when unset), the
    * database's login from `GRIT_DATABASE_USER` and `_PASSWORD`.
    */
  private def run(f: Map[String, String], cache: Boolean): Either[String, Unit] =
    for {
      dir <- need(f, "corpus").map(Path.of(_))
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
          s"failed ${footer.failed}, skipped ${footer.skipped}; spent $$${footer.spent}"
      )
      println(s"input tokens of the calls asked: reported $reported, estimated $estimatedTokens")
      println(s"log: ${out.getFileName}")
    }

  /** `determinism --corpus <dir> --log <file>`: how far apart the log's repeats answered,
    * and how far its answers are from what was kept live: the largest gap, and the questions
    * and cases over [[Spread.Tolerance]], by id.
    */
  private def determinism(f: Map[String, String]): Either[String, Unit] =
    for {
      dir <- need(f, "corpus").map(Path.of(_))
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

  /** `inputs --corpus <dir> --url <jdbc> --out <file> [--variant <name>]`, and `more` for
    * `--more`: every case's questions rebuilt through the shipped builders ([[Review]]), triage's
    * by the variant's recipe (default `live`'s, the shipped), written to `<file>`, one line
    * a case, in the corpus's order, with the database's login from `GRIT_DATABASE_USER` and
    * `_PASSWORD`. The file holds text and is never printed: this prints counts, and how each
    * request's digest compares to the corpus's.
    */
  private def inputs(f: Map[String, String], more: Boolean): Either[String, Unit] =
    for {
      dir <- need(f, "corpus").map(Path.of(_))
      url <- need(f, "url")
      out <- need(f, "out").map(Path.of(_))
      manifest <- read(dir.resolve("corpus.json")).flatMap(t =>
        Try(ujson.read(t)).toOption
          .toRight("corpus.json: not JSON")
          .flatMap(CorpusJson.readManifest)
      )
      cases <- readCases(dir)
      variant <- variantNamed(f.getOrElse("variant", Variant.name(Variant.Live)))
      config <- DbConfig.fromEnv(sys.env.updated(DbConfig.UrlVar, url)).left.map(_.message)
      shown <- opened(config)(reader =>
        Fields.each(cases)(c =>
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

  /** `score --eval <dir> --run <name> [--labels <file>]`: the run `<dir>/runs/<name>` scored
    * against its corpus (`<dir>/corpus/<its corpus>`) and the labels in `<file>` (default
    * `<dir>/labels.json`; none when it does not exist), written to `<dir>/reports/<name less
    * .jsonl>.md` and printed.
    */
  private def score(f: Map[String, String]): Either[String, Unit] =
    for {
      dir <- need(f, "eval").map(Path.of(_))
      labels <- labelsAt(dir, f)
      run <- need(f, "run").flatMap(scored(dir, _, labels))
      _ <- publish(dir, run.name.stripSuffix(".jsonl"), Report.score(run))
    } yield ()

  /** `compare --eval <dir> --a <name> --b <name> [--labels <file>] [--decide <rule file>]
    * [--replica <name> --spread <name>]`: run B against run A, both in `<dir>/runs/`, written
    * to `<dir>/reports/<A>-vs-<B>.md` (each name less `.jsonl`) and printed; the labels as for
    * `score`. With `--replica` (a pulled replica shadow's log) and `--spread` (a run with
    * repeats), A being live triage's kept log: the cases whose state moved on ([[MovedOn]])
    * left out of both, under the noise measured in the `--spread` run. With `--decide`, what
    * the rule makes of them too, and the exit code says it: 0 adopt B, 3 refused, 4 keep A.
    */
  private def compare(f: Map[String, String]): Either[String, Int] =
    for {
      dir <- need(f, "eval").map(Path.of(_))
      labels <- labelsAt(dir, f)
      runA <- need(f, "a").flatMap(scored(dir, _, labels))
      runB <- need(f, "b").flatMap(scored(dir, _, labels))
      replica <- f.get("replica").fold(Right(None))(n => scored(dir, n, labels).map(Some(_)))
      noise <- f
        .get("spread")
        .fold(Right(None))(n =>
          scored(dir, n, labels).flatMap(s =>
            MovedOn.noise(s.log.rows).map(Some(_)).toRight(s"--spread $n: no case answered twice")
          )
        )
      movedOn <- (replica, noise) match {
        case (Some(r), Some(n)) => Right(Some(MovedOn.of(r.answers, runA.answers, n)))
        case (None, None) => Right(None)
        case _ => Left("--replica and --spread are given together")
      }
      gone = movedOn.fold(Set.empty[CaseId])(_.cases.toSet)
      (a, b) = (runA.without(gone), runB.without(gone))
      rule <- f
        .get("decide")
        .fold(Right(None))(r => read(Path.of(r)).flatMap(Rule.read).map(Some(_)))
      decided = rule.map { r =>
        val (sa, sb) =
          (Scoring(a.cases, a.answers, labels), Scoring(b.cases, b.answers, labels))
        r -> Decision.of(r, b.log.header, Paired.of(r, sa, sb), Guarded.of(r, sa, sb))
      }
      _ <- publish(
        dir,
        s"${a.name.stripSuffix(".jsonl")}-vs-${b.name.stripSuffix(".jsonl")}",
        Report.compare(runA, runB, decided, movedOn)
      )
    } yield decided.fold(0)(_._2 match {
      case Decision.Adopted(_) => 0
      case Decision.Refused(_) => 3
      case Decision.Kept(_) => 4
    })

  /** `compare --eval <dir> --a <name> --b <name> --live <set> --gate <set> [--verdicts <name>]`:
    * run B, a pulled question set's shadow log asking the `--gate` set ([[Sets]]), against A,
    * live triage's pulled log asking the `--live` set, both in `<dir>/runs/`: each one's draft
    * by its own set's gate, by focus ([[Drafts]]), written to `<dir>/reports/<A>-vs-<B>.md`
    * (each name less `.jsonl`) and printed; the ids of the cases each decided alone, one a
    * line, to `<dir>/order/<yyyymmdd>-draft-<set>-live-only.txt` and `…-set-only.txt`, for
    * labelling. A live log an earlier build pulled by position is read under v1's names
    * ([[Log.named]]). With `--verdicts <name>`, a verdicts file `pull` wrote in `<dir>/runs/`
    * ([[Verdicts]]), each pick reason's verdicts against both drafts and each side's `to`
    * ([[Judgement]]); without it, the report says no verdicts were given. `--helps-at`,
    * `--decide`, `--replica` and `--spread` are refused beside it.
    */
  private def drafts(f: Map[String, String]): Either[String, Unit] =
    for {
      _ <- Vector("decide", "replica", "spread")
        .find(f.contains)
        .map(k => s"--$k is not taken with --gate")
        .toLeft(())
      _ <- Either.cond(
        !f.contains("helps-at"),
        (),
        "--helps-at is gone: name the set live triage asked with --live (its gate is the set's)"
      )
      dir <- need(f, "eval").map(Path.of(_))
      liveSet <- need(f, "live").flatMap(setNamed)
      set <- need(f, "gate").flatMap(setNamed)
      aName <- need(f, "a")
      a <- liveLog(dir, aName)
      bName <- need(f, "b")
      bText <- read(dir.resolve("runs").resolve(bName))
      b <- LogJson
        .read[VectorMap[QuestionName, Answer]](bText.linesIterator.filter(_.nonEmpty).toVector)(
          using LogJson.named
        )
        .left
        .map(e => s"$bName: not a question set's log: $e")
      (live, shadow) = (drafting(a, liveSet), drafting(b, set))
      found = Drafts.of(live, shadow)
      verdicts <- f
        .get("verdicts")
        .fold[Either[String, Option[Verdicts]]](Right(None))(n =>
          read(dir.resolve("runs").resolve(n)).flatMap(Verdicts.read).map(Some(_))
        )
      judged = verdicts.map(Judgement.of(live, shadow, _))
      day = Day.format(Instant.now().atOffset(ZoneOffset.UTC))
      alone = Vector(
        s"$day-draft-${set.name}-live-only.txt" -> found.gate.values.toVector.flatMap(_.aOnly),
        s"$day-draft-${set.name}-set-only.txt" -> found.gate.values.toVector
          .flatMap(_.bOnly)
      )
      order = dir.resolve("order")
      _ <- Try(Files.createDirectories(order)).toEither.left.map(e =>
        s"$order: ${e.getClass.getName}"
      )
      _ <- alone.foldLeft[Either[String, Unit]](Right(()))((done, named) =>
        done.flatMap(_ =>
          write(order.resolve(named._1), named._2.sorted.map(_.written + "\n").mkString)
        )
      )
      _ <- publish(
        dir,
        s"${aName.stripSuffix(".jsonl")}-vs-${bName.stripSuffix(".jsonl")}",
        Report.drafts(
          Report.Side(aName, a, liveSet.name, live.gate),
          Report.Side(bName, b, set.name, shadow.gate),
          found,
          judged
        )
      )
    } yield alone.foreach((name, ids) => println(s"order: ${ids.size} cases to order/$name"))

  /** The question set named `n`; why not, naming those there are. */
  private def setNamed(n: String): Either[String, QuestionSet] =
    Sets.named(n).toRight(s"no set $n: ${Sets.all.map(_.name).mkString(", ")}")

  /** `log`'s rows as a side of a draft comparison asking `set`. */
  private def drafting(log: Log[VectorMap[QuestionName, Answer]], set: QuestionSet): Drafting =
    Drafting(log.rows, set.questions.speak, set.durable, set.to)

  /** Live triage's pulled log `<dir>/runs/<name>`, under its questions' names; one an earlier
    * build pulled by position, under v1's.
    */
  private def liveLog(
      dir: Path,
      name: String
  ): Either[String, Log[VectorMap[QuestionName, Answer]]] =
    read(dir.resolve("runs").resolve(name)).flatMap { text =>
      val lines = text.linesIterator.filter(_.nonEmpty).toVector
      LogJson
        .read[VectorMap[QuestionName, Answer]](lines)(using LogJson.named)
        .orElse(
          LogJson
            .read[Vector[Weights]](lines)
            .map(Log.named(_, Sets.V1.questions.questions(KnowledgeSources.Empty)))
        )
        .left
        .map(e => s"$name: not live triage's log, under names or by position: $e")
    }

  /** `order --eval <dir> --a <name> [--b <name>] [--tag <question>] [--by spread|difference]`:
    * the case ids in labelling order ([[Order]]) on `<question>` (`kind`, a tag's name, or
    * `place`; default `durable`): by the difference between runs A and B (the default when `--b`
    * is given), or by run A's repeat spread (the default without), written one a line to
    * `<dir>/order/<yyyymmdd>-<question>-<by>.txt`. Prints how many, and how many unlabelled
    * lead.
    */
  private def order(f: Map[String, String]): Either[String, Unit] =
    for {
      dir <- need(f, "eval").map(Path.of(_))
      labels <- labelsAt(dir, f)
      q <- f
        .getOrElse("tag", "durable")
        .pipe(t => Target.read(t).toRight(s"--tag $t: not kind, place or a tag"))
      a <- need(f, "a").flatMap(scored(dir, _, labels))
      b <- f.get("b").fold(Right(None))(n => scored(dir, n, labels).map(Some(_)))
      how = f.getOrElse("by", if (b.isDefined) "difference" else "spread")
      by <- how match {
        case "spread" => Right(Order.spread(q, a.cases, a.answers, labels))
        case "difference" =>
          b.toRight("--by difference needs --b")
            .map(b => Order.difference(q, a.answers, b.answers, labels))
        case other => Left(s"--by $other: not spread or difference")
      }
      name = s"${Day.format(Instant.now().atOffset(ZoneOffset.UTC))}-" +
        s"${Target.written(q)}-$how.txt"
      out = dir.resolve("order").resolve(name)
      _ <- Try(Files.createDirectories(out.getParent)).toEither.left.map(e =>
        s"${out.getParent}: ${e.getClass.getName}"
      )
      _ <- write(out, by.map(_.written + "\n").mkString)
    } yield println(
      s"order: ${by.size} cases on ${Target.written(q)}, " +
        s"${by.count(id => !Target.labelled(q, labels.of(id)))} not yet labelled on it first; " +
        s"written to order/$name"
    )

  /** `pull --corpus <dir> --url <jdbc> --runs <dir> --shadows <name,…> [--since <instant>]`:
    * what the database kept of the heard messages tagged since `<instant>` (default: ever) and
    * the corpus in `<dir>` holds, as run logs ([[Pull]]): `<runs>/live-<yyyymmdd>.jsonl`, live
    * triage's tags, and `<runs>/shadow-<name>-<yyyymmdd>.jsonl` for each shadow named, with
    * the database's login from `GRIT_DATABASE_USER` and `_PASSWORD`; and
    * `<runs>/verdicts-<yyyymmdd>.json`, the verdicts standing on the messages a review
    * considered since `<instant>` ([[Pull.verdicts]], [[Verdicts.written]]). Prints counts, and
    * the ids of the messages no corpus holds yet, for the next capture.
    */
  private def pull(f: Map[String, String]): Either[String, Unit] =
    for {
      dir <- need(f, "corpus").map(Path.of(_))
      url <- need(f, "url")
      runs <- need(f, "runs").map(Path.of(_))
      names <- need(f, "shadows").flatMap(n =>
        Fields.each(n.split(',').toVector.filter(_.nonEmpty))(x =>
          ShadowName.of(x).left.map(w => s"--shadows $x: $w")
        )
      )
      since <- f
        .get("since")
        .fold(Right(Instant.EPOCH))(a =>
          Try(Instant.parse(a)).toOption.toRight(s"--since $a: not an instant")
        )
      manifest <- read(dir.resolve("corpus.json"))
      casesText <- read(dir.resolve("cases.jsonl"))
      cases <- readCases(dir)
      config <- DbConfig.fromEnv(sys.env.updated(DbConfig.UrlVar, url)).left.map(_.message)
      at = Instant.now()
      got <- opened(config)(reader =>
        for {
          pulled <- Pull(
            reader,
            dir.getFileName.toString,
            Digest.text(manifest + casesText),
            cases,
            names,
            since,
            at
          )
          standing <- Pull.verdicts(reader, since)
        } yield (pulled, standing)
      )
      (pulled, standing) = got
      day = Day.format(at.atOffset(ZoneOffset.UTC))
      verdicts = s"verdicts-$day.json"
      logs = Pulled(s"live-$day.jsonl", lines(pulled.live)(using LogJson.named)) +:
        pulled.shadows.flatMap { (s: Pull.Pulled.Shadow) =>
          val name = s"shadow-${ShadowName.value(s.name)}-$day.jsonl"
          s.log match {
            case ShadowLog.Worded(log) => Some(Pulled(name, lines(log)))
            case ShadowLog.Named(log) => Some(Pulled(name, lines(log)(using LogJson.named)))
            case ShadowLog.Mixed(_, _) => None
          }
        }
      _ <- Try(Files.createDirectories(runs)).toEither.left.map(e =>
        s"$runs: ${e.getClass.getName}"
      )
      _ <- logs.foldLeft[Either[String, Unit]](Right(()))((done, p: Pulled) =>
        done.flatMap(_ => write(runs.resolve(p.name), p.lines))
      )
      _ <- write(runs.resolve(verdicts), Verdicts.written(standing.verdicts))
    } yield {
      def counted[A](log: Log[A]): String = {
        val footer = log.footer.getOrElse(Footer.of(BigDecimal(0), log.rows))
        s"${log.rows.size} rows: answered ${footer.answered}, failed ${footer.failed}; " +
          s"spent $$${footer.spent}"
      }
      println(
        s"live (${Pull.Kept}): questions " +
          pulled.live.header.questions
            .fold("none answered")(_.map(QuestionName.value).mkString(", ")) +
          s"; ${counted(pulled.live)}; answered under other questions, left out: " +
          s"${pulled.renamed} (pull --since when live triage's question set changed); corpus " +
          s"cases with no rebuilt question, left out: ${pulled.unbuilt}"
      )
      pulled.shadows.foreach { s =>
        val kept = s.log match {
          case ShadowLog.Worded(log) => counted(log)
          case ShadowLog.Named(log) =>
            s"a question set's, ${log.header.questions.fold(0)(_.size)} questions; ${counted(log)}"
          case ShadowLog.Mixed(worded, named) =>
            s"refused, no log: $worded of its rows are a wording's and $named a question " +
              "set's; a shadow's name asks one question for its life, so a changed one is " +
              "declared under a new name"
        }
        println(
          s"shadow ${ShadowName.value(s.name)}: $kept; tagged since with no row: ended " +
            s"keeping nothing ${s.ended}, not yet shadowed ${s.waiting}"
        )
      }
      println(s"not in the corpus, for the next capture: ${pulled.uncaptured.size}")
      pulled.uncaptured.foreach(id => println(s"  ${id.written}"))
      println(
        s"verdicts standing: ${standing.verdicts.cases.size}; on a message no case id names, " +
          s"left out: ${standing.unnamed}"
      )
      println(s"written: ${(logs.map((p: Pulled) => p.name) :+ verdicts).mkString(", ")}")
    }

  /** A pulled log's file `name`, and its `lines`. */
  private final case class Pulled(name: String, lines: String)

  /** `log` as its file's lines: header, rows, then its footer when it has one. */
  private def lines[A: Codec](log: Log[A]): String =
    (Vector(LogJson.header(log.header)) ++
      log.rows.map((r: Row[A]) => LogJson.row(r)) ++
      log.footer.map(LogJson.footer).toVector).map(_ + "\n").mkString

  /** The labels `--labels` names, else `<dir>/labels.json`; none when that file does not exist. */
  private def labelsAt(dir: Path, f: Map[String, String]): Either[String, Labels] = {
    val file = f.get("labels").fold(dir.resolve("labels.json"))(Path.of(_))
    if (Files.isRegularFile(file)) read(file).flatMap(Labels.read) else Right(Labels.Empty)
  }

  /** The run `<dir>/runs/<name>`, with its corpus's cases and `labels`; a corpus whose files
    * changed since the run is reported, not refused. A log of answers under their names, as
    * pull writes live triage's and a question set's shadow's, is refused, naming `--live`.
    */
  private def scored(dir: Path, name: String, labels: Labels): Either[String, Scored] =
    for {
      text <- read(dir.resolve("runs").resolve(name))
      lines = text.linesIterator.filter(_.nonEmpty).toVector
      log <- LogJson
        .read[Vector[Weights]](lines)
        .left
        .map(why =>
          LogJson
            .read[VectorMap[QuestionName, Answer]](lines)(using LogJson.named)
            .fold(
              _ => s"$name: $why",
              _ =>
                s"$name: a question set's answers under their names (live triage's or a " +
                  "shadow's, as pull writes them): compare it with --live and --gate"
            )
        )
      corpus = dir.resolve("corpus").resolve(log.header.corpus)
      manifest <- read(corpus.resolve("corpus.json"))
      casesText <- read(corpus.resolve("cases.jsonl"))
      cases <- readCases(corpus)
    } yield {
      if (Digest.text(manifest + casesText) != log.header.corpusDigest)
        println(s"$name: its corpus ${log.header.corpus} changed since it ran")
      Scored(name, log, cases, labels)
    }

  /** `report` written to `<dir>/reports/<name>.md`, and printed. */
  private def publish(dir: Path, name: String, report: String): Either[String, Unit] = {
    val reports = dir.resolve("reports")
    for {
      _ <- Try(Files.createDirectories(reports)).toEither.left.map(e =>
        s"$reports: ${e.getClass.getName}"
      )
      _ <- write(reports.resolve(s"$name.md"), report)
    } yield print(report)
  }

  /** The variant named `n`; why not, naming those there are. */
  private def variantNamed(n: String): Either[String, Variant] =
    Variants.named(n).toRight(s"no variant $n: ${Variants.all.map(Variant.name).mkString(", ")}")

  /** The cases of the corpus in `dir`. */
  private def readCases(dir: Path): Either[String, Vector[Case]] =
    read(dir.resolve("cases.jsonl")).flatMap(text =>
      Fields.each(text.linesIterator.filter(_.nonEmpty).toVector)(l =>
        Try(ujson.read(l)).toOption
          .toRight("cases.jsonl: a line is not JSON")
          .flatMap(CorpusJson.readCase)
      )
    )

  /** Each suite's rebuilt states against the corpus's, as counts, and the ids of every case
    * that drifted under the shipped builder: ids only. A suite whose inputs the variant
    * changes (its tuning both, its recipe triage's) reports changes, not drift.
    */
  private def changes(variant: Variant, rebuilt: Vector[(Case, Rebuilt)]): Unit = {
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
  private def byTs(c: Case): BigDecimal =
    Try(BigDecimal(c.id.written.split('/').last)).getOrElse(BigDecimal(0))

  private val Stamp = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss'Z'")
  private val Day = DateTimeFormatter.ofPattern("yyyyMMdd")

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
    println(
      s"tags: v1's ${cases.count(_.tags.isInstanceOf[Live.Weighed])}, a question set's " +
        s"${cases.count(_.tags.isInstanceOf[Live.Named])}, unanswered " +
        s"${cases.count(_.tags.isInstanceOf[Live.Unanswered])}"
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
