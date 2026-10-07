package grit.eval.harness.main

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}

import scala.collection.immutable.VectorMap
import scala.util.Try
import scala.util.chaining.*
import scala.util.control.NonFatal

import grit.core.classify.{Answer, Classifier}
import grit.core.clock.{Clock, Fresh}
import grit.core.context.Width
import grit.core.host.ProcessIdentity
import grit.core.id.{QuestionName, ShadowName, TurnRef, WorkflowId}
import grit.core.message.Tokens
import grit.core.persona.Persona
import grit.core.place.Service
import grit.core.provider.Provider
import grit.core.tool.ToolSet
import grit.core.triage.Corpora
import grit.core.visibility.Visibility
import grit.dbos.engine.{Build, Engine}
import grit.dbos.internal.Reader
import grit.dbos.sql.DbConfig
import grit.eval.harness.capture.{
  Capture,
  CaptureJson,
  Case,
  CaseId,
  Digest,
  Dump,
  Ended,
  Fields,
  KnowledgeJson,
  Live,
  Manifest,
  PersonaJson,
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
import grit.eval.harness.reply.{
  Assembled,
  Expect,
  Queries,
  Rebuild,
  Reference,
  ReplyReview,
  Synthetic,
  TurnAnswers,
  TurnAsk,
  TurnTriage,
  TurnVariant,
  WindowOnly
}
import grit.eval.harness.report.{CaseJudged, Report, Scored, Varied}
import grit.eval.harness.run.{Call, Repeats, Run}
import grit.eval.harness.score.{
  Decision,
  Drafting,
  Drafts,
  Given,
  Guarded,
  Judgement,
  MovedOn,
  Order,
  Paired,
  Prices,
  Recipe,
  Rule,
  Scoring,
  Shapes,
  Spread,
  Target,
  TurnPair
}
import grit.eval.harness.sent.{Cut, HeardSent, SentMarkdown, TurnSent}
import grit.eval.harness.stats.Mills
import grit.kit.environment.DotEnv
import grit.lifecycle.triage.{TriageQuestions, TriageRecipe}
import grit.models.JevClassifier
import grit.models.JevConfig
import grit.models.{OpenRouterConfig, OpenRouterProvider, Seed}
import grit.turn.Turn

/** The eval harness's command line, run through `scripts/eval`, which says what each command
  * takes. It prints counts, ids and field names, never a message's text.
  */
object Main {

  def main(args: Array[String]): Unit = {
    val clock: Clock^ = Clock.system()
    args.toList match {
      case "capture" :: rest => exit(flags(rest).flatMap(capture))
      case "run" :: rest =>
        exit(
          flags(rest.filterNot(_ == "--no-cache"))
            .flatMap(run(_, !rest.contains("--no-cache"), clock))
        )
      case "determinism" :: rest => exit(flags(rest).flatMap(determinism))
      case "inputs" :: rest =>
        exit(flags(rest.filterNot(_ == "--more")).flatMap(inputs(_, rest.contains("--more"))))
      case "context" :: rest =>
        exit(flags(rest.filterNot(_ == "--full")).flatMap(context(_, rest.contains("--full"))))
      case "score" :: rest => exit(flags(rest).flatMap(score))
      case "order" :: rest => exit(flags(rest).flatMap(order(_, clock)))
      case "pull" :: rest => exit(flags(rest).flatMap(pull(_, clock)))
      case "replies" :: rest =>
        val switches = Set("--records", "--show")
        exit(
          flags(rest.filterNot(switches)).flatMap(
            replies(
              _,
              rest.contains("--records"),
              rest.contains("--show"),
              clock,
              Fresh.random()
            )
          )
        )
      case "reply-labels" :: rest => exit(flags(rest).flatMap(replyLabels))
      case "turns" :: rest => exit(flags(rest).flatMap(turns))
      case "rebuild" :: rest => exit(flags(rest).flatMap(rebuild(_, clock)))
      case "recipes" :: rest => exit(flags(rest).flatMap(recipes(_, clock)))
      case "reference-build" :: rest => exit(flags(rest).flatMap(referenceBuild(_, clock)))
      case "synthetic" :: rest => exit(flags(rest).flatMap(synthetic))
      case "compare" :: rest =>
        flags(rest).flatMap(f =>
          if (f.contains("gate")) drafts(f, clock).map(_ => 0) else compare(f)
        ) match {
          case Right(0) => ()
          case Right(code) => sys.exit(code)
          case Left(why) => exit(Left(why))
        }
      case _ =>
        exit(
          Left(
            "usage: scripts/eval capture|run|determinism|inputs|score|compare|order|pull|" +
              "replies|reply-labels|turns|rebuild|recipes|reference-build|synthetic " +
              "(scripts/eval says what each takes)"
          )
        )
    }
  }

  /** `capture --url <jdbc> --source <db> --restored <db> --sha256 <hex> --at <instant> --out
    * <dir>`: the capture of the restored database, written to `capture.json` and `cases.jsonl` in
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
        captured.cases.map(CaptureJson.writeCase(_).render() + "\n").mkString
      )
      _ <- write(
        out.resolve("turns.jsonl"),
        turns.cases.map(TurnJson.write(_).render() + "\n").mkString
      )
      _ <- write(out.resolve("verdicts.json"), Verdicts.written(standing.verdicts))
      _ <- write(
        out.resolve("capture.json"),
        CaptureJson.writeManifest(captured.manifest).render(2) + "\n"
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

  /** `replies --capture <dir> --url <jdbc> --out <dir> --labels <file> (--cases <id,...> |
    * --pick <n> [--by <order>, default random])`, with `records` for `--records` and `show`
    * for `--show`: a review of the capture's turns ([[ReplyReview]]), its text read from the
    * restored database, written to `<out>/replies-<capture>-<stamp>.md`, the stamp `clock`'s
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
      dir <- need(f, "capture").map(Path.of(_))
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
      capture = Option(dir.getFileName).fold("")(_.toString)
      config <- DbConfig.fromEnv(sys.env.updated(DbConfig.UrlVar, url)).left.map(_.message)
      review <- opened(config)(reader =>
        ReplyReview.review(capture, picked, at)(t => ReplyReview.read(reader, t, records))
      )
      path = out.resolve(s"replies-$capture-${Stamp.format(at.atOffset(ZoneOffset.UTC))}.md")
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

  /** `turns --eval <dir> --capture <yyyymmdd>`: the capture's recorded turns read structurally,
    * beside the verdicts standing in its `verdicts.json` (none when it has no such file),
    * written to `<dir>/reports/turns-<yyyymmdd>.md` and printed.
    */
  private def turns(f: Map[String, String]): Either[String, Unit] =
    for {
      dir <- need(f, "eval").map(Path.of(_))
      capture <- need(f, "capture").filterOrElse(_.matches("[0-9]{8}"), "--capture is yyyymmdd")
      at = dir.resolve("capture").resolve(capture)
      captured <- readTurns(at)
      standing <- {
        val file = at.resolve("verdicts.json")
        if (!Files.exists(file)) Right(Verdicts.Empty) else read(file).flatMap(Verdicts.read)
      }
      _ <- publish(dir, s"turns-$capture", Report.turns(capture, captured, standing))
    } yield ()

  /** `rebuild --capture <dir> --url <jdbc> --cache <dir> --spend <usd> [--window <tokens>]
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
    * `GRIT_DATABASE_USER` and `_PASSWORD`. Prints counts and workflow ids, never text. A
    * call's latency is measured on `clock`.
    */
  private def rebuild(f: Map[String, String], clock: Clock^): Either[String, Unit] =
    for {
      dir <- need(f, "capture").map(Path.of(_))
      url <- need(f, "url")
      cap <- need(f, "spend").flatMap(decimal("spend"))
      cacheDir <- need(f, "cache").map(Path.of(_))
      store = Cache.at[String](cacheDir)(using Queries.text)
      window <- f
        .get("window")
        .fold(Right(Assembled.Shipped.window))(positive("window")(_).map(Tokens(_)))
      tail <- f.get("tail").fold(Right(Assembled.Shipped.tail))(positive("tail")(_).map(Tokens(_)))
      assembled = Assembled.Shipped.copy(window = window, tail = tail)
      manifest <- read(dir.resolve("capture.json")).flatMap(t =>
        Try(ujson.read(t)).toOption
          .toRight("capture.json: not JSON")
          .flatMap(CaptureJson.readManifest)
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
              s"estimated ${Mills.figure(estimated)}, cap ${Mills.figure(cap)}"
          )
          budget <- Budget
            .of(cap, estimated)
            .left
            .map(refused)
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
            Queries.answer(requests, keyless, store, asking, budget, clock)
          }
          windows = asked.map((w, _) =>
            w -> WindowOnly.window(reader, w, assembled, answered.answers, Width.Deployed)
          )
        } yield {
          Rebuild.report(rebuilt).foreach(println)
          println(
            s"queries: asked ${answered.asked}, cached ${answered.cached}, skipped " +
              s"${answered.skipped}, failed ${answered.failed}; spent ${Mills.figure(answered.budget.spent)}"
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

  /** `recipes --eval <dir> --capture <yyyymmdd> --url <jdbc> --variants <name,…> --context
    * <tokens> --cache <dir> --spend <usd> [--reference <file>] [--labels <file>] [--window
    * <tokens>] [--tail <tokens>]`: each variant ([[TurnVariant.named]]) against shipped over
    * every turn of the capture, written to `<dir>/reports/recipes-<yyyymmdd>.md` and printed
    * ([[Report.recipes]]). Refused, before anything is read, when a variant draws a window over
    * `--context`, the reply model's context in tokens ([[grit.core.recipe.TurnRecipe.widens]]).
    * A turn's offering under a variant is decided over what it recorded
    * ([[TurnVariant.shape]]), by the answers [[TurnAnswers.of]] gives it. A turn whose answers
    * a variant reads and only asking gives ([[TurnAnswers.asks]]) has its message put live
    * triage's set ([[TurnTriage.ask]]) through Jev, once, with the corpora the
    * deployment that recorded the capture declares (its `knowledge.json`, [[KnowledgeJson.read]]),
    * the answer kept under `<cache>` so it is paid for once, under the `--spend` cap in USD:
    * and asked in the words of the persona it declares (its `persona.json`, [[PersonaJson.read]]),
    * refused before any call when such a turn's capture has no `knowledge.json` or no
    * `persona.json`, when the knowledge file
    * is not the declaration the turn's shape was decided under ([[TurnAnswers.declared]]), or
    * when the calls not kept are estimated over the cap; each call past it skipped. A message
    * with no case id (a TUI or task turn's) is not asked. Each width's window is rebuilt as of
    * the turn's assembly, its query replayed ([[Rebuild.recorded]]), and costed as capture costs
    * a window ([[TurnCapture.costed]]); its tools are those of the set it draws from
    * ([[grit.eval.harness.capture.Offered.drawn]]) the variant offers, their definitions costed
    * as capture costs them ([[TurnCapture.schema]]). The notes say, per variant, how many
    * shaped turns it decides as they were recorded ([[TurnVariant.asRecorded]]), naming those
    * it does not. The reference is `--reference` (a file of ids, [[Reference.read]]; none when
    * it does not exist) and the reply labels in `--labels` ([[Reference.labelled]]; none when
    * it does not exist). Jev's key comes from the environment over `GRIT_ENV_FILE` (`.env`
    * when unset), read only when a call is to be asked; the database's login from
    * `GRIT_DATABASE_USER` and `_PASSWORD`. A call's latency is measured on `clock`.
    */
  private def recipes(f: Map[String, String], clock: Clock^): Either[String, Unit] =
    for {
      eval <- need(f, "eval").map(Path.of(_))
      capture <- need(f, "capture").filterOrElse(_.matches("[0-9]{8}"), "--capture is yyyymmdd")
      dir = eval.resolve("capture").resolve(capture)
      url <- need(f, "url")
      named <- need(f, "variants").map(_.split(',').toVector.filter(_.nonEmpty))
      limit <- need(f, "context").flatMap(positive("context")).map(Tokens(_))
      cap <- need(f, "spend").flatMap(decimal("spend"))
      cacheDir <- need(f, "cache").map(Path.of(_))
      window <- f
        .get("window")
        .fold(Right(Assembled.Shipped.window))(positive("window")(_).map(Tokens(_)))
      tail <- f.get("tail").fold(Right(Assembled.Shipped.tail))(positive("tail")(_).map(Tokens(_)))
      assembled = Assembled.Shipped.copy(window = window, tail = tail)
      variants <- Fields.each(named)(TurnVariant.named(_, assembled))
      _ <- refuseWide(variants, limit)
      written <- f
        .get("reference")
        .map(Path.of(_))
        .filter(Files.exists(_))
        .fold[Either[String, Reference]](Right(Reference.Empty))(r =>
          read(r).flatMap(Reference.read)
        )
      labelled <- f
        .get("labels")
        .fold[Either[String, ReplyLabels]](Right(ReplyLabels.Empty))(l => replyLabelsAt(Path.of(l)))
      reference = Reference.labelled(labelled) ++ written
      manifest <- read(dir.resolve("capture.json")).flatMap(t =>
        Try(ujson.read(t)).toOption
          .toRight("capture.json: not JSON")
          .flatMap(CaptureJson.readManifest)
      )
      turns <- readTurns(dir)
      asking = turns.filter(TurnAnswers.asks(_, variants))
      knowledgeAt = dir.resolve("knowledge.json")
      // Read only when a turn is to be asked: its questions' words.
      knowledge <-
        if (asking.isEmpty) Right(Corpora.Empty)
        else if (Files.exists(knowledgeAt)) read(knowledgeAt).flatMap(KnowledgeJson.read)
        else
          Left(
            s"refused: ${asking.size} turns are to be asked and there is no $knowledgeAt: " +
              "write the corpora the deployment that recorded the capture declares there"
          )
      personaAt = dir.resolve("persona.json")
      // Read only when a turn is to be asked: its name is in to-grit's words.
      persona <-
        if (asking.isEmpty) Right(Persona.Grit)
        else if (Files.exists(personaAt)) read(personaAt).flatMap(PersonaJson.read)
        else
          Left(
            s"refused: ${asking.size} turns are to be asked and there is no $personaAt: " +
              "write the persona the deployment that recorded the capture declares there"
          )
      _ <- asking
        .map(TurnAnswers.declared(_, knowledge))
        .collectFirst { case Left(why) => why }
        .toLeft(())
        .left
        .map(why => s"refused: $why")
      env <- DotEnv.load(Path.of(sys.env.getOrElse("GRIT_ENV_FILE", ".env")), sys.env)
      config <- DbConfig.fromEnv(env.updated(DbConfig.UrlVar, url)).left.map(_.message)
      store = Cache.at[Vector[Weights]](cacheDir)
      text <- opened(config) { reader =>
        for {
          // The set each turn draws its tools from, by the id it recorded.
          sets <- Fields.each(turns)(t => drawn(reader, t).map(t.workflow -> _))
          drawnSets = sets.toMap
          noCase = asking.filter(t =>
            t.said match {
              case Said.Slack(_) => false
              case Said.Tui(_) | Said.Task(_) => true
            }
          )
          built = asking.collect {
            case t @ TurnCase(_, _, _, Said.Slack(id), _, _, _, _, _, _, _, _, _, _, _, _) =>
              (
                t,
                id,
                TurnTriage.ask(
                  reader,
                  t,
                  knowledge,
                  TriageQuestions.shipped(persona),
                  manifest.tuning
                )
              )
          }
          asks = built.collect { case (t, id, Right(a)) => (t, id, a) }
          calls = asks.map((t, id, a) =>
            Call(Suite.Triage, id, 0, a.posed.asking, a.posed.request, Some(t.focus))
          )
          model = Variants.LiveModel
          estimated = Run.estimate(
            calls,
            model,
            k => store.get(k).exists((c: Option[Cached[Vector[Weights]]]) => c.isDefined)
          )
          budget <- Budget
            .of(cap, estimated)
            .left
            .map(refused)
          // Jev's settings are read only when a call is to be asked.
          jevConfig <-
            if (estimated == 0) Right(None)
            else JevConfig.fromEnv(env).left.map(_.message).map(Some(_))
          ran = {
            val jev: Classifier^ = jevConfig match {
              case Some(c) => JevClassifier(c.copy(model = model))
              case None => Classifier.none("no call was to be asked")
            }
            Run(calls, jev, model, store, budget, clock)
          }
          asked = asks
            .zip(ran.rows)
            .flatMap { (ask: (TurnCase, CaseId, TurnAsk), r: Row[Vector[Weights]]) =>
              val (t, _, a) = ask
              r.outcome match {
                case Outcome.Answered(ws) => TurnTriage.named(a, ws).map(t.workflow -> _)
                case Outcome.Failed(_) | Outcome.Skipped => None
              }
            }
            .toMap
          // Each turn's window at each width a variant draws it at, rebuilt once.
          widths = (TurnVariant.Shipped +: variants)
            .flatMap(v => turns.map(t => (t.workflow, v.recipe.at(TurnVariant.rooted(t)).width)))
            .distinct
          windows = widths
            .map((w, width) =>
              (w, width) -> Rebuild
                .recorded(reader, w, assembled, width)
                .flatMap(r =>
                  TurnRef
                    .fromWorkflowId(w)
                    .toRight(s"${WorkflowId.value(w)} is not a turn")
                    .flatMap(TurnCapture.costed(reader, _, r.rebuilt))
                )
            )
            .toMap
        } yield {
          def answersOf(t: TurnCase) = TurnAnswers.of(t, asked.get(t.workflow))
          def build(v: TurnVariant, t: TurnCase): (Given, Option[Set[Service]]) = {
            val shaped = TurnVariant.shape(v, t, answersOf(t))
            val tools =
              drawnSets.getOrElse(t.workflow, Vector.empty).filter(e => shaped.offers(e.name))
            val drawnWindow = windows.get((t.workflow, shaped.width)).flatMap(_.toOption)
            (
              Given(tools.map(_.name), TurnCapture.schema(tools), drawnWindow),
              shaped.services
            )
          }
          val prices = Prices.of(turns.flatMap(_.spend))
          def varied(v: TurnVariant): Varied = {
            val pairs =
              turns.map(t => TurnPair(t, build(TurnVariant.Shipped, t)._1, build(v, t)._1))
            Varied(
              v.name,
              Recipe.of(pairs, prices),
              reference.turns.toVector.flatMap((w: WorkflowId, expected: Vector[Expect]) =>
                turns.find(_.workflow == w).map { t =>
                  val (g, services) = build(v, t)
                  (w, t.conversation, Reference.judge(expected, g.window, services))
                }
              )
            )
          }
          val notRebuilt = windows.collect { case ((w, _), Left(why)) =>
            s"  ${WorkflowId.value(w)}: $why"
          }
          val exact = variants.map { v =>
            val judged =
              turns.flatMap(t => TurnVariant.asRecorded(v, t, answersOf(t)).map(t.workflow -> _))
            val differ = judged.collect { case (w, false) => WorkflowId.value(w) }
            s"${v.name}: decides ${judged.count(_._2)} of ${judged.size} shaped turns' services " +
              "as recorded" + (if (differ.isEmpty) "" else s"; not: ${differ.mkString(", ")}")
          }
          val notes = Vector(
            Report.shapes(Shapes.of(turns)),
            s"turns: ${turns.size}; to be asked for answers ${asking.size}: answered " +
              s"${asked.size}, not asked for want of a case id ${noCase.size}, not built " +
              s"${built.count(_._3.isLeft)}, asked and unanswered or skipped " +
              s"${asks.size - asked.size}",
            s"Jev: calls ${calls.size}, estimated ${Mills.figure(estimated)}, cap " +
              s"${Mills.figure(cap)}, spent ${Mills.figure(ran.budget.spent)}",
            s"reference turns: ${reference.turns.size} (labelled ${Reference.labelled(labelled).turns.size})",
            s"windows rebuilt: ${windows.count(_._2.isRight)} of ${windows.size}"
          ) ++ exact ++ notRebuilt.toVector.sorted ++ built.collect { case (t, _, Left(why)) =>
            s"  not asked: ${WorkflowId.value(t.workflow)}: $why"
          }
          Report.recipes(capture, notes, prices, varied(TurnVariant.Shipped), variants.map(varied))
        }
      }
      _ <- publish(eval, s"recipes-$capture", text)
    } yield ()

  /** `reference-build --url <jdbc>`: every case of `grit.eval` ([[Synthetic.cases]]) written
    * into the database at `<jdbc>` ([[Synthetic.build]]) through an engine opened on it for the
    * purpose (its schema applied), with the database's login from `GRIT_DATABASE_USER` and
    * `_PASSWORD`. The database is the synthetic reference's, which `scripts/eval` creates
    * afresh first: a case already in it fails the build. Prints counts. A refused engine's
    * holder is named as of `clock`'s now.
    */
  private def referenceBuild(f: Map[String, String], clock: Clock^): Either[String, Unit] =
    for {
      url <- need(f, "url")
      all <- Synthetic.cases
      config <- DbConfig
        .fromEnv(sys.env.updated(DbConfig.UrlVar, url))
        .left
        .map(_.message)
      built <- withEngine(config, clock)(engine =>
        Synthetic.build(
          all,
          engine.jot,
          engine.conversations,
          engine.entries,
          engine.periods,
          engine.documents,
          engine.keeper
        )
      )
    } yield {
      println(s"cases: ${built.cases}, turns ${built.turns}, entries ${built.entries}")
      println(
        s"labelling no [must] or [never] entry: ${built.unlabelled.size}" +
          (if (built.unlabelled.isEmpty) "" else built.unlabelled.mkString(" (", ", ", ")"))
      )
    }

  /** `synthetic --eval <dir> --url <jdbc> --variants <name,…> --context <tokens> [--window
    * <tokens>] [--tail <tokens>]`: the synthetic reference, `grit.eval`'s cases found in the
    * database at `<jdbc>` ([[Synthetic.found]]), each judged ([[Reference.judge]]) under shipped
    * and each variant ([[TurnVariant.named]]) by its window drawn at the variant's width
    * ([[Synthetic.width]], [[Synthetic.window]]), each width's drawn once; written to
    * `<dir>/reports/recipes-synthetic.md` and printed ([[Report.synthetic]]). Refused, before
    * anything is read, when a variant draws a window over `--context`, the reply model's
    * context in tokens. The assembler is built with grit's own window and tail
    * ([[Assembled.Shipped]]) unless `--window` or `--tail` names the deployment's. Spends
    * nothing; the database's login from `GRIT_DATABASE_USER` and `_PASSWORD`.
    */
  private def synthetic(f: Map[String, String]): Either[String, Unit] =
    for {
      eval <- need(f, "eval").map(Path.of(_))
      url <- need(f, "url")
      named <- need(f, "variants").map(_.split(',').toVector.filter(_.nonEmpty))
      limit <- need(f, "context").flatMap(positive("context")).map(Tokens(_))
      window <- f
        .get("window")
        .fold(Right(Assembled.Shipped.window))(positive("window")(_).map(Tokens(_)))
      tail <- f.get("tail").fold(Right(Assembled.Shipped.tail))(positive("tail")(_).map(Tokens(_)))
      assembled = Assembled.Shipped.copy(window = window, tail = tail)
      variants <- Fields.each(named)(TurnVariant.named(_, assembled))
      _ <- refuseWide(variants, limit)
      all <- Synthetic.cases
      config <- DbConfig
        .fromEnv(sys.env.updated(DbConfig.UrlVar, url))
        .left
        .map(_.message)
      text <- opened(config) { reader =>
        Synthetic.found(reader, all).map { found =>
          val asked = found.asked
          val judging = TurnVariant.Shipped +: variants
          val widths = judging.map(Synthetic.width).distinct
          val windows = (for {
            w <- widths
            a <- asked
          } yield (a.name, w) -> Synthetic.window(reader, a, assembled, w)).toMap
          val judged = judging.map { v =>
            val w = Synthetic.width(v)
            v.name -> asked.map(a =>
              CaseJudged(
                a.name,
                a.of,
                Reference.judge(
                  a.expected.toVector,
                  windows.get((a.name, w)).flatMap(_.toOption),
                  None
                )
              )
            )
          }
          val notDrawn = windows.toVector.collect { case ((n, _), Left(why)) => s"  $n: $why" }
          val notes = Vector(
            s"cases: ${all.size}; turns judged (a case's in each variant) " +
              s"${asked.size}, left out for labelling no [must] or [never] entry " +
              s"${found.unlabelled.size}" +
              (if (found.unlabelled.isEmpty) "" else found.unlabelled.mkString(" (", ", ", ")")),
            s"windows drawn: ${windows.count(_._2.isRight)} of ${windows.size}, at " +
              s"${widths.size} width(s)"
          ) ++ notDrawn.sorted
          Report.synthetic(notes, judged)
        }
      }
      _ <- publish(eval, "recipes-synthetic", text)
    } yield ()

  /** Refused when one of `variants` draws a window over `limit`, the reply model's context in
    * tokens, naming the first and its widest budget.
    */
  private def refuseWide(variants: Vector[TurnVariant], limit: Tokens): Either[String, Unit] =
    variants
      .flatMap(v => v.recipe.widens(limit).map(v -> _))
      .headOption
      .fold[Either[String, Unit]](Right(())) { (v, b) =>
        Left(
          s"refused: ${v.name} draws a window of ${Tokens.value(b)} tokens, over the reply " +
            s"model's context of ${Tokens.value(limit)}"
        )
      }

  /** `f` of an engine opened on `config`'s database to write the synthetic reference, closed
    * after it; `Left` when it cannot open (another engine holds the database's lock, named as of
    * `clock`'s now) or what it writes throws.
    */
  private def withEngine[A](config: DbConfig, clock: Clock^)(
      f: Engine^ => Either[String, A]
  ): Either[String, A] =
    try {
      Engine.open(config, "eval-synthetic", Builder, Uncapped, Visibility.Shipped) match {
        case Left(refused) => Left(s"the engine could not open: ${refused.message(clock.now())}")
        case Right(engine) =>
          try f(engine)
          finally engine.close()
      }
    } catch {
      case NonFatal(e) => Left(s"the synthetic database was not written: ${e.getClass.getName}")
    }

  /** The process an engine opened only to write the synthetic reference's database says it
    * is: no edge attaches to that engine, so nothing need find this process by it.
    */
  private val Builder: ProcessIdentity = ProcessIdentity("eval-reference-build", 0)

  /** What that engine takes new messages under: no cap, as none is ever sent it. */
  private val Uncapped: grit.core.spend.Budget = grit.core.spend.Budget(ZoneOffset.UTC, None)

  /** The entries of the set `t` draws its tools from ([[Offered.drawn]]), read through
    * `reader`; none when it recorded no offer.
    */
  private def drawn(reader: Reader^, t: TurnCase): Either[String, Vector[ToolSet.Entry]] =
    t.offered.fold[Either[String, Vector[ToolSet.Entry]]](Right(Vector.empty))(o =>
      reader.all
        .read(reader.toolSets.get(o.drawn))
        .map(_.tools)
        .left
        .map(e => s"tool set of ${WorkflowId.value(t.workflow)} unread: ${Capture.kind(e)}")
    )

  /** The turns of the capture in `dir`, from `turns.jsonl`. */
  private def readTurns(dir: Path): Either[String, Vector[TurnCase]] =
    read(dir.resolve("turns.jsonl")).flatMap(text =>
      Fields.each(text.linesIterator.filter(_.nonEmpty).toVector)(l =>
        Try(ujson.read(l)).toOption
          .toRight("turns.jsonl: a line is not JSON")
          .flatMap(TurnJson.read)
      )
    )

  /** `run --capture <dir> --url <jdbc> --variant <name> --spend <usd> --cache <dir> --runs
    * <dir> [--repeats n (default [[Repeats.Default]])] [--first n] [--rule <file>] [--labels <file>]`, and `cache` false for
    * `--no-cache`: the capture's cases' questions rebuilt, how they changed from capture
    * reported, then asked of Jev under the cap, and the log written to `<runs>/<stamp>-<variant>.jsonl`.
    * The key comes from the environment over `GRIT_ENV_FILE` (`.env` when unset), the
    * database's login from `GRIT_DATABASE_USER` and `_PASSWORD`. The stamp and the header's
    * start are `clock`'s now, and each call's latency is measured on it.
    */
  private def run(f: Map[String, String], cache: Boolean, clock: Clock^): Either[String, Unit] =
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
      all <- Fields.each(casesText.linesIterator.filter(_.nonEmpty).toVector)(l =>
        Try(ujson.read(l)).toOption
          .toRight("cases.jsonl: a line is not JSON")
          .flatMap(CaptureJson.readCase)
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
        s"calls: ${calls.size} (${cases.size} cases × $repeats); estimated " +
          s"${Mills.figure(estimated)}, cap ${Mills.figure(cap)}"
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
          s"failed ${footer.failed}, skipped ${footer.skipped}; spent ${Mills.figure(footer.spent)}"
      )
      println(s"input tokens of the calls asked: reported $reported, estimated $estimatedTokens")
      println(s"log: ${out.getFileName}")
    }

  /** `determinism --capture <dir> --log <file>`: how far apart the log's repeats answered,
    * and how far its answers are from what was kept live: the largest gap, and the questions
    * and cases over [[Spread.Tolerance]], by id.
    */
  private def determinism(f: Map[String, String]): Either[String, Unit] =
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

  /** `inputs --capture <dir> --url <jdbc> --out <file> [--variant <name>]`, and `more` for
    * `--more`: every case's questions rebuilt through the shipped builders ([[Review]]), triage's
    * by the variant's recipe (default `live`'s, the shipped), written to `<file>`, one line
    * a case, in the capture's order, with the database's login from `GRIT_DATABASE_USER` and
    * `_PASSWORD`. The file holds text and is never printed: this prints counts, and how each
    * request's digest compares to the capture's.
    */
  /** `context`: one recorded turn's request as sent, text and all, written to `--out` for a
    * person to read; prints counts only.
    */
  private def context(f: Map[String, String], full: Boolean): Either[String, Unit] =
    if (f.contains("entry")) heardContext(f) else turnContext(f, full)

  /** `context` of a heard message (`--entry`): what Jev was asked of it, its triage request
    * worded for the persona and corpora in `--declared` (a capture's directory, its
    * `persona.json` and `knowledge.json`), else grit's and none.
    */
  private def heardContext(f: Map[String, String]): Either[String, Unit] = {
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
  private def turnContext(f: Map[String, String], full: Boolean): Either[String, Unit] =
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
              if (sent.epoch == Turn.Epoch) "" else s" (this build: ${Turn.Epoch})"
            }"
      )
    }

  private def inputs(f: Map[String, String], more: Boolean): Either[String, Unit] =
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
    * against its capture (`<dir>/capture/<its capture>`) and the labels in `<file>` (default
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
    * live triage's pulled log asking the `--live` set, both in `<dir>/runs/` (B may be A
    * itself, to read live's answers under another set's gate): each one's draft
    * by its own set's gate, by focus ([[Drafts]]), written to `<dir>/reports/<A>-vs-<B>.md`
    * (each name less `.jsonl`) and printed; the ids of the cases each decided alone, one a
    * line, to `<dir>/order/<yyyymmdd>-draft-<set>-live-only.txt` and `…-set-only.txt`, for
    * labelling. A live log an earlier build pulled by position is read under v1's names
    * ([[Log.named]]). With `--verdicts <name>`, a verdicts file `pull` wrote in `<dir>/runs/`
    * ([[Verdicts]]), each pick reason's verdicts against both drafts and each side's `to`
    * ([[Judgement]]); without it, the report says no verdicts were given. `--helps-at`,
    * `--decide`, `--replica` and `--spread` are refused beside it. `<yyyymmdd>` is `clock`'s
    * day in UTC.
    */
  private def drafts(f: Map[String, String], clock: Clock^): Either[String, Unit] =
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
      day = Day.format(clock.now().atOffset(ZoneOffset.UTC))
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
    Drafting(log.rows, set.speak, set.durable, set.to)

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
            .map(Log.named(_, TriageQuestions.V1.questions(Corpora.Empty)))
        )
        .left
        .map(e => s"$name: not live triage's log, under names or by position: $e")
    }

  /** `order --eval <dir> --a <name> [--b <name>] [--tag <question>] [--by spread|difference]`:
    * the case ids in labelling order ([[Order]]) on `<question>` (`kind`, a tag's name, or
    * `place`; default `durable`): by the difference between runs A and B (the default when `--b`
    * is given), or by run A's repeat spread (the default without), written one a line to
    * `<dir>/order/<yyyymmdd>-<question>-<by>.txt`, `<yyyymmdd>` `clock`'s day in UTC. Prints
    * how many, and how many unlabelled lead.
    */
  private def order(f: Map[String, String], clock: Clock^): Either[String, Unit] =
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
      name = s"${Day.format(clock.now().atOffset(ZoneOffset.UTC))}-" +
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

  /** `pull --capture <dir> --url <jdbc> --runs <dir> --shadows <name,…> [--since <instant>]`:
    * what the database kept of the heard messages tagged since `<instant>` (default: ever) and
    * the capture in `<dir>` holds, as run logs ([[Pull]]): `<runs>/live-<yyyymmdd>.jsonl`, live
    * triage's tags, and `<runs>/shadow-<name>-<yyyymmdd>.jsonl` for each shadow named, with
    * the database's login from `GRIT_DATABASE_USER` and `_PASSWORD`; and
    * `<runs>/verdicts-<yyyymmdd>.json`, the verdicts standing on the messages a review
    * considered since `<instant>` ([[Pull.verdicts]], [[Verdicts.written]]). Prints counts, and
    * the ids of the messages no capture holds yet, for the next capture. `<yyyymmdd>` is
    * `clock`'s day in UTC.
    */
  private def pull(f: Map[String, String], clock: Clock^): Either[String, Unit] =
    for {
      dir <- need(f, "capture").map(Path.of(_))
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
      manifest <- read(dir.resolve("capture.json"))
      casesText <- read(dir.resolve("cases.jsonl"))
      cases <- readCases(dir)
      config <- DbConfig.fromEnv(sys.env.updated(DbConfig.UrlVar, url)).left.map(_.message)
      at = clock.now()
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
          s"spent ${Mills.figure(footer.spent)}"
      }
      println(
        s"live (${Pull.Kept}): questions " +
          pulled.live.header.questions
            .fold("none answered")(_.map(QuestionName.value).mkString(", ")) +
          s"; ${counted(pulled.live)}; answered under other questions, left out: " +
          s"${pulled.renamed} (pull --since when live triage's question set changed); capture " +
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
      println(s"not in the capture, for the next capture: ${pulled.uncaptured.size}")
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

  /** The run `<dir>/runs/<name>`, with its capture's cases and `labels`; a capture whose files
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
      capture = dir.resolve("capture").resolve(log.header.capture)
      manifest <- read(capture.resolve("capture.json"))
      casesText <- read(capture.resolve("cases.jsonl"))
      cases <- readCases(capture)
    } yield {
      if (Digest.text(manifest + casesText) != log.header.captureDigest)
        println(s"$name: its capture ${log.header.capture} changed since it ran")
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

  /** The cases of the capture in `dir`. */
  private def readCases(dir: Path): Either[String, Vector[Case]] =
    read(dir.resolve("cases.jsonl")).flatMap(text =>
      Fields.each(text.linesIterator.filter(_.nonEmpty).toVector)(l =>
        Try(ujson.read(l)).toOption
          .toRight("cases.jsonl: a line is not JSON")
          .flatMap(CaptureJson.readCase)
      )
    )

  /** Each suite's rebuilt states against the capture's, as counts, and the ids of every case
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

  private def refused(r: Budget.Refused): String =
    s"refused: estimated ${Mills.figure(r.estimated)} is over the cap ${Mills.figure(r.cap)}"

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
