package grit.eval.harness.main

import java.nio.file.Path

import scala.util.Try

import grit.core.clock.Clock
import grit.core.context.Width
import grit.core.either.Traversal
import grit.core.message.{Cost, Tokens}
import grit.core.period.Probability
import grit.core.provider.Provider
import grit.core.secret.Secret
import grit.dbos.sql.DbConfig
import grit.eval.harness.capture.CaptureJson
import grit.eval.harness.jev.Budget
import grit.eval.harness.log.Cache
import grit.eval.harness.reply.{
  Admission,
  Answered,
  Assembled,
  Duplication,
  Queries,
  Rebuild,
  Summaries,
  WindowOnly
}
import grit.kit.environment.DotEnv
import grit.models.{OpenRouterConfig, OpenRouterProvider, Seed}

import Cli.*

/** The commands over the turns a restored database recorded: `rebuild`, `duplication`,
  * `summaries`.
  */
private[main] object RebuildCommands {

  /** `rebuild --capture <dir> --url <jdbc> --cache <dir> --spend <usd> [--window <tokens>]
    * [--tail <tokens>] [--judged <n>] [--pass <p>]`: every turn the restored database recorded
    * before its dump, its window rebuilt as of its assembly by the shipped assembler
    * ([[Rebuild.recorded]], its query replayed, its recorded verdicts admitting as
    * [[Admission.of]] `--judged` and `--pass` say, the turn's own [[Admission.Shipped]] when
    * neither is given) and set against the one it recorded ([[Rebuild.report]]); then every window-only
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
  private[main] def rebuild(f: Map[String, String], clock: Clock^): Either[String, Unit] =
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
      judged <- f
        .get("judged")
        .fold(Right(Admission.Shipped.judged))(v =>
          v.toIntOption.toRight(s"--judged $v: not a whole number")
        )
      pass <- f
        .get("pass")
        .fold(Right(Admission.Shipped.pass))(v =>
          v.toDoubleOption.flatMap(Probability.of).toRight(s"--pass $v: not between 0 and 1")
        )
      admission <- Admission.of(judged, pass).left.map(why => s"--judged: $why")
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
            w -> Rebuild.recorded(reader, w, assembled, admission, Width.Deployed)
          )
          only <- WindowOnly.all(reader, until)
          asked <- Traversal.traverse(only)(w =>
            WindowOnly.asked(reader, w, assembled, Width.Deployed).map(w -> _)
          )
          requests = asked.collect { case (_, WindowOnly.Query.Asked(Some(r))) => r }
          withheld = asked.count(_._2 == WindowOnly.Query.Withheld)
          pins <- Seed.catalog.map(_.pin)
          // The writer's settings name its model, so they key its answers; the key itself is
          // read only when a write is to be asked.
          settings = OpenRouterConfig.of(pins.query.first)
          estimated = Queries.estimate(requests, settings, store)
          _ = println(
            s"window-only cases: ${only.size}, withheld: $withheld; queries to write: " +
              s"${requests.distinct.size}, " +
              s"estimated ${Cost.cents(estimated)}, cap ${Cost.cents(cap)}"
          )
          budget <- Budget
            .of(cap, estimated)
            .left
            .map(refused)
          writer <-
            if (estimated == 0) Right(None)
            else Secret.read(env, OpenRouterConfig.KeyVar).left.map(_.message).map(Some(_))
          answered = {
            val asking: Provider^ = writer match {
              case Some(key) => new OpenRouterProvider(key, settings)
              case None => Queries.unasked()
            }
            Queries.answer(requests, settings, store, asking, budget, clock)
          }
          windows = asked.map((w, _) =>
            w -> WindowOnly.window(reader, w, assembled, answered.answers, Width.Deployed)
          )
        } yield {
          Rebuild.report(rebuilt).foreach(println)
          println(
            s"queries: asked ${answered.asked}, cached ${answered.cached}, skipped " +
              s"${answered.skipped}, failed ${answered.failed}; spent ${Cost.cents(answered.budget.spent)}"
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

  /** `duplication --capture <dir> --url <jdbc> [--window <tokens>] [--tail <tokens>]`: every
    * turn the restored database recorded before its dump, its window drawn again by the shipped
    * assembler as of its assembly but for its documents, read as the database holds them now
    * ([[Rebuild.redrawn]], its query replayed, nothing spent), and counted for a topic held
    * with one of its own leaves ([[Duplication.of]]): run over a copy the topics job was run
    * over since the dump, so the topics it wrote are found. The assembler is built as
    * `rebuild`'s. The database's login from `GRIT_DATABASE_USER` and `_PASSWORD`. Prints counts
    * and workflow ids, never text.
    */
  private[main] def duplication(f: Map[String, String], clock: Clock^): Either[String, Unit] =
    for {
      dir <- need(f, "capture").map(Path.of(_))
      url <- need(f, "url")
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
      env <- DotEnv.load(Path.of(sys.env.getOrElse("GRIT_ENV_FILE", ".env")), sys.env)
      config <- DbConfig.fromEnv(env.updated(DbConfig.UrlVar, url)).left.map(_.message)
      counted <- opened(config) { reader =>
        val now = clock.now()
        def unread(what: String)(e: grit.core.store.StoreError) =
          s"$what unread: ${e.kind}"
        for {
          workflows <- reader.turns(manifest.dump.at).left.map(unread("turn workflows"))
          windows = workflows.map((w, _) =>
            w -> Rebuild
              .redrawn(reader, w, assembled, Width.Deployed, now)
              .flatMap(drawn =>
                reader.all
                  .read(reader.documents.read(drawn.documents.distinct))
                  .left
                  .map(unread("its documents"))
              )
          )
          roots = Duplication.roots(windows.flatMap(_._2.toOption).flatten)
          links <- reader.all.read(reader.stitches.links(roots)).left.map(unread("links"))
        } yield Duplication.of(windows, links)
      }
    } yield counted.lines.foreach(println)

  /** `summaries --capture <dir> --url <jdbc> --models <id>,<id>… --cache <dir> --out <file>
    * --spend <usd> [--first <n>] [--dry-run true]`: the first `--first` turns (30 when not
    * given) the restored database recorded before its dump that kept a summary
    * ([[Summaries.of]]), each one's summary request asked again of each model named, an
    * OpenRouter model id, under the seed catalog's summary pin with its model replaced, each
    * answer kept under `<cache>` so it is paid for once, under the `--spend` cap in USD over
    * all the models: refused before any call when the asks not kept are estimated over it
    * ([[Queries.Usd]] each), and each call past it skipped. Written to `<file>`, the summaries'
    * text side by side for a person to read ([[Summaries.review]]); printed: counts, the
    * estimate and the spend. `--dry-run true` prints the cases and the estimate and asks
    * nothing. OpenRouter's key comes from the environment over `GRIT_ENV_FILE` (`.env` when
    * unset), read only when an ask is to be made; the database's login from
    * `GRIT_DATABASE_USER` and `_PASSWORD`. A call's latency is measured on `clock`.
    */
  private[main] def summaries(f: Map[String, String], clock: Clock^): Either[String, Unit] =
    for {
      dir <- need(f, "capture").map(Path.of(_))
      url <- need(f, "url")
      models <- need(f, "models").flatMap(ms =>
        Traversal.traverse(ms.split(",").toVector.map(_.trim).filter(_.nonEmpty))(m =>
          grit.core.model.ModelId.of(m).toRight(s"--models $m: not an OpenRouter model id")
        )
      )
      _ <- Either.cond(models.nonEmpty, (), "--models names none")
      cacheDir <- need(f, "cache").map(Path.of(_))
      store = Cache.at[String](cacheDir)(using Queries.text)
      out <- need(f, "out").map(Path.of(_))
      cap <- need(f, "spend").flatMap(decimal("spend"))
      first <- f.get("first").fold(Right(30))(positive("first"))
      dry = f.get("dry-run").contains("true")
      manifest <- read(dir.resolve("capture.json")).flatMap(t =>
        Try(ujson.read(t)).toOption
          .toRight("capture.json: not JSON")
          .flatMap(CaptureJson.readManifest)
      )
      env <- DotEnv.load(Path.of(sys.env.getOrElse("GRIT_ENV_FILE", ".env")), sys.env)
      config <- DbConfig.fromEnv(env.updated(DbConfig.UrlVar, url)).left.map(_.message)
      cases <- opened(config)(reader => Summaries.of(reader, manifest.dump.at, first))
      pin <- Seed.catalog.map(_.pin.summary.first)
      requests = cases.map(_.request)
      // Each model's settings name it, so they key its answers; the key itself is read only
      // when an ask is to be made.
      configs = models.map(m =>
        OpenRouterConfig.of(pin).copy(model = grit.core.model.ModelId.value(m))
      )
      estimated = configs.map(Queries.estimate(requests, _, store)).sum
      _ = println(
        s"summaries: ${cases.size} cases, ${cases.count(_.live.isDefined)} with a live summary; " +
          s"${models.size} models, estimated ${Cost.cents(estimated)}, cap ${Cost.cents(cap)}"
      )
      _ <-
        if (dry) Right(())
        else
          for {
            budget <- Budget.of(cap, estimated).left.map(refused)
            key <-
              if (estimated == 0) Right(None)
              else Secret.read(env, OpenRouterConfig.KeyVar).left.map(_.message).map(Some(_))
            answered = configs.foldLeft((budget, Vector.empty[Answered])) { (done, c) =>
              val asking: Provider^ = key match {
                case Some(k) => new OpenRouterProvider(k, c)
                case None => Queries.unasked()
              }
              val a = Queries.answer(requests, c, store, asking, done._1, clock)
              (a.budget, done._2 :+ a)
            }
            (spent, each) = answered
            _ <- write(
              out,
              Summaries.review(
                dir.getFileName.toString,
                cases,
                configs.map(_.model).zip(each.map(_.answers))
              )
            )
          } yield {
            configs.zip(each).foreach { (c, a) =>
              println(
                s"  ${c.model}: asked ${a.asked}, cached ${a.cached}, skipped ${a.skipped}, " +
                  s"failed ${a.failed}"
              )
            }
            println(s"spent ${Cost.cents(spent.spent)}; review: $out")
          }
    } yield ()
}
