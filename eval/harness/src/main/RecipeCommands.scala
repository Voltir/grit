package grit.eval.harness.main

import java.nio.file.{Files, Path}

import scala.util.Try

import grit.core.classify.Classifier
import grit.core.clock.Clock
import grit.core.either.Traversal
import grit.core.id.{TurnRef, WorkflowId}
import grit.core.message.{Cost, Tokens}
import grit.core.persona.Persona
import grit.core.place.Service
import grit.core.triage.Corpora
import grit.dbos.sql.DbConfig
import grit.eval.harness.capture.{
  CaptureJson,
  CaseId,
  KnowledgeJson,
  PersonaJson,
  Said,
  TurnCapture,
  TurnCase
}
import grit.eval.harness.jev.{Budget, Variants}
import grit.eval.harness.label.ReplyLabels
import grit.eval.harness.log.{Cache, Cached, Outcome, Row, Suite, Weights}
import grit.eval.harness.reply.{
  Admission,
  Assembled,
  Expect,
  Rebuild,
  Reference,
  Synthetic,
  TurnAnswers,
  TurnAsk,
  TurnTriage,
  TurnVariant
}
import grit.eval.harness.report.{CaseJudged, Report, Varied}
import grit.eval.harness.run.{Call, Run}
import grit.eval.harness.score.{Given, Prices, Recipe, Shapes, TurnPair}
import grit.kit.environment.DotEnv
import grit.lifecycle.triage.TriageQuestions
import grit.models.JevClassifier
import grit.models.JevConfig

import Cli.*
import ReplyCommands.*
import CaseFiles.*
import Engines.*

/** `recipes` and `synthetic`: assembly variants against shipped, over captured and hand-written
  * cases.
  */
private[main] object RecipeCommands {

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
    * it does not exist). Jev's key comes from the environment
    * ([[DotEnv.loadDefault]]), read only when a call is to be asked; the database's login from
    * `GRIT_DATABASE_USER` and `_PASSWORD`. A call's latency is measured on `clock`.
    */
  private[main] def recipes(f: Map[String, String], clock: Clock^): Either[String, Unit] =
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
      variants <- Traversal.traverse(named)(TurnVariant.named(_, assembled))
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
      env <- DotEnv.loadDefault(sys.env)
      config <- DbConfig.fromEnv(env.updated(DbConfig.UrlVar, url)).left.map(_.message)
      store = Cache.at[Vector[Weights]](cacheDir)
      text <- opened(config) { reader =>
        for {
          // The set each turn draws its tools from, by the id it recorded.
          sets <- Traversal.traverse(turns)(t => drawn(reader, t).map(t.workflow -> _))
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
                .recorded(reader, w, assembled, Admission.Shipped, width)
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
            s"Jev: calls ${calls.size}, estimated ${Cost.cents(estimated)}, cap " +
              s"${Cost.cents(cap)}, spent ${Cost.cents(ran.budget.spent)}",
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

  /** `synthetic --eval <dir> --url <jdbc> --variants <name,…> --context <tokens> [--window
    * <tokens>] [--tail <tokens>]`: the synthetic reference, each of `grit.eval`'s cases in each
    * variant written into a database of its own beside the one at `<jdbc>`
    * ([[Synthetic.apart]], `grit_eval_synthetic_{i}`, dropped after), through an engine opened
    * on it for the purpose ([[Synthetic.build]]), and found there ([[Synthetic.found]]); each
    * judged ([[Reference.judge]]) under shipped and each variant ([[TurnVariant.named]]) by its
    * window drawn at the variant's width ([[Synthetic.width]], [[Synthetic.window]]), each
    * width's drawn once; written to `<dir>/reports/recipes-synthetic.md` and printed
    * ([[Report.synthetic]]). Refused, before anything is written, when a variant draws a window
    * over `--context`, the reply model's context in tokens. The assembler is built with grit's
    * own window and tail ([[Assembled.Shipped]]) unless `--window` or `--tail` names the
    * deployment's. Spends nothing; the databases' login from `GRIT_DATABASE_USER` and
    * `_PASSWORD`. A refused engine's holder is named as of `clock`'s now.
    */
  private[main] def synthetic(f: Map[String, String], clock: Clock^): Either[String, Unit] =
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
      variants <- Traversal.traverse(named)(TurnVariant.named(_, assembled))
      _ <- refuseWide(variants, limit)
      all <- Synthetic.cases
      config <- DbConfig
        .fromEnv(sys.env.updated(DbConfig.UrlVar, url))
        .left
        .map(_.message)
      judging = TurnVariant.Shipped +: variants
      widths = judging.map(Synthetic.width).distinct
      drawn <- Synthetic.apart(config, "grit_eval_synthetic", Synthetic.laid(all)) { (db, one) =>
        for {
          _ <- withEngine(db, clock, "eval-synthetic", "the synthetic database was not written")(
            engine =>
              Synthetic.build(
                Vector(one),
                engine.jot,
                engine.conversations,
                engine.entries,
                engine.periods,
                engine.documents,
                engine.keeper
              )
          )
          found <- opened(db) { reader =>
            Synthetic.found(reader, Vector(one)).map { found =>
              found -> (for {
                w <- widths
                a <- found.asked
              } yield (a.name, w) -> Synthetic.window(reader, a, assembled, w)).toMap
            }
          }
        } yield found
      }
      text = {
        val found = Synthetic.Found(
          drawn.flatMap(_._1.asked),
          drawn.flatMap(_._1.unlabelled)
        )
        val asked = found.asked
        val windows = drawn.flatMap(_._2).toMap
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
      _ <- publish(eval, "recipes-synthetic", text)
    } yield ()

  /** Refused when one of `variants` draws a window over `limit`, the reply model's context in
    * tokens, naming the first and its widest budget.
    */
  private[main] def refuseWide(variants: Vector[TurnVariant], limit: Tokens): Either[String, Unit] =
    variants
      .flatMap(v => v.recipe.widens(limit).map(v -> _))
      .headOption
      .fold[Either[String, Unit]](Right(())) { (v, b) =>
        Left(
          s"refused: ${v.name} draws a window of ${Tokens.value(b)} tokens, over the reply " +
            s"model's context of ${Tokens.value(limit)}"
        )
      }
}
