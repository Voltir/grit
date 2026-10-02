package grit.eval.harness.corpus

import scala.util.Try

import grit.core.id.{ConversationId, EntryId, WorkflowId}
import grit.core.message.Tokens
import grit.core.period.Probability
import grit.core.stitch.{Offered, Tuning}
import grit.core.triage.Kind
import grit.dbos.engine.{Build, Reader}

import Fields.{each, opt}

/** A corpus's files as JSON: a case, one line of `cases.jsonl` each, and the manifest,
  * `corpus.json`. Each value is written with its fields in one fixed order, so equal values
  * write equal bytes. A read is `Left` naming the first field missing or not of its form.
  */
object CorpusJson {

  def writeCase(c: Case): ujson.Value = ujson.Obj(
    "id" -> c.id.written,
    "entry" -> EntryId.value(c.entry),
    "conversation" -> ConversationId.value(c.conversation),
    "tagged" -> c.tagged.toString,
    "triage" -> writeTriaged(c.triage),
    "tags" -> writeLive(c.tags),
    "asked" -> c.asked.fold[ujson.Value](ujson.Null)(writeAsked),
    "author" -> c.author.fold[ujson.Value](ujson.Null)(d => ujson.Str(d.hex)),
    "clusters" -> ujson.Obj(
      "conversation" -> c.clusters.conversation.written,
      "exchange" -> c.clusters.exchange.written
    ),
    "stitch" -> c.stitch.fold[ujson.Value](ujson.Null)(writeStitched),
    "tuning" -> c.tuning.fold[ujson.Value](ujson.Null)(writeTuning)
  )

  def readCase(v: ujson.Value): Either[String, Case] = {
    val f = Fields("case", v)
    for {
      id <- f.str("id").flatMap(CaseId.read)
      entry <- f.str("entry")
      conversation <- f.str("conversation")
      tagged <- f.instant("tagged")
      triage <- f.obj("triage").flatMap(readTriaged)
      tags <- f.obj("tags").flatMap(readLive)
      asked <- f.optional("asked").flatMap(opt(_)(readAsked))
      author <- f
        .optional("author")
        .flatMap(opt(_)(a => Fields.str("author", a).flatMap(Digest.read)))
      clusters <- f.obj("clusters").flatMap { c =>
        val cf = Fields("clusters", c)
        for {
          conversation <- cf.str("conversation").flatMap(CaseId.read)
          exchange <- cf.str("exchange").flatMap(CaseId.read)
        } yield Clusters(conversation, exchange)
      }
      stitch <- f.optional("stitch").flatMap(opt(_)(readStitched))
      tuning <- f.optional("tuning").flatMap(opt(_)(readTuning))
    } yield Case(
      id,
      EntryId(entry),
      ConversationId(conversation),
      tagged,
      triage,
      tags,
      asked,
      author,
      clusters,
      stitch,
      tuning
    )
  }

  def writeManifest(m: Manifest): ujson.Value = ujson.Obj(
    "source" -> m.source,
    "restored" -> m.restored,
    "dump" -> ujson.Obj("sha256" -> m.dump.sha256.hex, "at" -> m.dump.at.toString),
    "settings" -> writeSettings(m.settings),
    "tuning" -> writeTuning(m.tuning),
    "tunings" -> m.tunings,
    "constants" -> ujson.Obj(
      "thread_chars" -> m.constants.threadChars,
      "latest" -> m.constants.latest,
      "message_chars" -> m.constants.messageChars,
      "hits" -> m.constants.hits
    ),
    "capture" -> writeBuild(m.capture),
    "cases" -> m.cases,
    "stitched" -> m.stitched
  )

  def readManifest(v: ujson.Value): Either[String, Manifest] = {
    val f = Fields("manifest", v)
    for {
      source <- f.str("source")
      restored <- f.str("restored")
      dump <- f.obj("dump").flatMap { d =>
        val df = Fields("dump", d)
        for {
          sha <- df.str("sha256").flatMap(Digest.read)
          at <- df.instant("at")
        } yield Dump(sha, at)
      }
      settings <- f.obj("settings").flatMap(readSettings)
      tuning <- f.obj("tuning").flatMap(readTuning)
      tunings <- f.int("tunings")
      constants <- f.obj("constants").flatMap { c =>
        val cf = Fields("constants", c)
        for {
          thread <- cf.int("thread_chars")
          latest <- cf.int("latest")
          message <- cf.int("message_chars")
          hits <- cf.int("hits")
        } yield Constants(thread, latest, message, hits)
      }
      capture <- f.field("capture").flatMap(readBuild("capture", _))
      cases <- f.int("cases")
      stitched <- f.int("stitched")
    } yield Manifest(
      source,
      restored,
      dump,
      settings,
      tuning,
      tunings,
      constants,
      capture,
      cases,
      stitched
    )
  }

  private def readTriaged(v: ujson.Value): Either[String, Triaged] = {
    val f = Fields("triage", v)
    for {
      workflow <- f.str("workflow")
      recorded <- f
        .optional("recorded")
        .flatMap(opt(_) { r =>
          val rf = Fields("recorded", r)
          for {
            status <- rf.str("status")
            epoch <- rf.str("epoch")
            created <- rf.instant("created")
          } yield Reader.Recorded(status, epoch, created)
        })
      build <- f.field("build").flatMap(readBuild("triage", _))
    } yield Triaged(WorkflowId(workflow), recorded, build)
  }

  /** A build as [[writeBuild]] writes it, read as part of `what`. */
  def readBuild(what: String, v: ujson.Value): Either[String, Build] = v match {
    case ujson.Str("unknown") => Right(Build.Unknown)
    case o: ujson.Obj =>
      val f = Fields(s"$what build", o)
      for {
        commit <- f.str("commit")
        dirty <- f.bool("dirty")
      } yield Build.Known(commit, dirty)
    case _ => Left(s"$what: build is neither unknown nor a commit")
  }

  private def readLive(v: ujson.Value): Either[String, Live] = {
    val f = Fields("tags", v)
    (if (v.objOpt.exists(_.contains("unanswered"))) f.optional("unanswered") else Right(None))
      .flatMap {
        case Some(u) =>
          Fields.str("tags: unanswered", u).flatMap(readFailure("tags")).map(Live.Unanswered(_))
        case None =>
          for {
            kind <- f.str("kind").flatMap(k => Kind.read(k).toRight(s"tags: no kind $k"))
            kindP <- f.probability("kind_p")
            waiting <- f.probability("waiting")
            durable <- f.probability("durable")
            helps <- f.probability("helps")
            model <- f.str("model")
            cost <- f
              .optional("cost_usd")
              .flatMap(opt(_) { c =>
                Fields
                  .str("tags: cost_usd", c)
                  .flatMap(t =>
                    Try(BigDecimal(t)).toOption.toRight("tags: cost_usd is not a number")
                  )
              })
          } yield Live.Weighed(kind, kindP, waiting, durable, helps, model, cost)
      }
  }

  /** The failure written `name` ([[Failure.written]]), read as part of `what`. */
  def readFailure(what: String)(name: String): Either[String, Failure] =
    Failure.read(name).toRight(s"$what: no failure $name")

  private def readAsked(v: ujson.Value): Either[String, Asked] = {
    val f = Fields("asked", v)
    for {
      input <- f.obj("input").flatMap(readBuilt("asked"))
      message <- f.int("message")
      thread <- f.int("thread")
    } yield Asked(input, message, thread)
  }

  private def readBuilt(what: String)(v: ujson.Value): Either[String, Built] = {
    val f = Fields(s"$what input", v)
    for {
      state <- f.str("state").flatMap(Digest.read)
      request <- f.str("request").flatMap(Digest.read)
    } yield Built(state, request)
  }

  private def readStitched(v: ujson.Value): Either[String, Stitched] = {
    val f = Fields("stitch", v)
    for {
      placed <- f.obj("placed").flatMap(readPlacement)
      offered <- f
        .field("offered")
        .flatMap(
          _.arrOpt
            .toRight("stitch: offered is not an array")
            .flatMap(_.toVector.foldLeft[Either[String, Vector[Offering]]](Right(Vector.empty)) {
              (acc, o) => acc.flatMap(done => readOffering(o).map(done :+ _))
            })
        )
      input <- f.optional("input").flatMap(opt(_)(readBuilt("stitch")))
      rebuilt <- f
        .field("rebuilt")
        .flatMap(_.arrOpt.toRight("stitch: rebuilt is not an array"))
        .flatMap(xs => each(xs.toVector)(readSlot))
      seen <- f.field("seen").flatMap(readSeen)
      drift <- f.int("drift")
    } yield Stitched(placed, offered, input, rebuilt, seen, drift)
  }

  private def readPlacement(v: ujson.Value): Either[String, Placement] = {
    val f = Fields("placed", v)
    val keys = v.objOpt.fold(Set.empty[String])(_.keySet.toSet)
    if (keys.contains("follows"))
      for {
        root <- f.str("follows").flatMap(CaseId.read)
        p <- f.probability("p")
      } yield Placement.Follows(root, p)
    else if (keys.contains("begins")) f.probability("begins").map(Placement.Begins(_))
    else if (keys.contains("unread"))
      f.str("unread").flatMap(readFailure("placed")).map(Placement.Unread(_))
    else Left("placed: neither follows, begins nor unread")
  }

  private def readOffering(v: ujson.Value): Either[String, Offering] = {
    val f = Fields("offered", v)
    for {
      root <- f.str("root").flatMap(CaseId.read)
      why <- f.obj("why").flatMap(readWhy("offered"))
      p <- f
        .optional("p")
        .flatMap(
          opt(_)(x => x.numOpt.flatMap(Probability.of).toRight("offered: p is not a probability"))
        )
    } yield Offering(root, why, p)
  }

  private def readWhy(what: String)(w: ujson.Value): Either[String, Offered] = {
    val wf = Fields(s"$what why", w)
    if (w.objOpt.exists(_.contains("recent"))) wf.int("recent").map(Offered.Recent(_))
    else wf.num("lexical").map(Offered.Lexical(_))
  }

  private def readSlot(v: ujson.Value): Either[String, Slot] = {
    val f = Fields("rebuilt", v)
    for {
      root <- f.str("root").flatMap(CaseId.read)
      why <- f.obj("why").flatMap(readWhy("rebuilt"))
    } yield Slot(root, why)
  }

  private def readSeen(v: ujson.Value): Either[String, SeenCheck] = v match {
    case ujson.Str("match") => Right(SeenCheck.Match)
    case ujson.Str("unbuilt") => Right(SeenCheck.Unbuilt)
    case o: ujson.Obj =>
      val f = Fields("seen", o)
      def fields: Either[String, Set[SeenCheck.Field]] =
        f.field("fields")
          .flatMap(_.arrOpt.toRight("seen: fields is not an array"))
          .flatMap(xs =>
            each(xs.toVector)(x =>
              x.strOpt.flatMap(SeenCheck.Field.read).toRight(s"seen: no field ${x.render()}")
            )
          )
          .map(_.toSet)
          .filterOrElse(_.nonEmpty, "seen: differs in no field")
      f.str("kind").flatMap {
        case "message_differs" => fields.map(new SeenCheck.MessageDiffers(_))
        case "lexical_only" => fields.map(new SeenCheck.LexicalOnly(_))
        case "recent_differs" =>
          f.field("ranks")
            .flatMap(_.arrOpt.toRight("seen: ranks is not an array"))
            .flatMap(xs =>
              each(xs.toVector)(x =>
                x.numOpt.filter(_.isWhole).map(_.toInt).toRight("seen: a rank is not an integer")
              )
            )
            .filterOrElse(_.nonEmpty, "seen: no rank differs")
            .map(new SeenCheck.RecentDiffers(_))
        case "same_root_differs" =>
          for {
            roots <- f
              .field("roots")
              .flatMap(_.arrOpt.toRight("seen: roots is not an array"))
              .flatMap(xs =>
                each(xs.toVector)(x => Fields.str("seen: root", x).flatMap(CaseId.read))
              )
              .filterOrElse(_.nonEmpty, "seen: no root differs")
            fs <- fields
          } yield new SeenCheck.SameRootDiffers(roots, fs)
        case k => Left(s"seen: no kind $k")
      }
    case _ => Left("seen: neither match, unbuilt nor a difference")
  }

  /** A tuning as [[writeTuning]] writes it. */
  def readTuning(v: ujson.Value): Either[String, Tuning] = {
    val f = Fields("tuning", v)
    for {
      horizon <- f.millis("horizon_ms")
      recent <- f.int("recent")
      lexical <- f.int("lexical")
      followsAt <- f.probability("follows_at")
      window <- f.num("window_tokens").filterOrElse(_.isWhole, "tuning: window_tokens is not whole")
      strand <- f.int("strand_chars")
    } yield Tuning(horizon, recent, lexical, followsAt, Tokens(window.toLong), strand)
  }

  private def readSettings(v: ujson.Value): Either[String, Settings] = {
    val f = Fields("settings", v)
    for {
      idle <- f.millis("idle_ms")
      retention <- f.millis("retention_ms")
      ledger <- f.millis("ledger_ms")
      balance <- f.int("balance")
      settle <- f.millis("settle_ms")
      resolveAt <- f.probability("resolve_at")
      asks <- f.int("asks")
      scope <- f.str("scope")
      weight <- f.num("weight")
    } yield Settings(idle, retention, ledger, balance, settle, resolveAt, asks, scope, weight)
  }

  private def writeTriaged(t: Triaged): ujson.Value = ujson.Obj(
    "workflow" -> WorkflowId.value(t.workflow),
    "recorded" -> t.recorded.fold[ujson.Value](ujson.Null)(r =>
      ujson.Obj("status" -> r.status, "epoch" -> r.epoch, "created" -> r.created.toString)
    ),
    "build" -> writeBuild(t.build)
  )

  /** `"unknown"`, or the commit and whether it was dirty. */
  def writeBuild(b: Build): ujson.Value = b match {
    case Build.Known(commit, dirty) => ujson.Obj("commit" -> commit, "dirty" -> dirty)
    case Build.Unknown => ujson.Str("unknown")
  }

  private def writeLive(l: Live): ujson.Value = l match {
    case Live.Weighed(kind, kindP, waiting, durable, helps, model, cost) =>
      ujson.Obj(
        "kind" -> Kind.written(kind),
        "kind_p" -> Probability.value(kindP),
        "waiting" -> Probability.value(waiting),
        "durable" -> Probability.value(durable),
        "helps" -> Probability.value(helps),
        "model" -> model,
        "cost_usd" -> cost.fold[ujson.Value](ujson.Null)(c => ujson.Str(c.toString))
      )
    case Live.Unanswered(failure) => ujson.Obj("unanswered" -> Failure.written(failure))
  }

  private def writeAsked(a: Asked): ujson.Value = ujson.Obj(
    "input" -> writeBuilt(a.input),
    "message" -> a.message,
    "thread" -> a.thread
  )

  private def writeBuilt(b: Built): ujson.Value =
    ujson.Obj("state" -> b.state.hex, "request" -> b.request.hex)

  private def writeStitched(s: Stitched): ujson.Value = ujson.Obj(
    "placed" -> (s.placed match {
      case Placement.Follows(root, p) =>
        ujson.Obj("follows" -> root.written, "p" -> Probability.value(p))
      case Placement.Begins(p) => ujson.Obj("begins" -> Probability.value(p))
      case Placement.Unread(failure) => ujson.Obj("unread" -> Failure.written(failure))
    }),
    "offered" -> ujson.Arr.from(s.offered.map { o =>
      ujson.Obj(
        "root" -> o.root.written,
        "why" -> writeWhy(o.why),
        "p" -> o.p.fold[ujson.Value](ujson.Null)(p => ujson.Num(Probability.value(p)))
      )
    }),
    "input" -> s.input.fold[ujson.Value](ujson.Null)(writeBuilt),
    "rebuilt" -> ujson.Arr.from(
      s.rebuilt.map(r => ujson.Obj("root" -> r.root.written, "why" -> writeWhy(r.why)))
    ),
    "seen" -> (s.seen match {
      case SeenCheck.Match => ujson.Str("match")
      case SeenCheck.Unbuilt => ujson.Str("unbuilt")
      case SeenCheck.MessageDiffers(fields) =>
        ujson.Obj("kind" -> "message_differs", "fields" -> writeFields(fields))
      case SeenCheck.RecentDiffers(ranks) =>
        ujson.Obj("kind" -> "recent_differs", "ranks" -> ujson.Arr.from(ranks.map(ujson.Num(_))))
      case SeenCheck.SameRootDiffers(roots, fields) =>
        ujson.Obj(
          "kind" -> "same_root_differs",
          "roots" -> ujson.Arr.from(roots.map(r => ujson.Str(r.written))),
          "fields" -> writeFields(fields)
        )
      case SeenCheck.LexicalOnly(fields) =>
        ujson.Obj("kind" -> "lexical_only", "fields" -> writeFields(fields))
    }),
    "drift" -> s.drift
  )

  private def writeWhy(why: Offered): ujson.Value = why match {
    case Offered.Recent(rank) => ujson.Obj("recent" -> rank)
    case Offered.Lexical(score) => ujson.Obj("lexical" -> score)
  }

  private def writeFields(fields: Set[SeenCheck.Field]): ujson.Value =
    ujson.Arr.from(SeenCheck.Field.values.filter(fields.contains).map(SeenCheck.Field.written))

  /** Durations in milliseconds, and the window in tokens. */
  def writeTuning(t: Tuning): ujson.Value = ujson.Obj(
    "horizon_ms" -> t.horizon.toMillis.toDouble,
    "recent" -> t.recent,
    "lexical" -> t.lexical,
    "follows_at" -> Probability.value(t.followsAt),
    "window_tokens" -> Tokens.value(t.windowTokens).toDouble,
    "strand_chars" -> t.strandChars
  )

  private def writeSettings(s: Settings): ujson.Value = ujson.Obj(
    "idle_ms" -> s.idle.toMillis.toDouble,
    "retention_ms" -> s.retention.toMillis.toDouble,
    "ledger_ms" -> s.ledger.toMillis.toDouble,
    "balance" -> s.balance,
    "settle_ms" -> s.settle.toMillis.toDouble,
    "resolve_at" -> Probability.value(s.resolveAt),
    "asks" -> s.asks,
    "scope" -> s.scope,
    "weight" -> s.weight
  )
}
