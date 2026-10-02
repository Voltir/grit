package grit.eval.harness.log

import scala.util.Try

import grit.core.message.{Tokens, Usage}
import grit.eval.harness.corpus.Fields.{each, opt}
import grit.eval.harness.corpus.{CaseId, CorpusJson, Digest, Failure, Fields}

/** How a row's answer `A` is written in a run's log and its cache. */
trait Codec[A] {
  def write(a: A): ujson.Value
  def read(v: ujson.Value): Either[String, A]
}

/** A run's log, read back: its header, its rows in order, and its footer (`None` when the run
  * stopped before writing one).
  */
final case class Log[A](header: Header, rows: Vector[Row[A]], footer: Option[Footer])

/** A run's log as JSON lines: the header, one line per row, then the footer, each an object
  * with one field naming which (`header`, `row`, `footer`). Each value is written with its
  * fields in one fixed order, so equal values write equal bytes. A read is `Left` naming the
  * first field missing or not of its form.
  */
object LogJson {

  /** A classifier's answers, by position ([[Weights]]); the given for them. */
  val weights: Codec[Vector[Weights]] = new Codec[Vector[Weights]] {
    def write(a: Vector[Weights]): ujson.Value = ujson.Arr.from(a.map {
      case Weights.Choice(chosen, probabilities, confidence) =>
        ujson.Obj(
          "chosen" -> chosen,
          "probabilities" -> ujson.Arr.from(probabilities.map(ujson.Num(_))),
          "confidence" -> confidence
        )
      case Weights.YesNo(yes) => ujson.Obj("yes" -> yes)
    })
    def read(v: ujson.Value): Either[String, Vector[Weights]] =
      v.arrOpt.toRight("answers: not an array").flatMap { xs =>
        each(xs.toVector) { x =>
          val f = Fields("answer", x)
          if (x.objOpt.exists(_.contains("yes"))) f.num("yes").map(Weights.YesNo(_))
          else
            for {
              chosen <- f.int("chosen")
              ps <- f.arr("probabilities")
              probabilities <- each(ps)(_.numOpt.toRight("answer: a probability is not a number"))
              confidence <- f.num("confidence")
            } yield Weights.Choice(chosen, probabilities, confidence)
        }
      }
  }

  def header(h: Header): String = ujson
    .Obj(
      "header" -> ujson.Obj(
        "corpus" -> h.corpus,
        "corpus_digest" -> h.corpusDigest.hex,
        "labels" -> digest(h.labels),
        "variant" -> h.variant,
        "wording" -> digest(h.wording),
        "model" -> h.model,
        "tuning" -> h.tuning.fold[ujson.Value](ujson.Null)(CorpusJson.writeTuning),
        "build" -> CorpusJson.writeBuild(h.build),
        "repeats" -> h.repeats,
        "cache" -> h.cache,
        "cap_usd" -> h.cap.toString,
        "rule" -> digest(h.rule),
        "started" -> h.started.toString
      )
    )
    .render()

  def row[A](r: Row[A])(using codec: Codec[A]): String = ujson
    .Obj(
      "row" -> ujson.Obj(
        "suite" -> Suite.written(r.suite),
        "case" -> r.id.written,
        "repeat" -> r.repeat,
        "request" -> r.request.hex,
        "key" -> r.key.hex,
        "requested" -> r.requested,
        "reported" -> r.reported.fold[ujson.Value](ujson.Null)(ujson.Str(_)),
        "outcome" -> (r.outcome match {
          case Outcome.Answered(a) => ujson.Obj("answered" -> codec.write(a))
          case Outcome.Failed(failure) => ujson.Obj("failed" -> Failure.written(failure))
          case Outcome.Skipped => ujson.Str("skipped")
        }),
        "usage" -> writeUsage(r.usage),
        "latency_ms" -> r.latency.toMillis.toDouble,
        "cached" -> r.cached
      )
    )
    .render()

  def footer(f: Footer): String = ujson
    .Obj(
      "footer" -> ujson.Obj(
        "spent_usd" -> f.spent.toString,
        "answered" -> f.answered,
        "cached" -> f.cached,
        "failed" -> f.failed,
        "skipped" -> f.skipped
      )
    )
    .render()

  /** The log in `lines`: a header, rows, and at most one footer, last. `Left` naming the first
    * line that does not read, or when a footer is not last.
    */
  def read[A: Codec](lines: Vector[String]): Either[String, Log[A]] = {
    def parsed(i: Int): Either[String, ujson.Value] =
      lines
        .lift(i)
        .flatMap(l => Try(ujson.read(l)).toOption)
        .toRight(s"line ${i + 1}: not JSON")
    for {
      first <- parsed(0)
      header <- first.objOpt
        .flatMap(_.get("header"))
        .toRight("line 1: not a header")
        .flatMap(h => readHeader(h).left.map(e => s"line 1: $e"))
      rest <- each(lines.indices.drop(1).toVector)(i =>
        parsed(i).flatMap { v =>
          val o = v.objOpt
          o.flatMap(_.get("row")) match {
            case Some(r) => readRow[A](r).map(Left(_)).left.map(e => s"line ${i + 1}: $e")
            case None =>
              o.flatMap(_.get("footer"))
                .toRight(s"line ${i + 1}: neither a row nor a footer")
                .flatMap(f => readFooter(f).map(Right(_)).left.map(e => s"line ${i + 1}: $e"))
          }
        }
      )
      rows = rest.collect { case Left(r) => r }
      footers = rest.collect { case Right(f) => f }
      _ <- Either.cond(
        footers.isEmpty || rest.indexWhere(_.isRight) == rest.size - 1,
        (),
        "the footer is not the last line"
      )
    } yield Log(header, rows, footers.headOption)
  }

  private def readHeader(v: ujson.Value): Either[String, Header] = {
    val f = Fields("header", v)
    for {
      corpus <- f.str("corpus")
      corpusDigest <- f.str("corpus_digest").flatMap(Digest.read)
      labels <- f.optional("labels").flatMap(opt(_)(readDigest("labels")))
      variant <- f.str("variant")
      wording <- f.optional("wording").flatMap(opt(_)(readDigest("wording")))
      model <- f.str("model")
      tuning <- f.optional("tuning").flatMap(opt(_)(CorpusJson.readTuning))
      build <- f.field("build").flatMap(CorpusJson.readBuild("header", _))
      repeats <- f.int("repeats")
      cache <- f.bool("cache")
      cap <- f.decimal("cap_usd")
      rule <- f.optional("rule").flatMap(opt(_)(readDigest("rule")))
      started <- f.instant("started")
    } yield Header(
      corpus,
      corpusDigest,
      labels,
      variant,
      wording,
      model,
      tuning,
      build,
      repeats,
      cache,
      cap,
      rule,
      started
    )
  }

  private def readRow[A](v: ujson.Value)(using codec: Codec[A]): Either[String, Row[A]] = {
    val f = Fields("row", v)
    for {
      suite <- f.str("suite").flatMap(s => Suite.read(s).toRight(s"row: no suite $s"))
      id <- f.str("case").flatMap(CaseId.read)
      repeat <- f.int("repeat")
      request <- f.str("request").flatMap(Digest.read)
      key <- f.str("key").flatMap(CacheKey.read)
      requested <- f.str("requested")
      reported <- f.optional("reported").flatMap(opt(_)(Fields.str("row: reported", _)))
      outcome <- f.field("outcome").flatMap {
        case ujson.Str("skipped") => Right(Outcome.Skipped)
        case o: ujson.Obj if o.value.contains("answered") =>
          codec.read(o("answered")).map(Outcome.Answered(_))
        case o: ujson.Obj =>
          Fields("outcome", o)
            .str("failed")
            .flatMap(n => Failure.read(n).toRight(s"row: no failure $n"))
            .map(Outcome.Failed(_))
        case _ => Left("row: outcome is neither answered, failed nor skipped")
      }
      usage <- f.obj("usage").flatMap(readUsage)
      latency <- f.millis("latency_ms")
      cached <- f.bool("cached")
    } yield Row(
      suite,
      id,
      repeat,
      request,
      key,
      requested,
      reported,
      outcome,
      usage,
      latency,
      cached
    )
  }

  private def readFooter(v: ujson.Value): Either[String, Footer] = {
    val f = Fields("footer", v)
    for {
      spent <- f.decimal("spent_usd")
      answered <- f.int("answered")
      cached <- f.int("cached")
      failed <- f.int("failed")
      skipped <- f.int("skipped")
    } yield Footer(spent, answered, cached, failed, skipped)
  }

  /** Tokens as whole numbers; the cost as a decimal string, so it keeps every digit. */
  def writeUsage(u: Usage): ujson.Value = ujson.Obj(
    "input" -> Tokens.value(u.input).toDouble,
    "output" -> Tokens.value(u.output).toDouble,
    "cached_input" -> Tokens.value(u.cachedInput).toDouble,
    "cost_usd" -> u.costUsd.fold[ujson.Value](ujson.Null)(c => ujson.Str(c.toString))
  )

  /** A usage as [[writeUsage]] writes it. */
  def readUsage(v: ujson.Value): Either[String, Usage] = {
    val f = Fields("usage", v)
    for {
      input <- f.long("input")
      output <- f.long("output")
      cachedInput <- f.long("cached_input")
      cost <- f.optional("cost_usd").flatMap(opt(_)(_ => f.decimal("cost_usd")))
    } yield Usage(Tokens(input), Tokens(output), Tokens(cachedInput), cost)
  }

  private def digest(d: Option[Digest]): ujson.Value =
    d.fold[ujson.Value](ujson.Null)(x => ujson.Str(x.hex))

  private def readDigest(what: String)(v: ujson.Value): Either[String, Digest] =
    Fields.str(s"header: $what", v).flatMap(Digest.read)
}
