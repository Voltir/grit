package grit.eval.harness.log

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.classify.ClassifierError
import grit.core.message.{Tokens, Usage}
import grit.core.stitch.Tuning
import grit.core.store.Focus
import grit.dbos.engine.Build
import grit.eval.harness.corpus.{CaseId, Digest, Failure}

import utest.*

/** A run's log read back as written. Every id and digest here is synthetic. */
object LogJsonTests extends TestSuite {

  private def id(s: String): CaseId = CaseId.read(s).fold(sys.error, identity)
  private val key = CacheKey.read("ab" * 32).fold(sys.error, identity)
  private val usage = Usage(Tokens(1021), Tokens(3), Tokens.Zero, Some(BigDecimal("0.000042882")))

  private val header = Header(
    "20261002",
    Digest.text("corpus"),
    Some(Digest.text("labels")),
    "live",
    Some(Digest.text("wording")),
    "jev-1.13.0",
    Some(Tuning.Default),
    Some(Digest.text("recipe")),
    Build.Known("0123456789abcdef0123456789abcdef01234567", false),
    5,
    false,
    BigDecimal("0.01"),
    Some(Digest.text("rule")),
    Instant.parse("2026-10-02T21:00:00.123Z")
  )

  private val bareHeader =
    header.copy(
      labels = None,
      tuning = None,
      recipe = None,
      build = Build.Unknown,
      rule = None,
      wording = None
    )

  private val answered: Row[Vector[Weights]] = Row(
    Suite.Triage,
    id("C1/1727000000.000200"),
    2,
    Digest.text("request"),
    key,
    "jev-1.13.0",
    Some("jev-1.13.0"),
    Outcome.Answered(
      Vector(
        Weights.Choice(1, Vector(0.1, 0.7, 0.05, 0.05, 0.1), 0.625),
        Weights.YesNo(0.25),
        Weights.YesNo(0.9),
        Weights.YesNo(0.0)
      )
    ),
    usage,
    412.millis,
    true,
    Some(Focus.Open)
  )

  private val failed: Row[Vector[Weights]] = answered.copy(
    suite = Suite.Stitch,
    reported = None,
    outcome = Outcome.Failed(Failure.Unreadable),
    usage = Usage.Zero.copy(costUsd = None),
    cached = false,
    focus = None
  )

  private val skipped: Row[Vector[Weights]] =
    failed.copy(outcome = Outcome.Skipped, latency = Duration.Zero)

  private val footer = Footer(BigDecimal("0.000514"), 12, 3, 1, 2)

  val tests = Tests {
    test("a log is read back as written: header, every kind of row, footer") {
      val lines = Vector(
        LogJson.header(header),
        LogJson.row(answered),
        LogJson.row(failed),
        LogJson.row(skipped),
        LogJson.footer(footer)
      )
      LogJson.read[Vector[Weights]](lines) ==>
        Right(Log(header, Vector(answered, failed, skipped), Some(footer)))
    }

    test("a header with nothing optional, and a log without a footer, read back") {
      LogJson.read[Vector[Weights]](Vector(LogJson.header(bareHeader), LogJson.row(answered))) ==>
        Right(Log(bareHeader, Vector(answered), None))
    }

    test("a header without a recipe, and a row without a focus, as logged before both, read") {
      val h = ujson.read(LogJson.header(header))
      h("header").obj.remove("recipe")
      val r = ujson.read(LogJson.row(answered))
      r("row").obj.remove("focus")
      LogJson.read[Vector[Weights]](Vector(h.render(), r.render())) ==>
        Right(Log(header.copy(recipe = None), Vector(answered.copy(focus = None)), None))
    }

    test("a log that does not begin with its header is refused") {
      LogJson.read[Vector[Weights]](Vector(LogJson.row(answered))) ==>
        Left("line 1: not a header")
    }

    test("a log whose footer is not its last line is refused") {
      LogJson.read[Vector[Weights]](
        Vector(LogJson.header(header), LogJson.footer(footer), LogJson.row(answered))
      ) ==> Left("the footer is not the last line")
    }

    test("a row missing a field is refused, naming the line and the field") {
      val broken = ujson.read(LogJson.row(answered))
      broken("row").obj.remove("key")
      LogJson.read[Vector[Weights]](Vector(LogJson.header(header), broken.render())) ==>
        Left("line 2: row: no key")
    }

    // A Jev error body can echo the request; a failed row keeps the kind and nothing else.
    test("a row failed as unavailable with a body writes the kind and not the body") {
      val body = "HTTP 400: echoed state text"
      val row: Row[Vector[Weights]] = failed.copy(
        outcome = Outcome.Failed(Failure.of(ClassifierError.Unavailable(body)))
      )
      val line = LogJson.row(row)
      ujson.read(line)("row")("outcome") ==> ujson.Obj("failed" -> "unavailable")
      assert(!line.contains("echoed"))
    }

    // The line form is what scoring reads from runs/*.jsonl: its keys are pinned.
    test("a row's line keeps its keys in one order, answers by position") {
      LogJson.row(answered) ==>
        s"""{"row":{"suite":"triage","case":"C1/1727000000.000200","repeat":2,"request":"${Digest
            .text("request")
            .hex}","key":"${"ab" * 32}","requested":"jev-1.13.0","reported":"jev-1.13.0","outcome":{"answered":[{"chosen":1,"probabilities":[0.1,0.7,0.05,0.05,0.1],"confidence":0.625},{"yes":0.25},{"yes":0.9},{"yes":0}]},"usage":{"input":1021,"output":3,"cached_input":0,"cost_usd":"0.000042882"},"latency_ms":412,"cached":true,"focus":"open"}}"""
    }
  }
}
