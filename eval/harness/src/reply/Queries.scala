package grit.eval.harness.reply

import scala.concurrent.duration.*

import grit.core.clock.Clock
import grit.core.message.{AssistantBlock, Message}
import grit.core.provider.{ModelRequest, Provider, ProviderError}
import grit.eval.harness.jev.Budget
import grit.eval.harness.log.{Cache, CacheKey, Cached, Codec}
import grit.models.{OpenRouterConfig, OpenRouterJson}

/** What a run of query writes came to: each request's answer, how many were asked, answered
  * from the cache, skipped past the cap or failed, and the budget after them.
  */
final case class Answered(
    answers: Map[ModelRequest, Message.Assistant],
    asked: Int,
    cached: Int,
    skipped: Int,
    failed: Int,
    budget: Budget
)

/** The query writes window-only cases need, each kept in a cache under its request's key, so
  * an unchanged request is never paid for twice. A kept answer is the query's text.
  */
object Queries {

  /** What one query write is taken to cost before it is sent, in USD: about seven times what
    * one costs on the cheap query model (~$0.00007), so a cap set from it holds.
    */
  val Usd: BigDecimal = BigDecimal("0.0005")

  /** A kept answer's codec: the query's text. */
  given text: Codec[String] = new Codec[String] {
    def write(a: String): ujson.Value = ujson.Str(a)
    def read(v: ujson.Value): Either[String, String] = v.strOpt.toRight("not a string")
  }

  /** A writer for a run whose writes are all kept, so it is never asked; asked, it fails. */
  def unasked(): Provider^ = new Provider {
    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] =
      Left(ProviderError.Unavailable("no write was to be asked"))
  }

  /** The key `request`'s answer is kept under when `config` writes it: its wire body, which
    * names the model and its limit.
    */
  def key(config: OpenRouterConfig, request: ModelRequest): CacheKey =
    CacheKey.of(
      "query",
      OpenRouterJson
        .request(
          config.model,
          config.maxTokens,
          config.upstream,
          request,
          config.effort,
          config.replay
        )
        .render(),
      0
    )

  /** What asking the requests of `requests` that `cache` does not hold is estimated to cost
    * ([[Usd]] each), each distinct request once.
    */
  def estimate(
      requests: Vector[ModelRequest],
      config: OpenRouterConfig,
      cache: Cache[String]
  ): BigDecimal =
    Usd * requests.distinct.count(r => !cache.get(key(config, r)).exists(_.isDefined))

  /** Each distinct request of `requests` answered: from `cache` when it holds the answer, else
    * asked of `writer` (which writes as `config` says) while `budget` allows [[Usd]] more, and
    * kept, its cost what the answer reports or, unreported, [[Usd]]. A request past the cap is
    * skipped; one the writer, or the cache, fails is counted failed and has no answer.
    */
  def answer(
      requests: Vector[ModelRequest],
      config: OpenRouterConfig,
      cache: Cache[String],
      writer: Provider^,
      budget: Budget,
      clock: Clock^
  ): Answered =
    requests.distinct.foldLeft(Answered(Map.empty, 0, 0, 0, 0, budget)) { (done, r) =>
      val k = key(config, r)
      cache.get(k) match {
        case Left(_) => done.copy(failed = done.failed + 1)
        case Right(Some(c)) =>
          done.copy(
            answers = done.answers + (r -> WindowOnly.reply(c.answer, c.usage, c.reported)),
            cached = done.cached + 1
          )
        case Right(None) if !done.budget.allows(Usd) => done.copy(skipped = done.skipped + 1)
        case Right(None) =>
          val started = clock.millis()
          writer.complete(r) match {
            case Left(_) => done.copy(failed = done.failed + 1, budget = done.budget.spend(Usd))
            case Right(m) =>
              val text = m.blocks.collect { case AssistantBlock.Text(t) => t }.mkString
              val kept =
                cache.put(k, Cached(text, m.usage, m.model, (clock.millis() - started).millis))
              done.copy(
                answers = done.answers + (r -> m),
                asked = done.asked + 1,
                failed = done.failed + kept.fold(_ => 1, _ => 0),
                budget = done.budget.spend(m.usage.costUsd.getOrElse(Usd))
              )
          }
      }
    }
}
