package grit.eval.harness.run

import scala.concurrent.duration.*

import grit.core.classify.{Answers, Classifier, ClassifierError, Request}
import grit.core.clock.Clock
import grit.core.message.Usage
import grit.core.store.Focus
import grit.eval.harness.corpus.{CaseId, Digest, Failure}
import grit.eval.harness.jev.{Asking, Budget, Spend}
import grit.eval.harness.log.{Cache, CacheKey, Cached, Outcome, Row, Suite, Weights}
import grit.models.JevJson

/** One call a run makes: the case and suite it is for, which repeat, how it is asked, the
  * request the shipped builder made for it, and the focus its row records ([[Row.focus]]).
  */
final case class Call(
    suite: Suite,
    id: CaseId,
    repeat: Int,
    asking: Asking,
    request: Request,
    focus: Option[Focus]
)

/** What a run made: a row per call, in order, and what it spent. */
final case class Ran(rows: Vector[Row[Vector[Weights]]], budget: Budget)

/** A run's calls, asked of Jev. */
object Run {

  /** The tag Jev's calls are cached under ([[CacheKey.of]]). */
  val Provider = "jev"

  /** `request` as Jev's wire body for `model` ([[JevJson.body]]). */
  def body(model: String, request: Request): String =
    JevJson.body(model, request.state, request.questions)

  /** What `call` is cached under, asked of `model`. */
  def key(model: String, call: Call): CacheKey =
    CacheKey.of(Provider, body(model, call.request), call.repeat)

  /** What `calls` cost at most, asked of `model` ([[Spend.estimate]]), but those whose key
    * `cached` holds, which are free.
    */
  def estimate(calls: Vector[Call], model: String, cached: CacheKey => Boolean): BigDecimal =
    calls.filterNot(c => cached(key(model, c))).map(c => Spend.estimate(body(model, c.request))).sum

  /** Each of `calls`, in order, asked of `jev` through the shipped call it was built for
    * ([[Asking.ask]]), as `model` (the model its wire body names, and so its cache key's):
    * answered from `cache` when it holds the call's key, at no cost; else, when `budget`
    * allows the call's estimate ([[Spend.estimate]]), asked, timed by `clock`, kept in
    * `cache`, and its reported cost spent (its estimate, when none is reported); else
    * `Skipped`, as is every call after the first skipped that the cache cannot answer. A
    * cached answer that does not read is asked again. A row's request and key are those of
    * the request the shipped call sent.
    */
  def apply(
      calls: Vector[Call],
      jev: Classifier^,
      model: String,
      cache: Cache[Vector[Weights]]^,
      budget: Budget,
      clock: Clock^
  ): Ran = {
    val state = new State(budget)
    val rows: Vector[Row[Vector[Weights]]] = calls.map { call =>
      state.made = None
      val around = Classifier.around(jev)((request, ask) => {
        val wire = body(model, request)
        val key = CacheKey.of(Provider, wire, call.repeat)
        def row(
            outcome: Outcome[Vector[Weights]],
            reported: Option[String],
            usage: Usage,
            latency: FiniteDuration,
            cached: Boolean
        ): Unit = state.made = Some(
          Row(
            call.suite,
            call.id,
            call.repeat,
            Digest.request(request),
            key,
            model,
            reported,
            outcome,
            usage,
            latency,
            cached,
            call.focus
          )
        )
        val hit = cache
          .get(key)
          .toOption
          .flatten
          .flatMap(c => answers(request, c).map(c -> _))
        hit match {
          case Some((c, answered)) =>
            row(Outcome.Answered(c.answer), Some(c.reported), c.usage, c.latency, cached = true)
            Right(answered)
          case None =>
            val estimate = Spend.estimate(wire)
            if (state.stopped || !state.left.allows(estimate)) {
              state.stopped = true
              row(Outcome.Skipped, None, Usage.Zero, Duration.Zero, cached = false)
              Left(ClassifierError.Unavailable("skipped at the spend cap"))
            } else {
              val start = clock.millis()
              val result = ask()
              val latency = (clock.millis() - start).millis
              result match {
                case Right(a) =>
                  state.left = state.left.spend(a.usage.costUsd.getOrElse(estimate))
                  weights(request, a) match {
                    case Some(ws) =>
                      // A cache write that fails costs a later run this call, nothing more.
                      val _ = cache.put(key, Cached(ws, a.usage, a.model, latency))
                      row(Outcome.Answered(ws), Some(a.model), a.usage, latency, cached = false)
                    case None =>
                      row(
                        Outcome.Failed(Failure.Unreadable),
                        None,
                        a.usage,
                        latency,
                        cached = false
                      )
                  }
                case Left(e) =>
                  row(Outcome.Failed(Failure.of(e)), None, Usage.Zero, latency, cached = false)
              }
              result
            }
        }
      })
      Asking.ask(call.asking, around)
      state.made.getOrElse(
        // The shipped call sent nothing: its question repeats a key.
        Row(
          call.suite,
          call.id,
          call.repeat,
          Digest.request(call.request),
          key(model, call),
          model,
          None,
          Outcome.Failed(Failure.Unasked),
          Usage.Zero,
          Duration.Zero,
          cached = false,
          call.focus
        )
      )
    }
    Ran(rows, state.left)
  }

  /** What one [[apply]] has spent, whether it has stopped at the cap, and the row its current
    * call made.
    */
  private final class State(start: Budget) {
    // Each holds immutable values, only replaced. One `apply` makes the State and drops it
    // before returning; the classifiers built around it are asked only inside that call, one
    // at a time, so no other code can observe the replacements.
    @caps.unsafe.untrackedCaptures
    var left: Budget = start
    @caps.unsafe.untrackedCaptures
    var stopped: Boolean = false
    @caps.unsafe.untrackedCaptures
    var made: Option[Row[Vector[Weights]]] = None
  }

  /** `a`'s answers to `request`'s questions by position; `None` when one is not of its
    * question's kind, or they are not one each.
    */
  private def weights(request: Request, a: Answers): Option[Vector[Weights]] =
    Option
      .when(a.answers.size == request.questions.size)(
        request.questions.zip(a.answers).map(Weights.of(_, _))
      )
      .flatMap(ws => Option.when(ws.forall(_.isDefined))(ws.flatten))

  /** `c` as answers to `request`'s questions; `None` when it does not answer them. */
  private def answers(request: Request, c: Cached[Vector[Weights]]): Option[Answers] =
    Option
      .when(c.answer.size == request.questions.size)(
        request.questions.zip(c.answer).map(Weights.answer(_, _))
      )
      .flatMap(as => Option.when(as.forall(_.isDefined))(as.flatten))
      .map(Answers(_, c.usage, c.reported))
}
