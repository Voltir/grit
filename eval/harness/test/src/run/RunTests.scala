package grit.eval.harness.run

import java.nio.file.Files
import java.time.Instant

import scala.concurrent.duration.*

import grit.core.classify.Classifier
import grit.core.clock.Clock
import grit.core.store.Focus
import grit.core.triage.Corpora
import grit.eval.harness.corpus.{CaseId, Digest, Failure}
import grit.eval.harness.jev.{Asking, Budget, Spend}
import grit.eval.harness.log.{Cache, Outcome, Suite, Weights}
import grit.lifecycle.triage.{TriageQuestion, TriageQuestions}
import grit.models.StubClassifier

import utest.*

/** The run loop over the stub classifier: what the cache answers, where the cap stops, and
  * what a failure leaves. Every message here is synthetic.
  */
object RunTests extends TestSuite {

  private val Model = "jev-test"

  private def id(s: String): CaseId = CaseId.read(s).fold(sys.error, identity)

  private def call(n: Int, repeat: Int = 0, thread: String = "the thread before it"): Call = {
    val state = TriageQuestion.State(s"~0.${n} message $n", "Ann", thread)
    val wording = TriageQuestion.Wording.Shipped
    Call(
      Suite.Triage,
      id(s"C1/172700000$n.000100"),
      repeat,
      Asking.Triage(state, wording),
      TriageQuestions.v1(wording).request(state, Corpora.Empty),
      Some(Focus.Open)
    )
  }

  /** A clock whose every reading is 7 ms after the one before. */
  private def ticking(): Clock^ = new Clock {
    // An immutable Long, replaced; read only by the test that owns the clock.
    @caps.unsafe.untrackedCaptures
    private var at = 0L
    def now(): Instant = Instant.EPOCH.plusMillis(millis())
    def millis(): Long = { at += 7; at }
    def sleep(duration: FiniteDuration): Unit = ()
  }

  /** The stub, each call counted and charged its own estimate. */
  private final class Charged extends caps.SharedCapability {
    // An immutable Int, replaced; read only by the test that owns the stub.
    @caps.unsafe.untrackedCaptures
    var asked = 0
    val classifier: Classifier^ = Classifier.around(StubClassifier())((request, ask) => {
      asked += 1
      ask().map(a =>
        a.copy(usage = a.usage.copy(costUsd = Some(Spend.estimate(Run.body(Model, request)))))
      )
    })
  }

  private def budget(cap: BigDecimal): Budget =
    Budget.of(cap, BigDecimal(0)).fold(r => sys.error(r.toString), identity)

  val tests = Tests {
    test("a call the cache holds is answered from it, never asked, at no cost") {
      val cache = Cache.at[Vector[Weights]](Files.createTempDirectory("grit-run-test"))
      val calls = Vector(call(1), call(2))
      val first = Charged()
      val ran = Run(calls, first.classifier, Model, cache, budget(BigDecimal(1)), ticking())
      val again = Charged()
      val rerun = Run(calls, again.classifier, Model, cache, ran.budget, ticking())
      (first.asked, again.asked) ==> (2, 0)
      rerun.budget.spent ==> ran.budget.spent
      rerun.rows.map(_.cached) ==> Vector(true, true)
      rerun.rows.map(_.copy(cached = false)) ==> ran.rows
    }

    test("the estimate counts only the calls the cache does not hold") {
      val calls = Vector(call(1), call(2), call(3))
      val held = Set(Run.key(Model, calls(1)))
      Run.estimate(calls, Model, held.contains) ==>
        Spend.estimate(Run.body(Model, calls(0).request)) +
        Spend.estimate(Run.body(Model, calls(2).request))
    }

    test("the cap stops before the call that would cross it, and every call after is skipped") {
      // The third is long: after the first two, it does not fit, and the fourth alone would.
      val calls = Vector(call(1), call(2), call(3, thread = "words " * 400), call(4))
      def cost(i: Int) = Spend.estimate(Run.body(Model, calls(i).request))
      val cap = cost(0) + cost(1) + cost(3)
      val charged = Charged()
      val ran = Run(calls, charged.classifier, Model, Cache.off, budget(cap), ticking())
      charged.asked ==> 2
      ran.rows.map(_.outcome == Outcome.Skipped) ==> Vector(false, false, true, true)
      ran.budget.spent ==> cost(0) + cost(1)
    }

    test("each repeat is its own call, under its own key") {
      val cache = Cache.at[Vector[Weights]](Files.createTempDirectory("grit-run-test"))
      val charged = Charged()
      val ran =
        Run(
          Vector(call(1), call(1, repeat = 1)),
          charged.classifier,
          Model,
          cache,
          budget(BigDecimal(1)),
          ticking()
        )
      charged.asked ==> 2
      ran.rows.map(_.key).distinct.size ==> 2
      ran.rows.map(_.repeat) ==> Vector(0, 1)
    }

    test("an answered row keeps the request, the model reported, the answers, time and focus") {
      val ran = Run(
        Vector(call(3)),
        Charged().classifier,
        Model,
        Cache.off,
        budget(BigDecimal(1)),
        ticking()
      )
      val row = ran.rows.head
      (row.request, row.key, row.requested, row.reported, row.latency, row.cached, row.focus) ==>
        (
          Digest.request(call(3).request),
          Run.key(Model, call(3)),
          Model,
          Some(StubClassifier.Model),
          7.millis,
          false,
          Some(Focus.Open)
        )
      row.outcome match {
        case Outcome.Answered(
              Vector(Weights.Choice(_, ps, _), Weights.YesNo(w), Weights.YesNo(d), Weights.YesNo(h))
            ) =>
          (ps.size, w, d, h) ==> (5, 0.3, 0.3, 0.3)
        case other => throw new java.lang.AssertionError(s"not four answers: $other")
      }
    }

    test("a failed call keeps its kind, and no model, cost or cache entry") {
      val cache = Cache.at[Vector[Weights]](Files.createTempDirectory("grit-run-test"))
      val down = Classifier.none("HTTP 500: echoed request text")
      val ran = Run(Vector(call(1)), down, Model, cache, budget(BigDecimal(1)), ticking())
      ran.rows.map(r => (r.outcome, r.reported)) ==> Vector(
        (Outcome.Failed(Failure.Unavailable), None)
      )
      ran.budget.spent ==> BigDecimal(0)
      cache.get(Run.key(Model, call(1))) ==> Right(None)
    }
  }
}
