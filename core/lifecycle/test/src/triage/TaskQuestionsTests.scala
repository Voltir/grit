package grit.lifecycle.triage

import grit.core.allot.TaskKind
import grit.core.classify.{Answer, Request}
import grit.core.period.Probability

import utest.*

object TaskQuestionsTests extends TestSuite {

  private val state =
    TriageQuestion.State("why did signups drop in March?", "ana", "bo: the dashboard is up")

  private val lead =
    "Read new_message, which author sent to an AI assistant; thread is what came before it. "

  val tests = Tests {
    test(
      "the task questions ask kind among seven keys and hard, in the calibrated words, in one request"
    ) {
      def key(name: String, description: String) =
        ujson.Obj("name" -> name, "description" -> description)
      Request.json(Request.of(state, TaskQuestions.Shipped))("questions") ==> ujson.Arr(
        ujson.Obj(
          "kind" -> "choice",
          "instructions" -> (lead + "What does new_message ask the assistant to do? Analysis " +
            "computes over structured data; investigation reads and reconciles prose and history."),
          "keys" -> ujson.Arr(
            key("chat", "nothing to do: a greeting, thanks, an acknowledgement or small talk"),
            key("lookup", "find or recall one fact, link, name or document"),
            key(
              "investigation",
              "find out why something happened or what is going on by reading threads, docs, " +
                "notes, logs or tickets"
            ),
            key(
              "analysis",
              "compute over structured data such as tables, metrics, a database or a " +
                "spreadsheet: query, count, compare or chart"
            ),
            key("writing", "draft, rewrite, translate or summarise text"),
            key("coding", "write, fix, review or explain code"),
            key("decision", "weigh options and recommend or choose one")
          )
        ),
        ujson.Obj(
          "kind" -> "yes_no",
          "instructions" -> (lead + "Is what new_message asks hard: work a careful expert would " +
            "do in several steps, or by reading several sources, rather than with one quick " +
            "answer? A greeting, thanks or a single fact is not hard."),
          "yes" -> ujson.Null,
          "no" -> ujson.Null
        )
      )
    }

    test("an answer reads into each kind's weight and hard's probability") {
      val weights = Vector(0.05, 0.05, 0.2, 0.6, 0.05, 0.0, 0.05)
      val features = TaskQuestions.Shipped
        .read(
          Vector(
            Answer.Choice(
              "analysis",
              TaskKind.values.toVector.map(_.word).zip(weights).map(Answer.Weight(_, _)),
              0.5
            ),
            Answer.YesNo(0.72)
          )
        )
        .fold(e => throw new java.lang.AssertionError(e.toString), identity)
      features.kind ==> TaskKind.Analysis
      features.hard ==> Probability.clamped(0.72)
      features.kinds.values.map(Probability.value).toVector.map(p => math.round(p * 100)) ==>
        Vector(5L, 5L, 20L, 60L, 5L, 0L, 5L)
    }
  }
}
