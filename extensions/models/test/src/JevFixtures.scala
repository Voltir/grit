package grit.models

import grit.core.classify.{Ask, Criterion, Question}

/** The questions and answers of Jev's documented examples, as its API docs give them. */
object JevFixtures {

  val department: Vector[Question] = Ask
    .choice[Unit, String](
      "Which team should handle this?",
      Criterion("billing", "billing", Some("Payments, invoicing, refunds")),
      Criterion("technical", "technical", Some("Bugs, outages, integrations")),
      Criterion("sales", "sales", None)
    )
    .fold(d => throw new java.lang.AssertionError(s"keys repeat: $d"), _.questions)

  val urgent: Question.YesNo = Question.YesNo(
    "Does this convey urgency?",
    Some("Explicitly time-sensitive"),
    Some("No urgency expressed")
  )

  val frustration: Question.Score = Question
    .score("How frustrated is the customer?", "Calm", "Frustrated", "Very angry")
    .fold(t => throw new java.lang.AssertionError(s"levels: $t"), identity)

  /** The docs' answer to [[frustration]]: its weights keyed out of level order, a legend. */
  val ScoreBody: ujson.Value = ujson.read("""{
    "model": "jev-1.13.0",
    "answers": {
      "q1": {
        "type": "score",
        "score": 1.05,
        "legend": { "0": "Calm", "1": "Frustrated", "2": "Very angry" },
        "probabilities": { "2": 0.05, "0": 0.0, "1": 0.95 },
        "confidence": 0.92
      }
    },
    "usage": { "input_tokens": 304, "output_tokens": 18 }
  }""")

  /** The docs' answers to [[department]] then [[urgent]]. */
  val ChoiceBody: ujson.Value = ujson.read("""{
    "model": "jev-1.13.0",
    "answers": {
      "q1": {
        "type": "choice",
        "choice": "billing",
        "probabilities": { "sales": 0.0, "billing": 0.88, "technical": 0.12 },
        "confidence": 0.81
      },
      "q2": { "type": "noul", "noul": 0.95 }
    },
    "usage": { "input_tokens": 318, "output_tokens": 34 }
  }""")
}
