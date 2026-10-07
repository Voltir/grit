package grit.core.triage

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.QuestionName

/** Answer sets triage recorded, by shape: question names, choice keys and probabilities, never
  * a message's text. Drawn read-only from a deployment's database: its shadow of v2's question
  * set (each answers every v2 question, two corpora), and its replica of v1's,
  * recorded in order and named here as v1 names them ([[Tags.V1]]).
  */
object RecordedAnswers {

  private def y(p: Double): Answer = Answer.YesNo(p)

  private def c(chosen: String, weights: (String, Double)*): Answer =
    Answer.Choice(chosen, weights.toVector.map(Answer.Weight(_, _)), 0.0)

  private def named(answers: (String, Answer)*): VectorMap[QuestionName, Answer] =
    VectorMap.from(answers.map((n, a) => QuestionName.read(n).fold(sys.error, identity) -> a))

  private def v1(kind: Answer, waiting: Answer, durable: Answer, helps: Answer) =
    named("kind" -> kind, "waiting" -> waiting, "durable" -> durable, "helps" -> helps)

  /** v2's answers, in the order recorded. */
  val V2: Vector[VectorMap[QuestionName, Answer]] = Vector(
    named(
      "gap" -> c("asks", "asks" -> 0.98, "owes" -> 0.02, "closes" -> 0.0, "nothing" -> 0.0),
      "open" -> y(0.92),
      "to" -> y(0.17),
      "durable" -> y(0.15),
      "anchor" -> y(0.59),
      "source:conversations" -> y(0.43),
      "source:github" -> y(0.37)
    ),
    named(
      "gap" -> c("asks", "asks" -> 1.0, "owes" -> 0.0, "closes" -> 0.0, "nothing" -> 0.0),
      "open" -> y(0.89),
      "to" -> y(0.73),
      "durable" -> y(0.15),
      "anchor" -> y(0.29),
      "source:conversations" -> y(0.62),
      "source:github" -> y(0.56)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.0, "owes" -> 0.0, "closes" -> 0.46, "nothing" -> 0.54),
      "open" -> y(0.71),
      "to" -> y(0.18),
      "durable" -> y(0.3),
      "anchor" -> y(0.45),
      "source:conversations" -> y(0.25),
      "source:github" -> y(0.19)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.01, "owes" -> 0.38, "closes" -> 0.0, "nothing" -> 0.61),
      "open" -> y(0.63),
      "to" -> y(0.14),
      "durable" -> y(0.86),
      "anchor" -> y(0.46),
      "source:conversations" -> y(0.64),
      "source:github" -> y(0.27)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.01, "owes" -> 0.0, "closes" -> 0.02, "nothing" -> 0.97),
      "open" -> y(0.59),
      "to" -> y(0.17),
      "durable" -> y(0.93),
      "anchor" -> y(0.22),
      "source:conversations" -> y(0.36),
      "source:github" -> y(0.09)
    ),
    named(
      "gap" -> c("asks", "asks" -> 1.0, "owes" -> 0.0, "closes" -> 0.0, "nothing" -> 0.0),
      "open" -> y(0.89),
      "to" -> y(0.72),
      "durable" -> y(0.17),
      "anchor" -> y(0.93),
      "source:conversations" -> y(0.64),
      "source:github" -> y(0.49)
    ),
    named(
      "gap" -> c("closes", "asks" -> 0.01, "owes" -> 0.0, "closes" -> 0.86, "nothing" -> 0.13),
      "open" -> y(0.3),
      "to" -> y(0.44),
      "durable" -> y(0.86),
      "anchor" -> y(0.08),
      "source:conversations" -> y(0.41),
      "source:github" -> y(0.39)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.0, "owes" -> 0.15, "closes" -> 0.0, "nothing" -> 0.85),
      "open" -> y(0.6),
      "to" -> y(0.1),
      "durable" -> y(0.04),
      "anchor" -> y(0.3),
      "source:conversations" -> y(0.15),
      "source:github" -> y(0.12)
    ),
    named(
      "gap" -> c("owes", "asks" -> 0.02, "owes" -> 0.97, "closes" -> 0.0, "nothing" -> 0.01),
      "open" -> y(0.83),
      "to" -> y(0.36),
      "durable" -> y(0.72),
      "anchor" -> y(0.34),
      "source:conversations" -> y(0.39),
      "source:github" -> y(0.33)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.0, "owes" -> 0.01, "closes" -> 0.01, "nothing" -> 0.98),
      "open" -> y(0.54),
      "to" -> y(0.07),
      "durable" -> y(0.97),
      "anchor" -> y(0.09),
      "source:conversations" -> y(0.24),
      "source:github" -> y(0.16)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.0, "owes" -> 0.0, "closes" -> 0.0, "nothing" -> 1.0),
      "open" -> y(0.42),
      "to" -> y(0.12),
      "durable" -> y(0.04),
      "anchor" -> y(0.2),
      "source:conversations" -> y(0.15),
      "source:github" -> y(0.06)
    ),
    named(
      "gap" -> c("asks", "asks" -> 1.0, "owes" -> 0.0, "closes" -> 0.0, "nothing" -> 0.0),
      "open" -> y(0.91),
      "to" -> y(0.69),
      "durable" -> y(0.36),
      "anchor" -> y(0.15),
      "source:conversations" -> y(0.4),
      "source:github" -> y(0.44)
    ),
    named(
      "gap" -> c("asks", "asks" -> 0.45, "owes" -> 0.01, "closes" -> 0.37, "nothing" -> 0.17),
      "open" -> y(0.73),
      "to" -> y(0.58),
      "durable" -> y(0.93),
      "anchor" -> y(0.65),
      "source:conversations" -> y(0.49),
      "source:github" -> y(0.12)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.0, "owes" -> 0.0, "closes" -> 0.0, "nothing" -> 1.0),
      "open" -> y(0.59),
      "to" -> y(0.16),
      "durable" -> y(0.04),
      "anchor" -> y(0.41),
      "source:conversations" -> y(0.26),
      "source:github" -> y(0.15)
    ),
    named(
      "gap" -> c("asks", "asks" -> 1.0, "owes" -> 0.0, "closes" -> 0.0, "nothing" -> 0.0),
      "open" -> y(0.92),
      "to" -> y(0.14),
      "durable" -> y(0.27),
      "anchor" -> y(0.94),
      "source:conversations" -> y(0.48),
      "source:github" -> y(0.47)
    ),
    named(
      "gap" -> c("asks", "asks" -> 0.76, "owes" -> 0.05, "closes" -> 0.13, "nothing" -> 0.06),
      "open" -> y(0.5),
      "to" -> y(0.21),
      "durable" -> y(0.95),
      "anchor" -> y(0.13),
      "source:conversations" -> y(0.43),
      "source:github" -> y(0.45)
    ),
    named(
      "gap" -> c("asks", "asks" -> 0.97, "owes" -> 0.02, "closes" -> 0.01, "nothing" -> 0.0),
      "open" -> y(0.76),
      "to" -> y(0.36),
      "durable" -> y(0.86),
      "anchor" -> y(0.73),
      "source:conversations" -> y(0.62),
      "source:github" -> y(0.11)
    ),
    named(
      "gap" -> c("asks", "asks" -> 0.48, "owes" -> 0.01, "closes" -> 0.3, "nothing" -> 0.21),
      "open" -> y(0.84),
      "to" -> y(0.55),
      "durable" -> y(0.9),
      "anchor" -> y(0.5),
      "source:conversations" -> y(0.53),
      "source:github" -> y(0.64)
    ),
    named(
      "gap" -> c("owes", "asks" -> 0.0, "owes" -> 0.95, "closes" -> 0.0, "nothing" -> 0.05),
      "open" -> y(0.69),
      "to" -> y(0.61),
      "durable" -> y(0.79),
      "anchor" -> y(0.56),
      "source:conversations" -> y(0.29),
      "source:github" -> y(0.69)
    ),
    named(
      "gap" -> c("asks", "asks" -> 1.0, "owes" -> 0.0, "closes" -> 0.0, "nothing" -> 0.0),
      "open" -> y(0.89),
      "to" -> y(0.77),
      "durable" -> y(0.59),
      "anchor" -> y(0.53),
      "source:conversations" -> y(0.44),
      "source:github" -> y(0.7)
    ),
    named(
      "gap" -> c("asks", "asks" -> 0.95, "owes" -> 0.0, "closes" -> 0.02, "nothing" -> 0.03),
      "open" -> y(0.87),
      "to" -> y(0.18),
      "durable" -> y(0.78),
      "anchor" -> y(0.22),
      "source:conversations" -> y(0.61),
      "source:github" -> y(0.07)
    ),
    named(
      "gap" -> c("asks", "asks" -> 0.99, "owes" -> 0.01, "closes" -> 0.0, "nothing" -> 0.0),
      "open" -> y(0.92),
      "to" -> y(0.82),
      "durable" -> y(0.5),
      "anchor" -> y(0.77),
      "source:conversations" -> y(0.46),
      "source:github" -> y(0.72)
    ),
    named(
      "gap" -> c("asks", "asks" -> 0.86, "owes" -> 0.0, "closes" -> 0.01, "nothing" -> 0.13),
      "open" -> y(0.84),
      "to" -> y(0.27),
      "durable" -> y(0.04),
      "anchor" -> y(0.66),
      "source:conversations" -> y(0.25),
      "source:github" -> y(0.16)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.37, "owes" -> 0.07, "closes" -> 0.11, "nothing" -> 0.45),
      "open" -> y(0.78),
      "to" -> y(0.32),
      "durable" -> y(0.7),
      "anchor" -> y(0.49),
      "source:conversations" -> y(0.41),
      "source:github" -> y(0.17)
    ),
    named(
      "gap" -> c("asks", "asks" -> 1.0, "owes" -> 0.0, "closes" -> 0.0, "nothing" -> 0.0),
      "open" -> y(0.8),
      "to" -> y(0.81),
      "durable" -> y(0.24),
      "anchor" -> y(0.72),
      "source:conversations" -> y(0.75),
      "source:github" -> y(0.58)
    ),
    named(
      "gap" -> c("asks", "asks" -> 1.0, "owes" -> 0.0, "closes" -> 0.0, "nothing" -> 0.0),
      "open" -> y(0.71),
      "to" -> y(0.26),
      "durable" -> y(0.21),
      "anchor" -> y(0.14),
      "source:conversations" -> y(0.73),
      "source:github" -> y(0.24)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.02, "owes" -> 0.01, "closes" -> 0.48, "nothing" -> 0.49),
      "open" -> y(0.59),
      "to" -> y(0.39),
      "durable" -> y(0.8),
      "anchor" -> y(0.14),
      "source:conversations" -> y(0.52),
      "source:github" -> y(0.74)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.0, "owes" -> 0.0, "closes" -> 0.03, "nothing" -> 0.97),
      "open" -> y(0.13),
      "to" -> y(0.24),
      "durable" -> y(0.2),
      "anchor" -> y(0.28),
      "source:conversations" -> y(0.21),
      "source:github" -> y(0.3)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.0, "owes" -> 0.0, "closes" -> 0.0, "nothing" -> 1.0),
      "open" -> y(0.3),
      "to" -> y(0.24),
      "durable" -> y(0.02),
      "anchor" -> y(0.09),
      "source:conversations" -> y(0.15),
      "source:github" -> y(0.06)
    ),
    named(
      "gap" -> c("asks", "asks" -> 0.66, "owes" -> 0.03, "closes" -> 0.02, "nothing" -> 0.29),
      "open" -> y(0.83),
      "to" -> y(0.66),
      "durable" -> y(0.91),
      "anchor" -> y(0.78),
      "source:conversations" -> y(0.41),
      "source:github" -> y(0.13)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.01, "owes" -> 0.0, "closes" -> 0.0, "nothing" -> 0.99),
      "open" -> y(0.56),
      "to" -> y(0.14),
      "durable" -> y(0.02),
      "anchor" -> y(0.49),
      "source:conversations" -> y(0.25),
      "source:github" -> y(0.2)
    ),
    named(
      "gap" -> c("asks", "asks" -> 1.0, "owes" -> 0.0, "closes" -> 0.0, "nothing" -> 0.0),
      "open" -> y(0.93),
      "to" -> y(0.51),
      "durable" -> y(0.5),
      "anchor" -> y(0.11),
      "source:conversations" -> y(0.33),
      "source:github" -> y(0.11)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.0, "owes" -> 0.01, "closes" -> 0.26, "nothing" -> 0.73),
      "open" -> y(0.2),
      "to" -> y(0.5),
      "durable" -> y(0.19),
      "anchor" -> y(0.19),
      "source:conversations" -> y(0.27),
      "source:github" -> y(0.29)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.0, "owes" -> 0.01, "closes" -> 0.46, "nothing" -> 0.53),
      "open" -> y(0.13),
      "to" -> y(0.37),
      "durable" -> y(0.12),
      "anchor" -> y(0.13),
      "source:conversations" -> y(0.27),
      "source:github" -> y(0.13)
    ),
    named(
      "gap" -> c("closes", "asks" -> 0.0, "owes" -> 0.02, "closes" -> 0.6, "nothing" -> 0.38),
      "open" -> y(0.31),
      "to" -> y(0.37),
      "durable" -> y(0.21),
      "anchor" -> y(0.45),
      "source:conversations" -> y(0.35),
      "source:github" -> y(0.09)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.05, "owes" -> 0.0, "closes" -> 0.04, "nothing" -> 0.91),
      "open" -> y(0.57),
      "to" -> y(0.21),
      "durable" -> y(0.52),
      "anchor" -> y(0.52),
      "source:conversations" -> y(0.43),
      "source:github" -> y(0.41)
    ),
    named(
      "gap" -> c("asks", "asks" -> 1.0, "owes" -> 0.0, "closes" -> 0.0, "nothing" -> 0.0),
      "open" -> y(0.93),
      "to" -> y(0.14),
      "durable" -> y(0.18),
      "anchor" -> y(0.47),
      "source:conversations" -> y(0.44),
      "source:github" -> y(0.04)
    ),
    named(
      "gap" -> c("asks", "asks" -> 1.0, "owes" -> 0.0, "closes" -> 0.0, "nothing" -> 0.0),
      "open" -> y(0.88),
      "to" -> y(0.21),
      "durable" -> y(0.41),
      "anchor" -> y(0.09),
      "source:conversations" -> y(0.71),
      "source:github" -> y(0.73)
    ),
    named(
      "gap" -> c("asks", "asks" -> 1.0, "owes" -> 0.0, "closes" -> 0.0, "nothing" -> 0.0),
      "open" -> y(0.88),
      "to" -> y(0.43),
      "durable" -> y(0.11),
      "anchor" -> y(0.64),
      "source:conversations" -> y(0.32),
      "source:github" -> y(0.32)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.0, "owes" -> 0.0, "closes" -> 0.06, "nothing" -> 0.94),
      "open" -> y(0.31),
      "to" -> y(0.26),
      "durable" -> y(0.05),
      "anchor" -> y(0.18),
      "source:conversations" -> y(0.22),
      "source:github" -> y(0.17)
    ),
    named(
      "gap" -> c("asks", "asks" -> 0.99, "owes" -> 0.01, "closes" -> 0.0, "nothing" -> 0.0),
      "open" -> y(0.87),
      "to" -> y(0.17),
      "durable" -> y(0.85),
      "anchor" -> y(0.87),
      "source:conversations" -> y(0.5),
      "source:github" -> y(0.27)
    ),
    named(
      "gap" -> c("closes", "asks" -> 0.01, "owes" -> 0.01, "closes" -> 0.58, "nothing" -> 0.4),
      "open" -> y(0.14),
      "to" -> y(0.73),
      "durable" -> y(0.88),
      "anchor" -> y(0.11),
      "source:conversations" -> y(0.51),
      "source:github" -> y(0.62)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.01, "owes" -> 0.0, "closes" -> 0.0, "nothing" -> 0.99),
      "open" -> y(0.7),
      "to" -> y(0.3),
      "durable" -> y(0.04),
      "anchor" -> y(0.35),
      "source:conversations" -> y(0.24),
      "source:github" -> y(0.05)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.13, "owes" -> 0.01, "closes" -> 0.25, "nothing" -> 0.61),
      "open" -> y(0.51),
      "to" -> y(0.3),
      "durable" -> y(0.68),
      "anchor" -> y(0.39),
      "source:conversations" -> y(0.64),
      "source:github" -> y(0.18)
    ),
    named(
      "gap" -> c("closes", "asks" -> 0.01, "owes" -> 0.0, "closes" -> 0.75, "nothing" -> 0.24),
      "open" -> y(0.32),
      "to" -> y(0.23),
      "durable" -> y(0.62),
      "anchor" -> y(0.43),
      "source:conversations" -> y(0.62),
      "source:github" -> y(0.37)
    ),
    named(
      "gap" -> c("closes", "asks" -> 0.06, "owes" -> 0.21, "closes" -> 0.4, "nothing" -> 0.33),
      "open" -> y(0.61),
      "to" -> y(0.2),
      "durable" -> y(0.81),
      "anchor" -> y(0.43),
      "source:conversations" -> y(0.48),
      "source:github" -> y(0.16)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.42, "owes" -> 0.0, "closes" -> 0.01, "nothing" -> 0.57),
      "open" -> y(0.8),
      "to" -> y(0.31),
      "durable" -> y(0.4),
      "anchor" -> y(0.73),
      "source:conversations" -> y(0.4),
      "source:github" -> y(0.31)
    ),
    named(
      "gap" -> c("closes", "asks" -> 0.0, "owes" -> 0.06, "closes" -> 0.55, "nothing" -> 0.39),
      "open" -> y(0.35),
      "to" -> y(0.31),
      "durable" -> y(0.5),
      "anchor" -> y(0.14),
      "source:conversations" -> y(0.22),
      "source:github" -> y(0.21)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.04, "owes" -> 0.26, "closes" -> 0.17, "nothing" -> 0.53),
      "open" -> y(0.7),
      "to" -> y(0.33),
      "durable" -> y(0.17),
      "anchor" -> y(0.24),
      "source:conversations" -> y(0.41),
      "source:github" -> y(0.15)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.03, "owes" -> 0.19, "closes" -> 0.0, "nothing" -> 0.78),
      "open" -> y(0.76),
      "to" -> y(0.15),
      "durable" -> y(0.07),
      "anchor" -> y(0.36),
      "source:conversations" -> y(0.35),
      "source:github" -> y(0.33)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.01, "owes" -> 0.0, "closes" -> 0.01, "nothing" -> 0.98),
      "open" -> y(0.41),
      "to" -> y(0.1),
      "durable" -> y(0.5),
      "anchor" -> y(0.16),
      "source:conversations" -> y(0.22),
      "source:github" -> y(0.35)
    ),
    named(
      "gap" -> c("asks", "asks" -> 0.74, "owes" -> 0.01, "closes" -> 0.01, "nothing" -> 0.24),
      "open" -> y(0.78),
      "to" -> y(0.7),
      "durable" -> y(0.9),
      "anchor" -> y(0.48),
      "source:conversations" -> y(0.46),
      "source:github" -> y(0.25)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.03, "owes" -> 0.02, "closes" -> 0.0, "nothing" -> 0.95),
      "open" -> y(0.69),
      "to" -> y(0.66),
      "durable" -> y(0.71),
      "anchor" -> y(0.2),
      "source:conversations" -> y(0.25),
      "source:github" -> y(0.29)
    ),
    named(
      "gap" -> c("owes", "asks" -> 0.0, "owes" -> 0.98, "closes" -> 0.0, "nothing" -> 0.02),
      "open" -> y(0.43),
      "to" -> y(0.32),
      "durable" -> y(0.23),
      "anchor" -> y(0.5),
      "source:conversations" -> y(0.41),
      "source:github" -> y(0.35)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.25, "owes" -> 0.01, "closes" -> 0.01, "nothing" -> 0.73),
      "open" -> y(0.69),
      "to" -> y(0.79),
      "durable" -> y(0.18),
      "anchor" -> y(0.64),
      "source:conversations" -> y(0.22),
      "source:github" -> y(0.07)
    ),
    named(
      "gap" -> c("asks", "asks" -> 0.62, "owes" -> 0.24, "closes" -> 0.04, "nothing" -> 0.1),
      "open" -> y(0.47),
      "to" -> y(0.29),
      "durable" -> y(0.62),
      "anchor" -> y(0.58),
      "source:conversations" -> y(0.65),
      "source:github" -> y(0.14)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.0, "owes" -> 0.0, "closes" -> 0.0, "nothing" -> 1.0),
      "open" -> y(0.17),
      "to" -> y(0.16),
      "durable" -> y(0.03),
      "anchor" -> y(0.14),
      "source:conversations" -> y(0.28),
      "source:github" -> y(0.17)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.0, "owes" -> 0.0, "closes" -> 0.01, "nothing" -> 0.99),
      "open" -> y(0.23),
      "to" -> y(0.89),
      "durable" -> y(0.39),
      "anchor" -> y(0.25),
      "source:conversations" -> y(0.27),
      "source:github" -> y(0.27)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.01, "owes" -> 0.0, "closes" -> 0.0, "nothing" -> 0.99),
      "open" -> y(0.52),
      "to" -> y(0.18),
      "durable" -> y(0.1),
      "anchor" -> y(0.48),
      "source:conversations" -> y(0.32),
      "source:github" -> y(0.24)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.01, "owes" -> 0.36, "closes" -> 0.02, "nothing" -> 0.61),
      "open" -> y(0.39),
      "to" -> y(0.17),
      "durable" -> y(0.22),
      "anchor" -> y(0.16),
      "source:conversations" -> y(0.2),
      "source:github" -> y(0.19)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.0, "owes" -> 0.0, "closes" -> 0.0, "nothing" -> 1.0),
      "open" -> y(0.38),
      "to" -> y(0.75),
      "durable" -> y(0.09),
      "anchor" -> y(0.39),
      "source:conversations" -> y(0.15),
      "source:github" -> y(0.06)
    ),
    named(
      "gap" -> c("asks", "asks" -> 0.77, "owes" -> 0.03, "closes" -> 0.12, "nothing" -> 0.08),
      "open" -> y(0.5),
      "to" -> y(0.18),
      "durable" -> y(0.49),
      "anchor" -> y(0.19),
      "source:conversations" -> y(0.61),
      "source:github" -> y(0.54)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.01, "owes" -> 0.0, "closes" -> 0.0, "nothing" -> 0.99),
      "open" -> y(0.41),
      "to" -> y(0.17),
      "durable" -> y(0.18),
      "anchor" -> y(0.62),
      "source:conversations" -> y(0.21),
      "source:github" -> y(0.15)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.03, "owes" -> 0.0, "closes" -> 0.0, "nothing" -> 0.97),
      "open" -> y(0.62),
      "to" -> y(0.17),
      "durable" -> y(0.11),
      "anchor" -> y(0.7),
      "source:conversations" -> y(0.28),
      "source:github" -> y(0.21)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.0, "owes" -> 0.0, "closes" -> 0.09, "nothing" -> 0.91),
      "open" -> y(0.53),
      "to" -> y(0.29),
      "durable" -> y(0.08),
      "anchor" -> y(0.3),
      "source:conversations" -> y(0.24),
      "source:github" -> y(0.08)
    ),
    named(
      "gap" -> c("closes", "asks" -> 0.01, "owes" -> 0.1, "closes" -> 0.62, "nothing" -> 0.27),
      "open" -> y(0.19),
      "to" -> y(0.29),
      "durable" -> y(0.55),
      "anchor" -> y(0.6),
      "source:conversations" -> y(0.66),
      "source:github" -> y(0.42)
    ),
    named(
      "gap" -> c("closes", "asks" -> 0.16, "owes" -> 0.01, "closes" -> 0.55, "nothing" -> 0.28),
      "open" -> y(0.32),
      "to" -> y(0.33),
      "durable" -> y(0.58),
      "anchor" -> y(0.57),
      "source:conversations" -> y(0.71),
      "source:github" -> y(0.48)
    ),
    named(
      "gap" -> c("asks", "asks" -> 1.0, "owes" -> 0.0, "closes" -> 0.0, "nothing" -> 0.0),
      "open" -> y(0.82),
      "to" -> y(0.54),
      "durable" -> y(0.82),
      "anchor" -> y(0.88),
      "source:conversations" -> y(0.58),
      "source:github" -> y(0.29)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.08, "owes" -> 0.03, "closes" -> 0.06, "nothing" -> 0.83),
      "open" -> y(0.52),
      "to" -> y(0.3),
      "durable" -> y(0.84),
      "anchor" -> y(0.55),
      "source:conversations" -> y(0.4),
      "source:github" -> y(0.26)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.0, "owes" -> 0.02, "closes" -> 0.09, "nothing" -> 0.89),
      "open" -> y(0.22),
      "to" -> y(0.39),
      "durable" -> y(0.13),
      "anchor" -> y(0.13),
      "source:conversations" -> y(0.19),
      "source:github" -> y(0.1)
    ),
    named(
      "gap" -> c("asks", "asks" -> 1.0, "owes" -> 0.0, "closes" -> 0.0, "nothing" -> 0.0),
      "open" -> y(0.84),
      "to" -> y(0.49),
      "durable" -> y(0.84),
      "anchor" -> y(0.9),
      "source:conversations" -> y(0.33),
      "source:github" -> y(0.15)
    ),
    named(
      "gap" -> c("closes", "asks" -> 0.01, "owes" -> 0.08, "closes" -> 0.88, "nothing" -> 0.03),
      "open" -> y(0.23),
      "to" -> y(0.53),
      "durable" -> y(0.93),
      "anchor" -> y(0.64),
      "source:conversations" -> y(0.49),
      "source:github" -> y(0.15)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.04, "owes" -> 0.12, "closes" -> 0.02, "nothing" -> 0.82),
      "open" -> y(0.29),
      "to" -> y(0.31),
      "durable" -> y(0.43),
      "anchor" -> y(0.33),
      "source:conversations" -> y(0.57),
      "source:github" -> y(0.2)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.0, "owes" -> 0.0, "closes" -> 0.26, "nothing" -> 0.74),
      "open" -> y(0.31),
      "to" -> y(0.12),
      "durable" -> y(0.78),
      "anchor" -> y(0.08),
      "source:conversations" -> y(0.29),
      "source:github" -> y(0.29)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.01, "owes" -> 0.01, "closes" -> 0.19, "nothing" -> 0.79),
      "open" -> y(0.17),
      "to" -> y(0.79),
      "durable" -> y(0.61),
      "anchor" -> y(0.15),
      "source:conversations" -> y(0.59),
      "source:github" -> y(0.69)
    ),
    named(
      "gap" -> c("nothing", "asks" -> 0.15, "owes" -> 0.02, "closes" -> 0.01, "nothing" -> 0.82),
      "open" -> y(0.75),
      "to" -> y(0.12),
      "durable" -> y(0.48),
      "anchor" -> y(0.21),
      "source:conversations" -> y(0.36),
      "source:github" -> y(0.69)
    ),
    named(
      "gap" -> c("asks", "asks" -> 0.72, "owes" -> 0.26, "closes" -> 0.01, "nothing" -> 0.01),
      "open" -> y(0.94),
      "to" -> y(0.31),
      "durable" -> y(0.92),
      "anchor" -> y(0.44),
      "source:conversations" -> y(0.52),
      "source:github" -> y(0.52)
    ),
    named(
      "gap" -> c("asks", "asks" -> 0.89, "owes" -> 0.06, "closes" -> 0.0, "nothing" -> 0.05),
      "open" -> y(0.94),
      "to" -> y(0.16),
      "durable" -> y(0.71),
      "anchor" -> y(0.6),
      "source:conversations" -> y(0.35),
      "source:github" -> y(0.42)
    ),
    named(
      "gap" -> c("owes", "asks" -> 0.02, "owes" -> 0.97, "closes" -> 0.0, "nothing" -> 0.01),
      "open" -> y(0.54),
      "to" -> y(0.43),
      "durable" -> y(0.44),
      "anchor" -> y(0.26),
      "source:conversations" -> y(0.59),
      "source:github" -> y(0.67)
    )
  )

  /** v1's answers, in the order recorded. */
  val V1: Vector[VectorMap[QuestionName, Answer]] = Vector(
    v1(
      c(
        "question",
        "question" -> 0.99,
        "answer" -> 0.0,
        "decision" -> 0.0,
        "announcement" -> 0.0,
        "chatter" -> 0.01
      ),
      y(0.59),
      y(0.18),
      y(0.58)
    ),
    v1(
      c(
        "question",
        "question" -> 1.0,
        "answer" -> 0.0,
        "decision" -> 0.0,
        "announcement" -> 0.0,
        "chatter" -> 0.0
      ),
      y(0.85),
      y(0.19),
      y(0.68)
    ),
    v1(
      c(
        "answer",
        "question" -> 0.0,
        "answer" -> 0.85,
        "decision" -> 0.12,
        "announcement" -> 0.01,
        "chatter" -> 0.02
      ),
      y(0.21),
      y(0.3),
      y(0.41)
    ),
    v1(
      c(
        "announcement",
        "question" -> 0.0,
        "answer" -> 0.01,
        "decision" -> 0.01,
        "announcement" -> 0.81,
        "chatter" -> 0.17
      ),
      y(0.16),
      y(0.84),
      y(0.48)
    ),
    v1(
      c(
        "announcement",
        "question" -> 0.0,
        "answer" -> 0.01,
        "decision" -> 0.0,
        "announcement" -> 0.99,
        "chatter" -> 0.0
      ),
      y(0.53),
      y(0.92),
      y(0.57)
    ),
    v1(
      c(
        "question",
        "question" -> 1.0,
        "answer" -> 0.0,
        "decision" -> 0.0,
        "announcement" -> 0.0,
        "chatter" -> 0.0
      ),
      y(0.84),
      y(0.18),
      y(0.63)
    ),
    v1(
      c(
        "answer",
        "question" -> 0.0,
        "answer" -> 1.0,
        "decision" -> 0.0,
        "announcement" -> 0.0,
        "chatter" -> 0.0
      ),
      y(0.54),
      y(0.87),
      y(0.52)
    ),
    v1(
      c(
        "chatter",
        "question" -> 0.0,
        "answer" -> 0.0,
        "decision" -> 0.0,
        "announcement" -> 0.03,
        "chatter" -> 0.97
      ),
      y(0.14),
      y(0.03),
      y(0.37)
    ),
    v1(
      c(
        "decision",
        "question" -> 0.0,
        "answer" -> 0.24,
        "decision" -> 0.68,
        "announcement" -> 0.07,
        "chatter" -> 0.01
      ),
      y(0.84),
      y(0.77),
      y(0.58)
    ),
    v1(
      c(
        "announcement",
        "question" -> 0.0,
        "answer" -> 0.0,
        "decision" -> 0.04,
        "announcement" -> 0.96,
        "chatter" -> 0.0
      ),
      y(0.27),
      y(0.96),
      y(0.41)
    ),
    v1(
      c(
        "chatter",
        "question" -> 0.0,
        "answer" -> 0.0,
        "decision" -> 0.0,
        "announcement" -> 0.0,
        "chatter" -> 1.0
      ),
      y(0.13),
      y(0.03),
      y(0.46)
    ),
    v1(
      c(
        "question",
        "question" -> 1.0,
        "answer" -> 0.0,
        "decision" -> 0.0,
        "announcement" -> 0.0,
        "chatter" -> 0.0
      ),
      y(0.87),
      y(0.28),
      y(0.54)
    ),
    v1(
      c(
        "answer",
        "question" -> 0.0,
        "answer" -> 0.97,
        "decision" -> 0.03,
        "announcement" -> 0.0,
        "chatter" -> 0.0
      ),
      y(0.64),
      y(0.94),
      y(0.69)
    ),
    v1(
      c(
        "chatter",
        "question" -> 0.0,
        "answer" -> 0.0,
        "decision" -> 0.0,
        "announcement" -> 0.0,
        "chatter" -> 1.0
      ),
      y(0.13),
      y(0.04),
      y(0.48)
    ),
    v1(
      c(
        "question",
        "question" -> 0.98,
        "answer" -> 0.0,
        "decision" -> 0.0,
        "announcement" -> 0.02,
        "chatter" -> 0.0
      ),
      y(0.82),
      y(0.24),
      y(0.53)
    ),
    v1(
      c(
        "answer",
        "question" -> 0.0,
        "answer" -> 0.88,
        "decision" -> 0.0,
        "announcement" -> 0.12,
        "chatter" -> 0.0
      ),
      y(0.54),
      y(0.95),
      y(0.69)
    ),
    v1(
      c(
        "question",
        "question" -> 0.77,
        "answer" -> 0.22,
        "decision" -> 0.01,
        "announcement" -> 0.0,
        "chatter" -> 0.0
      ),
      y(0.57),
      y(0.88),
      y(0.75)
    ),
    v1(
      c(
        "answer",
        "question" -> 0.0,
        "answer" -> 0.99,
        "decision" -> 0.01,
        "announcement" -> 0.0,
        "chatter" -> 0.0
      ),
      y(0.74),
      y(0.9),
      y(0.56)
    ),
    v1(
      c(
        "announcement",
        "question" -> 0.0,
        "answer" -> 0.0,
        "decision" -> 0.42,
        "announcement" -> 0.58,
        "chatter" -> 0.0
      ),
      y(0.46),
      y(0.77),
      y(0.53)
    ),
    v1(
      c(
        "question",
        "question" -> 1.0,
        "answer" -> 0.0,
        "decision" -> 0.0,
        "announcement" -> 0.0,
        "chatter" -> 0.0
      ),
      y(0.86),
      y(0.65),
      y(0.54)
    ),
    v1(
      c(
        "question",
        "question" -> 0.69,
        "answer" -> 0.03,
        "decision" -> 0.04,
        "announcement" -> 0.03,
        "chatter" -> 0.21
      ),
      y(0.56),
      y(0.73),
      y(0.63)
    ),
    v1(
      c(
        "question",
        "question" -> 1.0,
        "answer" -> 0.0,
        "decision" -> 0.0,
        "announcement" -> 0.0,
        "chatter" -> 0.0
      ),
      y(0.75),
      y(0.5),
      y(0.58)
    ),
    v1(
      c(
        "question",
        "question" -> 0.86,
        "answer" -> 0.0,
        "decision" -> 0.0,
        "announcement" -> 0.0,
        "chatter" -> 0.14
      ),
      y(0.24),
      y(0.04),
      y(0.34)
    ),
    v1(
      c(
        "decision",
        "question" -> 0.0,
        "answer" -> 0.28,
        "decision" -> 0.63,
        "announcement" -> 0.06,
        "chatter" -> 0.03
      ),
      y(0.54),
      y(0.65),
      y(0.64)
    ),
    v1(
      c(
        "question",
        "question" -> 1.0,
        "answer" -> 0.0,
        "decision" -> 0.0,
        "announcement" -> 0.0,
        "chatter" -> 0.0
      ),
      y(0.59),
      y(0.25),
      y(0.69)
    ),
    v1(
      c(
        "question",
        "question" -> 1.0,
        "answer" -> 0.0,
        "decision" -> 0.0,
        "announcement" -> 0.0,
        "chatter" -> 0.0
      ),
      y(0.56),
      y(0.32),
      y(0.83)
    ),
    v1(
      c(
        "announcement",
        "question" -> 0.0,
        "answer" -> 0.34,
        "decision" -> 0.06,
        "announcement" -> 0.59,
        "chatter" -> 0.01
      ),
      y(0.5),
      y(0.81),
      y(0.67)
    ),
    v1(
      c(
        "chatter",
        "question" -> 0.0,
        "answer" -> 0.01,
        "decision" -> 0.0,
        "announcement" -> 0.01,
        "chatter" -> 0.98
      ),
      y(0.19),
      y(0.22),
      y(0.43)
    ),
    v1(
      c(
        "chatter",
        "question" -> 0.0,
        "answer" -> 0.0,
        "decision" -> 0.0,
        "announcement" -> 0.0,
        "chatter" -> 1.0
      ),
      y(0.12),
      y(0.02),
      y(0.35)
    ),
    v1(
      c(
        "decision",
        "question" -> 0.01,
        "answer" -> 0.01,
        "decision" -> 0.71,
        "announcement" -> 0.27,
        "chatter" -> 0.0
      ),
      y(0.72),
      y(0.92),
      y(0.56)
    ),
    v1(
      c(
        "chatter",
        "question" -> 0.0,
        "answer" -> 0.02,
        "decision" -> 0.0,
        "announcement" -> 0.02,
        "chatter" -> 0.96
      ),
      y(0.08),
      y(0.02),
      y(0.33)
    ),
    v1(
      c(
        "question",
        "question" -> 0.82,
        "answer" -> 0.05,
        "decision" -> 0.02,
        "announcement" -> 0.07,
        "chatter" -> 0.04
      ),
      y(0.8),
      y(0.55),
      y(0.6)
    ),
    v1(
      c(
        "decision",
        "question" -> 0.0,
        "answer" -> 0.36,
        "decision" -> 0.37,
        "announcement" -> 0.0,
        "chatter" -> 0.27
      ),
      y(0.24),
      y(0.23),
      y(0.45)
    ),
    v1(
      c(
        "answer",
        "question" -> 0.0,
        "answer" -> 0.61,
        "decision" -> 0.04,
        "announcement" -> 0.0,
        "chatter" -> 0.35
      ),
      y(0.17),
      y(0.15),
      y(0.48)
    ),
    v1(
      c(
        "answer",
        "question" -> 0.0,
        "answer" -> 0.79,
        "decision" -> 0.06,
        "announcement" -> 0.0,
        "chatter" -> 0.15
      ),
      y(0.34),
      y(0.28),
      y(0.59)
    ),
    v1(
      c(
        "chatter",
        "question" -> 0.0,
        "answer" -> 0.46,
        "decision" -> 0.01,
        "announcement" -> 0.06,
        "chatter" -> 0.47
      ),
      y(0.32),
      y(0.49),
      y(0.63)
    ),
    v1(
      c(
        "question",
        "question" -> 1.0,
        "answer" -> 0.0,
        "decision" -> 0.0,
        "announcement" -> 0.0,
        "chatter" -> 0.0
      ),
      y(0.62),
      y(0.22),
      y(0.64)
    ),
    v1(
      c(
        "question",
        "question" -> 1.0,
        "answer" -> 0.0,
        "decision" -> 0.0,
        "announcement" -> 0.0,
        "chatter" -> 0.0
      ),
      y(0.77),
      y(0.47),
      y(0.74)
    ),
    v1(
      c(
        "question",
        "question" -> 0.99,
        "answer" -> 0.0,
        "decision" -> 0.0,
        "announcement" -> 0.0,
        "chatter" -> 0.01
      ),
      y(0.38),
      y(0.11),
      y(0.45)
    ),
    v1(
      c(
        "chatter",
        "question" -> 0.0,
        "answer" -> 0.11,
        "decision" -> 0.0,
        "announcement" -> 0.0,
        "chatter" -> 0.89
      ),
      y(0.12),
      y(0.05),
      y(0.33)
    ),
    v1(
      c(
        "question",
        "question" -> 0.52,
        "answer" -> 0.0,
        "decision" -> 0.32,
        "announcement" -> 0.16,
        "chatter" -> 0.0
      ),
      y(0.61),
      y(0.85),
      y(0.59)
    ),
    v1(
      c(
        "announcement",
        "question" -> 0.0,
        "answer" -> 0.09,
        "decision" -> 0.0,
        "announcement" -> 0.91,
        "chatter" -> 0.0
      ),
      y(0.3),
      y(0.88),
      y(0.47)
    ),
    v1(
      c(
        "chatter",
        "question" -> 0.0,
        "answer" -> 0.01,
        "decision" -> 0.0,
        "announcement" -> 0.0,
        "chatter" -> 0.99
      ),
      y(0.19),
      y(0.04),
      y(0.5)
    ),
    v1(
      c(
        "answer",
        "question" -> 0.0,
        "answer" -> 0.58,
        "decision" -> 0.18,
        "announcement" -> 0.12,
        "chatter" -> 0.12
      ),
      y(0.33),
      y(0.7),
      y(0.65)
    ),
    v1(
      c(
        "decision",
        "question" -> 0.0,
        "answer" -> 0.08,
        "decision" -> 0.9,
        "announcement" -> 0.02,
        "chatter" -> 0.0
      ),
      y(0.38),
      y(0.67),
      y(0.6)
    ),
    v1(
      c(
        "decision",
        "question" -> 0.0,
        "answer" -> 0.06,
        "decision" -> 0.86,
        "announcement" -> 0.08,
        "chatter" -> 0.0
      ),
      y(0.45),
      y(0.82),
      y(0.6)
    ),
    v1(
      c(
        "answer",
        "question" -> 0.11,
        "answer" -> 0.61,
        "decision" -> 0.0,
        "announcement" -> 0.04,
        "chatter" -> 0.24
      ),
      y(0.47),
      y(0.37),
      y(0.54)
    ),
    v1(
      c(
        "answer",
        "question" -> 0.01,
        "answer" -> 0.95,
        "decision" -> 0.0,
        "announcement" -> 0.02,
        "chatter" -> 0.02
      ),
      y(0.42),
      y(0.46),
      y(0.38)
    ),
    v1(
      c(
        "answer",
        "question" -> 0.0,
        "answer" -> 0.43,
        "decision" -> 0.1,
        "announcement" -> 0.39,
        "chatter" -> 0.08
      ),
      y(0.63),
      y(0.2),
      y(0.69)
    ),
    v1(
      c(
        "chatter",
        "question" -> 0.0,
        "answer" -> 0.2,
        "decision" -> 0.0,
        "announcement" -> 0.07,
        "chatter" -> 0.73
      ),
      y(0.16),
      y(0.07),
      y(0.36)
    ),
    v1(
      c(
        "announcement",
        "question" -> 0.01,
        "answer" -> 0.0,
        "decision" -> 0.0,
        "announcement" -> 0.99,
        "chatter" -> 0.0
      ),
      y(0.16),
      y(0.46),
      y(0.37)
    ),
    v1(
      c(
        "decision",
        "question" -> 0.0,
        "answer" -> 0.01,
        "decision" -> 0.66,
        "announcement" -> 0.29,
        "chatter" -> 0.04
      ),
      y(0.61),
      y(0.89),
      y(0.57)
    ),
    v1(
      c(
        "announcement",
        "question" -> 0.0,
        "answer" -> 0.02,
        "decision" -> 0.0,
        "announcement" -> 0.87,
        "chatter" -> 0.11
      ),
      y(0.36),
      y(0.74),
      y(0.47)
    ),
    v1(
      c(
        "decision",
        "question" -> 0.0,
        "answer" -> 0.15,
        "decision" -> 0.51,
        "announcement" -> 0.19,
        "chatter" -> 0.15
      ),
      y(0.38),
      y(0.23),
      y(0.48)
    ),
    v1(
      c(
        "chatter",
        "question" -> 0.0,
        "answer" -> 0.01,
        "decision" -> 0.0,
        "announcement" -> 0.0,
        "chatter" -> 0.99
      ),
      y(0.37),
      y(0.21),
      y(0.43)
    ),
    v1(
      c(
        "answer",
        "question" -> 0.0,
        "answer" -> 0.48,
        "decision" -> 0.37,
        "announcement" -> 0.03,
        "chatter" -> 0.12
      ),
      y(0.61),
      y(0.69),
      y(0.71)
    ),
    v1(
      c(
        "chatter",
        "question" -> 0.0,
        "answer" -> 0.0,
        "decision" -> 0.0,
        "announcement" -> 0.0,
        "chatter" -> 1.0
      ),
      y(0.09),
      y(0.03),
      y(0.38)
    ),
    v1(
      c(
        "chatter",
        "question" -> 0.0,
        "answer" -> 0.0,
        "decision" -> 0.0,
        "announcement" -> 0.0,
        "chatter" -> 1.0
      ),
      y(0.17),
      y(0.36),
      y(0.5)
    ),
    v1(
      c(
        "chatter",
        "question" -> 0.0,
        "answer" -> 0.0,
        "decision" -> 0.0,
        "announcement" -> 0.02,
        "chatter" -> 0.98
      ),
      y(0.24),
      y(0.11),
      y(0.58)
    ),
    v1(
      c(
        "chatter",
        "question" -> 0.0,
        "answer" -> 0.03,
        "decision" -> 0.0,
        "announcement" -> 0.02,
        "chatter" -> 0.95
      ),
      y(0.31),
      y(0.22),
      y(0.53)
    ),
    v1(
      c(
        "chatter",
        "question" -> 0.0,
        "answer" -> 0.0,
        "decision" -> 0.0,
        "announcement" -> 0.0,
        "chatter" -> 1.0
      ),
      y(0.19),
      y(0.09),
      y(0.42)
    ),
    v1(
      c(
        "decision",
        "question" -> 0.02,
        "answer" -> 0.13,
        "decision" -> 0.67,
        "announcement" -> 0.16,
        "chatter" -> 0.02
      ),
      y(0.34),
      y(0.49),
      y(0.6)
    ),
    v1(
      c(
        "chatter",
        "question" -> 0.0,
        "answer" -> 0.0,
        "decision" -> 0.0,
        "announcement" -> 0.23,
        "chatter" -> 0.77
      ),
      y(0.21),
      y(0.17),
      y(0.39)
    ),
    v1(
      c(
        "chatter",
        "question" -> 0.01,
        "answer" -> 0.02,
        "decision" -> 0.0,
        "announcement" -> 0.13,
        "chatter" -> 0.84
      ),
      y(0.27),
      y(0.11),
      y(0.38)
    ),
    v1(
      c(
        "chatter",
        "question" -> 0.0,
        "answer" -> 0.08,
        "decision" -> 0.0,
        "announcement" -> 0.0,
        "chatter" -> 0.92
      ),
      y(0.18),
      y(0.08),
      y(0.47)
    ),
    v1(
      c(
        "decision",
        "question" -> 0.0,
        "answer" -> 0.41,
        "decision" -> 0.56,
        "announcement" -> 0.0,
        "chatter" -> 0.03
      ),
      y(0.3),
      y(0.65),
      y(0.53)
    ),
    v1(
      c(
        "decision",
        "question" -> 0.01,
        "answer" -> 0.18,
        "decision" -> 0.76,
        "announcement" -> 0.01,
        "chatter" -> 0.04
      ),
      y(0.35),
      y(0.62),
      y(0.55)
    ),
    v1(
      c(
        "question",
        "question" -> 0.91,
        "answer" -> 0.0,
        "decision" -> 0.05,
        "announcement" -> 0.04,
        "chatter" -> 0.0
      ),
      y(0.83),
      y(0.82),
      y(0.68)
    ),
    v1(
      c(
        "chatter",
        "question" -> 0.0,
        "answer" -> 0.21,
        "decision" -> 0.16,
        "announcement" -> 0.31,
        "chatter" -> 0.32
      ),
      y(0.37),
      y(0.83),
      y(0.62)
    ),
    v1(
      c(
        "chatter",
        "question" -> 0.0,
        "answer" -> 0.04,
        "decision" -> 0.03,
        "announcement" -> 0.0,
        "chatter" -> 0.93
      ),
      y(0.17),
      y(0.09),
      y(0.29)
    ),
    v1(
      c(
        "question",
        "question" -> 0.87,
        "answer" -> 0.13,
        "decision" -> 0.0,
        "announcement" -> 0.0,
        "chatter" -> 0.0
      ),
      y(0.71),
      y(0.84),
      y(0.7)
    ),
    v1(
      c(
        "answer",
        "question" -> 0.0,
        "answer" -> 0.62,
        "decision" -> 0.38,
        "announcement" -> 0.0,
        "chatter" -> 0.0
      ),
      y(0.43),
      y(0.94),
      y(0.57)
    ),
    v1(
      c(
        "chatter",
        "question" -> 0.0,
        "answer" -> 0.0,
        "decision" -> 0.0,
        "announcement" -> 0.0,
        "chatter" -> 1.0
      ),
      y(0.22),
      y(0.54),
      y(0.54)
    ),
    v1(
      c(
        "announcement",
        "question" -> 0.0,
        "answer" -> 0.02,
        "decision" -> 0.0,
        "announcement" -> 0.97,
        "chatter" -> 0.01
      ),
      y(0.27),
      y(0.77),
      y(0.51)
    ),
    v1(
      c(
        "announcement",
        "question" -> 0.0,
        "answer" -> 0.04,
        "decision" -> 0.0,
        "announcement" -> 0.96,
        "chatter" -> 0.0
      ),
      y(0.36),
      y(0.64),
      y(0.5)
    ),
    v1(
      c(
        "announcement",
        "question" -> 0.07,
        "answer" -> 0.0,
        "decision" -> 0.0,
        "announcement" -> 0.93,
        "chatter" -> 0.0
      ),
      y(0.36),
      y(0.45),
      y(0.51)
    ),
    v1(
      c(
        "announcement",
        "question" -> 0.0,
        "answer" -> 0.03,
        "decision" -> 0.16,
        "announcement" -> 0.5599999999999999,
        "chatter" -> 0.25
      ),
      y(0.58),
      y(0.93),
      y(0.74)
    ),
    v1(
      c(
        "decision",
        "question" -> 0.0,
        "answer" -> 0.0,
        "decision" -> 0.94,
        "announcement" -> 0.05,
        "chatter" -> 0.01
      ),
      y(0.24),
      y(0.72),
      y(0.57)
    ),
    v1(
      c(
        "decision",
        "question" -> 0.0,
        "answer" -> 0.04,
        "decision" -> 0.43,
        "announcement" -> 0.36,
        "chatter" -> 0.17
      ),
      y(0.59),
      y(0.52),
      y(0.72)
    )
  )
}
