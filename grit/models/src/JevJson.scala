package grit.models

import grit.core.classify.{Answer, Answers, ClassifierError, Question, QuestionId}
import grit.core.message.{Tokens, Usage}

/** Jev's `POST /v1/systemone` wire format, both ways. Pure. Checked against the TypeSafe
  * docs (API reference, Choice, Noul, Confidence) vendored in `.local/reference/jev` on
  * 2026-09-24.
  */
object JevJson {

  /** The request body asking `questions` about `state` on `model`. */
  def request(
      model: String,
      state: ujson.Value,
      questions: Vector[(QuestionId, Question)]
  ): ujson.Value =
    ujson.Obj(
      "model" -> model,
      "state" -> state,
      "questions" -> ujson.Obj.from(questions.map((id, q) => QuestionId.value(id) -> question(q)))
    )

  private def question(q: Question): ujson.Value = q match {
    case Question.Choice(instructions, criteria) =>
      ujson.Obj(
        "type" -> "choice",
        "instructions" -> instructions,
        "criteria" -> ujson.Obj.from(criteria.map((k, d) => k -> d.fold(ujson.Null)(ujson.Str(_))))
      )
    case Question.Noul(instructions, yes, no) =>
      val criteria =
        yes.map("true" -> ujson.Str(_)).toVector ++ no.map("false" -> ujson.Str(_)).toVector
      ujson.Obj.from(
        Vector[(String, ujson.Value)]("type" -> "noul", "instructions" -> instructions) ++
          Option.when(criteria.nonEmpty)("criteria" -> ujson.Obj.from(criteria))
      )
  }

  /** The answers in a 200 response to `questions`, choice probabilities in each question's
    * own option order, priced at [[JevConfig.UsdPerMillionInput]]; or why they are unusable.
    */
  def response(
      questions: Vector[(QuestionId, Question)],
      body: ujson.Value
  ): Either[ClassifierError, Answers] =
    for {
      root <- body.objOpt.toRight(unreadable("not an object"))
      answers <- root.get("answers").flatMap(_.objOpt).toRight(unreadable("no answers"))
      read <- questions.foldLeft[Either[ClassifierError, Map[QuestionId, Answer]]](Right(Map())) {
        case (acc, (id, q)) =>
          for {
            done <- acc
            raw <- answers
              .get(QuestionId.value(id))
              .flatMap(_.objOpt)
              .toRight(unreadable(s"no answer to ${QuestionId.value(id)}"))
            a <- answer(id, q, raw)
          } yield done.updated(id, a)
      }
    } yield {
      val usage = root.get("usage").flatMap(_.objOpt)
      def count(key: String): Long = usage.flatMap(_.get(key)).flatMap(_.numOpt).fold(0L)(_.toLong)
      val input = count("input_tokens")
      Answers(
        read,
        Usage(
          Tokens(input),
          Tokens(count("output_tokens")),
          Tokens.Zero,
          Some(BigDecimal(input) * JevConfig.UsdPerMillionInput / BigDecimal(1_000_000))
        ),
        root.get("model").flatMap(_.strOpt).getOrElse("unknown")
      )
    }

  private def answer(
      id: QuestionId,
      q: Question,
      raw: collection.Map[String, ujson.Value]
  ): Either[ClassifierError, Answer] = {
    val name = QuestionId.value(id)
    q match {
      case Question.Choice(_, criteria) =>
        for {
          choice <- raw.get("choice").flatMap(_.strOpt).toRight(unreadable(s"$name: no choice"))
          probabilities <- raw
            .get("probabilities")
            .flatMap(_.objOpt)
            .toRight(unreadable(s"$name: no probabilities"))
          _ <- Either.cond(
            criteria.exists(_._1 == choice),
            (),
            unreadable(s"$name: chose $choice, not an option")
          )
          confidence <- raw
            .get("confidence")
            .flatMap(_.numOpt)
            .toRight(unreadable(s"$name: no confidence"))
        } yield Answer.Choice(
          choice,
          criteria.map((k, _) => k -> probabilities.get(k).flatMap(_.numOpt).getOrElse(0.0)),
          confidence
        )
      case Question.Noul(_, _, _) =>
        raw.get("noul").flatMap(_.numOpt).map(Answer.Noul(_)).toRight(unreadable(s"$name: no noul"))
    }
  }

  /** The error in a non-200 response with status `status`. */
  def error(status: Int, body: String): ClassifierError = {
    val detail = scala.util
      .Try(ujson.read(body))
      .toOption
      .flatMap(_.objOpt)
      .flatMap(o => o.get("detail").orElse(o.get("error")).orElse(o.get("message")))
      .map(v => v.strOpt.getOrElse(ujson.write(v)).take(300))
      .getOrElse(body.take(300))
    ClassifierError.Unavailable(s"HTTP $status: $detail")
  }

  private def unreadable(why: String): ClassifierError =
    ClassifierError.Unreadable(s"unreadable response: $why")
}
