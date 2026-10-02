package grit.models

import grit.core.classify.{Answer, Answers, ClassifierError, Question}
import grit.core.message.{Tokens, Usage}

/** Jev's `POST /v1/systemone` wire format, both ways. Pure. Checked against the TypeSafe
  * docs (API reference, Choice, Noul, Confidence) as published on 2026-09-24.
  */
object JevJson {

  /** The request body asking `questions` about `state` on `model`. Jev keys questions by an
    * id the caller chooses and never shows the model; here it is the question's position.
    */
  def request(
      model: String,
      state: ujson.Value,
      questions: Vector[Question]
  ): ujson.Value =
    ujson.Obj(
      "model" -> model,
      "state" -> state,
      "questions" -> ujson.Obj.from(questions.zipWithIndex.map((q, i) => id(i) -> question(q)))
    )

  /** [[request]] as the text [[JevClassifier]] sends, byte for byte. */
  def body(model: String, state: ujson.Value, questions: Vector[Question]): String =
    ujson.write(request(model, state, questions))

  private def id(i: Int): String = s"q${i + 1}"

  private def question(q: Question): ujson.Value = q match {
    case c: Question.Choice =>
      ujson.Obj(
        "type" -> "choice",
        "instructions" -> c.instructions,
        "criteria" -> ujson.Obj.from(
          c.keys.map(k => k.name -> k.description.fold(ujson.Null)(ujson.Str(_)))
        )
      )
    case Question.YesNo(instructions, yes, no) =>
      val criteria =
        yes.map("true" -> ujson.Str(_)).toVector ++ no.map("false" -> ujson.Str(_)).toVector
      ujson.Obj.from(
        Vector[(String, ujson.Value)]("type" -> "noul", "instructions" -> instructions) ++
          Option.when(criteria.nonEmpty)("criteria" -> ujson.Obj.from(criteria))
      )
  }

  /** The answers in a 200 response to [[request]]'s `questions`, in their order, choice
    * probabilities in each question's own key order, priced at
    * [[JevConfig.UsdPerMillionInput]]; or why they are unusable.
    */
  def response(
      questions: Vector[Question],
      body: ujson.Value
  ): Either[ClassifierError, Answers] =
    for {
      root <- body.objOpt.toRight(unreadable("not an object"))
      answers <- root.get("answers").flatMap(_.objOpt).toRight(unreadable("no answers"))
      read <- questions.zipWithIndex.foldLeft[Either[ClassifierError, Vector[Answer]]](
        Right(Vector.empty)
      ) { case (acc, (q, i)) =>
        for {
          done <- acc
          raw <- answers.get(id(i)).flatMap(_.objOpt).toRight(unreadable(s"no answer to ${id(i)}"))
          a <- answer(id(i), q, raw)
        } yield done :+ a
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
      name: String,
      q: Question,
      raw: collection.Map[String, ujson.Value]
  ): Either[ClassifierError, Answer] =
    q match {
      case c: Question.Choice =>
        for {
          choice <- raw.get("choice").flatMap(_.strOpt).toRight(unreadable(s"$name: no choice"))
          probabilities <- raw
            .get("probabilities")
            .flatMap(_.objOpt)
            .toRight(unreadable(s"$name: no probabilities"))
          _ <- Either.cond(
            c.keys.exists(_.name == choice),
            (),
            unreadable(s"$name: chose $choice, not an option")
          )
          confidence <- raw
            .get("confidence")
            .flatMap(_.numOpt)
            .toRight(unreadable(s"$name: no confidence"))
        } yield Answer.Choice(
          choice,
          c.keys.map(k =>
            Answer.Weight(k.name, probabilities.get(k.name).flatMap(_.numOpt).getOrElse(0.0))
          ),
          confidence
        )
      case Question.YesNo(_, _, _) =>
        raw
          .get("noul")
          .flatMap(_.numOpt)
          .map(Answer.YesNo(_))
          .toRight(unreadable(s"$name: no noul"))
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
