package grit.core.classify

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

/** One request as a classifier receives it: the state's JSON and the questions, in order.
  * Made only from an [[Ask]] ([[Request.of]]), so it always holds a question.
  */
final case class Request private[classify] (state: ujson.Value, questions: Vector[Question]) {

  /** SHA-256, lower-case hex, of the request as JSON (`{"state", "questions"}`, fields in the
    * order built): any change to the state, or to a question's kind, words, keys or order,
    * changes it. The model is not part of it.
    */
  def digest: String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(Request.json(this).render().getBytes(StandardCharsets.UTF_8))
      .map(b => f"${b & 0xff}%02x")
      .mkString
}

object Request {

  /** `questions` about `state`, as [[Classifier.ask]] sends them. */
  def of[S: StateJson, T](state: S, questions: Ask[S, T]): Request =
    new Request(StateJson[S].json(state), questions.questions)

  /** `r` as JSON, `{"state", "questions"}`, the state as the classifier receives it and each
    * question's kind, words and keys: what [[Request.digest]] hashes, rendered compactly.
    */
  def json(r: Request): ujson.Value =
    ujson.Obj("state" -> r.state, "questions" -> ujson.Arr.from(r.questions.map(question)))

  private def question(q: Question): ujson.Value = q match {
    case c: Question.Choice =>
      ujson.Obj(
        "kind" -> "choice",
        "instructions" -> c.instructions,
        "keys" -> ujson.Arr.from(c.keys.map { k =>
          ujson.Obj("name" -> k.name, "description" -> optional(k.description))
        })
      )
    case Question.YesNo(instructions, yes, no) =>
      ujson.Obj(
        "kind" -> "yes_no",
        "instructions" -> instructions,
        "yes" -> optional(yes),
        "no" -> optional(no)
      )
  }

  private def optional(s: Option[String]): ujson.Value =
    s.fold[ujson.Value](ujson.Null)(ujson.Str(_))
}
