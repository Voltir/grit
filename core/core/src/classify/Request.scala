package grit.core.classify

import grit.core.id.Sha256

/** One request as a classifier receives it: the state's JSON and the questions, in order.
  * Made only from an [[Ask]] ([[Request.of]]), so it always holds a question.
  */
final case class Request private[classify] (state: ujson.Value, questions: Vector[Question]) {

  /** SHA-256, lower-case hex, of the request as JSON (`{"state", "questions"}`, fields in the
    * order built): any change to the state, or to a question's kind, words, keys, levels or
    * order, changes it. The model is not part of it.
    */
  def digest: String = Sha256.text(Request.json(this).render()).hex
}

object Request {

  /** `questions` about `state`, as [[Classifier.ask]] sends them. */
  def of[S: StateJson, T](state: S, questions: Ask[S, T]): Request =
    new Request(StateJson[S].json(state), questions.questions)

  /** `r` as JSON, `{"state", "questions"}`, the state as the classifier receives it and each
    * question's kind, words, and keys or levels: what [[Request.digest]] hashes, rendered
    * compactly.
    */
  def json(r: Request): ujson.Value =
    ujson.Obj("state" -> r.state, "questions" -> ujson.Arr.from(r.questions.map(question)))

  /** 32,000: the most tokens a classifier reads of one request. Jev refuses a request over it
    * (HTTP 400 `max_tokens_exceeded`) and never cuts one: measured, about 30,000 was answered
    * and about 40,000 refused.
    */
  val MostTokens: Int = 32000

  /** 95,200: the most characters of a request as [[json]] renders it ([[size]]) that stays
    * within [[MostTokens]], at 3.4 characters a token, as measured of English prose, less a
    * margin of 4,000 tokens for what that estimate and the classifier's own framing miss.
    */
  val MostChars: Int = (MostTokens - 4000) * 17 / 5

  /** How many characters `r` holds as [[json]] renders it, compactly. */
  def size(r: Request): Int = json(r).render().length

  private def question(q: Question): ujson.Value = q match {
    case c: Question.Choice =>
      ujson.Obj(
        "kind" -> "choice",
        "instructions" -> c.instructions,
        "keys" -> ujson.Arr.from(c.keys.map { k =>
          ujson.Obj("name" -> k.name, "description" -> optional(k.description))
        })
      )
    case s: Question.Score =>
      ujson.Obj(
        "kind" -> "score",
        "instructions" -> s.instructions,
        "levels" -> ujson.Arr.from(s.levels.map(ujson.Str(_)))
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
