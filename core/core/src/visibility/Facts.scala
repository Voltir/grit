package grit.core.visibility

/** What an answer, or one part of it, says as data beside its text: JSON its service's
  * extension wrote and renders that text from. Never shown to the model, and opaque to core:
  * core reads no key of it. Two are equal when their JSON is.
  */
final case class Facts private (private val value: ujson.Value) derives CanEqual {

  /** As JSON: a copy, so a change to it changes no facts. */
  def json: ujson.Value = ujson.copy(value)
}

object Facts {

  /** `v`, as facts: a copy, so a later change to `v` changes none. */
  def of(v: ujson.Value): Facts = new Facts(ujson.copy(v))
}
