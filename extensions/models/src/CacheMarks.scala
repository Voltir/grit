package grit.models

/** Where a request's prompt-cache breakpoints go, the same on either gateway that reaches
  * Anthropic's models.
  */
private[models] object CacheMarks {

  /** A cache breakpoint, `{"type":"ephemeral"}`: Anthropic's five-minute cache. A new object on
    * each call, so marking one block never marks another.
    */
  def ephemeral: ujson.Obj = ujson.Obj("type" -> "ephemeral")

  /** The block that takes a request's mark on message `stable` ([[grit.core.provider.ModelRequest.stable]]),
    * when the caller passes `messages` one per [[grit.core.message.Message]] of the request, in
    * order (`None` for one it does not send): the last block, in the `content` arrays of the
    * first `stable` (a negative count as 0, one past the end as all), that is not thinking
    * (`thinking`, `redacted_thinking`), which no gateway caches; a message whose `content` is
    * no array holds none. `None` when there is no such block. The block is `messages`' own:
    * setting its `cache_control` marks the request.
    */
  def stableMark(messages: Vector[Option[ujson.Obj]], stable: Int): Option[ujson.Obj] =
    messages
      .take(stable.max(0))
      .flatten
      .flatMap(_.value.get("content").flatMap(_.arrOpt).fold(Vector.empty)(_.toVector))
      .collect { case block: ujson.Obj => block }
      .findLast(block => !block.value.get("type").flatMap(_.strOpt).exists(Thinking.contains))

  /** The block types that are a model's thinking. */
  val Thinking: Set[String] = Set("thinking", "redacted_thinking")
}
