package grit.models

import grit.core.provider.Repeats

/** Where a request's prompt-cache breakpoints go, the same on either gateway that reaches
  * Anthropic's models.
  *
  * A breakpoint caches the prefix that ends at it, and a later request reads an entry only
  * from a breakpoint an earlier request placed (Anthropic's lookback looks for earlier writes,
  * not for content that happens to match), so each prefix a later request is to read needs its
  * own mark. The API allows four; every one here is five-minute.
  *
  * The four go where a continued task's prefixes stop changing: after the tools and system
  * prompt, which a task never changes; after what the task is, which every turn of it shows;
  * after what its earlier turns established, which a later boundary shows again; and after the
  * stable messages, which the calls within a turn share. So a turn drawn at a later boundary
  * reads the task and what earlier turns established from the cache, and pays to write only
  * what is new since.
  */
object CacheMarks {

  /** A cache breakpoint, `{"type":"ephemeral"}`: Anthropic's five-minute cache. A new object on
    * each call, so marking one block never marks another.
    */
  private[models] def ephemeral: ujson.Obj = ujson.Obj("type" -> "ephemeral")

  /** The block that takes a request's mark on its first `count` messages, when the caller
    * passes `messages` one per [[grit.core.message.Message]] of the request, in order (`None`
    * for one it does not send): the last block, in the `content` arrays of the first `count`
    * (a negative count as 0, one past the end as all), that is not thinking (`thinking`,
    * `redacted_thinking`), which no gateway caches; a message whose `content` is no array holds
    * none. `None` when there is no such block. The block is `messages`' own: setting its
    * `cache_control` marks the request.
    */
  private def markOn(messages: Vector[Option[ujson.Obj]], count: Int): Option[ujson.Obj] =
    messages
      .take(count.max(0))
      .flatten
      .flatMap(_.value.get("content").flatMap(_.arrOpt).fold(Vector.empty)(_.toVector))
      .collect { case block: ujson.Obj => block }
      .findLast(block => !block.value.get("type").flatMap(_.strOpt).exists(Thinking.contains))

  /** Marks a cached request's breakpoints, at most the four Anthropic allows: one after
    * `system`, or with no system prompt after the last of `tools`; one on the block ending
    * `repeats`' task messages, one on the block ending its established messages, and one on
    * the block ending its stable messages ([[markOn]], whose form `messages` takes). Two counts ending on the
    * same block mark it once. `tools` and `messages` are the request's own, marked in place;
    * the system prompt's one text block is returned, marked, `None` when `system` is empty.
    */
  private[models] def marked(
      system: String,
      tools: Vector[ujson.Obj],
      messages: Vector[Option[ujson.Obj]],
      repeats: Repeats
  ): Option[ujson.Obj] = {
    val block = Option.when(system.nonEmpty)(
      ujson.Obj("type" -> "text", "text" -> system, "cache_control" -> ephemeral)
    )
    if (block.isEmpty) tools.lastOption.foreach(_("cache_control") = ephemeral)
    val counts = Vector(repeats.task, repeats.established, repeats.stable)
    // Two counts ending on one block set the same key twice: one mark.
    counts.flatMap(markOn(messages, _)).foreach(_("cache_control") = ephemeral)
    block
  }

  /** The block types that are a model's thinking. */
  private[models] val Thinking: Set[String] = Set("thinking", "redacted_thinking")
}
