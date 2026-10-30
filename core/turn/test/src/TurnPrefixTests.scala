package grit.turn

import grit.core.durable.InMemoryDurable
import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.ReasoningReplay
import grit.core.provider.ModelRequest
import grit.core.store.{InMemoryEntryStore, InMemoryUsageLedger}
import grit.models.{AnthropicJson, StubProvider}

import utest.*
import TurnFixtures.*

/** A turn's calls as a provider's prompt cache sees them: what each call's request repeats of
  * the one before, written as Anthropic's provider writes it ([[AnthropicJson.prefix]]).
  */
object TurnPrefixTests extends TestSuite {

  /** A reply that thought, in Anthropic's own form, and then called `peek` as `id`. */
  private def peeking(id: String, also: AssistantBlock*): Message.Assistant = {
    val thought = s"Read a.txt as $id."
    Message.Assistant(
      AssistantBlock.Reasoning(
        thought,
        Some(ujson.Obj("type" -> "thinking", "thinking" -> thought, "signature" -> s"sig-$id"))
      ) +: also.toVector :+ AssistantBlock.ToolCall(
        ToolCallId(id),
        "peek",
        ujson.Obj("path" -> "a.txt")
      ),
      StopReason.ToolUse,
      Usage(Tokens(10), Tokens(5), Tokens.Zero, None),
      "claude-haiku-5-5"
    )
  }

  /** The first line of `earlier` that `later` does not repeat in its place, with that place's
    * line in `later`; `None` when `later` begins with all of `earlier`.
    */
  private def diverged(earlier: String, later: String): Option[(Int, String, String)] = {
    val (a, b) = (earlier.linesIterator.toVector, later.linesIterator.toVector)
    a.indices.find(i => b.lift(i) != Some(a(i))).map(i => (i, a(i), b.lift(i).getOrElse("")))
  }

  val tests = Tests {

    test(
      "a turn's every call begins with the one before it through its last breakpoint, byte for byte"
    ) {
      // A turn whose item is unsure, so every call is tagged and offered topic, of three calls,
      // its budget: topic and a read, a second read, then a reply, the last call noted as last.
      val entries = new InMemoryEntryStore
      val classifier = new CountingClassifier
      Vector("hello", "knots? ~0.1").foreach { text =>
        runTurn(
          new InMemoryDurable,
          entries,
          new RecordingProvider,
          say(entries, text),
          classifier = classifier
        )
      }
      val ws = new Files(Map("a.txt" -> "alpha"))
      val provider = new Scripted((r, n) =>
        if (n == 0)
          Right(
            peeking(
              "t1",
              AssistantBlock.ToolCall(ToolCallId("v"), "topic", ujson.Obj("about" -> "current"))
            )
          )
        else if (n == 1) Right(peeking("t2"))
        else new StubProvider().complete(r)
      )
      val turn = say(entries, "hm ~0.5")
      new InMemoryDurable().run(turn.workflowId)(
        tooledBody(
          entries,
          provider,
          new InMemoryUsageLedger,
          new StubProvider(),
          classifier,
          ws,
          tools(ws),
          3
        )
      )
      def prefix(r: ModelRequest): String = AnthropicJson.prefix(
        AnthropicJson.request("claude-haiku-5-5", 1024, None, ReasoningReplay.Details, false, r)
      )
      val prefixes = provider.requests.map(prefix)
      // A line of the earlier call missing from the later one's place is a cache miss from
      // there on. Its usual causes: tools offered in another order, an object's keys written in
      // another order (a stored reply's thinking or arguments read back), and text dated or
      // counted per call before the messages (the system prompt, or a tag moved).
      prefixes.zip(prefixes.drop(1)).map(diverged.tupled) ==> Vector(None, None)
      // Each call's breakpoint is past the one before, so each caches more than the last.
      prefixes.map(_.linesIterator.size).sliding(2).forall(p => p(0) < p(1)) ==> true
    }
  }
}
