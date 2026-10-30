package grit.turn

import grit.core.durable.InMemoryDurable
import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.ReasoningReplay
import grit.core.provider.ModelRequest
import grit.core.store.{InMemoryEntryStore, InMemoryUsageLedger}
import grit.models.{AnthropicJson, OpenRouterJson, StubProvider}

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
  /** `body`'s messages, each written as sent but without `cache_control`, which is no edit. */
  private def unmarked(body: ujson.Value): Vector[String] = {
    def strip(v: ujson.Value): ujson.Value = v match {
      case o: ujson.Obj =>
        val copy = ujson.Obj()
        o.value.foreach((k, x) => if (k != "cache_control") copy(k) = strip(x))
        copy
      case a: ujson.Arr => ujson.Arr.from(a.value.map(strip))
      case other => other
    }
    body("messages").arr.toVector.map(m => ujson.write(strip(m)))
  }

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

    test(
      "a gated turn's every call repeats the one before it whole, its system prompt and tools unchanged, on either gateway"
    ) {
      // peek's results are believed at nothing, and post requires member: from the second call
      // each result is hidden, so a gate speaks from round 1, as a note.
      val entries = new InMemoryEntryStore
      val ws = new Files(Map("a.txt" -> "alpha"))
      val provider = new Scripted((r, n) =>
        if (n < 2) Right(peeking(s"t${n + 1}")) else new StubProvider().complete(r)
      )
      val turn = say(entries, "read a.txt twice, then post it")
      new InMemoryDurable().run(turn.workflowId)(
        tooledBody(
          entries,
          provider,
          new InMemoryUsageLedger,
          new StubProvider(),
          NoClassifier,
          ws,
          gatedTools(ws),
          3
        )
      )
      val requests = provider.requests
      requests.size ==> 3
      requests.map(_.system).distinct.size ==> 1
      requests.map(_.tools).distinct.size ==> 1
      val anthropic =
        requests.map(
          AnthropicJson.request("claude-haiku-5-5", 1024, None, ReasoningReplay.Details, false, _)
        )
      val openRouter =
        requests.map(OpenRouterJson.request("anthropic/claude-haiku-5.5", 1024, None, _))
      // A message of the call before missing from its place in the next edits what that call's
      // reply was made from: on Anthropic, a thinking block in it is then refused.
      for (bodies <- Vector(anthropic, openRouter)) {
        val sent = bodies.map(unmarked)
        sent.sliding(2).map(p => p.drop(1).flatten.startsWith(p.take(1).flatten)).toVector ==>
          Vector(true, true)
      }
      val prefixes = anthropic.map(AnthropicJson.prefix)
      prefixes.zip(prefixes.drop(1)).map(diverged.tupled) ==> Vector(None, None)
      // The second call's gate hid the first result: its note, the guidance, is the last message.
      requests.lift(1).flatMap(_.messages.lastOption) match {
        case Some(Message.User(note)) =>
          (note.startsWith("<grit_note>"), note.contains(TurnGate.Guidance)) ==> (true, true)
        case other => throw new java.lang.AssertionError(s"no note last: $other")
      }
    }
  }
}
