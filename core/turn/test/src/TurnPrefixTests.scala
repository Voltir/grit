package grit.turn

import grit.core.durable.InMemoryDurable
import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.ReasoningReplay
import grit.core.provider.{ModelRequest, ProviderError}
import grit.core.store.{InMemoryEntryStore, InMemoryUsageLedger}
import grit.core.visibility.Integrity
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

  /** A reply that thought, in Anthropic's own form, then said "posted". */
  private def posted: Message.Assistant = {
    val thought = "Posting now."
    Message.Assistant(
      Vector(
        AssistantBlock.Reasoning(
          thought,
          Some(ujson.Obj("type" -> "thinking", "thinking" -> thought, "signature" -> "sig-r"))
        ),
        AssistantBlock.Text("posted")
      ),
      StopReason.EndTurn,
      Usage(Tokens(10), Tokens(5), Tokens.Zero, None),
      "claude-haiku-5-5"
    )
  }

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

    test(
      "a turn that reads a hidden result repeats every call whole: the stub stays, and the answer is its text"
    ) {
      // peek's result is hidden from the second call as h1; the second reply reads it, the
      // third peeks again, which the fall that read caused never shows in an earlier place.
      val entries = new InMemoryEntryStore
      val ws = new Files(Map("a.txt" -> "alpha"))
      val reading =
        AssistantBlock.ToolCall(ToolCallId("r"), "read_hidden", ujson.Obj("name" -> "h1"))
      val provider = new Scripted((r, n) =>
        n match {
          case 0 => Right(peeking("t1"))
          case 1 =>
            Right(
              Message.Assistant(
                Vector(
                  AssistantBlock.Reasoning(
                    "Read h1.",
                    Some(
                      ujson.Obj("type" -> "thinking", "thinking" -> "Read h1.", "signature" -> "s")
                    )
                  ),
                  reading
                ),
                StopReason.ToolUse,
                Usage(Tokens(10), Tokens(5), Tokens.Zero, None),
                "claude-haiku-5-5"
              )
            )
          case 2 => Right(peeking("t2"))
          case _ => new StubProvider().complete(r)
        }
      )
      val turn = say(entries, "read a.txt, then post it")
      new InMemoryDurable().run(turn.workflowId)(
        tooledBody(
          entries,
          provider,
          new InMemoryUsageLedger,
          new StubProvider(),
          NoClassifier,
          ws,
          gatedTools(ws),
          4
        )
      )
      val requests = provider.requests
      requests.size ==> 4
      val sent = requests
        .map(
          AnthropicJson.request("claude-haiku-5-5", 1024, None, ReasoningReplay.Details, false, _)
        )
        .map(unmarked)
      sent.sliding(2).map(p => p.drop(1).flatten.startsWith(p.take(1).flatten)).toVector ==>
        Vector(true, true, true)
      def result(r: ModelRequest, id: String): Option[String] = r.messages.collectFirst {
        case Message.ToolResult(call, content, _) if ToolCallId.value(call) == id => content
      }
      // The answer to read_hidden carries what the stub hid, believed as the item.
      (requests.lift(2).flatMap(result(_, "r")), requests.lift(3).flatMap(result(_, "t1"))) ==> (
        Some("<tool_output source=\"read_hidden h1\">\nalpha\n</tool_output>"),
        Some(
          "<tool_output source=\"peek a.txt\">\nHidden here as h1: a result from a place people " +
            "outside the team can write in. If your answer needs what it says, read it: call " +
            "read_hidden with name h1. After that you can't use post until you've answered. To " +
            "ask it a short question instead, answered yes or no, with a whole number, a date, " +
            "one of options you list or a short code, call extract with name h1: that never " +
            "stops you doing anything.\n</tool_output>"
        )
      )
    }

    test(
      "a turn after another in its window begins with the last one's last call, byte for byte, on either gateway"
    ) {
      // Three turns of one window: the first reads a file after thinking, in Anthropic's
      // signed form, then answers; the next two answer at once. Each later turn's first call
      // must repeat the call before it whole (every message of an ungated turn's last call is
      // stable), its system prompt and tools unchanged, the thinking block among them. Each
      // is said hours after the one before, the last on another day: a carried turn's dated
      // line is the one its own request sent, and the system prompt dates nothing.
      val entries = new InMemoryEntryStore
      val ws = new Files(Map("a.txt" -> "alpha"))
      def turn(
          text: String,
          hours: Int,
          script: (ModelRequest, Int) -> Either[ProviderError, Message.Assistant]
      ) = {
        val provider = new Scripted(script)
        val t = say(entries, text, at = java.time.Instant.EPOCH.plusSeconds(hours * 3600L))
        new InMemoryDurable().run(t.workflowId)(
          tooledBody(
            entries,
            provider,
            new InMemoryUsageLedger,
            new StubProvider(),
            NoClassifier,
            ws,
            tools(ws),
            // Calls the turns never reach the second-to-last of: its note (TurnLoop.LastTools)
            // is kept in no window record, so a turn told it is followed by a boundary.
            5
          )
        )
        provider.requests
      }
      val answers: (ModelRequest, Int) -> Either[ProviderError, Message.Assistant] =
        (r, _) => new StubProvider().complete(r)
      val calls = Vector(
        turn(
          "read a.txt",
          0,
          (r, n) => if (n == 0) Right(peeking("t1")) else new StubProvider().complete(r)
        ),
        turn("and again?", 5, answers),
        turn("thanks", 30, answers)
      )
      calls.map(_.size) ==> Vector(2, 1, 1)
      val pairs = calls.sliding(2).toVector.flatMap {
        case Vector(before, after) => before.lastOption.zip(after.headOption)
        case _ => None
      }
      pairs.map((last, _) => last.stable == last.messages.size) ==> Vector(true, true)
      def anthropicOf(r: ModelRequest): ujson.Value =
        AnthropicJson.request("claude-haiku-5-5", 1024, None, ReasoningReplay.Details, false, r)
      def openRouterOf(r: ModelRequest): ujson.Value =
        OpenRouterJson.request("anthropic/claude-haiku-5.5", 1024, None, r)
      val anthropic = pairs.map((a, b) => (anthropicOf(a), anthropicOf(b)))
      val openRouter = pairs.map((a, b) => (openRouterOf(a), openRouterOf(b)))
      for (bodies <- Vector(anthropic, openRouter))
        bodies.map((a, b) =>
          (
            a.obj.get("system") == b.obj.get("system"),
            a.obj.get("tools") == b.obj.get("tools"),
            unmarked(b).startsWith(unmarked(a))
          )
        ) ==> Vector((true, true, true), (true, true, true))
      // The first turn's thinking is sent again, signed, with the turns after it.
      anthropic.map((_, b) => ujson.write(b).contains("sig-t1")) ==> Vector(true, true)
    }

    test(
      "a turn past the limit repeats the last one's tools and system prompt, and draws its messages afresh"
    ) {
      // The first turn's message alone spends more than the window's limit, so the next turn
      // is a boundary: its messages begin anew, with no thinking of the first replayed, while
      // what comes before them is the same bytes, said hours later as it is.
      val entries = new InMemoryEntryStore
      val ws = new Files(Map("a.txt" -> "alpha"))
      def turn(
          text: String,
          hours: Int,
          script: (ModelRequest, Int) -> Either[ProviderError, Message.Assistant]
      ) = {
        val provider = new Scripted(script)
        val t = say(entries, text, at = java.time.Instant.EPOCH.plusSeconds(hours * 3600L))
        new InMemoryDurable().run(t.workflowId)(
          tooledBody(
            entries,
            provider,
            new InMemoryUsageLedger,
            new StubProvider(),
            NoClassifier,
            ws,
            tools(ws),
            3
          )
        )
        (t, provider.requests)
      }
      val (_, first) = turn(
        "read a.txt " + "x" * 100_000,
        0,
        (r, n) => if (n == 0) Right(peeking("t1")) else new StubProvider().complete(r)
      )
      val (second, next) = turn("and again?", 5, (r, _) => new StubProvider().complete(r))
      def anthropicOf(r: ModelRequest): ujson.Value =
        AnthropicJson.request("claude-haiku-5-5", 1024, None, ReasoningReplay.Details, false, r)
      def openRouterOf(r: ModelRequest): ujson.Value =
        OpenRouterJson.request("anthropic/claude-haiku-5.5", 1024, None, r)
      val sent = first.lastOption.zip(next.headOption).toVector.flatMap { (a, b) =>
        Vector((anthropicOf(a), anthropicOf(b)), (openRouterOf(a), openRouterOf(b)))
      }
      (
        windows(entries).collectFirst {
          case (id, w) if id == Turn.windowId(second) => w.carry.map(_.after)
        },
        sent.map((a, b) =>
          (
            a.obj.get("system") == b.obj.get("system"),
            a.obj.get("tools") == b.obj.get("tools"),
            unmarked(b).startsWith(unmarked(a)),
            ujson.write(b).contains("sig-t1")
          )
        )
      ) ==> (Some(Some(None)), Vector((true, true, false, false), (true, true, false, false)))
    }

    test(
      "a turn after one whose last call was forced draws its window afresh, never sending that turn's final reply without the message it answered"
    ) {
      // The first turn thinks and reads twice, so its third and last call is made with tools
      // off, the last-call message at its end. A later turn that carried it would send its
      // final reply without that message, an edit before the reply's thinking that a provider
      // refuses: it must either repeat that call whole or replay none of its thinking.
      val entries = new InMemoryEntryStore
      val ws = new Files(Map("a.txt" -> "alpha"))
      def run(text: String, provider: Scripted) = {
        val t = say(entries, text)
        new InMemoryDurable().run(t.workflowId)(
          tooledBody(
            entries,
            provider,
            new InMemoryUsageLedger,
            new StubProvider(),
            NoClassifier,
            ws,
            tools(ws),
            3
          )
        )
        t
      }
      // The forced call's reply thinks, then answers.
      val thought = peeking("t3")
      val answered: Message.Assistant = Message.Assistant(
        thought.blocks.take(1) :+ AssistantBlock.Text("done"),
        StopReason.EndTurn,
        thought.usage,
        thought.model
      )
      val first = new Scripted((_, n) => Right(if (n < 2) peeking(s"t${n + 1}") else answered))
      run("read a.txt twice", first)
      val later = new Scripted((r, _) => new StubProvider().complete(r))
      val second = run("and now?", later)
      def anthropicOf(r: ModelRequest): ujson.Value =
        AnthropicJson.request("claude-haiku-5-5", 1024, None, ReasoningReplay.Details, false, r)
      val replays = later.requests.exists(r => ujson.write(anthropicOf(r)).contains("sig-t"))
      (
        first.requests.lastOption.map(_.use),
        replays,
        windows(entries).collectFirst {
          case (id, w) if id == Turn.windowId(second) => w.carry.map(_.after)
        }
      ) ==> (Some(grit.core.provider.ToolUse.Off), false, Some(Some(None)))
    }

    test(
      "a turn after a gated one draws its window afresh, with nothing of an earlier turn's thinking to replay"
    ) {
      // The gated turn's notes and stubs are in no record, so a later turn cannot send its
      // calls again as they were: it is a boundary, and a boundary replays no thinking block,
      // which a provider refuses under another prefix.
      val entries = new InMemoryEntryStore
      val ws = new Files(Map("a.txt" -> "alpha"))
      def run(text: String, provider: Scripted) = {
        val t = say(entries, text)
        new InMemoryDurable().run(t.workflowId)(
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
        t
      }
      run(
        "read a.txt twice, then post it",
        new Scripted((_, n) => if (n < 2) Right(peeking(s"t${n + 1}")) else Right(posted))
      )
      val later = new Scripted((r, _) => new StubProvider().complete(r))
      val second = run("and now?", later)
      val replayed = later.requests
        .flatMap(_.messages)
        .collect { case a: Message.Assistant =>
          a.blocks.collect { case b @ AssistantBlock.Reasoning(_, Some(_)) => b }
        }
        .flatten
      (
        windows(entries).collectFirst {
          case (id, w) if id == Turn.windowId(second) => w.carry.map(_.after)
        },
        replayed,
        later.requests.headOption.exists(_.messages.exists {
          case a: Message.Assistant => a.blocks.contains(AssistantBlock.Text("posted"))
          case _ => false
        })
      ) ==> (Some(Some(None)), Vector.empty, true)
    }

    test(
      "a gated turn, an ungated one and one whose first gate withholds a tool send the same tools and system prompt, byte for byte, on either gateway"
    ) {
      // The offer is the tool set's: what a turn's gates decide never changes its prefix.
      val entries = new InMemoryEntryStore
      val ws = new Files(Map("a.txt" -> "alpha"))
      def run(text: String, by: Integrity, provider: Scripted) = {
        val t = say(entries, text, by)
        new InMemoryDurable().run(t.workflowId)(
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
        provider.requests
      }
      val answers = new Scripted((r, _) => new StubProvider().complete(r))
      val requests = Vector(
        run(
          "read a.txt twice, then post it",
          Integrity.Member,
          new Scripted((_, n) => if (n < 2) Right(peeking(s"t${n + 1}")) else Right(posted))
        ),
        run("and now?", Integrity.Member, new Scripted((r, _) => new StubProvider().complete(r))),
        run("post it for me", Integrity.Outside, answers)
      ).flatten
      val bodies = Vector(
        requests.map(
          AnthropicJson.request("claude-haiku-5-5", 1024, None, ReasoningReplay.Details, false, _)
        ),
        requests.map(OpenRouterJson.request("anthropic/claude-haiku-5.5", 1024, None, _))
      )
      bodies.map(_.map(b => (b.obj.get("system"), b.obj.get("tools"))).distinct.size) ==>
        Vector(1, 1)
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
          4
        )
      )
      val requests = provider.requests
      requests.size ==> 3
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
