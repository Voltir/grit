package grit.eval.harness.score

import java.time.Instant

import grit.core.id.{ConversationId, EntrySeq, TurnSeq, WorkflowId}
import grit.core.message.Tokens
import grit.core.store.Focus
import grit.core.tool.ToolName
import grit.dbos.engine.Build
import grit.eval.harness.corpus.{
  Call,
  Called,
  Ended,
  Part,
  Parts,
  Round,
  Said,
  Settled,
  Support,
  TurnCase
}
import grit.eval.harness.stats.Proportion
import grit.turn.TurnOffer

import utest.*

/** Variants against shipped, paired on hand-built turns whose every number is worked in the
  * comments. Every id here is made up.
  */
object RecipesTests extends TestSuite {

  private val read = ToolName("read")
  private val search = ToolName("search")
  private val github = ToolName("github_search_code")
  private val few = Proportion.Interval.TooFewClusters

  private def part(kind: Part.Kind, of: String, seq: Long, tokens: Long, s: Option[Double]) =
    Part(kind, ConversationId(of), Vector(EntrySeq(seq)), Tokens(tokens), s.flatMap(Support.read))

  private def turn(w: String, thread: String, calls: ToolName*) =
    TurnCase(
      WorkflowId(w),
      ConversationId(thread),
      TurnSeq(0),
      Said.Slack(Fixtures.id(s"C1/$w")),
      TurnOffer.Root.Addressed,
      Focus.Focused,
      Instant.parse("2026-10-02T17:05:00Z"),
      Build.Unknown,
      None,
      None,
      None,
      Vector(Round(calls.toVector.map(t => Call(Called.Tool(t), Settled.Ok(1))))),
      Ended.Replied(10, false),
      Vector.empty,
      None
    )

  private def offer(schema: Long, tools: ToolName*) =
    Given(tools.toVector, Tokens(schema), None)

  val tests = Tests {
    test("called-tool recall: a variant withholding one of two called tools recalls half") {
      // w1 called read and github (read twice, counted once); the variant offers read alone.
      val p = TurnPair(
        turn("w1", "A", read, github, read),
        offer(100, read, search, github),
        offer(60, read, search)
      )
      Recipe.of(Vector(p)).called ==> Proportion(1, 2, 1, few)
    }

    test("schema tokens saved are shipped's less the variant's over every turn, below 0 if added") {
      // w1 saves 40, w2 nothing, w3 adds 10: 30 saved; per turn 100 against (60+100+110)/3 = 90.
      val pairs = Vector(
        TurnPair(turn("w1", "A"), offer(100, read, github), offer(60, read)),
        TurnPair(turn("w2", "B"), offer(100, read), offer(100, read)),
        TurnPair(turn("w3", "C"), offer(100, read), offer(110, read, github))
      )
      val r = Recipe.of(pairs)
      (r.saved, r.schema, r.tools) ==> (30L, Some(Shift(100, 90)), Some(Shift(4.0 / 3, 4.0 / 3)))
      r.changed ==> Vector(WorkflowId("w1"), WorkflowId("w3"))
    }

    test("a variant that changes nothing reports no case") {
      val window = Some(Parts(Vector(part(Part.Kind.Open, "x", 1, 30, None)), Tokens(2), Tokens(8)))
      val same = Given(Vector(read), Tokens(50), window)
      val r = Recipe.of(Vector(TurnPair(turn("w1", "A", read), same, same)))
      (r.changed, r.saved, r.called) ==> (Vector.empty, 0L, Proportion(1, 1, 1, few))
    }

    test("used-section recall: of the parts the reply used, the share the variant's window holds") {
      // The reply used x:1 (0.5) and y:2 (0.3), not z:3 (0.1). Shipped's rebuilt window holds
      // both; the variant's, narrower, x:1 alone and in a part of another kind.
      val recorded = Parts(
        Vector(
          part(Part.Kind.Open, "x", 1, 30, Some(0.5)),
          part(Part.Kind.Closed, "y", 2, 40, Some(0.3)),
          part(Part.Kind.Along, "z", 3, 20, Some(0.1))
        ),
        Tokens.Zero,
        Tokens(10)
      )
      val shipped = Parts(
        Vector(
          part(Part.Kind.Open, "x", 1, 30, None),
          part(Part.Kind.Closed, "y", 2, 40, None),
          part(Part.Kind.Along, "z", 3, 20, None)
        ),
        Tokens.Zero,
        Tokens(10)
      )
      val narrow = Parts(Vector(part(Part.Kind.Along, "x", 1, 30, None)), Tokens.Zero, Tokens(10))
      val t = turn("w1", "A").copy(window = Some(recorded))
      val r = Recipe.of(
        Vector(
          TurnPair(
            t,
            Given(Vector.empty, Tokens.Zero, Some(shipped)),
            Given(Vector.empty, Tokens.Zero, Some(narrow))
          )
        )
      )
      (r.usedShipped, r.used) ==> (Proportion(2, 2, 1, few), Proportion(1, 2, 1, few))
      // Per turn, by kind in Part.Kind's order: open 30/0, closed 40/0, along 20/30; whole
      // 100 against 40.
      r.parts.toVector ==> Vector(
        Part.Kind.Open -> Shift(30, 0),
        Part.Kind.Closed -> Shift(40, 0),
        Part.Kind.Along -> Shift(20, 30)
      )
      r.window ==> Some(Shift(100, 40))
    }

    test("used-section recall is over no part when no reply used one") {
      val r = Recipe.of(
        Vector(
          TurnPair(
            turn("w1", "A"),
            offer(0).copy(window = Some(Parts(Vector.empty, Tokens.Zero, Tokens.Zero))),
            offer(0)
          )
        )
      )
      (r.used, r.used.rate, r.windows) ==> (Proportion(0, 0, 0, few), None, 0)
    }
  }
}
