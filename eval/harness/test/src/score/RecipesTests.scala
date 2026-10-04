package grit.eval.harness.score

import java.time.Instant

import scala.collection.immutable.VectorMap

import grit.core.id.{ConversationId, EntrySeq, TurnSeq, WorkflowId}
import grit.core.message.{Tokens, Usage}
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
  Spent,
  Support,
  TurnCase
}
import grit.eval.harness.stats.Proportion
import grit.turn.{TurnOffer, TurnRecord}

import utest.*

/** Variants against shipped, paired on hand-built turns whose every number is worked in the
  * comments. Every id here is made up.
  */
object RecipesTests extends TestSuite {

  private val read = ToolName("read")
  private val search = ToolName("search")
  private val github = ToolName("github_search_code")
  private val few = Proportion.Interval.TooFewClusters
  private val none = Prices(VectorMap.empty, VectorMap.empty)

  private def spent(role: TurnRecord.Role, model: String, in: Long, cached: Long) =
    Spent(Some(role), model, Usage(Tokens(in), Tokens(1), Tokens(cached), None), Tokens(in))

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
      TurnRecord.Weigh.Unrecorded,
      None,
      Vector(Round(calls.toVector.map(t => Call(Called.Tool(t), Settled.Ok(1))))),
      Ended.Replied(10, false),
      Vector.empty,
      None
    )

  /** `x` to 12 significant digits. */
  private def r12(x: Double): Double =
    if (x == 0) 0.0 else BigDecimal(x).round(new java.math.MathContext(12)).toDouble

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
      Recipe.of(Vector(p), none).called ==> Proportion(1, 2, 1, few)
    }

    test("schema tokens saved are shipped's less the variant's over every turn, below 0 if added") {
      // w1 saves 40, w2 nothing, w3 adds 10: 30 saved; per turn 100 against (60+100+110)/3 = 90.
      val pairs = Vector(
        TurnPair(turn("w1", "A"), offer(100, read, github), offer(60, read)),
        TurnPair(turn("w2", "B"), offer(100, read), offer(100, read)),
        TurnPair(turn("w3", "C"), offer(100, read), offer(110, read, github))
      )
      val r = Recipe.of(pairs, none)
      (r.saved, r.schema, r.tools) ==> (30L, Some(Shift(100, 90)), Some(Shift(4.0 / 3, 4.0 / 3)))
      r.changed ==> Vector(WorkflowId("w1"), WorkflowId("w3"))
    }

    test("a variant that changes nothing reports no case") {
      val window = Some(Parts(Vector(part(Part.Kind.Open, "x", 1, 30, None)), Tokens(2), Tokens(8)))
      val same = Given(Vector(read), Tokens(50), window)
      val r = Recipe.of(Vector(TurnPair(turn("w1", "A", read), same, same)), none)
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
        ),
        none
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
        ),
        none
      )
      (r.used, r.used.rate, r.windows) ==> (Proportion(0, 0, 0, few), None, 0)
    }
    test("tokens saved are priced a call, each call's share cached at the cached rate") {
      // 300 definitions' tokens saved a call. w1 (thread A, model m/a: input $0.30/M, cached
      // $0.03/M): its query is no main call; round 0 has 1000 input, none cached; the reply
      // 1000, 500 cached. w2 (thread B) is by m/z, which has no rate: 100 input, none cached.
      val rate = Rate(3e-7, Some(3e-8), 2.5e-6, 3, 1, 0.0)
      val w1 = turn("w1", "A").copy(spend =
        Vector(
          spent(TurnRecord.Role.Query, "m/a", 50, 0),
          spent(TurnRecord.Role.Round(0), "m/a", 1000, 0),
          spent(TurnRecord.Role.Reply, "m/a", 1000, 500)
        )
      )
      val w2 = turn("w2", "B").copy(spend = Vector(spent(TurnRecord.Role.Reply, "m/z", 100, 0)))
      val r = Recipe.of(
        Vector(
          TurnPair(w1, offer(400, read, github), offer(100, read)),
          TurnPair(w2, offer(400, read, github), offer(100, read))
        ),
        Prices(VectorMap("m/a" -> Right(rate)), VectorMap.empty)
      )
      val p = r.priced
      // Effective: 300 at the input rate (round 0), then 150 at it and 150 at the cached rate
      // (the reply, half cached): 9e-5 + 4.5e-5 + 4.5e-6 = 1.395e-4. Uncached: 600 at the
      // input rate, 1.8e-4; all cached: 600 at the cached rate, 1.8e-5.
      // No model has a scale: every token is grit's estimate.
      (p.tokens, p.raw, p.rawCalls, p.calls, p.unpriced) ==> (900.0, 900L, 3, 3, 1)
      (r12(p.effective), r12(p.uncached), p.cached.map(r12)) ==> (1.395e-4, 1.8e-4, Some(1.8e-5))
      p.hit ==> Proportion(500, 2100, 2, few)
    }

    test("rates are read from the ledger's priced rows, cached apart where a row has some") {
      // m/a: four priced rows at input $0.30/M, cached $0.03/M, output $2.50/M, exactly; the
      // unpriced row is left out. m/b: input $1/M, output $2/M, no row cached. m/c: one priced row. m/d: two rows,
      // one twice the other.
      def row(model: String, in: Long, cached: Long, out: Long, usd: Option[String]) =
        Spent(
          None,
          model,
          Usage(Tokens(in), Tokens(out), Tokens(cached), usd.map(BigDecimal(_))),
          Tokens(in)
        )
      val rates = Rates.fit(
        Vector(
          row("m/a", 1000, 0, 10, Some("0.000325")),
          row("m/a", 2000, 0, 100, Some("0.00085")),
          row("m/b", 100, 0, 10, Some("0.00012")),
          row("m/a", 4000, 1000, 50, Some("0.001055")),
          row("m/a", 500, 0, 0, None),
          row("m/a", 3000, 2000, 0, Some("0.00036")),
          row("m/b", 300, 0, 5, Some("0.00031")),
          row("m/c", 100, 0, 10, Some("0.0001")),
          row("m/d", 100, 0, 10, Some("0.0001")),
          row("m/d", 200, 0, 20, Some("0.0002"))
        )
      )
      def read(r: Rate) =
        (
          r12(r.input),
          r.cached.map(r12),
          r12(r.output),
          r.rows,
          r.cachedRows,
          math.round(r.worst * 1e9)
        )
      rates.view.mapValues(_.map(read)).toVector ==> Vector(
        "m/a" -> Right((3e-7, Some(3e-8), 2.5e-6, 4, 2, 0L)),
        "m/b" -> Right((1e-6, None, 2e-6, 2, 0, 0L)),
        "m/c" -> Left("1 priced row, fewer than the 2 prices read from them"),
        "m/d" -> Left("its rows do not tell the prices apart")
      )
    }
    test("tokens saved are scaled to the provider's count before they are priced") {
      // 1000 tokens estimated saved on one uncached reply by m/a, which counts 0.8 a token
      // estimated: 800 tokens, at $0.30/M input 2.4e-4.
      val rate = Rate(3e-7, Some(3e-8), 2.5e-6, 3, 1, 0.0)
      val w1 = turn("w1", "A").copy(spend = Vector(spent(TurnRecord.Role.Reply, "m/a", 5000, 0)))
      val p = Recipe
        .of(
          Vector(TurnPair(w1, offer(1500, read, github), offer(500, read))),
          Prices(VectorMap("m/a" -> Right(rate)), VectorMap("m/a" -> Right(Scale(0.8, 40))))
        )
        .priced
      (p.tokens, p.raw, p.rawCalls) ==> (800.0, 0L, 0)
      (r12(p.effective), r12(p.uncached)) ==> (2.4e-4, 2.4e-4)
    }

    test("a model's scale is read from its estimated and counted calls, none from too few") {
      // m/a: five calls each counted 0.8 of the estimate, and one with no estimate, left out.
      // m/b: four calls, under the five a scale is read from.
      def row(model: String, estimated: Long, input: Long) =
        Spent(None, model, Usage(Tokens(input), Tokens(1), Tokens.Zero, None), Tokens(estimated))
      val scales = Rates.scales(
        Vector(100L, 200L, 500L, 1000L, 4000L).map(e => row("m/a", e, e * 4 / 5)) ++
          Vector(row("m/a", 0, 300)) ++ Vector.fill(4)(row("m/b", 100, 50))
      )
      scales.view.mapValues(_.map(s => (r12(s.ratio), s.calls))).toVector ==> Vector(
        "m/a" -> Right((0.8, 5)),
        "m/b" -> Left("4 calls estimated and counted, fewer than the 5 a scale is read from")
      )
    }
  }
}
