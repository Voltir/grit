package grit.eval.harness.score

import java.time.Instant

import scala.collection.immutable.VectorMap

import grit.core.context.Width
import grit.core.id.{
  ConversationId,
  EntryId,
  EntrySeq,
  PrincipalId,
  ShadowName,
  TurnSeq,
  WorkflowId
}
import grit.core.message.{Tokens, Usage}
import grit.core.period.Probability
import grit.core.prompt.Layer
import grit.core.review.{Reason, Verdict}
import grit.core.store.Focus
import grit.core.tool.{ToolName, ToolSetId}
import grit.core.triage.{Tags, Weighing}
import grit.dbos.engine.Build
import grit.eval.harness.corpus.{
  Call,
  Called,
  Drafted,
  Ended,
  Offered,
  Part,
  Parts,
  Round,
  Said,
  Settled,
  Spent,
  Support,
  TurnCase
}
import grit.eval.harness.label.{Rated, Verdicts}
import grit.turn.{TurnOffer, TurnRecord, TurnShape, TurnWeighing}

/** Five recorded turns over three threads, every aggregate of which is worked by hand in
  * [[StructureTests]]; and the verdicts standing, one on a message no turn answers. Every id
  * here is made up.
  */
object TurnFixtures {

  private val at = Instant.parse("2026-10-02T17:05:00Z")
  private def slack(s: String) = Said.Slack(Fixtures.id(s))
  private def support(x: Double) = Support.read(x)
  private def part(kind: Part.Kind, tokens: Long, s: Option[Double] = None) =
    Part(kind, ConversationId("x"), Vector(EntrySeq(1)), Tokens(tokens), s.flatMap(support))
  private def spent(
      role: Option[TurnRecord.Role],
      usd: Option[String],
      in: Long,
      est: Long,
      cached: Long = 0
  ) =
    Spent(
      role,
      "m/x",
      Usage(Tokens(in), Tokens(1), Tokens(cached), usd.map(BigDecimal(_))),
      Tokens(est)
    )
  private def p(x: Double) = Some(Probability.clamped(x))

  /** A tool set's id, as the offers here were recorded under. */
  private val Set1: ToolSetId =
    ToolSetId.of("0123456789abcdef").fold(e => throw new java.lang.AssertionError(e), identity)

  val read = ToolName("read")
  val search = ToolName("search")
  private val both = Offered(
    Vector(read, search),
    Set1,
    Tokens(100),
    VectorMap(Layer.Base -> Tokens(500), Layer.Edge -> Tokens(50)),
    None,
    Vector.empty,
    None
  )

  private def turn(
      w: String,
      conversation: String,
      root: TurnOffer.Root,
      said: Said,
      ended: Ended
  ) =
    TurnCase(
      WorkflowId(w),
      ConversationId(conversation),
      TurnSeq(0),
      said,
      root,
      Focus.Open,
      at,
      Build.Unknown,
      None,
      None,
      TurnRecord.Weigh.Unrecorded,
      None,
      Vector.empty,
      ended,
      Vector.empty,
      None
    )

  // t1: thread A, heard, its draft passed and then posted; two rounds (topic and read, then
  // read again, failed).
  private val t1 = turn("w1", "A", TurnOffer.Root.Heard, slack("C1/1.1"), Ended.Replied(4, true))
    .copy(
      offered = Some(both),
      window = Some(
        Parts(Vector(part(Part.Kind.Open, 30), part(Part.Kind.Record, 10)), Tokens(2), Tokens(8))
      ),
      rounds = Vector(
        Round(Vector(Call(Called.Topic, Settled.Ok(5)), Call(Called.Tool(read), Settled.Ok(9)))),
        Round(Vector(Call(Called.Tool(read), Settled.Failed(3))))
      ),
      spend = Vector(
        spent(Some(TurnRecord.Role.Query), Some("0.001"), 12, 10),
        spent(Some(TurnRecord.Role.Round(0)), Some("0.002"), 80, 100),
        spent(Some(TurnRecord.Role.Reply), Some("0.003"), 100, 120, cached = 60)
      ),
      speech = Some(Drafted(Drafted.Kind.Posted, p(0.8), p(0.6), None))
    )

  // t2: thread A, heard, a draft that said something, held below the bar; one unnamed call.
  private val t2 = turn("w2", "A", TurnOffer.Root.Heard, slack("C1/1.2"), Ended.Replied(40, false))
    .copy(
      offered = Some(both.copy(shape = Some(TurnShape(Width.Deployed, Set1, Vector.empty)))),
      weighed = TurnRecord.Weigh.Recorded(
        Some(TurnWeighing.Weighed.Kept(Tags.Weighed(VectorMap.empty, "jev", Usage.Zero)))
      ),
      window = Some(
        Parts(
          Vector(part(Part.Kind.Open, 20, Some(0.5)), part(Part.Kind.Closed, 40, Some(0.1))),
          Tokens.Zero,
          Tokens.Zero
        )
      ),
      rounds = Vector(Round(Vector(Call(Called.Unnamed, Settled.Expired)))),
      spend = Vector(
        spent(Some(TurnRecord.Role.Reply), Some("0.002"), 0, 50),
        spent(Some(TurnRecord.Role.Judge), None, 30, 30, cached = 10)
      ),
      speech = Some(Drafted(Drafted.Kind.Below, p(0.4), p(0.7), p(0.5)))
    )

  // t3: thread B, a TUI turn that failed at assembly.
  private val t3 = turn(
    "w3",
    "B",
    TurnOffer.Root.Addressed,
    Said.Tui(EntryId("e3")),
    Ended.Failed("assemble", Ended.Why.Assembly)
  ).copy(weighed = TurnRecord.Weigh.Recorded(None))

  // t4: thread C, a Slack thread asked directly, replied; no tool loop.
  private val t4 =
    turn("w4", "C", TurnOffer.Root.Addressed, slack("C2/2.1"), Ended.Replied(90, false))
      .copy(
        offered = Some(
          Offered(
            Vector(read),
            Set1,
            Tokens(40),
            VectorMap(Layer.Base -> Tokens(500)),
            None,
            Vector.empty,
            None
          )
        ),
        window = Some(
          Parts(
            Vector(part(Part.Kind.Recent, 60, Some(0.2)), part(Part.Kind.Recalled, 15, Some(0.0))),
            Tokens.Zero,
            Tokens(4)
          )
        ),
        spend = Vector(
          spent(Some(TurnRecord.Role.Reply), Some("0.004"), 100, 200, cached = 20),
          spent(Some(TurnRecord.Role.Summary), Some("0.0005"), 50, 60)
        )
      )

  // t5: thread B, a task's turn never finished.
  private val t5 =
    turn("w5", "B", TurnOffer.Root.Addressed, Said.Task(EntryId("e5")), Ended.Unfinished("PENDING"))
      .copy(weighed =
        TurnRecord.Weigh.Recorded(Some(TurnWeighing.Weighed.Failed(Weighing.Unweighed.Late)))
      )

  val turns: Vector[TurnCase] = Vector(t1, t2, t3, t4, t5)

  private def rated(reason: Reason, verdict: Verdict) =
    Rated(ShadowName.of("s").fold(sys.error, identity), reason, verdict, PrincipalId("u"), at)

  val verdicts = Verdicts(
    Map(
      Fixtures.id("C1/1.1") -> rated(Reason.Both, Verdict.Welcome),
      Fixtures.id("C1/1.2") -> rated(Reason.ShadowOnly, Verdict.Interruption),
      Fixtures.id("C9/9.9") -> rated(Reason.Neither, Verdict.CutIn)
    )
  )

}
