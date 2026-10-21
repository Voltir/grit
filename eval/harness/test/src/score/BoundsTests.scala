package grit.eval.harness.score

import java.time.Instant

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.{
  ConversationId,
  EntrySeq,
  PluginName,
  QuestionName,
  ShadowName,
  TurnRef,
  TurnSeq
}
import grit.core.identity.TestAccounts
import grit.core.message.{Tokens, Usage}
import grit.core.period.{Judgement, Probability}
import grit.core.place.{Namespace, Place}
import grit.core.review.{Reason, Verdict}
import grit.core.speech.{Heard, Limits, Reach}
import grit.core.spend.DailyCap
import grit.core.triage.{Bound, Cues, Fact, Gate, Reading, Tags}
import grit.core.visibility.Integrity
import grit.eval.harness.capture.CaseId
import grit.eval.harness.label.{Rated, Verdicts}

import utest.*

/** The bound sweep: grid points decided by the shipped speech decision. Every id is synthetic. */
object BoundsTests extends TestSuite {

  private val At = Instant.parse("2026-10-20T09:00:00Z")
  private def p(x: Double) = Probability.clamped(x)
  private val usage = Usage(Tokens(1), Tokens.Zero, Tokens.Zero, None)
  private val q = QuestionName.read("open").fold(sys.error, identity)
  private val shadow = ShadowName.of("v5").fold(sys.error, identity)
  private val rater = TestAccounts.account("slack:T1/U1")
  private val limits =
    Limits.suggested(
      DailyCap.of("1").fold(sys.error, identity),
      Gate.bounds(Bound.Below(Reading.Yes(q), p(0)))
    )

  private val (roomA, roomB) =
    (
      Place.under(Namespace.Slack, Vector("T1", "A")),
      Place.under(Namespace.Slack, Vector("T1", "B"))
    )

  private def id(s: String): CaseId = CaseId.read(s).fold(sys.error, identity)

  /** Case `ts` in `room`, answering `open` at `yes` (none when `None`), with `reach` and `cues`. */
  private def heard(
      ts: String,
      room: Place,
      yes: Option[Double],
      reach: Reach = Reach(Some("addr"), Set.empty),
      cues: Cues = Cues.Empty
  ): Bounds.Case =
    Bounds.Case(
      id(s"C1/$ts"),
      Heard(
        TurnRef(ConversationId(s"c$ts"), TurnSeq(1)),
        EntrySeq(1),
        room,
        At,
        reach,
        Tags.Weighed(VectorMap.from(yes.map(y => q -> Answer.YesNo(y))), "jev", usage),
        Integrity.Member,
        cues
      )
    )

  private def rated(v: Verdict): Rated = Rated(shadow, Reason.Both, v, rater, At)

  /** Each point's name, drafted hits/n/clusters, welcome hits/n, interrupting and welcome held. */
  private def shape(points: Vector[Bounds.Point]) =
    points.map(x =>
      (
        x.name,
        (x.drafted.hits, x.drafted.n, x.drafted.clusters),
        (x.welcome.hits, x.welcome.n),
        x.interrupting,
        x.welcomeHeld
      )
    )

  val tests = Tests {
    test(
      "a grid point drafts exactly the cases Speech.decide drafts under its gate, and counts welcome among them"
    ) {
      val cases = Vector(
        heard("1.0", roomA, Some(0.9)),
        heard("2.0", roomA, Some(0.2)),
        heard("3.0", roomB, Some(0.9)),
        // No address: held before any gate.
        heard("4.0", roomB, Some(0.9), reach = Reach(None, Set.empty)),
        // Names someone: decided by the gate alone, which reads no fact here.
        heard("5.0", roomB, Some(0.9), reach = Reach(Some("addr"), Set(rater))),
        heard("6.0", roomA, Some(0.1)),
        // Its gate's question unanswered: held as unasked by a gate reading it.
        heard("7.0", roomA, None)
      )
      val verdicts = Verdicts(
        Map(
          id("C1/1.0") -> rated(Verdict.Welcome),
          id("C1/2.0") -> rated(Verdict.Interruption),
          id("C1/3.0") -> rated(Verdict.CutIn),
          id("C1/4.0") -> rated(Verdict.Welcome)
        )
      )
      val grid = Bounds.Grid(
        Vector("open" -> Gate.Open, "yes" -> Gate.bounds(Bound.AtLeast(Reading.Yes(q), p(0.5)))),
        limits
      )
      shape(Bounds.swept(grid, cases, verdicts)) ==> Vector(
        ("open", (6L, 7L, 2), (1L, 3L), 2, 1),
        ("yes", (3L, 7L, 2), (1L, 2L), 1, 1)
      )
    }

    test(
      "a fact gate reads each case's kept cues: a cued row passes every fact, a row without cues fails each"
    ) {
      val notes = PluginName.of("notes").fold(sys.error, identity)
      val cued = Cues(
        named = true,
        None,
        None,
        Some(Judgement.Weighed(p(0.1), p(0.9), p(0), "settle")),
        Some(Cues.Match(notes, Fact.PlainMatch, 1))
      )
      val cases =
        Vector(heard("1.0", roomA, Some(0.9), cues = cued), heard("2.0", roomB, Some(0.9)))
      val grid = Bounds.Grid(
        Fact.values.toVector.map(f =>
          f.toString -> Gate.bounds(Bound.AtLeast(Reading.Fact(f), p(0.5)))
        ),
        limits
      )
      shape(Bounds.swept(grid, cases, Verdicts(Map(id("C1/1.0") -> rated(Verdict.Welcome))))) ==>
        Fact.values.toVector.map(f => (f.toString, (1L, 2L, 2), (1L, 1L), 0, 0))
    }

    test(
      "a grid file reads its points in order, each gate as written, and refuses a name given twice"
    ) {
      val bound = """{"reading": {"reads": "yes", "name": "open"}, "bound": "at_least", "p": 0.5}"""
      val fact = """{"reading": {"reads": "fact", "fact": "named"}, "bound": "below", "p": 0.5}"""
      val text =
        s"""{"points": [{"name": "b", "gate": {"all": [$bound, {"any": [$fact]}]}}, {"name": "a", "gate": $bound}]}"""
      val atLeast = Bound.AtLeast(Reading.Yes(q), p(0.5))
      (
        Bounds.Grid.read(text, limits).map(_.points),
        Bounds.Grid.read(text.replace("\"a\"", "\"b\""), limits).left.map(_.contains("b"))
      ) ==> (
        Right(
          Vector(
            "b" -> Gate.All(
              Vector(
                Gate.Holds(atLeast),
                Gate.AnyOf(Gate.Holds(Bound.Below(Reading.Fact(Fact.Named), p(0.5))), Vector.empty)
              )
            ),
            "a" -> Gate.Holds(atLeast)
          )
        ),
        Left(true)
      )
    }
  }
}
