package grit.eval.harness.score

import scala.collection.immutable.VectorMap
import scala.concurrent.duration.*

import grit.core.classify.Answer
import grit.core.id.QuestionName
import grit.core.message.Usage
import grit.core.period.Probability
import grit.core.store.Focus
import grit.core.triage.Kind
import grit.eval.harness.corpus.{CaseId, Digest, Failure}
import grit.eval.harness.log.{CacheKey, Outcome, Row, Suite, Weights}
import grit.lifecycle.triage.TriageQuestions

import utest.*
import Fixtures.id

/** A question set's draft beside live's helps gate, over synthetic rows. */
object DraftsTests extends TestSuite {

  private def name(n: String): QuestionName = QuestionName.read(n).fold(sys.error, identity)

  private val key = CacheKey.read("ab" * 32).fold(sys.error, identity)

  /** Live's kept row of `c`: `kind` chosen, `durable` and `helps` as given. */
  private def live(
      c: CaseId,
      kind: Kind,
      durable: Double,
      helps: Double,
      focus: Option[Focus]
  ): Row[Vector[Weights]] =
    Fixtures.row(
      Suite.Triage,
      c,
      0,
      Outcome.Answered(
        Vector(
          Weights.Choice(kind.ordinal, Kind.values.toVector.map(_ => 0.2), 0.0),
          Weights.YesNo(0.5),
          Weights.YesNo(durable),
          Weights.YesNo(helps)
        )
      ),
      focus = focus
    )

  private def setRow(
      c: CaseId,
      outcome: Outcome[VectorMap[QuestionName, Answer]],
      focus: Option[Focus]
  ): Row[VectorMap[QuestionName, Answer]] =
    Row(
      Suite.Triage,
      c,
      0,
      Digest.text("r"),
      key,
      "m",
      None,
      outcome,
      Usage.Zero,
      1.milli,
      false,
      focus
    )

  private def gap(asks: Double): Answer = {
    val ws: Vector[Answer.Weight] =
      Vector(Answer.Weight("asks", asks), Answer.Weight("nothing", 1 - asks))
    Answer.Choice(
      if (asks >= 0.5) "asks" else "nothing",
      ws,
      Answer.confidence(ws.map(_.probability))
    )
  }

  /** V2's gated answers, and `durable`; `anchor` left out when `None`. */
  private def answers(
      asks: Double,
      open: Double,
      to: Double,
      anchor: Option[Double],
      durable: Double
  ): Outcome[VectorMap[QuestionName, Answer]] =
    Outcome.Answered(
      VectorMap(
        name("gap") -> gap(asks),
        name("open") -> Answer.YesNo(open),
        name("to") -> Answer.YesNo(to),
        name("durable") -> Answer.YesNo(durable)
      ) ++ anchor.map(a => name("anchor") -> Answer.YesNo(a))
    )

  private val v2Gate = TriageQuestions.V2.speak
  private val helpsAt = Probability.clamped(0.6)

  val tests = Tests {
    test(
      "each case both answered falls in one cell of live's helps gate against the set's draft, by focus; one whose gate reads a missing answer is undecided"
    ) {
      val (open, focused) = (Some(Focus.Open), Some(Focus.Focused))
      val cs = (1 to 7).map(n => id(s"C1/$n")).toVector
      val lives = Vector(
        live(cs(0), Kind.Question, 0.3, 0.7, open), // gate: yes
        live(cs(1), Kind.Chatter, 0.5, 0.9, focused), // chatter: no
        live(cs(2), Kind.Question, 0.2, 0.6, focused), // helps at helpsAt: yes
        live(cs(3), Kind.Answer, 0.9, 0.1, None), // no
        live(cs(4), Kind.Question, 0.7, 0.9, open),
        live(cs(5), Kind.Question, 0.7, 0.9, open), // the set has no row
        live(cs(6), Kind.Question, 0.7, 0.9, open)
      )
      val sets = Vector(
        setRow(cs(0), answers(0.8, 0.9, 0.1, Some(0.2), 0.6), open), // drafts
        setRow(cs(1), answers(0.6, 0.5, 0.3, Some(0.1), 0.5), focused), // open at 0.5: drafts
        setRow(cs(2), answers(0.8, 0.9, 0.5, Some(0.1), 0.1), focused), // to at 0.5: not
        setRow(cs(3), answers(0.1, 0.9, 0.1, Some(0.1), 0.9), None), // not
        setRow(cs(4), answers(0.8, 0.9, 0.1, None, 0.2), open), // no anchor: undecided
        setRow(cs(6), Outcome.Failed(Failure.Unavailable), open)
      )
      val d = Drafts.of(lives, sets, v2Gate, helpsAt, Some(name("durable")))
      (d.gate, d.undecided) ==> (
        Map(
          open -> Cells(Vector(cs(0)), Vector.empty, Vector.empty, Vector.empty),
          focused -> Cells(Vector.empty, Vector(cs(2)), Vector(cs(1)), Vector.empty),
          None -> Cells(Vector.empty, Vector.empty, Vector.empty, Vector(cs(3)))
        ),
        1
      )
    }

    test(
      "durable compares the set's question with live's durable tag, each yes at 0.5, over every case both answered"
    ) {
      val cs = (1 to 5).map(n => id(s"C1/$n")).toVector
      val lives = Vector(
        live(cs(0), Kind.Question, 0.3, 0.7, None),
        live(cs(1), Kind.Question, 0.5, 0.7, None),
        live(cs(2), Kind.Question, 0.2, 0.7, None),
        live(cs(3), Kind.Question, 0.9, 0.7, None),
        live(cs(4), Kind.Question, 0.7, 0.7, None)
      )
      val sets = Vector(
        setRow(cs(0), answers(0.8, 0.9, 0.1, Some(0.2), 0.6), None),
        setRow(cs(1), answers(0.8, 0.9, 0.1, Some(0.2), 0.5), None),
        setRow(cs(2), answers(0.8, 0.9, 0.1, Some(0.2), 0.1), None),
        setRow(cs(3), answers(0.8, 0.9, 0.1, Some(0.2), 0.9), None),
        // Undecided on the gate, still compared on durable.
        setRow(cs(4), answers(0.8, 0.9, 0.1, None, 0.49), None)
      )
      Drafts.of(lives, sets, v2Gate, helpsAt, Some(name("durable"))).durable ==> Some(
        Cells(Vector(cs(1), cs(3)), Vector(cs(4)), Vector(cs(0)), Vector(cs(2)))
      )
    }

    test(
      "each of the set's probabilities has its mean and spread over the cases it answered, a choice by key, in the order first asked"
    ) {
      val (a, b) = (id("C1/1"), id("C1/2"))
      def asked(asks: Double, open: Double, more: (QuestionName, Answer)*) =
        Outcome.Answered(
          VectorMap(name("gap") -> gap(asks), name("open") -> Answer.YesNo(open)) ++ more
        )
      val sets = Vector(
        setRow(a, asked(0.75, 0.5, name("source:github") -> Answer.YesNo(0.5)), None),
        setRow(b, asked(0.25, 1.0), None)
      )
      Drafts.of(Vector.empty, sets, v2Gate, helpsAt, None).columns ==> Vector(
        Column("gap.asks", 0.5, 0.25, 2),
        Column("gap.nothing", 0.5, 0.25, 2),
        Column("open", 0.75, 0.25, 2),
        Column("source:github", 0.5, 0.0, 1)
      )
    }
  }
}
