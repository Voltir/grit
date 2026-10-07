package grit.eval.harness.score

import scala.collection.immutable.VectorMap
import scala.concurrent.duration.*

import grit.core.classify.Answer
import grit.core.id.QuestionName
import grit.core.message.Usage
import grit.core.period.Probability
import grit.core.store.Focus
import grit.core.triage.{Kind, Tags}
import grit.eval.harness.capture.{CaseId, Digest, Failure}
import grit.eval.harness.jev.Sets
import grit.eval.harness.log.{CacheKey, Outcome, Row, Suite}

import utest.*
import Fixtures.id

/** A question set's draft beside live's, each by its own set's gate, over synthetic rows. */
object DraftsTests extends TestSuite {

  private def name(n: String): QuestionName = QuestionName.read(n).fold(sys.error, identity)

  private val key = CacheKey.read("ab" * 32).fold(sys.error, identity)

  /** Live's kept row of `c` before v2: `kind` chosen, `durable` and `helps` as given. */
  private def live(
      c: CaseId,
      kind: Kind,
      durable: Double,
      helps: Double,
      focus: Option[Focus]
  ): Row[VectorMap[QuestionName, Answer]] =
    setRow(
      c,
      Outcome.Answered(Tags.V1.answers(kind, p(0.4), p(0.5), p(durable), p(helps))),
      focus
    )

  private def p(d: Double): Probability = Probability.clamped(d)

  /** Live asking v1, by its gate. */
  private def v1(rows: Vector[Row[VectorMap[QuestionName, Answer]]]): Drafting =
    Drafting(rows, Sets.V1.speak, Sets.V1.durable, Sets.V1.to)

  /** A set asking v2, by its gate. */
  private def v2(rows: Vector[Row[VectorMap[QuestionName, Answer]]]): Drafting =
    Drafting(rows, Sets.V2.speak, Sets.V2.durable, Sets.V2.to)

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

  val tests = Tests {
    test("v1's draft is live's v1 gate, and its durable live's, on the same answers") {
      val cs = (1 to 4).map(n => id(s"C1/$n")).toVector
      // Each case's kind, its weight, durable and helps, answered alike by live and by v1.
      val asked = Vector(
        (cs(0), Kind.Question, 0.75, 0.5, 0.5),
        (cs(1), Kind.Question, 0.75, 0.25, 0.49),
        (cs(2), Kind.Chatter, 0.4, 0.75, 0.9),
        (cs(3), Kind.Decision, 0.5, 0.49, 0.75)
      )
      val lives = asked.map((c, kind, _, durable, helps) => live(c, kind, durable, helps, None))
      val sets = asked.map { (c, kind, top, durable, helps) =>
        val ws = Kind.values.toVector.map(k =>
          Answer.Weight(Kind.written(k), if (k == kind) top else (1 - top) / 4)
        )
        setRow(
          c,
          Outcome.Answered(
            VectorMap(
              name("kind") -> Answer.Choice(Kind.written(kind), ws, 0.0),
              name("waiting") -> Answer.YesNo(0.5),
              name("durable") -> Answer.YesNo(durable),
              name("helps") -> Answer.YesNo(helps)
            )
          ),
          None
        )
      }
      val d = Drafts.of(v1(lives), v1(sets))
      (d.gate, d.durable) ==> (
        Map(None -> Cells(Vector(cs(0), cs(3)), Vector.empty, Vector.empty, Vector(cs(1), cs(2)))),
        Some(Cells(Vector(cs(0), cs(2)), Vector.empty, Vector.empty, Vector(cs(1), cs(3))))
      )
    }

    test(
      "each case both answered falls in one cell of live's draft against the set's, by focus; one whose gate reads a missing answer is undecided"
    ) {
      val (open, focused) = (Some(Focus.Open), Some(Focus.Focused))
      val cs = (1 to 7).map(n => id(s"C1/$n")).toVector
      val lives = Vector(
        live(cs(0), Kind.Question, 0.3, 0.7, open), // gate: yes
        live(cs(1), Kind.Chatter, 0.5, 0.9, focused), // chatter: no
        live(cs(2), Kind.Question, 0.2, 0.5, focused), // helps at 0.5: yes
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
      val d = Drafts.of(v1(lives), v2(sets))
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
      "durable compares the set's durable question with live's, each yes at 0.5, over every case both answered"
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
      Drafts.of(v1(lives), v2(sets)).durable ==> Some(
        Cells(Vector(cs(1), cs(3)), Vector(cs(4)), Vector(cs(0)), Vector(cs(2)))
      )
    }

    test(
      "live's draft is its own set's gate: live asking v2 against a shadow asking v1, both ways round"
    ) {
      val cs = (1 to 4).map(n => id(s"C1/$n")).toVector
      val asV2 = Vector(
        setRow(cs(0), answers(0.8, 0.9, 0.1, Some(0.1), 0.6), None), // drafts
        setRow(cs(1), answers(0.8, 0.9, 0.7, Some(0.1), 0.6), None), // to someone: not
        setRow(cs(2), answers(0.1, 0.9, 0.1, Some(0.1), 0.2), None), // nothing asked: not
        setRow(cs(3), answers(0.8, 0.9, 0.1, Some(0.1), 0.2), None) // drafts
      )
      val asV1 = Vector(
        live(cs(0), Kind.Question, 0.4, 0.9, None), // drafts
        live(cs(1), Kind.Question, 0.4, 0.9, None), // drafts
        live(cs(2), Kind.Chatter, 0.4, 0.9, None), // chatter: not
        live(cs(3), Kind.Question, 0.4, 0.2, None) // helps below: not
      )
      (
        Drafts.of(v2(asV2), v1(asV1)).gate,
        Drafts.of(v1(asV1), v2(asV2)).gate
      ) ==> (
        Map(None -> Cells(Vector(cs(0)), Vector(cs(3)), Vector(cs(1)), Vector(cs(2)))),
        Map(None -> Cells(Vector(cs(0)), Vector(cs(1)), Vector(cs(3)), Vector(cs(2))))
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
      Drafts.of(v1(Vector.empty), v2(sets)).columns ==> Vector(
        Column("gap.asks", 0.5, 0.25, 2),
        Column("gap.nothing", 0.5, 0.25, 2),
        Column("open", 0.75, 0.25, 2),
        Column("source:github", 0.5, 0.0, 1)
      )
    }

    test(
      "v3's gate over v4's live rows, beside v4's own: a fact asked of a named person drafts " +
        "under v4 alone, an opinion under neither"
    ) {
      val cs = (1 to 3).map(n => id(s"C1/$n")).toVector
      // The room's fact, Bob's fact and Bob's opinion, as v4 answered them live.
      val rows =
        Vector((cs(0), 0.05, 0.16, 0.17), (cs(1), 0.87, 0.18, 0.16), (cs(2), 0.86, 0.95, 0.95))
          .map((c, to, anchor, record) =>
            setRow(
              c,
              Outcome.Answered(
                VectorMap(
                  name("gap") -> gap(1.0),
                  name("open") -> Answer.YesNo(0.93),
                  name("to") -> Answer.YesNo(to),
                  name("to-grit") -> Answer.YesNo(0.04),
                  name("durable") -> Answer.YesNo(0.1),
                  name("anchor") -> Answer.YesNo(anchor),
                  name("anchor-record") -> Answer.YesNo(record)
                )
              ),
              None
            )
          )
      def as(set: String) =
        Sets.named(set).map(s => Drafting(rows, s.speak, s.durable, s.to))
      as("v4").zip(as("v3")).map((live, v3) => Drafts.of(live, v3).gate) ==>
        Some(Map(None -> Cells(Vector(cs(0)), Vector(cs(1)), Vector.empty, Vector(cs(2)))))
    }
  }
}
