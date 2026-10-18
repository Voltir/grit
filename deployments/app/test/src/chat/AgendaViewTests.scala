package grit.app.chat

import java.time.Instant

import grit.core.agenda.{Agenda, Band, Placement, Weights}
import grit.core.id.{ConversationId, EntryId, EntrySeq, TurnSeq}
import grit.core.store.{Entry, Payload}
import grit.core.visibility.Integrity

import utest.*

/** The agenda tab's view, from the conversation's entries alone. */
object AgendaViewTests extends TestSuite {

  private val c = ConversationId("c")
  private val (a, b) = (Agenda.ItemId("a"), Agenda.ItemId("b"))

  private def recorded(seq: Long, turn: Long, events: Agenda.Event*): Entry =
    Entry(
      EntryId(s"t$seq"),
      c,
      TurnSeq(turn),
      None,
      EntrySeq(seq),
      Payload.Agenda(events.toVector),
      Instant.EPOCH,
      Integrity.Member
    )

  private def whole(turn: Long, t: Agenda.ItemId, by: Placement) =
    Agenda.Event.Placed(TurnSeq(turn), Weights.whole(t), by)

  private def weights(elsewhere: Double, shares: (Agenda.ItemId, Double)*): Weights =
    Weights
      .of(shares.toVector.map(Weights.Share(_, _)), elsewhere)
      .fold(e => sys.error(s"not weights: $e"), identity)

  private val entries: Vector[Entry] = Vector(
    recorded(0, 0, Agenda.Event.Opened(a), whole(0, a, Placement.First)),
    recorded(1, 0, Agenda.Event.Described(a, "Knots", "which knot holds")),
    recorded(
      2,
      1,
      Agenda.Event.Opened(b),
      Agenda.Event.Placed(
        TurnSeq(1),
        weights(0.18, a -> 0.1, b -> 0.72),
        Placement.Classified(
          0.1,
          Placement.Outcome.Changed(
            Vector(Placement.Chance(Some(a), 0.2), Placement.Chance(None, 0.8))
          )
        )
      )
    ),
    recorded(
      3,
      2,
      Agenda.Event.Placed(
        TurnSeq(2),
        Weights.same(b, 0.6),
        Placement.Classified(0.6, Placement.Outcome.Uncertain)
      )
    ),
    recorded(
      4,
      2,
      whole(2, a, Placement.Asked(Agenda.Verdict.Earlier("Knots"), Some("dropped a call")))
    )
  )

  val tests = Tests {
    test("items most recent first, with their messages; the current one marked") {
      AgendaView.of(entries, None).items ==> Vector(
        AgendaView.Row("Knots", 2, current = true),
        AgendaView.Row("new topic", 1, current = false)
      )
    }

    test("the latest turn's placing: the classifier's numbers beside the model's verdict") {
      val p = AgendaView.of(entries, None).placing.getOrElse(sys.error("no placing"))
      p.turn ==> TurnSeq(2)
      (p.pSame, p.band) ==> (Some(0.6), Some(Band.Uncertain))
      p.verdict ==> Some(Agenda.Verdict.Earlier("Knots"))
      p.anomaly ==> Some("dropped a call")
      p.placed ==> Some("Knots")
      p.weights ==> Vector("Knots" -> 1.0)
      // Leaning same (0.6) while the model said an earlier item.
      p.disagree ==> true
    }

    test("a pinned turn's placing: level two's choice by name, most probable first") {
      val p = AgendaView.of(entries, Some(TurnSeq(1))).placing.getOrElse(sys.error("no placing"))
      p.choice ==> Vector(AgendaView.NewOption -> 0.8, "Knots" -> 0.2)
      p.weights ==> Vector("new topic" -> 0.72, "Knots" -> 0.1)
      assert(math.abs(p.elsewhere - 0.18) < 1e-9, !p.disagree, p.verdict.isEmpty)
    }

    test("the first message shows as such") {
      AgendaView.of(entries, Some(TurnSeq(0))).placing.map(_.first) ==> Some(true)
    }

    test("a pinned turn never placed shows the latest placed turn's; none placed, none") {
      AgendaView.of(entries, Some(TurnSeq(5))).placing.map(_.turn) ==> Some(TurnSeq(2))
      AgendaView.of(Vector.empty, Some(TurnSeq(5))) ==> AgendaView(Vector.empty, None)
    }

    test("agreement is no flag: leaning same, and the model said current") {
      val agreed = entries.dropRight(1) :+
        recorded(4, 2, whole(2, b, Placement.Asked(Agenda.Verdict.Current, None)))
      AgendaView.of(agreed, None).placing.map(_.disagree) ==> Some(false)
    }
  }
}
