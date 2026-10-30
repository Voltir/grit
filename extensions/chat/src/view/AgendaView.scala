package grit.chat.view

import grit.core.agenda.{Agenda, Band, Placement}
import grit.core.id.TurnSeq
import grit.core.store.{Entry, EntryAgenda}

/** The conversation's agenda, as the panel's agenda tab shows it: each item, most
  * recently spoken in first, and how one turn's message was placed. Pure data, built by
  * [[AgendaView.of]] from the conversation's entries.
  *
  * @param items every item, most recent first
  * @param placing how the shown turn's message was placed, once it was
  */
final case class AgendaView(items: Vector[AgendaView.Row], placing: Option[AgendaView.Placing])

object AgendaView {

  /** An item: its `name` as shown, how many `messages` are in it, and whether it is the
    * `current` one.
    */
  final case class Row(name: String, messages: Int, current: Boolean)

  /** How the message of `turn` was placed.
    *
    * @param first it opened the conversation's first item, and nothing was asked
    * @param unclassified the classifier did not answer, and why
    * @param pSame the classifier's p(same), and its `band`
    * @param choice the classifier's choice among the earlier items and a new one, by name,
    *   when it was asked (most probable first)
    * @param verdict the main model's, when it was asked, and what went wrong asking
    * @param weights the heaviest items of the last placement, by name, most first (at
    *   most three), and the part `elsewhere`
    * @param placed the item it is in, by name
    * @param disagree the classifier leant one way (p(same) at or above one half) and the
    *   model said the other
    */
  final case class Placing(
      turn: TurnSeq,
      first: Boolean,
      unclassified: Option[String],
      pSame: Option[Double],
      band: Option[Band],
      choice: Vector[(String, Double)],
      verdict: Option[Agenda.Verdict],
      anomaly: Option[String],
      weights: Vector[(String, Double)],
      elsewhere: Double,
      placed: Option[String],
      disagree: Boolean
  )

  /** How a new item is named among a choice's options. */
  val NewOption = "something new"

  /** The most weights a placing lists. */
  val Weights = 3

  /** The agenda `entries` record as they stood through `turn` (the newest turn, without
    * one): those the closing before its period carried, and those its period spoke in
    * ([[EntryAgenda]]); and the placing of `turn`'s message (the latest placed turn's,
    * without one).
    */
  def of(entries: Vector[Entry], turn: Option[TurnSeq]): AgendaView = {
    val through = turn.orElse(entries.map(_.turnSeq).maxByOption(TurnSeq.value))
    val agenda = through.fold(Agenda.empty)(EntryAgenda.through(entries, _))
    val current = agenda.current.map(_.id)
    def name(id: Agenda.ItemId): String = agenda.get(id).fold(Agenda.ItemId.value(id))(_.shown)
    val rows = agenda.items.map(t => Row(t.shown, t.turns.size, current.contains(t.id)))
    val shown = turn
      .filter(agenda.placements.contains)
      .orElse(agenda.placements.keys.maxByOption(TurnSeq.value))
    AgendaView(rows, shown.flatMap(t => placing(t, agenda, name)))
  }

  private def placing(
      turn: TurnSeq,
      agenda: Agenda,
      name: Agenda.ItemId => String
  ): Option[Placing] =
    agenda.placements.get(turn).flatMap(_.lastOption).map { last =>
      val all = agenda.placements.getOrElse(turn, Vector.empty).map(_.by)
      val classified = all.collectFirst { case c: Placement.Classified => c }
      val asked = all.collectFirst { case a: Placement.Asked => a }
      val disagree = classified.zip(asked).exists { (c, a) =>
        a.verdict match {
          case Agenda.Verdict.Unreadable(_) => false
          case v => (c.pSame >= 0.5) != (v == Agenda.Verdict.Current)
        }
      }
      Placing(
        turn = turn,
        first = all.contains(Placement.First),
        unclassified = all.collectFirst { case Placement.Unclassified(r) => r },
        pSame = classified.map(_.pSame),
        band = classified.map(_.outcome.band),
        choice = classified.toVector
          .flatMap {
            _.outcome match {
              case Placement.Outcome.Changed(choice) => choice
              case Placement.Outcome.Same | Placement.Outcome.Uncertain => Vector.empty
            }
          }
          .map(c => (c.item.fold(NewOption)(name), c.probability))
          .sortBy(-_._2),
        verdict = asked.map(_.verdict),
        anomaly = asked.flatMap(_.anomaly),
        weights = last.weights.byItem
          .sortBy(-_.weight)
          .take(Weights)
          .map(s => (name(s.item), s.weight)),
        elsewhere = last.weights.elsewhere,
        placed = Some(name(last.weights.heaviest)),
        disagree = disagree
      )
    }
}
