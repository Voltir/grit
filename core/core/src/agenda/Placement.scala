package grit.core.agenda

/** Who placed a message, and on what evidence. */
enum Placement {

  /** Nothing to compare it with: the conversation had no agenda item yet. */
  case First

  /** No classifier answered, for `reason`: the message stays in the current item. */
  case Unclassified(reason: String)

  /** By the classifier: its `pSame`, that the message is about the current item, and what
    * that meant under the thresholds of the time. Later thresholds do not revise it.
    */
  case Classified(pSame: Double, outcome: Placement.Outcome)

  /** The main model's `verdict`, asked because the classifier was unsure. `anomaly` says
    * what went wrong in asking, when something did; the verdict stands either way.
    */
  case Asked(verdict: Agenda.Verdict, anomaly: Option[String])
}

object Placement {

  /** What the classifier's p(same) meant when it was read. */
  enum Outcome {

    /** [[Band.Same]]: the message stays. */
    case Same

    /** [[Band.Uncertain]]: it stays for now, and the main model is asked. */
    case Uncertain

    /** [[Band.Changed]]: the classifier's `choice` among the earlier items and a new one. */
    case Changed(choice: Vector[Chance])

    /** The band it fell in. */
    def band: Band = this match {
      case Same => Band.Same
      case Uncertain => Band.Uncertain
      case Changed(_) => Band.Changed
    }
  }

  /** The `probability` the classifier gave `item`: an earlier one, or a new one (`None`). */
  final case class Chance(item: Option[Agenda.ItemId], probability: Double)
}

/** How sure the classifier is that a message is about the current item. */
enum Band {

  /** Sure it is: the message stays. */
  case Same

  /** Unsure: the message stays for now, and the main model is asked. */
  case Uncertain

  /** Sure it is not: the classifier is asked where it went instead. */
  case Changed
}

object Band {

  /** From this p(same) up, [[Same]]. */
  val SameFrom = 0.8

  /** Below this p(same), [[Changed]]. */
  val ChangedBelow = 0.2

  /** The band of `pSame`. */
  def of(pSame: Double): Band =
    if (pSame >= SameFrom) Same else if (pSame < ChangedBelow) Changed else Uncertain
}
