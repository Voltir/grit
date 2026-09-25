package grit.core.topic

import grit.core.id.TurnSeq

/** Something that happened to a conversation's topics. Recorded, never rewritten: the
  * topics as they stand are a fold over every event so far ([[Topics.fold]]).
  */
enum TopicEvent {

  /** A new topic, with no name until it is [[Described]]. */
  case Opened(topic: TopicId)

  /** The user's message that started `turn`, placed `by` someone. A turn placed again is
    * where its latest placement puts it; the earlier ones stay on record.
    */
  case Placed(turn: TurnSeq, weights: Weights, by: Placement)

  /** `topic`'s name (a few words) and a one-line summary of it, as of now. */
  case Described(topic: TopicId, name: String, summary: String)
}

/** Who placed a message, and on what evidence. */
enum Placement {

  /** Nothing to compare it with: the conversation had no topic yet. */
  case First

  /** No classifier answered, for `reason`: the message stays in the current topic. */
  case Unclassified(reason: String)

  /** By the classifier: its `pSame`, that the message is about the current topic, and what
    * that meant under the thresholds of the time. Later thresholds do not revise it.
    */
  case Classified(pSame: Double, outcome: Placement.Outcome)

  /** The main model's `verdict`, asked because the classifier was unsure. `anomaly` says
    * what went wrong in asking, when something did; the verdict stands either way.
    */
  case Asked(verdict: Verdict, anomaly: Option[String])
}

object Placement {

  /** What the classifier's p(same) meant when it was read. */
  enum Outcome {

    /** [[Band.Same]]: the message stays. */
    case Same

    /** [[Band.Uncertain]]: it stays for now, and the main model is asked. */
    case Uncertain

    /** [[Band.Changed]]: the classifier's `choice` among the earlier topics and a new one. */
    case Changed(choice: Vector[Chance])

    /** The band it fell in. */
    def band: Band = this match {
      case Same => Band.Same
      case Uncertain => Band.Uncertain
      case Changed(_) => Band.Changed
    }
  }

  /** The `probability` the classifier gave `topic`: an earlier one, or a new one (`None`). */
  final case class Chance(topic: Option[TopicId], probability: Double)
}

/** How sure the classifier is that a message is about the current topic. */
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

/** What the main model said a message is about. */
enum Verdict {

  /** The current topic. */
  case Current

  /** The earlier topic it called `name`. */
  case Earlier(name: String)

  /** A new topic, which it may have suggested a `name` for. */
  case New(name: Option[String])

  /** Nothing readable: `why` says what it sent instead. */
  case Unreadable(why: String)
}
