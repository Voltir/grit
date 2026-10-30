package grit.turn

import grit.core.id.EntryId

/** What a turn's window is drawn on, as the `carried` step found it ([[TurnSteps.Env.carried]]). */
enum TurnBasis extends caps.Pure {

  /** Drawn against the window record `record`, which renders now as its turn's last call
    * sent it.
    */
  case Against(record: EntryId)

  /** Drawn afresh, at a boundary. */
  case Afresh(why: TurnBasis.Why)

  /** The record drawn against; `None` when afresh. */
  def head: Option[EntryId] = this match {
    case Against(h) => Some(h)
    case Afresh(_) => None
  }
}

object TurnBasis {

  /** Why a window is drawn afresh; `word` is how its step records it and a report shows it. */
  enum Why(val word: String) {

    /** No earlier turn of the conversation kept a window record the turn reads. */
    case NoWindow extends Why("no-window")

    /** The newest kept no [[Payload.Carry]]: recorded before windows were carried. */
    case NoCarry extends Why("no-carry")

    /** A record it follows back to its boundary is not read
      * ([[grit.core.context.CarriedWindow.chain]]).
      */
    case Unchained extends Why("unchained")

    /** Its turn kept no [[Payload.Prefix]]: it failed or stalled before `append`. */
    case NoPrefix extends Why("no-prefix")

    /** The window does not render as a request now: a part names an entry since gone. */
    case Unrendered extends Why("unrendered")

    /** The request, to the same pair ([[PairDiffers]] otherwise), would differ from that prefix
      * in some byte of its system prompt, tools or messages; the prefix keeps one digest of
      * the three, so which is not known.
      */
    case PrefixDiffers extends Why("prefix-differs")

    /** The turn's calls go to another model pair than the one that prefix was sent to
      * ([[Payload.Prefix]]), or the prefix names none (kept before pairs were): a pair caches
      * only what was sent to it, and a carried reply's reasoning is bound to its pair.
      */
    case PairDiffers extends Why("pair-differs")

    /** The turn's first gate would hide an item the window carries. */
    case Hidden extends Why("hidden")
  }
}
