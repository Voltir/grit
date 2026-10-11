package grit.knowledge.card

import grit.core.act.MoveName

/** What a sync run's move does to one unit, by the word its name starts with. */
enum Phase(val word: String) {

  /** List a directory. */
  case List extends Phase("list")

  /** Read a file. */
  case Read extends Phase("read")

  /** Ask for a file's summary. */
  case Summary extends Phase("summary")

  /** Keep a unit's card. */
  case Write extends Phase("write")

  /** Copy an unchanged unit's card to another label. */
  case Relabel extends Phase("relabel")
}

/** The names of a sync run's moves: one per phase and unit, and its two whole-run keeps. */
object CardMoves {

  /** `{phase}/{key}`: at most 72 characters. */
  def of(phase: Phase, key: CardKey): MoveName = known(s"${phase.word}/${CardKey.value(key)}")

  /** `held`: the keep reading the cards a run holds. */
  val Held: MoveName = known("held")

  /** `drop`: the keep withdrawing the cards of units gone. */
  val Drop: MoveName = known("drop")

  // Every name here is a phase's word or a literal, lowercase letters, then at most `/` and 64
  // hex digits: within MoveName's 200 characters, with no `:` or control character, so `of`
  // never refuses it; `CardMovesTests` holds each.
  private def known(text: String): MoveName =
    MoveName.of(text).fold(why => throw new IllegalStateException(why), identity)
}
