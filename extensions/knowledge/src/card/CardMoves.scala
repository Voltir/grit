package grit.knowledge.card

import grit.core.act.MoveName
import grit.knowledge.ref.CardKey

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

  /** Keep or withdraw the pieces of a file whose card was kept or relabelled. */
  case Piece extends Phase("piece")
}

/** The names of knowledge's runs' moves: a sync run's, one per phase and unit, and its three
  * whole-run keeps; and a dreaming run's, numbered within it.
  */
object CardMoves {

  /** `{phase}/{key}`: at most 72 characters. */
  def of(phase: Phase, key: CardKey): MoveName = known(s"${phase.word}/${CardKey.value(key)}")

  /** `vet/{key}/{window}`: the check of window `window`, from 0, of what a run read of `key`'s
    * unit: at most 79 characters.
    */
  def vet(key: CardKey, window: Int): MoveName = known(s"vet/${CardKey.value(key)}/$window")

  /** `held`: the keep reading the cards a run holds. */
  val Held: MoveName = MoveName("held")

  /** `drop`: the keep withdrawing the cards of units gone. */
  val Drop: MoveName = MoveName("drop")

  /** `drop-pieces`: the piece move withdrawing the pieces of the files gone. */
  val DropPieces: MoveName = MoveName("drop-pieces")

  /** `due-pieces`: the piece move finding the held files whose pieces are due. */
  val DuePieces: MoveName = MoveName("due-pieces")

  /** `{kind}-{i}`: a dreaming run's `i`th move of `kind`, lower-case words joined by `-`. */
  private[knowledge] def numbered(kind: String, i: Int): MoveName = known(s"$kind-$i")

  // Every name here is a phase's word or a dreaming move's kind, lowercase words, then at most
  // `/` and 64 hex digits, for a vet `/` and a window's decimal digits, and for a numbered move
  // `-` and its decimal digits: within MoveName's 200 characters, with no `:` or control
  // character, so `of` never refuses it; `CardMovesTests` holds each sync run's.
  private def known(text: String): MoveName =
    MoveName.of(text).fold(why => throw new IllegalStateException(why), identity)
}
