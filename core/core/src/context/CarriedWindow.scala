package grit.core.context

import grit.core.id.{EntryId, TurnSeq}
import grit.core.message.Tokens
import grit.core.store.{Entry, Payload, StoreError}

/** The window a conversation carries into its next turn: each part drawn since its last
  * boundary, oldest first. As the model is shown it, each part is followed by its turn's own
  * messages, as that turn was shown them, led by the lines its window record keeps
  * ([[Payload.Carry]]).
  */
final case class CarriedWindow private (parts: Vector[CarriedWindow.Part]) {

  /** What `projection`, drawn for `turn`, leaves carried into the turn after it: a boundary's
    * part alone, or an appended part after these. `turn` is the turn `projection` was drawn
    * for, after every turn of these parts.
    */
  def after(projection: Projection, turn: TurnSeq): CarriedWindow = {
    val part = CarriedWindow.Part(projection.window, projection.cost, turn)
    projection match {
      case _: Projection.Boundary => CarriedWindow(Vector(part))
      case _: Projection.Appended => CarriedWindow(parts :+ part)
    }
  }
}

object CarriedWindow {

  /** `window`, drawn for `turn`, costing `cost` estimated tokens as shown by the assembler
    * that drew it. Its turn's own messages, which follow it, are not counted: an assembler
    * costs them as they are sent.
    */
  final case class Part private[CarriedWindow] (window: Window, cost: Tokens, turn: TurnSeq)

  /** Nothing carried: the next window is a boundary. */
  val Empty: CarriedWindow = CarriedWindow(Vector.empty)

  /** A window record of `turn`, read back as its turn's part of a carried window. */
  final case class Record(turn: TurnSeq, window: Window, carry: Payload.Carry) {

    /** Appended after the record it follows, or drawn at a boundary. */
    def projection: Projection =
      carry.after.fold(Projection.Boundary(window, carry.cost))(_ =>
        Projection.Appended(window, carry.cost)
      )
  }

  /** The window records that the one with id `last` follows back to its boundary, then it,
    * oldest first, each read by `get`. Empty when `last` or a record it follows is not read
    * (gone, or kept above what `get`'s reader is cleared for), is not a window record, kept no
    * [[Payload.Carry]] (recorded before windows were carried), or follows a record of the same
    * or a later turn. A failure of `get` is returned.
    */
  def chain(last: EntryId)(
      get: EntryId => Either[StoreError, Option[Entry]]
  ): Either[StoreError, Vector[Record]] = {
    // Each record follows one of an earlier turn, so the walk ends.
    @annotation.tailrec
    def walk(id: EntryId, later: List[Record]): Either[StoreError, Vector[Record]] =
      get(id) match {
        case Left(e) => Left(e)
        case Right(Some(Entry(_, _, turn, _, _, w @ Payload.Window(_, _, _, _, _, Some(c)), _, _)))
            if later.headOption.forall(next => TurnSeq.value(turn) < TurnSeq.value(next.turn)) =>
          val done = Record(turn, Window.of(w), c) :: later
          c.after match {
            case Some(before) => walk(before, done)
            case None => Right(done.toVector)
          }
        case Right(_) => Right(Vector.empty)
      }
    walk(last, Nil)
  }

  /** The window `records`, oldest first, carry into the turn after the last of them. */
  def of(records: Vector[Record]): CarriedWindow =
    records.foldLeft(Empty)((carried, r) => carried.after(r.projection, r.turn))
}

/** A turn's window, as it stands to the window carried into it. */
enum Projection {

  /** A boundary: `window` replaces whatever was carried. */
  case Boundary(window: Window, cost: Tokens)

  /** `window` follows the carried window, after its last part's turn's own messages; nothing
    * carried is redrawn.
    */
  case Appended(window: Window, cost: Tokens)

  /** The window drawn now: the whole window at a boundary, else what follows the carried one. */
  def window: Window

  /** What `window` costs as shown, by the estimate of the assembler that drew it. */
  def cost: Tokens
}
