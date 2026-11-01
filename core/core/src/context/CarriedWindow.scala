package grit.core.context

import grit.core.id.{EntryId, EntrySeq, TurnSeq}
import grit.core.message.Tokens
import grit.core.store.{Entry, Payload, Speakers, StoreError}

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
  ): Either[StoreError, Vector[Record]] =
    walked(last)(get).map(_.map((turn, w, c) => Record(turn, Window.of(w), c)))

  /** [[chain]]'s records as kept, each with its turn and carry. */
  private def walked(last: EntryId)(
      get: EntryId => Either[StoreError, Option[Entry]]
  ): Either[StoreError, Vector[(TurnSeq, Payload.Window, Payload.Carry)]] = {
    type Kept = (TurnSeq, Payload.Window, Payload.Carry)
    // Each record follows one of an earlier turn, so the walk ends.
    @annotation.tailrec
    def walk(id: EntryId, later: List[Kept]): Either[StoreError, Vector[Kept]] =
      get(id) match {
        case Left(e) => Left(e)
        case Right(
              Some(Entry(_, _, turn, _, _, w @ Payload.Window(_, _, _, _, _, Some(c), _, _), _, _))
            ) if later.headOption.forall(next => TurnSeq.value(turn) < TurnSeq.value(next._1)) =>
          val done = (turn, w, c) :: later
          c.after match {
            case Some(before) => walk(before, done)
            case None => Right(done.toVector)
          }
        case Right(_) => Right(Vector.empty)
      }
    walk(last, Nil)
  }

  /** The window the window record with id `last` showed, read from `all`, its conversation's
    * entries, as one record kept whole, as a record was before windows were carried: the
    * entries of each record [[chain]] reads, a carried record's followed by those of its turn's
    * own entries [[Shown.turn]] shows (`all` may hold other conversations' too), and every record's recalled turns, sections, documents
    * and pieces, oldest first, with no carry. A record kept with no carry is itself. `None`
    * when `last` is not among `all` or is not a window record, or its chain is broken.
    */
  def whole(last: EntryId, all: Vector[Entry]): Option[Payload.Window] =
    all
      .find(_.id == last)
      .flatMap(kept =>
        kept.payload match {
          case w @ Payload.Window(_, _, _, _, _, None, _, _) => Some(w)
          case _: Payload.Window =>
            val records = walked(last)(id => Right(all.find(_.id == id))).getOrElse(Vector.empty)
            Option.when(records.nonEmpty) {
              val shownOwn = (t: TurnSeq) =>
                all
                  .filter(e =>
                    e.conversationId == kept.conversationId && e.turnSeq == t &&
                      Shown.turn(Vector(e), Speakers.none).nonEmpty
                  )
                  .sortBy(e => EntrySeq.value(e.seq))
                  .map(_.seq)
              val entries = records.zipWithIndex.flatMap { case ((t, w, _), i) =>
                w.entries ++ (if (i < records.size - 1) shownOwn(t) else Vector.empty)
              }
              Payload.Window(
                entries,
                records.flatMap(_._2.recalled),
                records.flatMap(_._2.nearby),
                records.flatMap(_._2.documents),
                records.flatMap(_._2.pieces),
                None
              )
            }
          case _ => None
        }
      )

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
