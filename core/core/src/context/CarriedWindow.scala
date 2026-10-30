package grit.core.context

import grit.core.id.TurnSeq
import grit.core.message.Tokens

/** The window a conversation carries into its next turn: each part drawn since its last
  * boundary, oldest first. As the model is shown it, each part is followed by its turn's own
  * messages, as that turn was shown them, dated by that turn's first entry and the zones its
  * `offer` recorded (ADR 0016).
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
