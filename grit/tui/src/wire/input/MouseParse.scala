package grit.tui.wire.input

import grit.tui.model.input.{Button, MouseEvent, MouseKind, Mods}
import grit.tui.model.surface.Pos

/** Decoding of SGR (mode 1006) mouse reports.
  *
  * Separate from [[Decoder]] so the wire format is under test on its own: the framing
  * question ("where does this report end") and the encoding question ("what does this
  * report say") fail in different ways and are worth failing separately.
  */
object MouseParse {

  /** Decode the parameter payload of an SGR mouse report.
    *
    * The wire form is `ESC [ < b ; x ; y M|m`; `payload` is the `b;x;y` between `<` and
    * the final byte, `finalByte` is `'M'` (press/drag/move/wheel) or `'m'` (release).
    * Wire coordinates are 1-based; the returned [[Pos]] is 0-based.
    *
    * `None` if the payload is not three integers.
    */
  def sgr(payload: String, finalByte: Char): Option[MouseEvent] =
    payload.split(';') match {
      case Array(bs, xs, ys) =>
        for {
          b <- bs.toIntOption
          x <- xs.toIntOption
          y <- ys.toIntOption
        } yield {
          val mods = Mods(shift = (b & 4) != 0, alt = (b & 8) != 0, ctrl = (b & 16) != 0)
          val motion = (b & 32) != 0
          val wheel = (b & 64) != 0
          val low = b & 3
          val button =
            if (wheel) {
              if (low == 0) { Button.WheelUp }
              else { Button.WheelDown }
            } else {
              low match {
                case 0 => Button.Left
                case 1 => Button.Middle
                case 2 => Button.Right
                case _ => Button.None
              }
            }
          val kind =
            if (wheel) { MouseKind.Wheel }
            else if (finalByte == 'm') { MouseKind.Release }
            else if (motion && button == Button.None) { MouseKind.Move }
            else if (motion) { MouseKind.Drag }
            else { MouseKind.Press }
          MouseEvent(kind, button, Pos(y - 1, x - 1), mods)
        }
      case _ => None
    }
}
