package grit.tui.model.input

import grit.tui.model.surface.{Pos, Size}

/** A key, already decoded from whatever escape sequence carried it.
  *
  * `Printable` carries one UTF-16 char, so an astral code point arrives as two events --
  * the same limit the cell grid has (ROADMAP open question 1), and it moves with it.
  * The navigation keys carry [[Mods]]: the terminal's `ESC[1;5D` is Ctrl-Left, and a
  * binding table that cannot see the difference cannot bind word-wise motion. The
  * default is no modifier, so a bare `Key.Up()` still matches only a plain arrow.
  */
enum Key {
  case Printable(ch: Char)
  case Ctrl(ch: Char)
  case Alt(ch: Char)
  case Enter, Tab, BackTab, Backspace, Escape
  case Up(mods: Mods = Mods.none)
  case Down(mods: Mods = Mods.none)
  case Left(mods: Mods = Mods.none)
  case Right(mods: Mods = Mods.none)
  case Home(mods: Mods = Mods.none)
  case End(mods: Mods = Mods.none)
  case PageUp(mods: Mods = Mods.none)
  case PageDown(mods: Mods = Mods.none)
  case Delete(mods: Mods = Mods.none)

  /** A sequence that arrived intact but means nothing here -- the raw characters,
    * escape byte included, so a report that should have decoded is debuggable.
    */
  case Unknown(raw: String)
}

/** Which button an event is about.
  *
  * `WheelUp`/`WheelDown` are buttons rather than a separate axis because that is how the
  * wire encodes them. `None` means the report named no button -- a bare motion event. It
  * shadows `scala.None` inside this package; write `Button.None` at every use site.
  */
enum Button {
  case Left, Middle, Right, WheelUp, WheelDown, None
}

/** What happened to the button. `Drag` is motion with a button held, `Move` is motion
  * with none; a `Wheel` event is one detent and has no press/release pairing.
  */
enum MouseKind {
  case Press, Release, Drag, Move, Wheel
}

/** The modifiers the terminal reported. Only these three are distinguishable on the
  * wire; super/meta is not.
  */
final case class Mods(shift: Boolean, alt: Boolean, ctrl: Boolean)

object Mods {

  /** No modifier held. */
  val none: Mods = Mods(false, false, false)
}

/** One decoded mouse report, in 0-based screen coordinates -- the same space
  * [[grit.tui.model.surface.Hit.paneAt]] takes.
  *
  * `button` is `Button.None` exactly when `kind` is `Move`; a `Wheel` event always
  * carries `WheelUp` or `WheelDown`.
  */
final case class MouseEvent(kind: MouseKind, button: Button, pos: Pos, mods: Mods)

/** Everything the terminal can tell the app, in one type.
  *
  * Binding -- what an input *means* -- is deliberately not here and not anywhere in the
  * library: it depends on the app's own message type and on the state the event arrives
  * in (a modal swallows nearly everything). grit.tui decodes; the app decides.
  *
  * `Resize` never comes out of [[Decoder]]: it arrives from SIGWINCH, not from the byte
  * stream, and is folded into the same stream by the runtime so an app has one input
  * type. Its size is the paintable screen ([[grit.tui.model.surface.Size.screen]]), not the
  * terminal's -- rule 2 is translated before the app ever sees a size.
  */
enum Input {
  case Keyboard(key: Key)
  case Mouse(event: MouseEvent)
  case Paste(text: String)
  case Focus(gained: Boolean)
  case Resize(size: Size)
}
