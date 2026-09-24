package grit.tui.components.tree

import grit.tui.components.pane.Anchor
import grit.tui.model.select.{Doc, Selection}
import grit.tui.model.surface.Style

/** A scrollable, selectable document as a component: its state, its messages, its
  * update and its view, as one value an app nests with `Node.map`. The runtime routes the
  * pane's input (the wheel, the thumb, a drag, the page keys); this turns what it routed
  * into state.
  *
  * The state is only what the user decided: where they are reading from, and what they
  * are selecting, both in document coordinates. Nothing laid out is kept.
  */
object Scroller {

  final case class State(anchor: Anchor = Anchor.Bottom, selection: Option[Selection] = None)

  enum Msg {
    case Scrolled(anchor: Anchor)
    case Selected(selection: Option[Selection])

    /** A finished drag's text. The parent decides what copying means (an `Effect`). */
    case Copied(text: String, expired: Boolean)
  }

  val init: State = State()

  val update: (Msg, State) -> State = (msg, s) =>
    msg match {
      case Msg.Scrolled(a) => s.copy(anchor = a)
      case Msg.Selected(sel) => s.copy(selection = sel)
      case Msg.Copied(_, _) => s
    }

  /** `doc` as a pane keyed `key`, reading from and selecting what `s` says. */
  def view(
      key: PaneKey,
      doc: Doc,
      s: State,
      bar: Option[(Style, Style)] = None,
      focused: Boolean = false
  ): Node[Msg] =
    Node
      .DocPane[Msg](key, doc, s.anchor, s.selection, bar, focused)
      .onScroll(Msg.Scrolled(_))
      .onSelect(Msg.Selected(_))
      .onCopy(Msg.Copied(_, _))
}
