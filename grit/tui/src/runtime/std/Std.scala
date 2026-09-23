package grit.tui.runtime.std

import grit.tui.runtime.TimerId
import grit.tui.components.editor.Editor
import grit.tui.components.overlay.Popup
import grit.tui.model.input.MouseEvent
import grit.tui.model.surface.{PaneId, Placements, Pos, Size}

/** The messages every pane-driven app handles the same way -- quit, resize delivery,
  * scroll, and the whole pointer/selection lifecycle -- as one opt-in vocabulary.
  *
  * An app combines these with its own messages by union: `Msg = Std | Own`. The layer's
  * [[StdApp]] dispatches `Std` itself and delegates everything else, so the app's `Own`
  * enum only names what is genuinely its own -- a modal toggle, a submission, a stream
  * tick. Every case here is plain data (rule 3): [[Pointer]] and [[Edge]] carry the
  * `Placements` they were resolved against for the same reason Demo2 carried them --
  * a position means nothing without the layout it was a position *in*.
  */
enum Std {

  /** The terminal's size, delivered on the one path by which an app learns it -- the
    * same path at startup as at every SIGWINCH. Already the paintable screen
    * ([[grit.tui.model.surface.Size.screen]]): rule 2 was translated at the boundary, and
    * laying out is the app's ([[StdApp.onResize]]); the layer only guarantees the
    * message arrives.
    */
  case Resized(size: Size)

  /** One message for every mouse report -- press, drag, release, wheel -- carrying the
    * frame it was resolved against. `update` reads the event's kind; nothing else
    * varies with it.
    */
  case Pointer(e: MouseEvent, at: Placements)

  /** The autoscroll tick: a drag parked at a pane's edge scrolls that pane one row per
    * tick and extends the selection to the pointer again.
    */
  case Edge(at: Placements)

  /** A drag held off the edge has gone too long without moving; release it. */
  case Deadline

  /** The focused pane scrolls `delta` rows, negative walking back through the document. */
  case Scroll(delta: Int)

  /** The focused pane jumps to the start of its document. */
  case ToTop

  /** The focused pane jumps to the end of its document. */
  case ToBottom

  /** The prompt's editor, replaced by an edit the editor itself claimed. Plain data
    * like every case here (rule 3): an `Editor` is a string, a caret and a history.
    */
  case EditTo(editor: Editor)

  /** The completion list, replaced -- `None` closes it. Moving the selection down a
    * list is mechanism on the same argument that made scrolling mechanism: it is not a
    * decision any app makes differently.
    */
  case PopupTo(popup: Option[Popup])

  /** The completion list's selection was accepted. The layer closes the list; what the
    * choice *means* is the app's ([[StdApp.onChose]]), because a completion and a
    * command palette agree on the keystroke and on nothing after it.
    */
  case Chose(item: String)

  /** Leave the loop and restore the terminal. */
  case Quit
}

object Std {

  /** What the machine owns across messages, embedded in the app's `State` as one field.
    *
    * `pointer` is where the pointer last reported; `edge` is which way it is pulling
    * (-1 above the dragging pane, +1 below, 0 inside); `grabbing` marks a press the app
    * claimed ([[StdApp.onPress]]) so drags and releases route to the app's grab hooks
    * instead of the selection machine; `track` names the scrollbar the pointer is
    * holding, which the layer claims for itself ([[StdApp.scrolls]]).
    *
    * `track` is a `PaneId` and not a second boolean because motion has to be re-anchored
    * against the *same* scroll pane the press landed on, and an app may show more than
    * one.
    *
    * `size` is the screen as [[Resized]] last delivered it. It is here and not in the
    * app's own state because it is not derived from anything -- it is the one piece of
    * input an app was told and then threw away, and a hook that runs between resizes
    * (`onPrompt`, after every edit) has no other way to lay anything out. Five fields,
    * and still not one of them is a rect: geometry lives in the placement map, as
    * everywhere else. A size is not a placement.
    */
  final case class State(
      pointer: Pos = Pos(0, 0),
      edge: Int = 0,
      grabbing: Boolean = false,
      track: Option[PaneId] = None,
      size: Size = Size(0, 0)
  )

  /** The layer's named timers. Scheduling a pending id replaces it, so the autoscroll
    * chain exists at most once by construction -- the rule that makes the escaped drag
    * safe without generation counters.
    */
  val Autoscroll: TimerId = TimerId.of("std-autoscroll")
  val DragEnd: TimerId = TimerId.of("std-drag-end")

  /** One autoscroll tick every 45ms -- one row per tick. */
  private[std] val AutoscrollMs: Long = 45L

  /** How long a drag may go without motion before it is treated as released. Under mode
    * 1002 a pointer that leaves the window stops reporting entirely: no motion, no
    * release, nothing to wait for.
    */
  private[std] val DragEndMs: Long = 1500L

  /** Rows one wheel detent scrolls. */
  private[std] val WheelRows: Int = 3
}
