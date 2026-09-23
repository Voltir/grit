package grit.tui.runtime.std

import grit.tui.runtime.Effect
import grit.tui.components.editor.Editor
import grit.tui.components.overlay.{Modal, Popup}
import grit.tui.model.input.Input
import grit.tui.model.surface.PaneId

/** A modal over the app: while one is open its [[Modal.Route]] governs, and there is no
  * pass-through case, so what the modal does not use is swallowed by construction.
  *
  * Extends [[Scrolling]] because `Modal.Route` names scroll messages -- a modal with a
  * document in it pages and jumps like any other pane -- and a declared dependency is
  * better than a message nothing answers.
  */
trait Modals[State, Own] extends Scrolling[State, Own] {

  /** The modal that is open, and the pane its body was painted under -- or None. */
  val modal: State -> Option[(Modal, PaneId)] = _ => None

  /** What closing the modal means. Required: only the app can name the message. */
  val onModalClose: State -> Own

  /** The modal's capture. `at(pane)` is None only before the modal's first frame, and
    * then there is nothing to route *against* -- which is a swallow, not a pass: an
    * input the app beneath must not see does not become visible by arriving early.
    */
  val modalLayer: Layer = (input, state, at) =>
    modal(state) match {
      case None => Claim.Pass(input)
      case Some((m, pane)) =>
        at(pane) match {
          case None => Claim.Swallowed
          case Some(body) =>
            m.route(input, body) match {
              case Modal.Route.Close => Claim.Handled(onModalClose(state))
              case Modal.Route.Scroll(delta) => Claim.Handled(Std.Scroll(delta))
              case Modal.Route.ToTop => Claim.Handled(Std.ToTop)
              case Modal.Route.ToBottom => Claim.Handled(Std.ToBottom)
              case Modal.Route.Pointer(_) =>
                input match {
                  case Input.Mouse(e) => Claim.Handled(Std.Pointer(e, at))
                  case _ => Claim.Swallowed
                }
              case Modal.Route.Swallowed => Claim.Swallowed
              case Modal.Route.Ambient(ambient) => Claim.Pass(ambient)
            }
        }
    }
}

/** A text prompt: the editor consumes what it means to edit text, and what it declines
  * falls through to whatever is below it in the chain.
  */
trait Prompting[State, Own] extends StdBase[State, Own] {

  /** Where the prompt editor is, and the editor to route against -- or None. */
  val prompt: State -> Option[(PaneId, Editor)] = _ => None

  /** `state` with the prompt's editor replaced. Required rather than defaulted: an app
    * that paints a prompt and cannot store an edit is an app whose prompt does not work.
    */
  val withEditor: (State, Editor) -> State

  /** The prompt just changed under the layer's own hand. The sibling of
    * [[Scrolling.onScroll]], and for the same reason: the state change is the machine's,
    * and whatever chrome an app hangs off it is the app's.
    */
  val onPrompt: State -> State = s => s

  /** The rect the editor is given is the one the box was painted into, and the editor
    * takes its wrapping width from that.
    */
  val promptLayer: Layer = (input, state, at) =>
    prompt(state) match {
      case None => Claim.Pass(input)
      case Some((pane, editor)) =>
        at(pane).flatMap(box => editor.route(input, box)) match {
          case Some(next) => Claim.Handled(Std.EditTo(next))
          case None => Claim.Pass(input)
        }
    }

  val editStep: Step = (msg, state) =>
    msg match {
      case Std.EditTo(next) => Some((onPrompt(withEditor(state, next)), Effect.NoOp))
      case _ => None
    }
}

/** A completion list over the prompt.
  *
  * Extends [[Prompting]] because you cannot complete what you cannot type, and because
  * an edit has to re-filter the list it is typed under -- a completion popup that did not
  * narrow as you type would be a list of the wrong things. That re-filter is this trait's
  * business, not the prompt's, which is why [[editStep]] is overridden here rather than
  * the prompt knowing what a popup is.
  */
trait Completing[State, Own] extends Prompting[State, Own] {

  /** The popup that is open, and the pane its box was painted under -- or None. */
  val popup: State -> Option[(Popup, PaneId)] = _ => None

  /** `state` with the completion list replaced -- `None` closes it. The writing half of
    * the reader above, and the reason an app no longer mirrors [[Popup.Route]] into its
    * own messages: arrowing down a list and dismissing it are mechanism.
    */
  val withPopup: (State, Option[Popup]) -> State

  /** What accepting `item` from the list means. The list is already closed by the time
    * this is consulted, so an app says only what the choice *is*.
    */
  val onChose: String -> Option[Own] = _ => None

  /** The list takes the keys that mean *this list* and hands the rest on -- a popup that
    * ate the keystrokes filtering it would be one you could not type into.
    */
  val popupLayer: Layer = (input, state, at) =>
    popup(state) match {
      case None => Claim.Pass(input)
      case Some((p, pane)) =>
        at(pane) match {
          case None => Claim.Pass(input)
          case Some(box) =>
            p.route(input, box) match {
              case Popup.Route.Pass(i) => Claim.Pass(i)
              case Popup.Route.Stay(next) => Claim.Handled(Std.PopupTo(Some(next)))
              case Popup.Route.Dismissed => Claim.Handled(Std.PopupTo(None))
              case Popup.Route.Chose(item) => Claim.Handled(Std.Chose(item))
            }
        }
    }

  /** The prompt's edit, plus the re-filter the list needs. */
  override val editStep: Step = (msg, state) =>
    msg match {
      case Std.EditTo(next) =>
        val edited = withEditor(state, next)
        val filtered = popup(edited) match {
          case Some((p, _)) => withPopup(edited, Some(p.withQuery(next.text)))
          case None => edited
        }
        Some((onPrompt(filtered), Effect.NoOp))
      case _ => None
    }

  val popupStep: Step = (msg, state) =>
    msg match {
      case Std.PopupTo(next) => Some((onPrompt(withPopup(state, next)), Effect.NoOp))
      case Std.Chose(item) =>
        val closed = onPrompt(withPopup(state, None))
        onChose(item) match {
          case Some(own) => Some(ownUpdate(own, closed))
          case None => Some((closed, Effect.NoOp))
        }
      case _ => None
    }
}
