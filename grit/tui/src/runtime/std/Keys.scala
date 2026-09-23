package grit.tui.runtime.std

import grit.tui.runtime.Effect
import grit.tui.model.input.Input
import grit.tui.model.surface.Size

/** Keys bound *before* anything can capture them: quit, and the app's own modes.
  *
  * This and [[FreeKeys]] are the two halves of binding, and the difference between them
  * is precedence rather than key shape. A hotkey reaches the app whatever is open --
  * capture governs what reaches the panes, not what reaches the program -- so this is
  * where a modal must never be able to eat ctrl-q.
  */
trait Hotkeys[State, Own] extends StdBase[State, Own] {

  /** The app's hotkeys. May produce either half of the union: quit is a hotkey like any
    * other, and [[Std.Quit]] is how it is said.
    */
  val hotkeys: (State, Input) -> Option[Std | Own] = (_, _) => None

  val hotkeyLayer: Layer = (input, state, at) => {
    val _ = at
    hotkeys(state, input) match {
      case Some(own) => Claim.Handled(own)
      case None => Claim.Pass(input)
    }
  }
}

/** The terminal's own facts, and the two messages no app has an opinion about.
  *
  * A resize is not an interaction with a frozen app, so it is claimed above the modal;
  * shift-drag belongs to the user's terminal (rule 9) and is swallowed here so that no
  * layer below can quietly bind it. Quit and resize delivery ride along because they are
  * the same kind of thing: facts, not decisions. The one decision -- what a resize lays
  * out -- is [[onResize]].
  *
  * Effectively every app wants this mixin. It is separate anyway because `onResize` is
  * abstract, and a trait with an abstract member is a trait an app must answer.
  */
trait Ambient[State, Own] extends StdBase[State, Own] {

  /** What `Std.Resized` means: laying out is the app's, at the sizes it will paint at
    * (rule 7 keeps wrapping out of `view`).
    */
  val onResize: (State, Size) -> State

  /** [[StdBase.relayout]] is `onResize` at the screen the layer last recorded: an app
    * that can lay itself out at a size can lay itself out again at the same one.
    */
  override val relayout: State -> State = s => onResize(s, std(s).size)

  val ambientLayer: Layer = (input, state, at) => {
    val _ = (state, at)
    input match {
      case Input.Resize(size) => Claim.Handled(Std.Resized(size))
      case Input.Mouse(e) if e.mods.shift => Claim.Swallowed
      case _ => Claim.Pass(input)
    }
  }

  val ambientStep: Step = (msg, state) =>
    msg match {
      case Std.Quit => Some((state, Effect.Quit))
      // Recorded before the app is told, so `onResize` and every later hook see the
      // same screen -- a hook that re-lays out between resizes reads it from here.
      case Std.Resized(size) =>
        val noted = withStd(state, std(state).copy(size = size))
        Some((onResize(noted, size), Effect.NoOp))
      case _ => None
    }
}

/** Keys bound only after every component has declined -- Enter, Tab, whatever means
  * something only here.
  *
  * Binding here rather than in [[Hotkeys]] is what keeps Enter from submitting through an
  * open modal, and what lets the editor have the printable characters it needs. The whole
  * difference between the two is where in [[StdBase.layers]] each one sits.
  */
trait FreeKeys[State, Own] extends StdBase[State, Own] {

  val free: (State, Input) -> Option[Own] = (_, _) => None

  val freeLayer: Layer = (input, state, at) => {
    val _ = at
    free(state, input) match {
      case Some(own) => Claim.Handled(own)
      case None => Claim.Pass(input)
    }
  }
}
