package grit.tui.runtime.std

import grit.tui.model.input.Input

/** What one routing layer did with an input.
  *
  * The router is a chain of layers and this is the only thing a layer may say. Three
  * cases, and the third is the one that matters: [[Pass]] carries an input rather than
  * being a bare "not mine", so a layer may *rewrite* what the rest of the chain sees.
  * That is what `Modal.Route.Ambient` and `Popup.Route.Pass` had been doing by calling
  * the prompt's router directly from inside the modal's and the popup's -- two
  * re-entrant calls that fixed the order of everything downstream of them and made the
  * chain unreadable as a chain. Said as data, the same behaviour is one step forward.
  *
  * [[Swallowed]] is deliberately distinct from `Pass`: an input a modal consumes must
  * not reach what is painted behind it, and a chain whose only vocabulary was "handled
  * or not" could not say that. It is the capture rule of `Modal.Route`, restated for
  * every layer.
  */
enum Claim[+Msg] {

  /** This layer produced a message; the chain stops here. */
  case Handled(msg: Msg)

  /** Consumed, and it means nothing. The chain stops and no message is produced -- the
    * app beneath must not see it.
    */
  case Swallowed

  /** Not this layer's. The next layer sees `input`, which need not be the one that
    * arrived: a modal hands its ambient facts on, a popup hands typing on.
    */
  case Pass(input: Input)
}
