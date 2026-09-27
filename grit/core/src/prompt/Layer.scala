package grit.core.prompt

/** Where a fragment of the system prompt comes from, in the order a prompt holds them: the
  * most stable first, since a provider's prompt cache hits only on a byte-identical prefix.
  */
enum Layer(val key: String) {

  /** grit's own words, changed by a release. */
  case Base extends Layer("base")

  /** What the conversation's edge is, and what it renders. */
  case Edge extends Layer("edge")

  /** How the person the conversation is with wants to be talked to: their choice, never
    * grit's own words.
    */
  case Person extends Layer("person")

  /** grit's words on where the conversation is, and what a turn may reach there. */
  case Reach extends Layer("reach")

  /** The place's own instruction files, verbatim: data from the place, never grit's words. */
  case Place extends Layer("place")
}

object Layer {

  /** The layer whose [[Layer.key]] is `key`; `None` for no layer's. */
  def of(key: String): Option[Layer] = Layer.values.find(_.key == key)
}
