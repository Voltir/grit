package grit.core.id

/** What a claim's holder is the holder of, read from its form ([[ClaimHolder.form]]). */
enum HolderForm extends caps.Pure {

  /** A session's, as [[ClaimHolder.session]] makes it. */
  case Session

  /** An agent's, as [[ClaimHolder.agent]] makes it: its type, or `None` when the holder keeps the
    * type by its hash, it being no [[AgentType]].
    */
  case Agent(kind: Option[AgentType])

  /** Neither's: text of another form. */
  case Other
}
