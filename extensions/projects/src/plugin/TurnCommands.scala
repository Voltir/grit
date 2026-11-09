package grit.projects.plugin

import grit.projects.name.{KindName, StateName}

/** Which of the projects plugin's commands a turn is offered as tools, and what a turn may not
  * do with them (ADR 0053); a person gives every command through an edge either way.
  */
enum TurnCommands extends caps.Pure {

  /** The reads, `project_new`, `project_keep` and `project_move`; a turn's move into a state
    * `guarded` pairs with its artifact's kind is refused ([[Projects.guardedMove]]): only a
    * person moves an artifact there.
    */
  case Shipped(guarded: Set[(KindName, StateName)])

  /** `project_propose` alone, which keeps a new artifact of `kind` in its kind's first state and
    * changes nothing kept already; every other command is a person's.
    */
  case Proposing(kind: KindName)
}

object TurnCommands {

  /** [[Shipped]], guarding no state. */
  val Unguarded: TurnCommands = Shipped(Set.empty)
}
