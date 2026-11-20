package grit.projects.plugin

import grit.projects.name.KindName

/** Which of the projects plugin's commands a turn is offered as tools (ADR 0053); a person gives
  * every command through an edge either way. What a turn may not move is its method's
  * ([[grit.projects.method.Movers]]).
  */
enum TurnCommands extends caps.Pure {

  /** The reads, `project_new`, `project_keep`, `project_edit`, `project_reorder`, `project_split`
    * and `project_move`.
    */
  case Shipped

  /** `project_propose` alone, which keeps a new artifact of `kind` in its kind's first state and
    * changes nothing kept already; every other command is a person's.
    */
  case Proposing(kind: KindName)
}
