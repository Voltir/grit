package grit.core.allot

/** Whether a turn's route lays an effort of its own over its pin's ([[Router.allot]]). */
enum EffortRouting derives CanEqual {

  /** A chat root asks [[Router.ChatEffort]] and a hard one [[Router.HardEffort]]. */
  case Routed

  /** Every root asks its pin's own effort, whatever grade it is routed to; `Low` headroom still
    * lowers it to at most [[Router.LowEffort]].
    */
  case Held
}
