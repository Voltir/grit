package grit.core.job

/** Where a schedule's runs report beyond their own conversations, which keep every run. */
enum Report {
  case Kept

  /** At `address`, its edge's own form of where the reply of the turn that asked for it was
    * posted (`grit.core.edge.Deliveries`).
    */
  case Posted(address: String)
}
