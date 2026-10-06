package grit.core.job

import grit.core.id.EdgeName

/** Where a schedule's runs report beyond their own conversations, which keep every run. */
enum Report {
  case Kept

  /** At `to`, where the reply of the turn that asked for it was posted. */
  case Posted(to: Destination)
}

/** Where a run's reply is posted: through the edge `edge`, at `address`, that edge's own form
  * of where (`grit.core.edge.Deliveries`). A stored schedule names its edge, and not only an
  * address, so it keeps its meaning whichever edges a deployment serves: a destination elsewhere
  * (another channel, an email address, a person's preferred place resolved when the run is due)
  * is another value of this type, or another case of [[Report]], and never a rewrite of the
  * schedules already stored. Deliveries are not yet claimed by edge: every edge that delivers
  * reads every awaited reply (`grit.core.edge.Deliveries.pending`).
  */
final case class Destination(edge: EdgeName, address: String)
