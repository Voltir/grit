package grit.core.edge

import grit.core.id.Harness

/** A harness's adapter, as a deployment gives it: `harness`, the harness it adapts, and `join`,
  * the script served at `/join/<harness>` that sets a worker's harness up to reach the edge.
  * The edge serves the text as it is and runs nothing of it.
  */
final case class Adapter(harness: Harness, join: String) extends caps.Pure
