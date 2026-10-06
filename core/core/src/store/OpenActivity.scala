package grit.core.store

import grit.core.period.Activity

/** An open period as the sweep reads it: as its deadline sees it, and where its conversation
  * comes from.
  */
final case class OpenActivity(activity: Activity, origin: Origin)
