package grit.core.tool

import grit.core.model.{ArgRepair, NameRepair}

/** What is repaired in a model's tool call before it is read: its name as `names` says, and
  * its arguments by each repair in `args`; nothing else is.
  */
final case class Repairs(names: NameRepair, args: Set[ArgRepair])

object Repairs {

  /** Every repair grit makes: how a pair with no profile is read. */
  val All: Repairs = Repairs(NameRepair.HarmonyCut, ArgRepair.values.toSet)
}
