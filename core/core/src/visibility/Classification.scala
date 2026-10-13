package grit.core.visibility

/** What a transaction knows is classified at: its floor, as
  * [[grit.core.store.Tx.classification]] reads it, never a label its holder picked.
  */
opaque type Classification = Label

object Classification {

  private[core] def apply(floor: Label): Classification = floor

  extension (c: Classification) def label: Label = c
}
