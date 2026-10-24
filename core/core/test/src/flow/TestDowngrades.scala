package grit.core.flow

/** Test fixture: what a downgrade makes of its input, as its [[Raise]] would, for suites outside
  * this package that need a downgrade's output without running a flow.
  */
object TestDowngrades {

  /** What `node` makes of `in`. */
  def raised[A, B](node: Downgrade[A, B], in: A): B = node.rule(in)
}
