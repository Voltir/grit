package grit.outline.fixture

/** Lent to all of grit, as a deployment's internals are. */
private[grit] object Lent {

  /** Lent to all of grit. */
  private[grit] def wide: Int = 0

  /** Lent to this package alone. */
  private[fixture] def narrow: Int = 0
}
