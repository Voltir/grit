package grit.core.visibility

import grit.core.place.Place

/** What the visibility suites build their fixtures from, failing the test on a bad literal. */
object TestLabels {

  def compartment(name: String): Compartment =
    Compartment.of(name).fold(e => throw new java.lang.AssertionError(e), identity)

  def place(text: String): Place =
    Place.read(text).fold(e => throw new java.lang.AssertionError(e), identity)

  def group(name: String): GroupName =
    GroupName.of(name).fold(e => throw new java.lang.AssertionError(e), identity)
}
