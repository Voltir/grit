package grit.core.visibility

import grit.core.identity.{Account, TestAccounts}
import grit.core.place.Place

/** What the visibility suites, and the store contracts' labelled cases, build their fixtures
  * from, failing the test on a bad literal.
  */
object TestLabels {

  def compartment(name: String): Compartment =
    Compartment.of(name).fold(e => throw new java.lang.AssertionError(e), identity)

  def place(text: String): Place =
    Place.read(text).fold(e => throw new java.lang.AssertionError(e), identity)

  def group(name: String): GroupName =
    GroupName.of(name).fold(e => throw new java.lang.AssertionError(e), identity)

  /** The compartment the contracts label rooms and documents with. */
  val trial: Compartment = compartment("trial")

  /** Public, in [[trial]]. */
  val Trial: Label = Label.at(Level.Public, trial)

  /** The account of the one person [[Trialled]] clears for [[Trial]]. */
  val Trialist: Account = TestAccounts.account("slack:T1/U-trialist")

  /** [[trial]] declared, every room public, and [[Trialist]] cleared for [[Trial]]. */
  val Trialled: Visibility =
    (for {
      compartments <- Compartments.of(Vector(trial)).left.map(_.toString)
      v <- Visibility
        .of(
          compartments,
          RoomLabels.Public,
          Vector(Group(group("trialists"), Set(Trialist))),
          Vector(Grant(group("trialists"), Trial))
        )
        .left
        .map(_.toString)
    } yield v).fold(e => throw new java.lang.AssertionError(e), identity)
}
