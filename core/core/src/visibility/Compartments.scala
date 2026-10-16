package grit.core.visibility

/** The compartments a deployment declares: the only ones a label may hold beside the unmapped
  * mark ([[Label.Unmapped]]), which every deployment has and none declares.
  */
final case class Compartments private (declared: Set[Compartment]) {

  /** The first compartment of `label` not declared, or `None`. */
  def undeclared(label: Label): Option[Compartment] =
    Label.compartments(label).find(c => !declared.contains(c))

  /** `label` without the compartments not declared, joined with [[Label.Unmapped]] when it
    * held any: what a mapping invents fails high.
    */
  def admit(label: Label): Label = {
    val (kept, invented) = Label.compartments(label).partition(declared.contains)
    val admitted: Label = Label.at(Label.level(label), kept*)
    if (invented.isEmpty && !Label.unmapped(label)) admitted else admitted.join(Label.Unmapped)
  }

  /** [[Level.Restricted]] in every declared compartment and the unmapped mark: what grit
    * itself is cleared for.
    */
  def top: Label = Label.at(Level.Restricted, declared.toVector*).join(Label.Unmapped)

  /** Whether a database whose labels were made under `before` may run under these: each of
    * `before` is still declared. Dropping or renaming one could change what a stored label
    * means.
    */
  def keeps(before: Compartments): Boolean = before.declared.subsetOf(declared)
}

object Compartments {

  /** None declared. */
  val Shipped: Compartments = new Compartments(Set.empty)

  /** These, or the one named twice. */
  def of(declared: Vector[Compartment]): Either[Compartment, Compartments] =
    declared.diff(declared.distinct).headOption match {
      case Some(twice) => Left(twice)
      case None => Right(new Compartments(declared.toSet))
    }
}
