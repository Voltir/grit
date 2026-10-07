package grit.core.visibility

/** The compartments a deployment declares, and [[Compartment.Unmapped]]: the only ones a
  * declared label may hold.
  */
final case class Compartments private (declared: Set[Compartment]) {

  /** The first compartment of `label` not declared, or `None`. */
  def undeclared(label: Label): Option[Compartment] =
    Label.compartments(label).find(c => !declared.contains(c))

  /** `label` without the compartments not declared, joined with [[Compartment.Unmapped]] when
    * it held any: what a mapping invents fails high.
    */
  def admit(label: Label): Label = {
    val (kept, invented) = Label.compartments(label).partition(declared.contains)
    val admitted = Label.at(Label.level(label), kept*)
    if (invented.isEmpty) admitted else admitted.join(Label.at(Level.Public, Compartment.Unmapped))
  }

  /** [[Level.Restricted]] in every declared compartment: what grit itself is cleared for. */
  def top: Label = Label.at(Level.Restricted, declared.toVector*)

  /** Whether a database whose labels were made under `before` may run under these: each of
    * `before` is still declared. Dropping or renaming one could change what a stored label
    * means.
    */
  def keeps(before: Compartments): Boolean = before.declared.subsetOf(declared)
}

object Compartments {

  /** [[Compartment.Unmapped]] alone. */
  val Shipped: Compartments = new Compartments(Set(Compartment.Unmapped))

  /** These and [[Compartment.Unmapped]], or the one named twice. */
  def of(declared: Vector[Compartment]): Either[Compartment, Compartments] =
    declared.diff(declared.distinct).headOption match {
      case Some(twice) => Left(twice)
      case None => Right(new Compartments(declared.toSet + Compartment.Unmapped))
    }
}
