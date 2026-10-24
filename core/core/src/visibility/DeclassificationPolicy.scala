package grit.core.visibility

/** A compartment's declassification policy in force, without its text: the one act it does
  * (`over`) and its `version`. Its text, kept at its compartment's own group's grant, is read
  * only by the move that rewrites under it. It rewrites only documents an application names
  * ([[grit.core.document.PolicyApplication]]).
  */
final case class DeclassificationPolicy private[grit] (
    over: DeclassificationPolicy.Over,
    version: Int
) extends caps.Pure {

  /** `label` without its compartment; `None` when `label` does not hold it, or holds the
    * unmapped mark.
    */
  def lowered(label: Label): Option[Label] = over match {
    case DeclassificationPolicy.Over.Compartment(c) =>
      val marks = Label.marks(label)
      Option.when(!Label.unmapped(label) && marks.contains(c))(
        Label.marked(Label.level(label), marks.filterNot(_ == c))
      )
  }
}

object DeclassificationPolicy {

  /** The one element of a label a policy acts on: a compartment, which it removes. */
  enum Over extends caps.Pure {
    case Compartment(c: grit.core.visibility.Compartment)

    /** Its stored form, `compartment:<name>`. */
    def written: String = this match {
      case Compartment(c) => s"compartment:${grit.core.visibility.Compartment.name(c)}"
    }
  }

  object Over {

    /** What [[Over.written]] wrote, or why not. */
    def read(text: String): Either[String, Over] =
      text.split(":", 2) match {
        case Array("compartment", name) =>
          grit.core.visibility.Compartment.of(name).map(Over.Compartment(_))
        case _ => Left(s"$text is not what a policy acts on: compartment:<name>")
      }
  }
}
