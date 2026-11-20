package grit.core.model

/** A model's tool call left as markup inside a field it sent (`<parameter name="…">`,
  * `</parameter>`), which no prose a reader keeps ever holds.
  */
object CallMarkup {

  /** Nothing, or why `text` is refused: it holds call markup, which
    * [[ArgRepair.CarriedParameter]] did not read.
    */
  def refused(text: String): Either[String, Unit] =
    Either.cond(
      Markup.findFirstIn(text).isEmpty,
      (),
      s"it holds call markup: ${Markup.findFirstMatchIn(text).fold("")(m => text.drop(m.start).take(60))}"
    )

  private val Markup = """<parameter name=|</parameter>""".r
}
