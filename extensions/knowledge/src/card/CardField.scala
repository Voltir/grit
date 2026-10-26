package grit.knowledge.card

/** The fields of a card's data, as a reply names them. */
object CardField {

  /** A field's value as a reply names it: a string as it is, anything else as JSON. */
  def shown(value: ujson.Value): String = value.strOpt.getOrElse(ujson.write(value))
}
