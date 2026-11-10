package grit.eval.harness.sent

import scala.util.Try

import grit.core.json.Fields
import grit.core.persona.Persona

/** The persona a deployment declares, as a `persona.json` written by hand from its
  * declaration holds it: grit's database keeps no declaration.
  */
object PersonaJson {

  /** The persona `text` holds: `{"name": "Pip"}`, its name as [[Persona.of]] reads it. Why
    * not, when it is not of that form or the name is refused.
    */
  def read(text: String): Either[String, Persona] =
    for {
      root <- Try(ujson.read(text)).toOption.toRight("persona: not JSON")
      name <- Fields("persona", root).str("name")
      persona <- Persona.of(name).left.map(why => s"persona: $why")
    } yield persona
}
