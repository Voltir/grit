package grit.eval.harness.corpus

import scala.util.Try

import grit.core.persona.Persona

/** The persona the deployment that recorded a corpus declares, as its `persona.json` holds
  * it: grit's database keeps no declaration, so the file is written beside the corpus from
  * the deployment's.
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
