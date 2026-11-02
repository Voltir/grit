package grit.models

import grit.core.secret.{Secret, Variable}

/** OpenRouter's key as a hand-run probe that sends its own HTTP request needs it: refused as
  * [[Secret.read]] refuses it, and otherwise the variable's trimmed text, which test sources may
  * not take from a [[Secret]].
  */
object ProbeKey {

  def of(env: Map[String, String]): Either[String, String] = {
    val name = Variable.value(OpenRouterConfig.KeyVar)
    Secret
      .read(env, OpenRouterConfig.KeyVar)
      .left
      .map(_.message)
      .map(_ => env.getOrElse(name, "").trim)
  }
}
