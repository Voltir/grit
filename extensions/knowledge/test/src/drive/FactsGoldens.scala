package grit.knowledge.drive

import grit.core.tool.Outcome
import grit.core.visibility.Facts

/** The goldens under `test/resources/drive/facts/`, each the facts a reader keeping every part
  * of one Drive answer states, `{"answer"?:…,"parts":[…]}`: knowledge's one reader of them.
  */
object FactsGoldens {

  /** The golden `name` as JSON. */
  def json(name: String): ujson.Value = {
    val in = Option(getClass.getResourceAsStream(s"/drive/facts/$name.json"))
      .getOrElse(throw new java.lang.AssertionError(s"no golden drive/facts/$name.json"))
    try ujson.read(scala.io.Source.fromInputStream(in).mkString)
    finally in.close()
  }

  /** `v`, a golden's form, as what a reader states. */
  def stated(v: ujson.Value): Outcome.Stated =
    Outcome.Stated(
      v.obj.get("answer").map(Facts.of),
      v.obj.get("parts").toVector.flatMap(_.arr.toVector).map(Facts.of)
    )

  /** The golden `name` as what a reader states. */
  def stated(name: String): Outcome.Stated = stated(json(name))
}
