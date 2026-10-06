package grit.app.main

import utest.*

/** Which plugins `GRIT_PLUGINS` turns on. */
object PluginChoiceTests extends TestSuite {

  private def names(env: Map[String, String]) =
    Main
      .pluginChoice(env)
      .map(_.map(p => (p.getClass.getSimpleName, grit.core.id.PluginName.value(p.name))))

  val tests = Tests {
    test("GRIT_PLUGINS: unset is none; a list of names turns each on once") {
      names(Map.empty) ==> Right(Vector())
      names(Map("GRIT_PLUGINS" -> " digest, ,digest ")) ==> Right(Vector(("Digest", "digest")))
    }

    test("GRIT_PLUGINS: remind turns on the reminders, beside digest or alone") {
      (names(Map("GRIT_PLUGINS" -> "remind")), names(Map("GRIT_PLUGINS" -> "digest,remind"))) ==> (
        Right(Vector(("Reminders", "remind"))),
        Right(Vector(("Digest", "digest"), ("Reminders", "remind")))
      )
    }

    test("a plugin grit does not have, or a name no plugin could have, is refused") {
      Main.pluginChoice(Map("GRIT_PLUGINS" -> "digest,wiki")) ==>
        Left("GRIT_PLUGINS: no plugin wiki; there are digest and remind")
      Main.pluginChoice(Map("GRIT_PLUGINS" -> "Digest")) ==> Left(
        "GRIT_PLUGINS: a plugin's name is lowercase letters, digits and dashes, starting with a letter"
      )
    }
  }
}
