package grit.app.main

import grit.digest.Digest

import utest.*

/** Which plugins `GRIT_PLUGINS` turns on. */
object PluginChoiceTests extends TestSuite {

  private def names(env: Map[String, String]) =
    Main
      .pluginChoice(env)
      .map(_.map(p => (p.getClass.getSimpleName, grit.core.plugin.PluginName.value(p.name))))

  val tests = Tests {
    test("GRIT_PLUGINS: unset is none; a list of names turns each on once") {
      names(Map.empty) ==> Right(Vector())
      names(Map("GRIT_PLUGINS" -> " digest, ,digest ")) ==> Right(Vector(("Digest", "digest")))
      assert(
        Main.pluginChoice(Map("GRIT_PLUGINS" -> "digest")).exists(_.forall(_.isInstanceOf[Digest]))
      )
    }

    test("a plugin grit does not have, or a name no plugin could have, is refused") {
      Main.pluginChoice(Map("GRIT_PLUGINS" -> "digest,wiki")) ==>
        Left("GRIT_PLUGINS: no plugin wiki; there is digest")
      Main.pluginChoice(Map("GRIT_PLUGINS" -> "Digest")) ==> Left(
        "GRIT_PLUGINS: a plugin's name is lowercase letters, digits and dashes, starting with a letter"
      )
    }
  }
}
