package grit.app.main

import grit.app.config.Prefs
import grit.app.look.Theme

import utest.*

/** The theme a run starts in: `GRIT_THEME` over the one last kept, over the default. */
object ThemeChoiceTests extends TestSuite {

  val tests = Tests {
    test("GRIT_THEME wins over the kept theme") {
      Main.theme(Map("GRIT_THEME" -> "abyss"), Prefs(Some("nightshade"))) ==> Right(Theme.Abyss)
      // One grit does not have is an error listing those it has; the kept one is no fallback.
      Main.theme(Map("GRIT_THEME" -> "sandstone"), Prefs(Some("abyss"))) ==>
        Left("GRIT_THEME is none of frost, tokyo-night, tokyo-storm, tokyo-moon, abyss, nightshade")
    }

    test("unset, the kept theme; one grit no longer has, or none, is the default") {
      Main.theme(Map.empty, Prefs(Some("nightshade"))) ==> Right(Theme.Nightshade)
      Main.theme(Map.empty, Prefs(Some("sandstone"))) ==> Right(Theme.Default)
      Main.theme(Map.empty, Prefs.empty) ==> Right(Theme.Default)
    }
  }
}
