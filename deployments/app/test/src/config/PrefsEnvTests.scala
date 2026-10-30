package grit.app.config

import java.nio.file.Path

import utest.*

object PrefsEnvTests extends TestSuite {

  val tests = Tests {
    test("it lives under XDG_CONFIG_HOME, or else under HOME's .config") {
      PrefsEnv.path(Map("XDG_CONFIG_HOME" -> "/x", "HOME" -> "/h")) ==>
        Some(Path.of("/x/grit/prefs"))
      PrefsEnv.path(Map("XDG_CONFIG_HOME" -> "", "HOME" -> "/h")) ==>
        Some(Path.of("/h/.config/grit/prefs"))
      PrefsEnv.path(Map.empty) ==> None
    }
  }
}
