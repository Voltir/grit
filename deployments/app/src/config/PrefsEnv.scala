package grit.app.config

import java.nio.file.Path

/** Where the environment keeps the chat's preferences ([[grit.chat.settings.Prefs]]). */
object PrefsEnv {

  /** Where the preferences live for `env`: `grit/prefs` under `XDG_CONFIG_HOME`, or under
    * `.config` in `HOME` when that is unset or empty. `None` when neither is set.
    */
  def path(env: Map[String, String]): Option[Path] =
    env
      .get("XDG_CONFIG_HOME")
      .filter(_.nonEmpty)
      .map(Path.of(_))
      .orElse(env.get("HOME").filter(_.nonEmpty).map(Path.of(_, ".config")))
      .map(_.resolve("grit").resolve("prefs"))
}
