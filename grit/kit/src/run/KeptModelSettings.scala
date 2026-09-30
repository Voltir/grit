package grit.kit.run

import java.time.ZoneOffset

import grit.core.clock.Clock
import grit.core.model.{ModelSetting, ModelSettings}
import grit.core.store.{Jot, ModelSettingStore}

/** [[ModelSettings]] over `store`, each setting kept in a short transaction of its own through
  * `jot`, dated by `clock` (its UTC day) and approved by the person answering in the chat.
  */
final class KeptModelSettings(jot: Jot^, store: ModelSettingStore, clock: Clock^)
    extends ModelSettings {

  def keep(setting: ModelSetting): Either[String, Unit] = {
    val now = clock.now()
    jot
      .write(store.keep(setting.profile(now.atOffset(ZoneOffset.UTC).toLocalDate), "chat", now))
      .left
      .map(e => s"the model setting could not be kept: $e")
  }
}
