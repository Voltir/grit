package grit.core.occasion

import grit.core.id.{PluginName, SourceId}

/** 1 to 80 characters of `[a-z0-9:-]`: the plugin's part of its occasion's entry's source id
  * ([[OccasionKey.source]]), bounded and plugin-scoped, which a raw [[SourceId]] is not.
  */
opaque type OccasionKey = String

object OccasionKey {

  /** `text` as a key; `None` when it is empty, over 80 characters, or holds a character other
    * than a lowercase letter, a digit, `:` or `-`.
    */
  def of(text: String): Option[OccasionKey] =
    Option.when(
      text.nonEmpty && text.length <= 80 &&
        text.forall(c => (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == ':' || c == '-')
    )(text)

  def value(key: OccasionKey): String = key

  /** The source id of `plugin`'s occasion of `key`: `occasion:{plugin}:{key}`. */
  def source(plugin: PluginName, key: OccasionKey): SourceId =
    SourceId(s"occasion:${PluginName.value(plugin)}:$key")
}
