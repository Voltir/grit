package grit.core.id

/** A schedule's id: `asked:{hash}` for one a tool wrote, its call's [[CallSlot.key]] hashed
  * ([[ShortHash]]); `declared:deployment:{key}` or `declared:plugin:{name}:{key}` for one
  * declared. A stored form: a schedule row and every slot's key hold it.
  */
opaque type ScheduleId = String

object ScheduleId {

  private val Hash = "[0-9a-f]{16}".r

  def asked(call: CallSlot): ScheduleId = s"asked:${ShortHash.of(call.key)}"

  def declared(by: Declarer, key: ScheduleKey): ScheduleId = by match {
    case Declarer.Deployment => s"declared:deployment:$key"
    case Declarer.Plugin(name) => s"declared:plugin:$name:$key"
  }

  /** The id `text` writes, or why it is none: neither form above. */
  def of(text: String): Either[String, ScheduleId] = {
    val written = text.split(":", -1).toVector match {
      case Vector("asked", hash) => Hash.matches(hash)
      case Vector("declared", "deployment", key) => ScheduleKey.of(key).isRight
      case Vector("declared", "plugin", name, key) =>
        PluginName.of(name).isRight && ScheduleKey.of(key).isRight
      case _ => false
    }
    Either.cond(written, text, s"$text is no schedule's id")
  }

  def value(id: ScheduleId): String = id
}
