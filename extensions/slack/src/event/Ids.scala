package grit.slack.event

/** A Slack workspace's id (`T…`). */
opaque type TeamId = String

object TeamId {
  def apply(value: String): TeamId = value
  def value(id: TeamId): String = id
}

/** A channel's id (`C…`). */
opaque type ChannelId = String

object ChannelId {
  def apply(value: String): ChannelId = value
  def value(id: ChannelId): String = id

  /** `raw`, trimmed, as a public channel's id: `C` then capital letters and digits; `None`
    * for anything else, a channel's `#name` included.
    */
  def read(raw: String): Option[ChannelId] =
    Some(raw.trim).filter(_.matches("C[A-Z0-9]+"))
}

/** A user's id (`U…`, `W…`), a person's or a bot's. */
opaque type UserId = String

object UserId {
  def apply(value: String): UserId = value
  def value(id: UserId): String = id
}

/** A message's timestamp: its id within its channel, and a thread's id is its root's. */
opaque type Ts = String

object Ts {
  def apply(value: String): Ts = value
  def value(ts: Ts): String = ts
}
