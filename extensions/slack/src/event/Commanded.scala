package grit.slack.event

/** A slash command, as [[Events.command]] reads it: `command`, its name with its `/` (`/grit`),
  * asked by `user` of `team`, the workspace it was asked in, in `channel`, with `text`, the
  * words after its name as Slack sent them (a mention as `<@U…|name>`), its answers going to
  * `answerAt`.
  */
final case class Commanded(
    team: TeamId,
    channel: ChannelId,
    user: UserId,
    command: String,
    text: String,
    answerAt: ResponseUrl
) {

  /** Whether it was asked in a direct message (a `D…` channel), whoever that is with. */
  def direct: Boolean = ChannelId.value(channel).startsWith("D")
}
