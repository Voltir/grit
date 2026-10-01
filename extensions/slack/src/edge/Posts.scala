package grit.slack.edge

import grit.core.speech.Rate
import grit.slack.event.ChannelId

/** Where the Slack edge may post at a turn's request (`slack_post`): the channels `to`, by
  * id, and at most `rate` posts across all of them.
  */
final case class Posts private (to: Set[ChannelId], rate: Rate)

object Posts {
  def apply(rate: Rate, first: ChannelId, rest: ChannelId*): Posts =
    new Posts(rest.toSet + first, rate)
}
