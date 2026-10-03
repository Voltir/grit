package grit.slack.edge

import grit.slack.event.{ChannelId, UserId}

/** A deployment's review as its Slack edge answers it: prompts posted at the top of `place`, a
  * private channel or a DM grit's bot is in, and `rater`, the one person whose reactions to
  * them are kept.
  */
final case class SlackReview(place: ChannelId, rater: UserId)
