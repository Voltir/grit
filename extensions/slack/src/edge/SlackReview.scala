package grit.slack.edge

import grit.core.place.Place
import grit.core.store.Origin
import grit.slack.event.{ChannelId, TeamId, UserId}

/** A deployment's review as its Slack edge answers it: prompts posted at the top of `place`, a
  * channel or DM of the bot's own team, `channel` of `team`, where nothing said is heard, even
  * with the bot a member; `rater`, the one person whose reactions to them are kept; and the
  * reviews it prompts for after triage's, in order (`prompting`).
  */
final case class SlackReview private (
    place: Place,
    team: TeamId,
    channel: ChannelId,
    rater: UserId,
    prompting: Vector[Prompting.OverDocuments]
) {

  /** This review, prompting for `more` after the reviews it prompts for already. */
  def also(more: Vector[Prompting.OverDocuments]): SlackReview = copy(prompting = prompting ++ more)
}

object SlackReview {

  /** `place`'s review of triage alone for `rater`; why not, when `place` is not
    * `slack:{team}/{channel id}`.
    */
  def of(place: Place, rater: UserId): Either[String, SlackReview] =
    place.segments match {
      // A channel's id (C…, G…) or a direct message's (D…).
      case Vector(_, team, channel)
          if place == Origin.channel(team, channel) && channel.matches("[CDG][A-Z0-9]+") =>
        Right(new SlackReview(place, TeamId(team), ChannelId(channel), rater, Vector.empty))
      case _ => Left(s"a review's place is slack:{team}/{channel id}, not ${place.written}")
    }
}
