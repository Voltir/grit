package grit.slack.edge

import grit.core.identity.Account
import grit.slack.event.{TeamId, UserId}

/** How grit names Slack's people, for the edge and for a deployment's groups and declarations
  * alike.
  */
object SlackAccounts {

  /** `user` of `team`: `slack:{team}/{user}`; why not, when either id is blank or holds `/` or
    * whitespace, as no id Slack sends does.
    */
  def account(team: TeamId, user: UserId): Either[String, Account] = {
    def id(what: String, text: String): Either[String, String] =
      Either.cond(
        text.nonEmpty && !text.exists(c => c == '/' || c.isWhitespace),
        text,
        s"a Slack $what id is not blank and holds no / or whitespace: $text"
      )
    for {
      t <- id("team", TeamId.value(team))
      u <- id("user", UserId.value(user))
      a <- Account.of("slack", s"$t/$u")
    } yield a
  }
}
