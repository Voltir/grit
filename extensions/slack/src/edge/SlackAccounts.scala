package grit.slack.edge

import grit.core.id.AttesterName
import grit.core.identity.{Account, Realm}
import grit.slack.event.{TeamId, UserId}

/** How grit names Slack's people, for the edge and for a deployment's groups and vouchings
  * alike.
  */
object SlackAccounts {

  /** `user` of `team`: `slack:{team}/{user}`; why not, when either id is blank or holds `/` or
    * whitespace, as no id Slack sends does.
    */
  def account(team: TeamId, user: UserId): Either[String, Account.Sourced] =
    for {
      t <- id("team", TeamId.value(team))
      u <- id("user", UserId.value(user))
      a <- Account.of("slack", s"$t/$u")
    } yield a

  /** Every account of `team`, `slack:{team}/…`: what a deployment trusts [[Attester]] for, and
    * what a group naming the team's full members names; why not, as [[account]] refuses a
    * team's id.
    */
  def realm(team: TeamId): Either[String, Realm] =
    id("team", TeamId.value(team)).flatMap(Realm.of("slack", _))

  /** The user `account` names in `team`, when it is `slack:{team}/{user}`; `None` for an
    * account of another team or source.
    */
  def user(account: Account, team: TeamId): Option[UserId] =
    Some(Account.written(account))
      .filter(_.startsWith(s"slack:${TeamId.value(team)}/"))
      .map(_.drop(s"slack:${TeamId.value(team)}/".length))
      .filter(u => u.nonEmpty && !u.contains('/'))
      .map(UserId(_))

  /** `slack`, the attester the Slack edge is: what a deployment's vouching for a team's realm
    * names ([[grit.core.identity.Vouching]]).
    */
  val Attester: AttesterName = AttesterName("slack")

  /** `text` as a Slack id of `what`, or why not. */
  private def id(what: String, text: String): Either[String, String] =
    Either.cond(
      text.nonEmpty && !text.exists(c => c == '/' || c.isWhitespace),
      text,
      s"a Slack $what id is not blank and holds no / or whitespace: $text"
    )
}
