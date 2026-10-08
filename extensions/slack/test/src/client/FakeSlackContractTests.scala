package grit.slack.client

import grit.core.identity.Standing

/** The Slack contract, kept by the fake. */
object FakeSlackContractTests extends SlackContract {
  import SlackContract.*

  protected def withSlack[A](limited: Int, down: Boolean)(body: Slack => A): A = {
    val slack = new FakeSlack
    slack.me = Self(Team, Bot)
    slack.domain = Domain
    slack.names = Map(Ana -> Some("Ana"), Bot -> Some("grit"), Gia -> Some("Gia"))
    slack.standings = Map(Ana -> AnaStanding, Bot -> Standing.Outside, Gia -> Standing.Outside)
    slack.channelNames =
      Map(Public -> "grit-contract", Private -> "grit-private", Outside -> "grit-outside")
    slack.privateChannels = Set(Private)
    slack.notIn = Set(Outside)
    slack.histories = Map(Public -> Workspace.listed)
    slack.limited = limited
    slack.down = down
    body(slack)
  }
}
