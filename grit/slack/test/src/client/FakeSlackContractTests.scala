package grit.slack.client

/** The Slack contract, kept by the fake. */
object FakeSlackContractTests extends SlackContract {
  import SlackContract.*

  protected def withSlack[A](limited: Int, down: Boolean)(body: Slack => A): A = {
    val slack = new FakeSlack
    slack.me = Self(Team, Bot)
    slack.names = Map(Ana -> Some("Ana"), Bot -> Some("grit"))
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
