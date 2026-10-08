package grit.slack.client

import grit.core.identity.Standing
import grit.slack.event.ResponseUrl

/** The Slack contract, kept by the fake. */
object FakeSlackContractTests extends SlackContract {
  import SlackContract.*

  private def fake(limited: Int, down: Boolean): FakeSlack = {
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
    slack
  }

  protected def withSlack[A](limited: Int, down: Boolean)(body: Slack => A): A =
    body(fake(limited, down))

  protected def withCommand[A](down: Boolean)(body: (Slack, Hooks^) => A): A = {
    val slack = fake(0, down)
    val asked = ResponseUrl("https://hooks.slack.com/commands/T0000000001/1/live")
    val late = ResponseUrl("https://hooks.slack.com/commands/T0000000001/2/expired")
    slack.expired = Set(late)
    body(
      slack,
      new Hooks {
        def live: ResponseUrl = asked
        def expired: ResponseUrl = late
        // The fake shows an answer only to its asker, in the words it was given.
        def answered(): Vector[Answered] =
          slack.responses.map((url, text) => Answered(url, ephemeral = true, text))
      }
    )
  }
}
