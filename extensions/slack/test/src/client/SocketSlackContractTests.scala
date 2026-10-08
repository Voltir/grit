package grit.slack.client

import grit.slack.event.ResponseUrl

/** The Slack contract, kept by [[SocketSlack]] against [[SlackStub]], which answers with
  * Slack's own recorded JSON.
  */
object SocketSlackContractTests extends SlackContract {
  import SlackContract.*

  private val bot =
    BotToken.of("xoxb-contract").fold(e => throw new java.lang.AssertionError(e), identity)
  private val app =
    AppToken.of("xapp-contract").fold(e => throw new java.lang.AssertionError(e), identity)

  protected def withSlack[A](limited: Int, down: Boolean)(body: Slack => A): A = {
    val stub = new SlackStub(limited)
    // Down is a stub no longer listening: its port refuses.
    if (down) stub.close()
    val slack = new SocketSlack(bot, app, stub.api)
    try body(slack)
    finally {
      slack.close()
      stub.close()
    }
  }

  protected def withCommand[A](down: Boolean)(body: (Slack, Hooks^) => A): A = {
    val stub = new SlackStub(0)
    if (down) stub.close()
    val slack = new SocketSlack(bot, app, stub.api)
    try
      body(
        slack,
        new Hooks {
          def live: ResponseUrl = ResponseUrl(stub.commands("live"))
          def expired: ResponseUrl = ResponseUrl(stub.commands("expired"))
          def answered(): Vector[Answered] =
            stub.responses.map((url, body) => shown(ResponseUrl(url), ujson.read(body)))
        }
      )
    finally {
      slack.close()
      stub.close()
    }
  }

  /** `json`, posted at `url`, as its asker reads it in Slack: ephemeral when its
    * `response_type` says so; its text with Slack's escapes undone, and with no markup only
    * when `mrkdwn` is off (else marked as such).
    */
  private def shown(url: ResponseUrl, json: ujson.Value): Answered = {
    def field(name: String): Option[ujson.Value] = json.objOpt.flatMap(_.get(name))
    val text = field("text").flatMap(_.strOpt).getOrElse("")
    val plain = text.replace("&lt;", "<").replace("&gt;", ">").replace("&amp;", "&")
    Answered(
      url,
      field("response_type").flatMap(_.strOpt).contains("ephemeral"),
      if (field("mrkdwn").flatMap(_.boolOpt).contains(false)) plain else s"(as markup) $plain"
    )
  }
}
