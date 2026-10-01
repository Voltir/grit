package grit.slack.client

/** The Slack contract, kept by [[SocketSlack]] against [[SlackStub]], which answers with
  * Slack's own recorded JSON.
  */
object SocketSlackContractTests extends SlackContract {

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
}
