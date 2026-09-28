package grit.app.serve

import grit.core.host.ProcessIdentity
import grit.dbos.sql.DbConfig

import utest.*

/** What `grit serve` refuses before it touches the database or Slack. */
object ServeTests extends TestSuite {

  // Never reached: every case here is refused before the database is opened.
  private val nowhere = DbConfig("jdbc:postgresql://127.0.0.1:1/none", "none", "none")

  private def run(env: Map[String, String]): Option[String] =
    Serve.run(
      env,
      nowhere,
      "test",
      ProcessIdentity("m", 1),
      grit.core.spend.Budget(java.time.ZoneOffset.UTC, None),
      _ => ()
    )

  val tests = Tests {
    test(
      "without its tokens, or with the wrong kind, it refuses to start, naming the variable and never the token"
    ) {
      run(Map.empty) ==> Some("SLACK_BOT_TOKEN is not set")
      run(Map("SLACK_BOT_TOKEN" -> "xoxb-1")) ==> Some("SLACK_APP_TOKEN is not set")
      run(Map("SLACK_BOT_TOKEN" -> "xoxp-secret", "SLACK_APP_TOKEN" -> "xapp-1")) ==>
        Some("SLACK_BOT_TOKEN: a bot token starts xoxb-")
      run(Map("SLACK_BOT_TOKEN" -> "xoxb-1", "SLACK_APP_TOKEN" -> "xoxb-secret")) ==>
        Some("SLACK_APP_TOKEN: an app-level token starts xapp-")
    }
  }
}
