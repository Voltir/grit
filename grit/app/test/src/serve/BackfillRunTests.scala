package grit.app.serve

import grit.core.host.ProcessIdentity
import grit.dbos.sql.DbConfig

import utest.*

/** What `grit backfill` refuses before it touches the database or Slack. */
object BackfillRunTests extends TestSuite {

  // Never reached: every case here is refused before the database is opened.
  private val nowhere = DbConfig("jdbc:postgresql://127.0.0.1:1/none", "none", "none")

  private def run(env: Map[String, String]): Option[String] =
    Backfill.run(
      env,
      nowhere,
      "test",
      ProcessIdentity("m", 1),
      grit.core.spend.Budget(java.time.ZoneOffset.UTC, None),
      _ => (),
      _ => true,
      _ => ()
    )

  private val tokens = Map("SLACK_BOT_TOKEN" -> "xoxb-1", "SLACK_APP_TOKEN" -> "xapp-1")

  val tests = Tests {
    test(
      "with no channel listened in, or its days malformed, it refuses, naming the variable, before opening anything"
    ) {
      (
        run(tokens),
        run(tokens + ("GRIT_SLACK_LISTEN" -> "C123ABC456") + ("GRIT_BACKFILL_DAYS" -> "two"))
      ) ==> (
        Some("GRIT_SLACK_LISTEN names no channel: there is nothing to backfill"),
        Some("GRIT_BACKFILL_DAYS is a whole number of days above zero, not 'two'")
      )
    }
  }
}
