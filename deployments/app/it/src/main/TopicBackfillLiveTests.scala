package grit.app.main

import java.time.{Instant, LocalDate}

import scala.concurrent.duration.*

import grit.core.job.Ending
import grit.core.store.Tx
import grit.dbos.engine.LiveEngine
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}
import grit.kit.deployment.{Deployment, Offered}
import grit.kit.run.{Kit, KitFailure, Launch}
import grit.knowledge.plugin.TopicDocuments

import utest.*

/** `grit backfill topics`'s runs, [[Kit.runOnce]] over [[TopicDocuments.backfill]], against a
  * real Postgres and the stub model: refused on a database the deployment was never served on,
  * and otherwise run one day at a time, in order.
  */
object TopicBackfillLiveTests extends TestSuite {

  private val Env = Map("GRIT_PLUGINS" -> "topics", "GRIT_TOPIC_LABELS" -> "public")

  /** `Env` with `config`'s database. */
  private def on(config: DbConfig): Map[String, String] =
    Env ++ Map(
      DbConfig.UrlVar -> config.jdbcUrl,
      DbConfig.UserVar -> config.user,
      DbConfig.PasswordVar -> config.password
    )

  private val deployment: Deployment =
    Deploying
      .deployment(Env, Offered.Read, Vector.empty, Vector.empty, java.time.ZoneOffset.UTC, None)
      .fold(sys.error, identity)

  private val topics: TopicDocuments =
    deployment.plugins.collectFirst { case t: TopicDocuments => t }.getOrElse(sys.error("topics"))

  private val days: Vector[LocalDate] = Vector(LocalDate.of(2026, 10, 1), LocalDate.of(2026, 10, 2))

  /** How many schedules `config`'s database holds whose id names a backfill. */
  private def backfills(config: DbConfig): Int =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      scala.util.Using.resource(
        conn.prepareStatement(
          "SELECT count(*) FROM grit.schedules WHERE id LIKE '%topics-backfill%'"
        )
      ) { ps =>
        scala.util.Using.resource(ps.executeQuery()) { rs =>
          if (rs.next()) rs.getInt(1) else 0
        }
      }
    }

  private val Nothing = "topics: 0 written (0 opened), 0 withdrawn, 0 unchanged, 0 set aside, " +
    "0 left for the next run"

  val tests = Tests {
    test("a database the deployment was never served on is refused, and nothing is declared") {
      val config = TestPostgres.freshDatabase("app_topic_backfill_unserved")
      LiveEngine.open(config, "test").close()
      val runs = topics.backfill(days, 3, Instant.EPOCH).fold(sys.error, identity)
      (
        Kit.runOnce(deployment, topics.name, runs, "", 1.minute, on(config), _ => true, _ => ()),
        backfills(config)
      ) ==> (
        Left(
          KitFailure.Unrunnable(
            s"the plugin topics has never been served on ${config.endpoint}: run grit serve on it first"
          )
        ),
        0
      )
    }

    test(
      "each day runs in turn and ends ran, a day again writing nothing; declined, nothing runs"
    ) {
      val config = TestPostgres.freshDatabase("app_topic_backfill_served")
      // Served once: the deployment's own schedules stored, as a start of grit serve leaves them.
      val engine = LiveEngine.open(config, "test")
      try {
        val _ = Launch.reconcile(engine.schedules, engine.jot, deployment, Instant.now())
      } finally engine.close()
      val declined =
        Kit.runOnce(
          deployment,
          topics.name,
          topics.backfill(days, 3, Instant.EPOCH).fold(sys.error, identity),
          "",
          1.minute,
          on(config),
          _ => false,
          _ => ()
        )
      val declinedBackfills = backfills(config)
      val runs = topics.backfill(days, 3, Instant.ofEpochSecond(1)).fold(sys.error, identity) ++
        topics.backfill(days.take(1), 3, Instant.ofEpochSecond(2)).fold(sys.error, identity)
      val ran = Kit
        .runOnce(deployment, topics.name, runs, "", 1.minute, on(config), _ => true, _ => ())
        .map(_.map(r => (grit.core.id.ScheduleKey.value(r.key), r.ended, r.reply, r.spent)))
      (declined, declinedBackfills, ran) ==> (
        Right(Vector.empty),
        0,
        Right(
          Vector(
            ("topics-backfill-public-2026-10-01-1", Some(Ending.Ran), Some(Nothing), BigDecimal(0)),
            ("topics-backfill-public-2026-10-02-1", Some(Ending.Ran), Some(Nothing), BigDecimal(0)),
            ("topics-backfill-public-2026-10-01-2", Some(Ending.Ran), Some(Nothing), BigDecimal(0))
          )
        )
      )
    }
  }
}
