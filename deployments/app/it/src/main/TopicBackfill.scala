package grit.app.main

import java.time.{Instant, LocalDate, LocalTime, ZoneOffset}

import scala.concurrent.duration.*

import grit.core.id.PluginName
import grit.core.message.Cost
import grit.core.spend.DailyCap
import grit.core.visibility.{Label, Subject, Visibility}
import grit.dbos.engine.LiveEngine
import grit.dbos.sql.DbConfig
import grit.kit.deployment.Deployment
import grit.kit.run.Kit
import grit.knowledge.plugin.TopicDocuments

/** The topics job run over a copy of a captured database, once for each day named and then once
  * more, to see each day's second run change nothing: run by hand, through `scripts/eval
  * backfill`, never by the suites.
  *
  * Args: the day's cap in USD, the most topics a run writes, then each day (`yyyy-mm-dd`, UTC),
  * oldest first. The database is `GRIT_DATABASE_*`'s, a copy no one else runs on, in which no
  * workflow is left pending or enqueued (else the engine would run it); OpenRouter's key and the
  * models' variables come from the environment over `GRIT_ENV_FILE` (`.env` when unset). The
  * engine runs the topics plugin `topics` at public, its cards read from the plugin `knowledge`'s
  * documents, and keeps each other plugin the database declares documents for declared, so its
  * documents stay searched. The runs are [[grit.kit.run.Kit.runOnce]]'s, one at a time: for
  * each day `topics-dream` for that day, then again, each a once schedule
  * of [[TopicDocuments.backfill]]'s; it prints, by shape alone, each run's line (how it ended, its
  * reply line, its spend), for each day whether its second run wrote nothing and spent nothing
  * (`unchanged`), then the runs' spend in all.
  */
object TopicBackfill {

  def main(args: Array[String]): Unit =
    run(args.toVector).left.foreach { why =>
      System.err.println(s"topic-backfill: $why")
      sys.exit(1)
    }

  import RoomRun.right

  private val Topics = PluginName.of("topics").fold(sys.error, identity)

  private val CardsName = PluginName.of("knowledge").fold(sys.error, identity)

  /** The longest one run is waited for. */
  private val Longest: FiniteDuration = 10.minutes

  private def run(args: Vector[String]): Either[String, Unit] =
    for {
      cap <- args.headOption.toRight("no cap").flatMap(DailyCap.of)
      perRun <- args
        .lift(1)
        .toRight("no number of topics")
        .flatMap(n => n.toIntOption.filter(_ > 0).toRight(s"no number of topics: $n"))
      days <- args
        .drop(2)
        .foldLeft[Either[String, Vector[LocalDate]]](Right(Vector.empty))((acc, d) =>
          acc.flatMap(done =>
            scala.util.Try(LocalDate.parse(d)).toOption.map(done :+ _).toRight(s"no day: $d")
          )
        )
      _ <- Either.cond(days.nonEmpty, (), "no day")
      env <- RoomRun.env
      config <- DbConfig.fromEnv(env).left.map(_.message)
      cards = new TopicRuns.Cards(CardsName, TopicRuns.CardTerms)
      topics <- TopicDocuments.of(
        Topics,
        Vector(Label.Public),
        LocalTime.of(5, 0),
        ZoneOffset.UTC,
        Visibility.Shipped,
        Some(cards),
        None
      )
      first <- RoomRun.deployment(env, Vector(topics, cards), cap)
      keeping = kept(config, first._1)
      deployment <- RoomRun.deployment(env, Vector(topics, cards) ++ keeping, cap)
      // Each day twice, the second to see it change nothing; each run made due as it is reached.
      // Two starts a second apart key each day's two runs apart.
      started = Instant.now()
      firsts <- topics.backfill(days, perRun, started)
      seconds <- topics.backfill(days, perRun, started.plusSeconds(1))
      runs = firsts.zip(seconds).flatMap((a, b) => Vector(a, b))
      ran <- Kit
        .runOnce(deployment._1, Topics, runs, "", Longest, env, _ => true, println)
        .left
        .map(_.message)
    } yield {
      days.zipWithIndex.foreach { (day, i) =>
        val unchanged = ran
          .lift(2 * i + 1)
          .exists(r => r.reply.exists(_.startsWith("topics: 0 written")) && r.spent.signum == 0)
        println(s"day $day: second run ${if (unchanged) "unchanged" else "NOT unchanged"}")
      }
      println(s"spent ${Cost.cents(ran.map(_.spent).sum)}")
    }

  /** Every other plugin whose documents the copy declares, kept declared. */
  private def kept(config: DbConfig, d: Deployment): Vector[TopicRuns.Keeping] = {
    val engine = LiveEngine.open(config, d.runsUnder, d.budget, d.visibility)
    try
      right(engine.db.read(Subject.Public)(engine.documents.declared())).collect {
        case (name, terms) if name != Topics && name != CardsName =>
          new TopicRuns.Keeping(name, terms)
      }
    finally engine.close()
  }
}
