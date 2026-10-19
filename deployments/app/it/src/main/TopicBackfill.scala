package grit.app.main

import java.time.{Duration, Instant, LocalDate, LocalTime, ZoneOffset}

import grit.core.id.{PluginName, ScheduleKey}
import grit.core.spend.DailyCap
import grit.core.visibility.{Label, Subject, Visibility}
import grit.dbos.engine.{Engine, LiveEngine}
import grit.dbos.sql.DbConfig
import grit.job.clock.ClockEdge
import grit.kit.deployment.Deployment
import grit.kit.environment.Secrets
import grit.kit.run.Launch
import grit.knowledge.plugin.TopicDocuments
import grit.knowledge.topic.{TopicDream, TopicParams}

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
  * documents stay searched; it makes no sweep, so it closes, posts and collects nothing, and each
  * run is started by a clock edge's pass made here. For each day it runs `topics-dream` for that
  * day, then again, each a once schedule `topics-backfill-{day}-{n}` due a few seconds on, and
  * prints, by shape alone: how each ended and its reply line, and for the second whether it wrote
  * nothing and spent nothing (`unchanged`); then the day's spend.
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
  private val Longest: Duration = Duration.ofMinutes(10)

  private def key(day: LocalDate, n: Int): ScheduleKey =
    ScheduleKey.of(s"topics-backfill-$day-$n").fold(sys.error, identity)

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
      config <- DbConfig.fromEnv(env).left.map(_.toString)
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
      dream <- topics.jobs.collectFirst { case d: TopicDream => d }.toRight("no topics job")
      first <- RoomRun.deployment(env, Vector(topics, cards), cap)
    } yield {
      val engine = LiveEngine.open(
        config,
        first._1.runsUnder,
        first._1.budget,
        first._1.visibility
      )
      try {
        // Every other plugin whose documents the copy declares, kept declared.
        val keeping = right(engine.db.read(Subject.Public)(engine.documents.declared()))
          .collect {
            case (name, terms) if name != Topics && name != CardsName =>
              new TopicRuns.Keeping(name, terms)
          }
        def deployment(day: LocalDate, n: Int, at: Instant): Either[String, (Deployment, Secrets)] =
          RoomRun.deployment(
            env,
            Vector(
              new TopicRuns.DayRun(
                topics,
                dream,
                key(day, n),
                at,
                TopicParams(perRun, TopicDocuments.Recent, TopicDocuments.Recent, Some(day), None),
                Label.Public
              ),
              cards
            ) ++ keeping,
            cap
          )
        // Launched once, with the first day's schedule; each run after declares its own.
        days.headOption.foreach { day =>
          deployment(day, 0, Instant.now().plusSeconds(5)) match {
            case Left(why) => sys.error(why)
            case Right((d, secrets)) =>
              val _ = Launch(engine, d, secrets, Launch.Run.Served, sweeping = false, println)
          }
        }
        days.foreach { day =>
          val lines = (0 to 1).map { n =>
            val at = Instant.now().plusSeconds(5)
            deployment(day, n, at) match {
              case Left(why) => (s"day $day run ${n + 1}: not run: $why", false)
              case Right((d, _)) =>
                right(Launch.reconcile(engine.schedules, engine.jot, d, Instant.now()))
                val edge =
                  new ClockEdge(engine.inbox, engine.schedules, engine.db, engine.clock, d.allJobs)
                val before = RoomRun.spent(engine)
                val ended = TopicRuns.waited(
                  engine,
                  Topics,
                  key(day, n),
                  Longest,
                  () => {
                    val _ = edge.tick()
                  }
                )
                val reply = TopicRuns.replied(engine, Topics, key(day, n), at)
                val unchanged =
                  reply.startsWith("topics: 0 written") && RoomRun.spent(engine) == before
                (s"day $day run ${n + 1}: $ended; $reply", unchanged)
            }
          }
          lines.foreach((line, _) => println(line))
          println(
            s"day $day: second run ${
                if (lines.lift(1).exists(_._2)) "unchanged" else "NOT unchanged"
              }"
          )
        }
        println(s"spent ${RoomRun.spent(engine)}")
      } finally engine.close()
    }
}
