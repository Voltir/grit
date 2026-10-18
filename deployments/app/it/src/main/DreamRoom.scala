package grit.app.main

import java.time.{Duration, Instant, LocalTime, ZoneOffset}

import grit.core.document.DocText
import grit.core.id.PluginName
import grit.core.spend.DailyCap
import grit.core.stitch.StrandShelf
import grit.core.visibility.{Label, Subject, Visibility}
import grit.dbos.engine.LiveEngine
import grit.dbos.sql.DbConfig
import grit.kit.run.Launch
import grit.knowledge.plugin.StrandDocuments

/** A real dream run over a room of the eval's shape (`eval/resources/rooms`), for a person to
  * read what the strands plugin writes: run by hand, never by the suites.
  *
  * Args: the room's JSON file, then the day's cap in USD. The database is `GRIT_DATABASE_*`'s,
  * which must be empty and is left as the run leaves it; OpenRouter's key and the models'
  * variables come from the environment over `GRIT_ENV_FILE` (`.env` when unset). The room's
  * messages are written as heard, shifted so its last was said an hour ago, each top-level one
  * a Slack thread whose replies are the messages naming it as their `thread`; each top-level
  * message with a topic follows the room's first earlier one with that topic, as the room's
  * truth places it. Then the strands plugin at public, its one run a minute ahead, is launched
  * and waited on; prints each strand at public the run could read (counts only), each document
  * it wrote, and the day's spend.
  */
object DreamRoom {

  def main(args: Array[String]): Unit =
    run(args.toVector).left.foreach { why =>
      System.err.println(s"dream-room: $why")
      sys.exit(1)
    }

  import RoomRun.right

  private def run(args: Vector[String]): Either[String, Unit] =
    for {
      file <- args.headOption.toRight("no room file")
      cap <- args.lift(1).toRight("no cap").flatMap(DailyCap.of)
      env <- RoomRun.env
      config <- DbConfig.fromEnv(env).left.map(_.toString)
      room <- RoomRun.room(file)
      last <- room.messages.map(_.at).maxOption.toRight("an empty room")
      now = Instant.now()
      shift = Duration.between(last, now.minusSeconds(3600))
      due = now.plusSeconds(60)
      name <- PluginName.of("strands")
      strands <- StrandDocuments.of(
        name,
        Vector(Label.Public),
        LocalTime.ofInstant(due, ZoneOffset.UTC),
        ZoneOffset.UTC,
        Visibility.Shipped
      )
      (deployment, secrets) <- RoomRun.deployment(env, Vector(strands), cap)
    } yield {
      val engine = LiveEngine.open(
        config,
        deployment.runsUnder,
        deployment.budget,
        deployment.visibility
      )
      try {
        val _ = RoomRun.seeded(engine, room, shift)
        Launch(engine, deployment, secrets, Launch.Run.Served, sweeping = true, println)
        val until = due.plusSeconds(240)
        def done = Instant.now().isAfter(due.plusSeconds(45)) && engine.unfinished().contains(0)
        while (!done && Instant.now().isBefore(until)) Thread.sleep(2000)
        val seen = right(
          engine.db.read(Subject.Public)(
            StrandShelf
              .over(engine.stitches, engine.conversations)
              .atFloor(due.minus(Duration.ofDays(2)), due)
          )
        )
        seen.foreach(s =>
          println(
            s"strand ${s.root}: ${s.members.size} threads, ${s.said} messages, ${s.chars} chars"
          )
        )
        val documents = right(
          engine.db.read(Subject.Public)(
            strands.documents
              .map(d => engine.keeper(name, d.terms).newest(Int.MaxValue))
              .getOrElse(Right(Vector.empty))
          )
        )
        documents.foreach(d => println(s"\n---- ${d.key}\n${DocText.value(d.text)}"))
        println(s"\n${documents.size} documents; ${RoomRun.spent(engine)}")
      } finally engine.close()
    }
}
