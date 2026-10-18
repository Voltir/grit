package grit.app.main

import java.nio.file.Path
import java.time.{Duration, Instant, LocalTime, ZoneOffset}

import scala.concurrent.duration.*

import grit.core.document.DocText
import grit.core.id.{ConversationId, EntryId, PluginName}
import grit.core.identity.Account
import grit.core.message.{Tokens, Usage}
import grit.core.period.{LifecycleSettings, Probability}
import grit.core.persona.Persona
import grit.core.speech.Speaking
import grit.core.spend.{Budget, DailyCap, Day}
import grit.core.stitch.{StitchJson, StrandShelf, Tuning}
import grit.core.store.{Entry, Origin, Payload, PayloadJson, StoreError}
import grit.core.visibility.{Integrity, Label, Subject, Visibility}
import grit.dbos.engine.LiveEngine
import grit.dbos.sql.DbConfig
import grit.kit.deployment.{Assembly, Deployment, Offer, Offered, Placer}
import grit.kit.environment.{DotEnv, Secrets}
import grit.kit.run.Launch
import grit.knowledge.plugin.StrandDocuments
import grit.models.{OpenRouterConfig, Seed}
import grit.turn.TurnLoop

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

  private def right[A](e: Either[StoreError, A]): A = e.fold(x => sys.error(x.toString), identity)

  private final case class Said(
      id: String,
      at: Instant,
      topic: Option[String],
      thread: Option[String],
      text: String
  )

  private def run(args: Vector[String]): Either[String, Unit] =
    for {
      file <- args.headOption.toRight("no room file")
      cap <- args.lift(1).toRight("no cap").flatMap(DailyCap.of)
      env <- DotEnv.load(Path.of(sys.env.getOrElse("GRIT_ENV_FILE", ".env")), sys.env)
      config <- DbConfig.fromEnv(env).left.map(_.toString)
      room = ujson.read(os.read(os.Path(Path.of(file).toAbsolutePath)))
      said = room("messages").arr.toVector.map { m =>
        Said(
          m("id").str,
          Instant.parse(m("at").str),
          m.obj.get("topic").flatMap(_.strOpt),
          m.obj.get("thread").flatMap(_.strOpt),
          m("text").str
        )
      }
      last <- said.map(_.at).maxOption.toRight("an empty room")
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
      seed <- Seed.catalog
      policy <-
        if (!env.contains(OpenRouterConfig.KeyVar)) Left(s"${OpenRouterConfig.KeyVar} is unset")
        else OpenRouterConfig.policy(env, seed.policy).left.map(_.message)
      deployment <- Deployment
        .of(
          edges = Vector.empty,
          worksIn = Vector.empty,
          plugins = Vector(strands),
          policy = policy,
          offer = Offer(Offered.Read, TurnLoop.Rounds.of(2).getOrElse(sys.error("rounds"))),
          assembly = Assembly.Linear(Tokens(4000)),
          placer = Placer.Off("the room's truth places its openings"),
          lifecycle = LifecycleSettings.Default,
          budget = Budget(ZoneOffset.UTC, Some(cap)),
          speaking = Speaking.Off,
          sweep = 30.seconds,
          persona = Persona.Grit
        )
        .left
        .map(_.message)
      secrets <- Secrets.of(env, deployment).left.map(_.message)
    } yield {
      val engine = LiveEngine.open(
        config,
        deployment.runsUnder,
        deployment.budget,
        deployment.visibility
      )
      try {
        seeded(engine, room("team").str, room("channel").str, said, shift)
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
        val spent = right(
          engine.db.read(Subject.Public)(
            engine.spending.on(Day.at(Instant.now(), ZoneOffset.UTC))
          )
        )
        println(s"\n${documents.size} documents; ${spent.calls} calls, ${spent.cost}")
      } finally engine.close()
    }

  /** `said` written into `engine`'s room `team`/`channel`, each `shift` later. */
  private def seeded(
      engine: grit.dbos.engine.Engine^,
      team: String,
      channel: String,
      said: Vector[Said],
      shift: Duration
  ): Unit = {
    val ts = said.map(s => s.id -> s"${s.at.plus(shift).getEpochSecond}.000000").toMap
    val roots = said.filter(_.thread.isEmpty)
    val written =
      said.sortBy(_.at).foldLeft(Map.empty[String, (ConversationId, EntryId)]) { (done, s) =>
        val thread = s.thread.getOrElse(s.id)
        val at = s.at.plus(shift)
        val entry = right(engine.jot.write(Subject.Public) {
          for {
            c <- engine.conversations.findOrCreate(
              Origin.Slack(team, channel, ts.getOrElse(thread, thread)),
              Account.Local,
              Label.Public
            )
            next <- engine.entries.lockNext(c.id)
            e = Entry(
              EntryId(s"${ConversationId.value(c.id)}:${next.seq}"),
              c.id,
              next.turnSeq,
              None,
              next.seq,
              Payload.Heard(s.text),
              at,
              Integrity.Member
            )
            _ <- engine.entries.insert(e)
          } yield (c.id, e.id)
        })
        done + (s.id -> entry)
      }
    roots.foreach { s =>
      val root = s.topic.flatMap(t => roots.find(r => r.topic.contains(t) && r.at.isBefore(s.at)))
      for {
        r <- root
        (rootConversation, _) <- written.get(r.id)
        (_, first) <- written.get(s.id)
      } {
        // Only the stitch makes a placement; the room's truth is one in its stored form.
        val t = Tuning.Default
        val placed = StitchJson
          .read(
            ujson.Obj(
              "kind" -> "follows",
              "root" -> ConversationId.value(rootConversation),
              "p" -> 0.9,
              "model" -> "truth",
              "usage" -> PayloadJson.writeUsage(Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None)),
              "seen" -> ujson.Obj(
                "state" -> ujson.Obj(),
                "offered" -> ujson.Arr(),
                "tuning" -> ujson.Obj(
                  "horizon_seconds" -> t.horizon.toSeconds.toDouble,
                  "recent" -> t.recent,
                  "lexical" -> t.lexical,
                  "follows_at" -> Probability.value(t.followsAt),
                  "window_tokens" -> Tokens.value(t.windowTokens).toDouble,
                  "strand_chars" -> t.strandChars
                )
              )
            )
          )
          .fold(sys.error, identity)
        val _ = right(
          engine.jot.write(Subject.Public)(
            engine.stitches.record(first, placed, s.at.plus(shift))
          )
        )
      }
    }
  }
}
