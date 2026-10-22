package grit.app.main

import java.nio.file.Path
import java.time.{Duration, Instant, ZoneOffset}

import scala.concurrent.duration.*

import grit.core.files.PagedText
import grit.core.id.{ConversationId, EntryId}
import grit.core.identity.Account
import grit.core.message.{Tokens, Usage}
import grit.core.period.{LifecycleSettings, Probability}
import grit.core.persona.Persona
import grit.core.plugin.Plugin
import grit.core.speech.Speaking
import grit.core.spend.{Budget, DailyCap, Day}
import grit.core.stitch.{StitchJson, Tuning}
import grit.core.store.{Entry, Origin, Payload, PayloadJson, StoreError}
import grit.core.visibility.{Integrity, Label, Subject}
import grit.dbos.engine.Engine
import grit.eval.Room
import grit.kit.deployment.{Assembly, Deployment, Offer, Offered, Placer}
import grit.kit.environment.{DotEnv, Secrets}
import grit.models.{OpenRouterConfig, Seed}
import grit.turn.TurnLoop

/** What a run by hand over a room of the eval's shape (`eval/resources/rooms`) shares
  * ([[DreamRoom]], [[TopicRoom]]): the room read, the environment, a deployment of its plugins,
  * the room written into an engine as heard, and the day's spend.
  */
private[main] object RoomRun {

  def right[A](e: Either[StoreError, A]): A = e.fold(x => sys.error(x.toString), identity)

  /** The room in the file at `file` ([[Room.read]]), named for the file less `.json`. */
  def room(file: String): Either[String, Room] = {
    val path = Path.of(file).toAbsolutePath
    Room.read(path.getFileName.toString.stripSuffix(".json"), os.read(os.Path(path)))
  }

  /** The environment over `GRIT_ENV_FILE` (`.env` when unset): the environment wins. */
  def env: Either[String, Map[String, String]] =
    DotEnv.load(Path.of(sys.env.getOrElse("GRIT_ENV_FILE", ".env")), sys.env)

  /** A deployment of `plugins` alone, no edge, its day capped at `cap`, its models
    * OpenRouter's under `env`; and its secrets.
    */
  def deployment(
      env: Map[String, String],
      plugins: Vector[Plugin],
      cap: DailyCap
  ): Either[String, (Deployment, Secrets)] =
    for {
      seed <- Seed.catalog
      policy <-
        if (!env.contains(OpenRouterConfig.KeyVar)) Left(s"${OpenRouterConfig.KeyVar} is unset")
        else OpenRouterConfig.policy(env, seed.policy).left.map(_.message)
      deployment <- Deployment
        .of(
          edges = Vector.empty,
          worksIn = Vector.empty,
          plugins = plugins,
          policy = policy,
          offer = Offer(Offered.Read, TurnLoop.Rounds.of(2).getOrElse(sys.error("rounds"))),
          assembly = Assembly.Linear(Tokens(4000)),
          placer = Placer.Off("the room's truth places its openings"),
          lifecycle = LifecycleSettings.Default,
          budget = Budget(ZoneOffset.UTC, Some(cap)),
          speaking = Speaking.Off,
          sweep = 30.seconds,
          persona = Persona.Grit,
          formats = PagedText.formats
        )
        .left
        .map(_.message)
      secrets <- Secrets.of(env, deployment).left.map(_.message)
    } yield (deployment, secrets)

  /** `room`'s messages written into `engine` as heard in its team's channel, each `shift`
    * later, each opening a Slack thread whose replies are the messages naming it, each opening
    * the truth has follow another placed so ([[Room.truth]]): each opening's conversation, by
    * its id.
    */
  def seeded(engine: Engine^, room: Room, shift: Duration): Map[String, ConversationId] = {
    val ts =
      room.messages.map(m => m.id -> s"${m.at.plus(shift).getEpochSecond}.000000").toMap
    val written =
      room.messages.foldLeft(Map.empty[String, (ConversationId, EntryId)]) { (done, m) =>
        val thread = m.thread.getOrElse(m.id)
        val at = m.at.plus(shift)
        val entry = right(engine.jot.write(Subject.Public) {
          for {
            c <- engine.conversations.findOrCreate(
              Origin.Slack(room.team, room.channel, ts.getOrElse(thread, thread)),
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
              Payload.Heard(m.text),
              at,
              Integrity.Member
            )
            _ <- engine.entries.insert(e)
          } yield (c.id, e.id)
        })
        done + (m.id -> entry)
      }
    Room.truth(room).foreach { t =>
      for {
        r <- t.follows
        (rootConversation, _) <- written.get(r)
        (_, first) <- written.get(t.opening.id)
      } {
        val _ = right(
          engine.jot.write(Subject.Public)(
            engine.stitches.record(first, follows(rootConversation), t.opening.at.plus(shift))
          )
        )
      }
    }
    room.openings.flatMap(o => written.get(o.id).map(w => o.id -> w._1)).toMap
  }

  /** The stitch's placement following `root`, by the model `truth`: only the stitch makes a
    * placement, and the room's truth is one in its stored form.
    */
  private def follows(root: ConversationId) = {
    val t = Tuning.Default
    StitchJson
      .read(
        ujson.Obj(
          "kind" -> "follows",
          "root" -> ConversationId.value(root),
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
  }

  /** The day's spend so far, in UTC: `{n} calls, {cost}`. */
  def spent(engine: Engine^): String = {
    val s = right(
      engine.db.read(Subject.Public)(engine.spending.on(Day.at(Instant.now(), ZoneOffset.UTC)))
    )
    s"${s.calls} calls, ${s.cost}"
  }
}
