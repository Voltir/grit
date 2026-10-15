package grit.app.main

import java.time.{Instant, LocalTime, ZoneId}

import grit.core.act.{Called, MoveError, ScriptedKeeping}
import grit.core.flow.TestTainted
import grit.core.id.{PluginName, ScheduleKey}
import grit.core.job.{Declared, JobRun, KeepingJob, PlainJob, SlotRule}
import grit.core.place.Service
import grit.core.plugin.Plugin
import grit.core.visibility.{
  Cleared,
  Compartments,
  Label,
  Labelled,
  Level,
  Party,
  RoomLabels,
  Visibility
}

import utest.*

/** Which plugins `GRIT_PLUGINS` turns on. */
object PluginChoiceTests extends TestSuite {

  private val zone = ZoneId.of("Europe/Berlin")

  private def choice(
      env: Map[String, String],
      visibility: Visibility = Visibility.Shipped
  ): Either[String, Vector[Plugin]] =
    Main.pluginChoice(env, visibility, zone)

  private def names(env: Map[String, String]) =
    choice(env).map(_.map(p => (p.getClass.getSimpleName, PluginName.value(p.name))))

  /** Every variable the knowledge plugin reads, set. */
  private val github = Map(
    "GITHUB_MCP_TOKEN" -> "token",
    "GRIT_GITHUB_OWNER" -> "octocat",
    "GRIT_GITHUB_REPO" -> "Hello-World",
    "GRIT_GITHUB_REF" -> "main"
  )

  /** Each schedule of `env`'s plugins: its key, rule, parameters and clearance. */
  private def scheduled(env: Map[String, String]) =
    choice(env).map(
      _.flatMap(_.schedules).map((d: Declared[?]) =>
        (ScheduleKey.value(d.key), d.rule, d.written, d.clearance)
      )
    )

  /** What each keeping schedule of `env`'s plugins replies when run once, its first call
    * failing with its arguments as the call's error.
    */
  private def firstCalls(env: Map[String, String]): Either[String, Vector[String]] =
    choice(env).map(_.flatMap(p => p.schedules.map(runOnce(p, _))))

  private def runOnce[P <: caps.Pure](plugin: Plugin, d: Declared[P]): String =
    d.job match {
      case k: KeepingJob[P] =>
        plugin.documents.fold("a keeping job of a plugin keeping no documents") { documents =>
          k.run(
            TestTainted.trusted(JobRun(d.params, Instant.EPOCH, Instant.EPOCH)),
            new ScriptedKeeping(
              k.limits,
              d.clearance,
              plugin.name,
              documents.terms,
              _ => Left(MoveError.Model("no text ask")),
              (_, _, args) => Right(Called.Failed(ujson.write(args)))
            )
          )
        }
      case _: PlainJob[P] => "not a keeping job"
    }

  val tests = Tests {
    test("GRIT_PLUGINS: unset is none; a list of names turns each on once") {
      names(Map.empty) ==> Right(Vector())
      names(Map("GRIT_PLUGINS" -> " digest, ,digest ")) ==> Right(Vector(("Digest", "digest")))
    }

    test("GRIT_PLUGINS: remind turns on the reminders, beside digest or alone") {
      (names(Map("GRIT_PLUGINS" -> "remind")), names(Map("GRIT_PLUGINS" -> "digest,remind"))) ==> (
        Right(Vector(("Reminders", "remind"))),
        Right(Vector(("Digest", "digest"), ("Reminders", "remind")))
      )
    }

    test("a plugin grit does not have, or a name no plugin could have, is refused") {
      choice(Map("GRIT_PLUGINS" -> "digest,wiki")) ==>
        Left("GRIT_PLUGINS: no plugin wiki; there are digest, remind and knowledge")
      choice(Map("GRIT_PLUGINS" -> "Digest")) ==> Left(
        "GRIT_PLUGINS: a plugin's name is lowercase letters, digits and dashes, starting with a letter"
      )
    }

    test(
      "knowledge, with GitHub's token and repository set, syncs that repository at its ref, from its root, daily at 03:00 at public"
    ) {
      val env = github + ("GRIT_PLUGINS" -> "knowledge")
      (names(env), firstCalls(env), scheduled(env)) ==> (
        Right(Vector(("Knowledge", "knowledge"))),
        Right(
          Vector(
            "github: 0 written, 0 relabelled, 0 withdrawn, 0 unchanged, 0 left for the next run; " +
              "stopped: listing /: the call to github failed: " +
              """{"owner":"octocat","repo":"Hello-World","path":"/","ref":"main"}"""
          )
        ),
        Right(
          Vector(
            (
              "github-daily",
              SlotRule.Daily(LocalTime.of(3, 0), zone),
              ujson.Obj("source" -> "github", "perRun" -> 30),
              Label.Public
            )
          )
        )
      )
    }

    test("knowledge with GitHub's token or a part of its repository unset is refused, naming it") {
      val refused =
        Vector("GITHUB_MCP_TOKEN", "GRIT_GITHUB_OWNER", "GRIT_GITHUB_REPO", "GRIT_GITHUB_REF")
          .map(v => choice(github - v + ("GRIT_PLUGINS" -> "knowledge")))
      val blank = choice(github + ("GRIT_GITHUB_REPO" -> " ") + ("GRIT_PLUGINS" -> "knowledge"))
      (refused, blank) ==> (
        Vector(
          Left("GRIT_PLUGINS: knowledge reads GitHub, and GITHUB_MCP_TOKEN is unset"),
          Left(
            "GRIT_PLUGINS: knowledge reads the repository GRIT_GITHUB_OWNER names, and it is unset or blank"
          ),
          Left(
            "GRIT_PLUGINS: knowledge reads the repository GRIT_GITHUB_REPO names, and it is unset or blank"
          ),
          Left(
            "GRIT_PLUGINS: knowledge reads the repository GRIT_GITHUB_REF names, and it is unset or blank"
          )
        ),
        Left(
          "GRIT_PLUGINS: knowledge reads the repository GRIT_GITHUB_REPO names, and it is unset or blank"
        )
      )
    }

    test("with knowledge unnamed, no source or schedule exists, whatever else is set") {
      val env = github + ("GRIT_PLUGINS" -> "digest,remind")
      (names(env), scheduled(env)) ==> (
        Right(Vector(("Digest", "digest"), ("Reminders", "remind"))),
        Right(Vector())
      )
    }

    test("knowledge whose source the deployment's labels refuse is refused, saying why") {
      val github = Service.of("github").fold(sys.error, identity)
      val internal = Label.at(Level.Internal)
      val labelled = Visibility
        .of(
          Compartments.Shipped,
          RoomLabels
            .of(Vector.empty, Labelled.Mapped(internal))
            .fold(p => sys.error(p.written), identity),
          Vector.empty,
          Vector.empty,
          Vector(Cleared(Party.Service(github), internal))
        )
        .fold(r => sys.error(r.toString), identity)
      choice(this.github + ("GRIT_PLUGINS" -> "knowledge"), labelled) ==> Left(
        "GRIT_PLUGINS: knowledge: the source github is labelled [level: public], below its service " +
          "github's place, [level: internal]: its runs could not read the service"
      )
    }
  }
}
