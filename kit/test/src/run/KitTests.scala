package grit.kit.run

import java.time.{Instant, LocalTime, ZoneOffset}

import scala.concurrent.duration.*

import grit.core.edge.{EdgeRefusal, ServedEdge, Variable}
import grit.core.id.{AttesterName, EdgeName, PluginName, ScheduleKey}
import grit.core.identity.{Domain, Identities, Realm, Vouching}
import grit.core.job.{Declared, Grace, Job, SlotRule}
import grit.core.plugin.Plugin
import grit.core.secret.Secret
import grit.core.visibility.{Label, Level}
import grit.kit.deployment.{Deployments, Placer, TestPlugins}
import grit.kit.environment.SecretsRefusal
import grit.models.JevConfig

import utest.*

/** What [[Kit.serve]] refuses before it opens the engine, and what each edge it serves
  * attests for.
  */
object KitTests extends TestSuite {

  private def deployment(placer: Placer) =
    Deployments.accepted(
      edges = Vector(
        Deployments
          .edge("slack", asks = ServedEdge.Asks.Unanswered, needs = Vector("SLACK_BOT_TOKEN"))
      ),
      placer = placer
    )

  private def key(s: String): ScheduleKey = ScheduleKey.of(s).fold(sys.error, identity)

  private val nightlyJob = new TestPlugins.Named("nightly-job")

  /** A plugin with one job and its nightly schedule. */
  private object Nightly extends Plugin {
    val name: PluginName = TestPlugins.name("nightly")
    val version: Int = 1
    override val jobs: Vector[Job[?]] = Vector(nightlyJob)
    override val schedules: Vector[Declared[?]] = Vector(
      Declared(
        key("nightly"),
        nightlyJob,
        SlotRule.Daily(LocalTime.of(5, 0), ZoneOffset.UTC),
        TestPlugins.Named.None
      )
    )
  }

  private def once(
      k: String,
      job: TestPlugins.Named = nightlyJob,
      clearance: Label = Label.Public
  ): Declared[?] =
    Declared(
      key(k),
      job,
      SlotRule.Once(Instant.EPOCH, Grace.Zero),
      TestPlugins.Named.None,
      clearance
    )

  // Never reached when refused before the engine opens: nothing listens on port 1.
  private val nowhere = Map("GRIT_DATABASE_URL" -> "jdbc:postgresql://127.0.0.1:1/none")

  val tests = Tests {
    test(
      "an edge's variable unset refuses the start before the engine opens, naming the edge and the variable"
    ) {
      Kit.serve(deployment(Placer.Stub), nowhere) ==>
        Left(KitFailure.Edge(EdgeName("slack"), EdgeRefusal.Missing(Variable("SLACK_BOT_TOKEN"))))
    }

    test(
      "an edge records for the realms its own attester is trusted for, and one attesting nothing for none; each for every claimed domain"
    ) {
      def ok[A](e: Either[String, A]): A =
        e.fold(why => throw new java.lang.AssertionError(why), identity)
      val (t1, t2) = (ok(Realm.of("slack", "T1")), ok(Realm.of("slack", "T2")))
      val example = ok(Domain.of("example.com"))
      val (chat, directory) = (AttesterName("chat"), AttesterName("directory"))
      val identities =
        Identities
          .of(Vector(Vouching(chat, t1), Vouching(directory, t2)), Set(example))
          .fold(r => throw new java.lang.AssertionError(r.message), identity)
      Vector(Some(chat), Some(directory), None).map(Kit.trustOf(identities, _)) ==> Vector(
        (Set(t1), Set(example)),
        (Set(t2), Set(example)),
        (Set(), Set(example))
      )
    }

    test("Jev as placer without JEV_API_KEY refuse the start before the engine opens") {
      Kit.serve(deployment(Placer.Jev), nowhere + ("SLACK_BOT_TOKEN" -> "xoxb-1")) ==>
        Left(
          KitFailure.Unconfigured(
            SecretsRefusal.Key(Secret.Refusal.Unset(JevConfig.KeyVar))
          )
        )
    }

    test(
      "runOnce refuses a plugin not the deployment's, a job not its, its own schedule's key, and a clearance none of its schedules hold, before the engine opens"
    ) {
      val d = Deployments.of(plugins = Vector(Nightly)).fold(r => sys.error(r.toString), identity)
      def tried(plugin: String, runs: Declared[?]*) =
        Kit.runOnce(
          d,
          TestPlugins.name(plugin),
          runs.toVector,
          "",
          1.second,
          nowhere,
          _ => true,
          _ => ()
        )
      Vector(
        tried("absent", once("a")),
        tried("nightly", once("a"), once("b", new TestPlugins.Named("other-job"))),
        tried("nightly", once("nightly")),
        tried("nightly", once("c", clearance = Label.at(Level.Internal)))
      ) ==> Vector(
        Left(KitFailure.Unrunnable("the deployment has no plugin absent")),
        Left(KitFailure.Unrunnable("the job other-job is not the plugin nightly's")),
        Left(KitFailure.Unrunnable("the schedule nightly is one the deployment declares")),
        Left(
          KitFailure.Unrunnable(
            "the schedule c's clearance [level: internal] is none of the plugin nightly's schedules'"
          )
        )
      )
    }
  }
}
