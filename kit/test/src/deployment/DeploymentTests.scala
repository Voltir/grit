package grit.kit.deployment

import scala.concurrent.duration.DurationInt

import grit.core.id.EdgeName
import grit.core.tool.ToolName
import grit.core.triage.Gate

import utest.*

/** What [[Deployment.of]] refuses. */
object DeploymentTests extends TestSuite {
  import Deployments.edge
  import TestPlugins.name

  private val github: grit.core.place.Service =
    grit.core.place.Service.of("github").fold(e => sys.error(e), identity)
  private val repo: grit.core.id.KnowledgeSourceName =
    grit.core.id.KnowledgeSourceName.of("repo").fold(e => sys.error(e), identity)
  private val inGithub: Vector[grit.core.place.WorksIn] = Vector(
    grit.core.place.WorksIn(grit.core.place.Place.Everywhere, github)
  )

  /** One source, `repo`, supplied by `github`'s tools. */
  private val repoInGithub = grit.core.triage.KnowledgeSources
    .of(
      Vector(
        grit.core.triage
          .KnowledgeSource(repo, "the repository", grit.core.place.Place.Everywhere, Some(github))
      )
    )
    .fold(n => sys.error(grit.core.id.KnowledgeSourceName.value(n)), identity)

  /** Heard turns offered a service's tools when one of its sources reads at least 0.2. */
  private val bySource = grit.core.recipe.TurnRecipe(
    grit.core.recipe.ByFocus.both(
      grit.core.recipe.Shaping(
        grit.core.context.Width.Deployed,
        grit.core.recipe.Offering.BySource(grit.core.period.Probability.clamped(0.2))
      )
    ),
    grit.core.recipe.TurnRecipe.Shipped.addressed
  )

  private def of(edges: Vector[grit.core.edge.ServedEdge], tools: Offered = Offered.Read) =
    Deployments.of(edges = edges, tools = tools).map(_ => ())

  val tests = Tests {
    test("tools that ask first are refused when an edge served cannot answer an ask, naming it") {
      (
        of(Vector(edge("slack", asks = false), edge("pager", asks = true)), Offered.All),
        of(Vector(edge("pager", asks = true)), Offered.All),
        of(Vector(edge("slack", asks = false)), Offered.Read),
        of(Vector.empty, Offered.All)
      ) ==> (
        Left(DeploymentRefusal.AsksUnanswered(Vector(EdgeName("slack")))),
        Right(()),
        Right(()),
        Right(())
      )
    }

    test("two edges of one name are refused") {
      of(Vector(edge("slack", asks = false), edge("slack", asks = false))) ==>
        Left(DeploymentRefusal.EdgeRepeated(EdgeName("slack")))
    }

    test("speaking unprompted with topics off is refused: no classifier could judge a draft") {
      val limits = grit.core.speech.Limits.suggested(
        grit.core.spend.DailyCap.of("0.25").getOrElse(sys.error("a cap")),
        grit.lifecycle.triage.TriageQuestions.ShippedSpeak
      )
      val off = Topics.Off("no key")
      (
        Deployments
          .of(topics = off, speaking = grit.core.speech.Speaking.Within(limits))
          .map(_ => ()),
        Deployments
          .of(topics = off, speaking = grit.core.speech.Speaking.Shadow(limits))
          .map(_ => ()),
        Deployments.of(topics = off).map(_ => ()),
        Deployments
          .of(topics = Topics.Stub, speaking = grit.core.speech.Speaking.Within(limits))
          .map(_ => ())
      ) ==> (
        Left(DeploymentRefusal.SpeaksUnjudged("no key")),
        Left(DeploymentRefusal.SpeaksUnjudged("no key")),
        Right(()),
        Right(())
      )
    }

    test(
      "two shadows of one name are refused, and shadows with topics off: no classifier could be asked"
    ) {
      def variant(name: String) = grit.lifecycle.shadow.ShadowVariant(
        grit.core.id.ShadowName.of(name).getOrElse(sys.error("a name")),
        grit.lifecycle.triage.TriageQuestions.V1,
        None,
        grit.core.spend.DailyCap.of("0.01").getOrElse(sys.error("a cap")),
        java.time.Instant.EPOCH
      )
      def shadowed(topics: Topics, shadows: String*) =
        Deployments.of(topics = topics, shadows = shadows.toVector.map(variant)).map(_ => ())
      (
        shadowed(Topics.Jev, "words", "replica", "words"),
        shadowed(Topics.Off("no key"), "words"),
        shadowed(Topics.Off("no key")),
        shadowed(Topics.Jev, "words", "replica"),
        shadowed(Topics.Stub, "words")
      ) ==> (
        Left(
          DeploymentRefusal.ShadowRepeated(
            grit.core.id.ShadowName.of("words").getOrElse(sys.error("a name"))
          )
        ),
        Left(DeploymentRefusal.ShadowsUnasked("no key")),
        Right(()),
        Right(()),
        Right(())
      )
    }

    test(
      "a review is refused unless it names a declared shadow, whose set's gate it keeps, " +
        "or while speaking is off: live's gate would never be reached"
    ) {
      def name(s: String) = grit.core.id.ShadowName.of(s).getOrElse(sys.error("a name"))
      def variant(called: String, questions: grit.lifecycle.triage.TriageQuestions) =
        grit.lifecycle.shadow.ShadowVariant(
          name(called),
          questions,
          None,
          grit.core.spend.DailyCap.of("0.01").getOrElse(sys.error("a cap")),
          java.time.Instant.EPOCH
        )
      val shadows = Vector(
        variant("v2", grit.lifecycle.triage.TriageQuestions.V2),
        variant("v1", grit.lifecycle.triage.TriageQuestions.V1)
      )
      val speaking = grit.core.speech.Speaking.Shadow(
        grit.core.speech.Limits.suggested(
          grit.core.spend.DailyCap.of("0.25").getOrElse(sys.error("a cap")),
          grit.lifecycle.triage.TriageQuestions.ShippedSpeak
        )
      )
      def reviewing(of: String) =
        grit.core.review.Reviewing.of(name(of), 8, 20, 24.hours).getOrElse(sys.error("a review"))
      def reviewed(of: String, speaks: grit.core.speech.Speaking = speaking) =
        Deployments
          .of(speaking = speaks, shadows = shadows, review = Some(reviewing(of)))
          .map(_.review.map(r => (r.reviewing.shadow, r.gate)))
      (
        reviewed("undeclared"),
        reviewed("v1"),
        reviewed("v2", grit.core.speech.Speaking.Off),
        reviewed("v2"),
        Deployments.of(speaking = speaking, shadows = shadows).map(_.review)
      ) ==> (
        Left(DeploymentRefusal.ReviewUngated(name("undeclared"))),
        Right(Some((name("v1"), grit.lifecycle.triage.TriageQuestions.V1.speak))),
        Left(DeploymentRefusal.ReviewUnspoken),
        Right(Some((name("v2"), grit.lifecycle.triage.TriageQuestions.V2.speak))),
        Right(None)
      )
    }

    test(
      "speaking by a gate that reads a question live triage does not ask is refused, naming it"
    ) {
      def speaks(gate: grit.core.triage.Gate) = {
        val limits = grit.core.speech.Limits.suggested(
          grit.core.spend.DailyCap.of("0.25").getOrElse(sys.error("a cap")),
          gate
        )
        (
          Deployments.of(speaking = grit.core.speech.Speaking.Within(limits)).map(_ => ()),
          Deployments.of(speaking = grit.core.speech.Speaking.Shadow(limits)).map(_ => ())
        )
      }
      val chatter = grit.core.triage.Reading.Chosen(grit.core.triage.Tags.V1.kind, "chatter")
      (
        speaks(grit.lifecycle.triage.TriageQuestions.V1.speak),
        speaks(grit.lifecycle.triage.TriageQuestions.ShippedSpeak)
      ) ==> (
        (
          Left(DeploymentRefusal.SpeechUnread(chatter)),
          Left(DeploymentRefusal.SpeechUnread(chatter))
        ),
        (Right(()), Right(()))
      )
    }

    test(
      "live triage that does not ask durable as a yes/no is refused: every period would earn a closing"
    ) {
      import grit.lifecycle.triage.TriageQuestions
      import grit.lifecycle.triage.TriageQuestions.Item
      def asking(question: grit.core.classify.Question) =
        TriageQuestions
          .of(
            Item.One(grit.core.triage.Earning.Durable, question),
            Vector.empty,
            Gate.Open
          )
          .getOrElse(sys.error("a set"))
      val open = grit.core.id.QuestionName.of("open").getOrElse(sys.error("a name"))
      val unasked = TriageQuestions
        .of(
          Item.One(open, grit.core.classify.Question.YesNo("?", None, None)),
          Vector.empty,
          Gate.Open
        )
        .getOrElse(sys.error("a set"))
      val chosen = grit.core.classify.Question
        .choice(
          "?",
          grit.core.classify.Question.Key("yes", None),
          grit.core.classify.Question.Key("no", None)
        )
        .getOrElse(sys.error("a choice"))
      (
        Deployment.earning(TriageQuestions.shipped(grit.core.persona.Persona.Grit)),
        Deployment.earning(TriageQuestions.V1),
        Deployment.earning(asking(grit.core.classify.Question.YesNo("?", None, None))),
        Deployment.earning(unasked),
        Deployment.earning(asking(chosen))
      ) ==> (
        Right(()),
        Right(()),
        Right(()),
        Left(DeploymentRefusal.DurableUnasked),
        Left(DeploymentRefusal.DurableUnasked)
      )
    }

    test(
      "a recipe offering a service by a question live triage does not ask is refused, naming it"
    ) {
      import grit.lifecycle.triage.TriageQuestions
      val read = grit.core.triage.Reading.Yes(
        grit.core.id.QuestionName.per(grit.core.triage.Tags.V2.sourcePrefix, repo)
      )
      (
        Deployment.read(
          bySource,
          repoInGithub,
          TriageQuestions.shipped(grit.core.persona.Persona.Grit)
        ),
        Deployment.read(bySource, repoInGithub, TriageQuestions.V1),
        Deployment.read(grit.core.recipe.TurnRecipe.Shipped, repoInGithub, TriageQuestions.V1),
        Deployments
          .of(worksIn = inGithub, knowledge = repoInGithub, recipe = bySource)
          .map(_ => ())
      ) ==> (
        Right(()),
        Left(DeploymentRefusal.RecipeUnread(read)),
        Right(()),
        Right(())
      )
    }

    test("a recipe offering by source with topics off is refused: nothing could weigh a message") {
      val addressed = grit.core.recipe.TurnRecipe.Shipped.copy(addressed = bySource.heard.focused)
      def declared(recipe: grit.core.recipe.TurnRecipe, topics: Topics) = Deployments
        .of(worksIn = inGithub, knowledge = repoInGithub, recipe = recipe, topics = topics)
        .map(_ => ())
      (
        declared(addressed, Topics.Off("no key")),
        declared(bySource, Topics.Off("no key")),
        declared(addressed, Topics.Stub),
        declared(grit.core.recipe.TurnRecipe.Shipped, Topics.Off("no key"))
      ) ==> (
        Left(DeploymentRefusal.RecipeUnweighed("no key")),
        Left(DeploymentRefusal.RecipeUnweighed("no key")),
        Right(()),
        Right(())
      )
    }

    test(
      "a recipe offering by source with no knowledge source supplying a service is refused"
    ) {
      val addressed = grit.core.recipe.TurnRecipe.Shipped.copy(addressed = bySource.heard.focused)
      val unsupplying = grit.core.triage.KnowledgeSources
        .of(repoInGithub.all.map(_.copy(supplies = None)))
        .fold(n => sys.error(n.toString), identity)
      def declared(
          recipe: grit.core.recipe.TurnRecipe,
          knowledge: grit.core.triage.KnowledgeSources
      ) =
        Deployments.of(worksIn = inGithub, knowledge = knowledge, recipe = recipe).map(_ => ())
      (
        declared(addressed, grit.core.triage.KnowledgeSources.Empty),
        declared(bySource, unsupplying),
        declared(addressed, repoInGithub),
        declared(grit.core.recipe.TurnRecipe.Shipped, grit.core.triage.KnowledgeSources.Empty)
      ) ==> (
        Left(DeploymentRefusal.RecipeUnsourced),
        Left(DeploymentRefusal.RecipeUnsourced),
        Right(()),
        Right(())
      )
    }

    test(
      "a knowledge source supplying a service no worksIn or reaches link offers is refused"
    ) {
      def declared(
          worksIn: Vector[grit.core.place.WorksIn],
          reaches: Vector[grit.core.place.Reaches]
      ) = Deployments
        .of(worksIn = worksIn, reaches = reaches, knowledge = repoInGithub)
        .map(_ => ())
      (
        declared(Vector.empty, Vector.empty),
        declared(inGithub, Vector.empty),
        declared(
          Vector.empty,
          Vector(grit.core.place.Reaches(grit.core.place.Place.Everywhere, github))
        )
      ) ==> (
        Left(DeploymentRefusal.OffersUnlinked(repo, github)),
        Right(()),
        Right(())
      )
    }

    test("a recipe drawing a window wider than the assembly's is refused; at or under it is not") {
      // Deployments.of assembles within 1000 tokens.
      def drawn(budget: Long) = Deployments
        .of(recipe =
          grit.core.recipe.TurnRecipe.Shipped.copy(addressed =
            grit.core.recipe.Shaping(
              grit.core.context.Width.Within(grit.core.message.Tokens(budget), 4),
              grit.core.recipe.Offering.All
            )
          )
        )
        .map(_ => ())
      (drawn(1001), drawn(1000), drawn(400)) ==> (
        Left(
          DeploymentRefusal
            .Widens(grit.core.message.Tokens(1001), grit.core.message.Tokens(1000))
        ),
        Right(()),
        Right(())
      )
    }

    test("a sweep under a second is refused; a second is not") {
      (
        Deployments.of(sweep = 999.millis).map(_ => ()),
        Deployments.of(sweep = 1.second).map(_ => ())
      ) ==> (Left(DeploymentRefusal.SweepTooOften(999.millis)), Right(()))
    }

    test("two plugins of one name are refused, naming it") {
      Deployments
        .of(plugins =
          Vector(
            new TestPlugins.Keys(name("a")),
            new TestPlugins.Keys(name("a"), ToolName("other"))
          )
        )
        .map(_ => ()) ==> Left(DeploymentRefusal.PluginRepeated(name("a")))
    }

    test("a plugin needing one the deployment lacks is refused, naming both; present, accepted") {
      val keys = new TestPlugins.Keys(name("keys"))
      val reading = new TestPlugins.Reading(name("reading"), keys, declared = true)
      (
        Deployments.of(plugins = Vector(reading)).map(_ => ()),
        Deployments.of(plugins = Vector(keys, reading)).map(_ => ())
      ) ==> (Left(DeploymentRefusal.PluginUnmet(name("reading"), name("keys"))), Right(()))
    }

    test("a plugin's tool named as another plugin's, or as one of grit's own, is refused") {
      val keys = new TestPlugins.Keys(name("keys"))
      (
        Deployments
          .of(plugins = Vector(keys, new TestPlugins.Keys(name("more"))))
          .map(_ => ()),
        Deployments
          .of(plugins = Vector(new TestPlugins.Keys(name("asks"), ToolName("about"))))
          .map(_ => ()),
        Deployments
          .of(plugins = Vector(new TestPlugins.Keys(name("verdict"), ToolName("topic"))))
          .map(_ => ())
      ) ==> (
        Left(DeploymentRefusal.ToolRepeated(ToolName("keys"))),
        Left(DeploymentRefusal.ToolRepeated(ToolName("about"))),
        Left(DeploymentRefusal.ToolRepeated(ToolName("topic")))
      )
    }

    test("a plugin's tool asking for a plugin its own does not need is refused, naming both") {
      val keys = new TestPlugins.Keys(name("keys"))
      Deployments
        .of(plugins =
          Vector(keys, new TestPlugins.Reading(name("reading"), keys, declared = false))
        )
        .map(_ => ()) ==> Left(DeploymentRefusal.ToolUnneeded(name("reading"), name("keys")))
    }

    test("a plugin's tool booking a job not its plugin's is refused, naming both; its own is not") {
      val nudge = new TestPlugins.Named("nudge")
      def booking(jobs: Vector[grit.core.job.Job[?]]) =
        Deployments
          .of(plugins = Vector(new TestPlugins.Booking(name("books"), nudge, jobs)))
          .map(_ => ())
      (booking(Vector(nudge)), booking(Vector(new TestPlugins.Named("remind")))) ==> (
        Right(()),
        Left(
          DeploymentRefusal.JobUnowned(
            name("books"),
            grit.core.id.JobName.of("nudge").fold(sys.error, identity)
          )
        )
      )
    }
  }
}
