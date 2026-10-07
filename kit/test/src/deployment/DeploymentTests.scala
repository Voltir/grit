package grit.kit.deployment

import scala.concurrent.duration.DurationInt

import grit.core.id.EdgeName
import grit.core.tool.ToolName
import grit.core.triage.Gate
import grit.core.visibility.{Compartment, Compartments, Label, Level, RoomLabels, Visibility}

import utest.*

/** What [[Deployment.of]] refuses. */
object DeploymentTests extends TestSuite {
  import Deployments.edge
  import TestPlugins.name

  private def job(s: String): grit.core.id.JobName =
    grit.core.id.JobName.of(s).fold(sys.error, identity)

  private def key(s: String): grit.core.id.ScheduleKey =
    grit.core.id.ScheduleKey.of(s).fold(sys.error, identity)

  /** A schedule declared under `k`, running `of` daily at 09:00 UTC, cleared for `clearance`. */
  private def declared(
      k: String,
      of: TestPlugins.Named,
      clearance: Label = Label.Public
  ): grit.core.job.Declared[?] =
    grit.core.job.Declared(
      key(k),
      of,
      grit.core.job.SlotRule.Daily(java.time.LocalTime.of(9, 0), java.time.ZoneOffset.UTC),
      TestPlugins.Named.None,
      clearance
    )

  private def compartment(s: String): Compartment = Compartment.of(s).fold(sys.error, identity)

  private val trial = compartment("trial")
  private val client = compartment("client")

  /** `trial` declared, every room public, no group. */
  private val trialDeclared: Visibility =
    Visibility
      .of(
        Compartments.of(Vector(trial)).fold(c => sys.error(Compartment.name(c)), identity),
        RoomLabels.Public,
        Vector.empty,
        Vector.empty
      )
      .fold(r => sys.error(r.toString), identity)

  private val github: grit.core.place.Service =
    grit.core.place.Service.of("github").fold(e => sys.error(e), identity)
  private val repo: grit.core.id.CorpusName =
    grit.core.id.CorpusName.of("repo").fold(e => sys.error(e), identity)
  private val inGithub: Vector[grit.core.place.WorksIn] = Vector(
    grit.core.place.WorksIn(grit.core.place.Place.Everywhere, github)
  )

  /** One source, `repo`, supplied by `github`'s tools. */
  private val repoInGithub = grit.core.triage.Corpora
    .of(
      Vector(
        grit.core.triage
          .Corpus(repo, "the repository", grit.core.place.Place.Everywhere, Some(github))
      )
    )
    .fold(n => sys.error(grit.core.id.CorpusName.value(n)), identity)

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
      val prompting = edge(
        "slack",
        asks = false,
        reviews = Some(grit.core.place.Place.read("slack:T1/C1").fold(sys.error, identity))
      )
      def reviewed(of: String, speaks: grit.core.speech.Speaking = speaking) =
        Deployments
          .of(
            edges = Vector(prompting),
            speaking = speaks,
            shadows = shadows,
            review = Some(reviewing(of))
          )
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
      "a review is refused unless exactly one edge posts its prompts, naming those that do"
    ) {
      def name(s: String) = grit.core.id.ShadowName.of(s).getOrElse(sys.error("a name"))
      val shadows = Vector(
        grit.lifecycle.shadow.ShadowVariant(
          name("v2"),
          grit.lifecycle.triage.TriageQuestions.V2,
          None,
          grit.core.spend.DailyCap.of("0.01").getOrElse(sys.error("a cap")),
          java.time.Instant.EPOCH
        )
      )
      val speaking = grit.core.speech.Speaking.Shadow(
        grit.core.speech.Limits.suggested(
          grit.core.spend.DailyCap.of("0.25").getOrElse(sys.error("a cap")),
          grit.lifecycle.triage.TriageQuestions.ShippedSpeak
        )
      )
      val review =
        grit.core.review.Reviewing.of(name("v2"), 8, 20, 24.hours).getOrElse(sys.error("a review"))
      def at(text: String) = grit.core.place.Place.read(text).fold(sys.error, identity)
      def reviewing(called: String, place: String) =
        edge(called, asks = false, reviews = Some(at(place)))
      def reviewed(edges: grit.core.edge.ServedEdge*) =
        Deployments
          .of(
            edges = edges.toVector,
            speaking = speaking,
            shadows = shadows,
            review = Some(review)
          )
          .map(_.review.map(_.place))
      val quiet = edge("mcp", asks = false)
      (
        reviewed(quiet),
        reviewed(reviewing("slack", "slack:T1/C1"), reviewing("other", "slack:T1/C2")),
        reviewed(quiet, reviewing("slack", "slack:T1/C1"))
      ) ==> (
        Left(DeploymentRefusal.ReviewUnposted(Vector.empty)),
        Left(DeploymentRefusal.ReviewUnposted(Vector(EdgeName("slack"), EdgeName("other")))),
        Right(Some(at("slack:T1/C1")))
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
      "a recipe offering by source with no corpus supplying a service is refused"
    ) {
      val addressed = grit.core.recipe.TurnRecipe.Shipped.copy(addressed = bySource.heard.focused)
      val unsupplying = grit.core.triage.Corpora
        .of(repoInGithub.all.map(_.copy(supplies = None)))
        .fold(n => sys.error(n.toString), identity)
      def declared(
          recipe: grit.core.recipe.TurnRecipe,
          knowledge: grit.core.triage.Corpora
      ) =
        Deployments.of(worksIn = inGithub, knowledge = knowledge, recipe = recipe).map(_ => ())
      (
        declared(addressed, grit.core.triage.Corpora.Empty),
        declared(bySource, unsupplying),
        declared(addressed, repoInGithub),
        declared(grit.core.recipe.TurnRecipe.Shipped, grit.core.triage.Corpora.Empty)
      ) ==> (
        Left(DeploymentRefusal.RecipeUnsourced),
        Left(DeploymentRefusal.RecipeUnsourced),
        Right(()),
        Right(())
      )
    }

    test(
      "a corpus supplying a service no worksIn or reaches link offers is refused"
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

    test("two jobs of one name, among the plugins' and the deployment's own, are refused") {
      val nudge = new TestPlugins.Named("nudge")
      def books(n: String) = new TestPlugins.Booking(name(n), nudge, Vector(nudge))
      (
        Deployments.of(plugins = Vector(books("a")), jobs = Vector(nudge)).map(_ => ()),
        Deployments
          .of(plugins =
            Vector(
              new TestPlugins.Declaring(name("a"), Vector(nudge), Vector()),
              new TestPlugins.Declaring(name("b"), Vector(new TestPlugins.Named("nudge")), Vector())
            )
          )
          .map(_ => ()),
        Deployments
          .of(plugins = Vector(books("a")), jobs = Vector(new TestPlugins.Named("standup")))
          .map(_ => ())
      ) ==> (
        Left(DeploymentRefusal.JobRepeated(job("nudge"))),
        Left(DeploymentRefusal.JobRepeated(job("nudge"))),
        Right(())
      )
    }

    test("two schedules one declarer declares under one key are refused; two declarers' are not") {
      val nudge = new TestPlugins.Named("nudge")
      (
        Deployments
          .of(
            jobs = Vector(nudge),
            schedules = Vector(declared("daily", nudge), declared("daily", nudge))
          )
          .map(_ => ()),
        Deployments
          .of(
            plugins = Vector(new TestPlugins.Declaring(name("p"), Vector(), Vector())),
            jobs = Vector(nudge),
            schedules = Vector(declared("daily", nudge))
          )
          .map(_ => ())
      ) ==> (
        Left(
          DeploymentRefusal.ScheduleRepeated(
            grit.core.id.ScheduleId.declared(grit.core.id.Declarer.Deployment, key("daily"))
          )
        ),
        Right(())
      )
      // The same key under a plugin and under the deployment names two schedules.
      val owned = new TestPlugins.Named("owned")
      Deployments
        .of(
          plugins = Vector(
            new TestPlugins.Declaring(name("p"), Vector(owned), Vector(declared("daily", owned)))
          ),
          jobs = Vector(nudge),
          schedules = Vector(declared("daily", nudge))
        )
        .map(_.declared.map((by, d) => d.id(by))) ==> Right(
        Vector(
          grit.core.id.ScheduleId.declared(grit.core.id.Declarer.Plugin(name("p")), key("daily")),
          grit.core.id.ScheduleId.declared(grit.core.id.Declarer.Deployment, key("daily"))
        )
      )
    }

    test("a declared schedule whose job is not the deployment's job of that name is refused") {
      val nudge = new TestPlugins.Named("nudge")
      val id = grit.core.id.ScheduleId.declared(grit.core.id.Declarer.Deployment, key("daily"))
      (
        Deployments.of(schedules = Vector(declared("daily", nudge))).map(_ => ()),
        // Another job under the same name: its runs would read their parameters with this one.
        Deployments
          .of(
            jobs = Vector(nudge),
            schedules = Vector(declared("daily", new TestPlugins.Named("nudge")))
          )
          .map(_ => ()),
        Deployments
          .of(jobs = Vector(nudge), schedules = Vector(declared("daily", nudge)))
          .map(_ => ())
      ) ==> (
        Left(DeploymentRefusal.ScheduleJobless(id, job("nudge"))),
        Left(DeploymentRefusal.ScheduleJobless(id, job("nudge"))),
        Right(())
      )
    }

    test("a plugin declaring a schedule of a job not its own is refused, naming both") {
      val nudge = new TestPlugins.Named("nudge")
      (
        Deployments
          .of(
            plugins = Vector(
              new TestPlugins.Declaring(name("owner"), Vector(nudge), Vector()),
              new TestPlugins.Declaring(name("other"), Vector(), Vector(declared("daily", nudge)))
            )
          )
          .map(_ => ()),
        Deployments
          .of(plugins =
            Vector(
              new TestPlugins.Declaring(
                name("owner"),
                Vector(nudge),
                Vector(declared("daily", nudge))
              )
            )
          )
          .map(_ => ())
      ) ==> (Left(DeploymentRefusal.ScheduleUnowned(name("other"), job("nudge"))), Right(()))
    }

    test(
      "a plugin naming a compartment its deployment's visibility does not declare is refused, naming both"
    ) {
      def naming(cs: Compartment*) = Vector(new TestPlugins.Naming(name("p"), cs.toVector))
      (
        Deployments.of(plugins = naming(trial, client), visibility = trialDeclared).map(_ => ()),
        Deployments.of(plugins = naming(trial)).map(_ => ()),
        Deployments.of(plugins = naming(trial), visibility = trialDeclared).map(_ => ()),
        // Every deployment declares unmapped.
        Deployments.of(plugins = naming(Compartment.Unmapped)).map(_ => ())
      ) ==> (
        Left(DeploymentRefusal.CompartmentUndeclared(Requirer.ByPlugin(name("p")), client)),
        Left(DeploymentRefusal.CompartmentUndeclared(Requirer.ByPlugin(name("p")), trial)),
        Right(()),
        Right(())
      )
    }

    test(
      "an edge naming a compartment its deployment's visibility does not declare is refused, naming both"
    ) {
      def naming(cs: Compartment*) = Vector(edge("slack", asks = false, naming = cs.toVector))
      (
        Deployments.of(edges = naming(trial, client), visibility = trialDeclared).map(_ => ()),
        Deployments.of(edges = naming(trial)).map(_ => ()),
        Deployments.of(edges = naming(trial), visibility = trialDeclared).map(_ => ())
      ) ==> (
        Left(DeploymentRefusal.CompartmentUndeclared(Requirer.ByEdge(EdgeName("slack")), client)),
        Left(DeploymentRefusal.CompartmentUndeclared(Requirer.ByEdge(EdgeName("slack")), trial)),
        Right(())
      )
    }

    test(
      "a declared schedule cleared for a compartment its deployment's visibility does not declare is refused, naming both"
    ) {
      val nudge = new TestPlugins.Named("nudge")
      val inClient = Label.at(Level.Internal, client)
      val inTrial = Label.at(Level.Confidential, trial)
      (
        Deployments
          .of(
            jobs = Vector(nudge),
            schedules = Vector(declared("daily", nudge, inClient)),
            visibility = trialDeclared
          )
          .map(_ => ()),
        Deployments
          .of(
            plugins = Vector(
              new TestPlugins.Declaring(
                name("p"),
                Vector(nudge),
                Vector(declared("daily", nudge, inClient))
              )
            ),
            visibility = trialDeclared
          )
          .map(_ => ()),
        Deployments
          .of(
            jobs = Vector(nudge),
            schedules = Vector(declared("daily", nudge, inTrial)),
            visibility = trialDeclared
          )
          .map(_ => ())
      ) ==> (
        Left(
          DeploymentRefusal.CompartmentUndeclared(
            Requirer.BySchedule(
              grit.core.id.ScheduleId.declared(grit.core.id.Declarer.Deployment, key("daily"))
            ),
            client
          )
        ),
        Left(
          DeploymentRefusal.CompartmentUndeclared(
            Requirer.BySchedule(
              grit.core.id.ScheduleId
                .declared(grit.core.id.Declarer.Plugin(name("p")), key("daily"))
            ),
            client
          )
        ),
        Right(())
      )
    }

    test(
      "a deployment that may label a room above public is refused while an edge posts out, naming it"
    ) {
      val labelled = Visibility
        .of(
          Compartments.Shipped,
          RoomLabels
            .of(
              Vector(
                grit.core.place.Place
                  .read("slack:acme/#trial")
                  .fold(sys.error, identity) -> Label.at(Level.Internal)
              ),
              grit.core.visibility.Labelled.Mapped(Label.Public)
            )
            .fold(p => sys.error(p.written), identity),
          Vector.empty,
          Vector.empty
        )
        .fold(r => sys.error(r.toString), identity)
      val posting = edge("slack", asks = false, posts = true)
      val quiet = edge("mcp", asks = false)
      (
        Deployments.of(edges = Vector(quiet, posting), visibility = labelled).map(_ => ()),
        Deployments.of(edges = Vector(quiet), visibility = labelled).map(_ => ()),
        Deployments.of(edges = Vector(quiet, posting)).map(_ => ())
      ) ==> (Left(DeploymentRefusal.PostsBelow(EdgeName("slack"))), Right(()), Right(()))
    }

    test("an accepted deployment runs every job, its plugins' and its own, by name") {
      val (nudge, standup) = (new TestPlugins.Named("nudge"), new TestPlugins.Named("standup"))
      Deployments
        .of(
          plugins = Vector(new TestPlugins.Declaring(name("p"), Vector(nudge), Vector())),
          jobs = Vector(standup)
        )
        .map(d =>
          (
            d.allJobs.named(job("nudge")),
            d.allJobs.named(job("standup")),
            d.allJobs.named(job("other"))
          )
        ) ==> Right((Some(nudge), Some(standup), None))
    }

    test("a deployment declared with every argument but jobs and schedules has none of either") {
      val assigned = grit.core.model.Assignment(
        grit.core.model.ModelRef(
          grit.core.model.ModelId.of("openai/gpt-oss-120b").getOrElse(sys.error("model id")),
          None
        ),
        100,
        None
      )
      Deployment
        .of(
          edges = Vector.empty,
          worksIn = Vector.empty,
          reaches = Vector.empty,
          plugins = Vector.empty,
          policy = grit.core.model.Policy(assigned, assigned, assigned, assigned),
          offer =
            Offer(Offered.Read, grit.turn.TurnLoop.Budget.of(4).getOrElse(sys.error("rounds"))),
          assembly = Assembly.Linear(grit.core.message.Tokens(1000)),
          topics = Topics.Stub,
          lifecycle = grit.core.period.LifecycleSettings.Default,
          budget = grit.core.spend.Budget(java.time.ZoneOffset.UTC, None),
          speaking = grit.core.speech.Speaking.Off,
          sweep = 30.seconds,
          shadows = Vector.empty,
          knowledge = grit.core.triage.Corpora.Empty,
          recipe = grit.core.recipe.TurnRecipe.Shipped,
          persona = grit.core.persona.Persona.Grit,
          review = None
        )
        .map(d => (d.jobs, d.schedules, d.declared)) ==> Right((Vector(), Vector(), Vector()))
    }
  }
}
