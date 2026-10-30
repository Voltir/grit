package grit.kit.deployment

import scala.concurrent.duration.{DurationInt, FiniteDuration}

import grit.core.clock.Clock
import grit.core.edge.{EdgeRefusal, EdgeStores, ServedEdge, Variable}
import grit.core.files.PagedText
import grit.core.id.{AttesterName, EdgeName}
import grit.core.message.Tokens
import grit.core.model.{Assignment, ModelId, ModelRef, Policy}
import grit.core.period.LifecycleSettings
import grit.core.place.Place
import grit.core.spend.Budget
import grit.core.visibility.{Compartment, Visibility}
import grit.turn.TurnLoop

/** Deployments for tests: every field but the ones a test varies fixed. */
object Deployments {

  /** An edge named `called` that needs `needs`, does `asks` with a call that asks first, posts
    * review prompts at `reviews`, names `naming`, is the attester `attests`, says its replies
    * show `shows`, and never opens.
    */
  def edge(
      called: String,
      asks: ServedEdge.Asks,
      needs: Vector[String] = Vector.empty,
      naming: Vector[Compartment] = Vector.empty,
      reviews: Option[Place] = None,
      attests: Option[String] = None,
      shows: Option[String] = None
  ): ServedEdge = {
    val wanted = needs.map(Variable(_))
    val asked = asks
    new ServedEdge {
      def name: EdgeName = EdgeName(called)
      def needs: Vector[Variable] = wanted
      def asks: ServedEdge.Asks = asked
      override def reviewsAt: Option[Place] = reviews
      override def compartments: Vector[Compartment] = naming
      override def attester: Option[AttesterName] = attests.map(AttesterName(_))
      override def renders: Option[String] = shows
      def open(
          stores: EdgeStores^,
          env: Map[String, String],
          clock: Clock^,
          log: String => Unit
      ): Either[EdgeRefusal, ServedEdge.Open^{stores, clock, log, caps.any}] =
        Left(EdgeRefusal.Refused("never opened here"))
    }
  }

  private val assigned = Assignment(
    ModelRef(ModelId.of("openai/gpt-oss-120b").getOrElse(sys.error("model id")), None),
    100,
    None
  )

  def of(
      edges: Vector[ServedEdge] = Vector.empty,
      tools: Offered = Offered.Read,
      placer: Placer = Placer.Stub,
      sweep: FiniteDuration = 30.seconds,
      speaking: grit.core.speech.Speaking = grit.core.speech.Speaking.Off,
      shadows: Vector[grit.lifecycle.shadow.ShadowVariant] = Vector.empty,
      review: Option[grit.core.review.Reviewing] = None,
      worksIn: Vector[grit.core.place.WorksIn] = Vector.empty,
      reaches: Vector[grit.core.place.Reaches] = Vector.empty,
      knowledge: grit.core.triage.Corpora = grit.core.triage.Corpora.Empty,
      recipe: grit.core.recipe.TurnRecipe = grit.core.recipe.TurnRecipe.Shipped,
      plugins: Vector[grit.core.plugin.Plugin] = Vector.empty,
      jobs: Vector[grit.core.job.PlainJob[?]] = Vector.empty,
      schedules: Vector[grit.core.job.Declared[?]] = Vector.empty,
      visibility: Visibility = Visibility.Shipped,
      identities: grit.core.identity.Identities = grit.core.identity.Identities.Shipped,
      version: DeploymentVersion = DeploymentVersion.First,
      policy: Policy = Policy.one(assigned, assigned, assigned, assigned)
  ): Either[DeploymentRefusal, Deployment] =
    Deployment.of(
      edges = edges,
      worksIn = worksIn,
      plugins = plugins,
      policy = policy,
      offer = Offer(tools, TurnLoop.Rounds.of(4).getOrElse(sys.error("rounds"))),
      assembly = Assembly.Linear(Tokens(1000)),
      placer = placer,
      lifecycle = LifecycleSettings.Default,
      budget = Budget(java.time.ZoneOffset.UTC, None),
      speaking = speaking,
      sweep = sweep,
      persona = grit.core.persona.Persona.Grit,
      shadows = shadows,
      review = review,
      reaches = reaches,
      knowledge = knowledge,
      recipe = recipe,
      jobs = jobs,
      schedules = schedules,
      visibility = visibility,
      identities = identities,
      version = version,
      formats = PagedText.formats
    )

  /** [[of]], which the test expects to be accepted. */
  def accepted(edges: Vector[ServedEdge] = Vector.empty, placer: Placer = Placer.Stub): Deployment =
    of(edges = edges, placer = placer).fold(r => sys.error(r.message), identity)
}
