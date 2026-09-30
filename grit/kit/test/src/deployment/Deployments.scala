package grit.kit.deployment

import scala.concurrent.duration.{DurationInt, FiniteDuration}

import grit.core.edge.{EdgeRefusal, EdgeStores, ServedEdge, Variable}
import grit.core.id.EdgeName
import grit.core.message.Tokens
import grit.core.model.{Assignment, ModelId, ModelRef, Policy}
import grit.core.period.LifecycleSettings
import grit.core.spend.Budget
import grit.turn.TurnLoop

/** Deployments for tests: every field but the ones a test varies fixed. */
object Deployments {

  /** An edge named `called` that needs `needs`, says whether it can answer an ask, and never
    * opens.
    */
  def edge(called: String, asks: Boolean, needs: Vector[String] = Vector.empty): ServedEdge = {
    val wanted = needs.map(Variable(_))
    new ServedEdge {
      def name: EdgeName = EdgeName(called)
      def needs: Vector[Variable] = wanted
      def answersAsks: Boolean = asks
      def open(
          stores: EdgeStores^,
          env: Map[String, String],
          log: String => Unit
      ): Either[EdgeRefusal, ServedEdge.Open^{stores, log, caps.any}] =
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
      topics: Topics = Topics.Stub,
      sweep: FiniteDuration = 30.seconds,
      speaking: grit.core.speech.Speaking = grit.core.speech.Speaking.Off
  ): Either[DeploymentRefusal, Deployment] =
    Deployment.of(
      edges = edges,
      plugins = Vector.empty,
      policy = Policy(assigned, assigned, assigned, assigned),
      offer = Offer(tools, TurnLoop.Budget.of(4).getOrElse(sys.error("rounds"))),
      assembly = Assembly.Linear(Tokens(1000)),
      topics = topics,
      lifecycle = LifecycleSettings.Default,
      budget = Budget(java.time.ZoneOffset.UTC, None),
      speaking = speaking,
      sweep = sweep
    )

  /** [[of]], which the test expects to be accepted. */
  def accepted(edges: Vector[ServedEdge] = Vector.empty, topics: Topics = Topics.Stub): Deployment =
    of(edges = edges, topics = topics).fold(r => sys.error(r.message), identity)
}
