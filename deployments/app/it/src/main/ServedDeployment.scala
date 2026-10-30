package grit.app.main

import scala.concurrent.duration.*

import grit.core.edge.ServedEdge
import grit.core.files.PagedText
import grit.core.message.Tokens
import grit.core.model.{Assignment, ModelId, ModelRef, Policy}
import grit.core.period.LifecycleSettings
import grit.core.speech.Speaking
import grit.core.spend.Budget
import grit.dbos.sql.DbConfig
import grit.kit.deployment.{Assembly, Deployment, Offer, Offered, Placer}
import grit.turn.TurnLoop

/** A deployment that serves the edges it is given and nothing else, for a suite that opens or
  * serves it: no plugins, no classifier, and a model nothing here calls.
  */
object ServedDeployment {

  private val assigned =
    Assignment(
      ModelRef(ModelId.of("openai/gpt-oss-120b").getOrElse(sys.error("id")), None),
      100,
      None
    )

  /** The deployment serving `edges`. */
  def of(edges: ServedEdge*): Deployment =
    Deployment
      .of(
        edges = edges.toVector,
        worksIn = Vector.empty,
        plugins = Vector.empty,
        policy = Policy.one(assigned, assigned, assigned, assigned),
        offer = Offer(Offered.Read, TurnLoop.Rounds.of(2).getOrElse(sys.error("rounds"))),
        assembly = Assembly.Linear(Tokens(4000)),
        placer = Placer.Off("no classifier here"),
        lifecycle = LifecycleSettings.Default,
        budget = Budget(java.time.ZoneOffset.UTC, None),
        speaking = Speaking.Off,
        sweep = 30.seconds,
        persona = grit.core.persona.Persona.Grit,
        formats = PagedText.formats
      )
      .fold(r => sys.error(r.message), identity)

  /** The environment naming `config`'s database, as a serving process reads it. */
  def env(config: DbConfig): Map[String, String] =
    Map(
      DbConfig.UrlVar -> config.jdbcUrl,
      DbConfig.UserVar -> config.user,
      DbConfig.PasswordVar -> config.password
    )
}
