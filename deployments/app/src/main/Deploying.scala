package grit.app.main

import grit.app.config.{Budgets, Claimed, LifecycleEnv}
import grit.assembly.linear.LinearAssembler
import grit.assembly.retrieval.RetrievalAssembler
import grit.core.edge.ServedEdge
import grit.core.files.{FileArea, Formats}
import grit.core.identity.{Identities, Vouching}
import grit.core.model.Policy
import grit.core.persona.Persona
import grit.core.place.{Reaches, WorksIn}
import grit.core.speech.Speaking
import grit.core.visibility.{Cap, Cleared, Compartments, Integrity, Party}
import grit.formats.choice.Defaults
import grit.kit.deployment.{Assembly, Deployment, Offer, Offered, Placer}
import grit.models.{JevConfig, OpenRouterConfig, Seed}
import grit.slack.edge.SlackAccounts
import grit.slack.event.TeamId

import RunChoices.*
import PluginChoices.*
import Clearances.*
import ServedEdges.*

/** The reference `Deployment`, read from the environment. */
private[main] object Deploying {

  /** The formats its files are read with, and so the kinds of Drive file knowledge cards. */
  private[main] val Formatted: Formats = Defaults.formats

  private[main] val StubPlacerVar = "GRIT_STUB_TOPICS"

  /** Jev when `JEV_API_KEY` is set (its value is [[Secrets]]'s to read); else the stub when
    * `GRIT_STUB_TOPICS` is `1`; else none, saying why.
    */
  private[main] def placer(env: Map[String, String]): Placer =
    if (env.contains(JevConfig.KeyVar)) Placer.Jev
    else if (env.get(StubPlacerVar).map(_.trim).contains("1")) Placer.Stub
    else Placer.Off(s"${JevConfig.KeyVar} is not set")

  /** The model policy: the seed catalog's ([[Seed]]), with the roles' variables laid over it
    * ([[OpenRouterConfig.policy]]) when OpenRouter's key is set.
    */
  private[main] def policy(env: Map[String, String]): Either[String, Policy] =
    Seed.catalog.flatMap { seed =>
      if (!env.contains(OpenRouterConfig.KeyVar)) Right(seed.policy)
      else OpenRouterConfig.policy(env, seed.policy).left.map(_.message)
    }

  /** The reference deployment as `env` declares it, serving `edges` with the tools `offered`,
    * the conversations `worksIn` links working in their services and `reaches` links reaching
    * theirs, its days beginning in
    * `zone`. Serving any edge, it caps a day at [[Budgets.ServeDefault]] when
    * `GRIT_DAILY_USD` is. When Slack is served from the workspace `slackIn`
    * ([[SlackEdge.installedIn]]), it trusts the Slack attester for that workspace's accounts
    * ([[SlackAccounts.realm]]), and refuses unless one of `edges` is that attester; its
    * administrators are those `GRIT_ADMINS` names there ([[visibility]]). With
    * [[DriveKeyVar]] set, `service:drive` is shared ([[visibility]]), its results are believed
    * up to member, and Drive is cleared for the top label, since its files are already
    * Google's. The first
    * variable malformed, or the deployment refused, is the failure.
    */
  private[main] def deployment(
      env: Map[String, String],
      offered: Offered,
      edges: Vector[ServedEdge],
      worksIn: Vector[WorksIn],
      zone: java.time.ZoneId,
      slackIn: Option[TeamId] = None,
      reaches: Vector[Reaches] = Vector.empty
  ): Either[String, Deployment] = {
    val serving = edges.nonEmpty
    for {
      window <- tokens(env, BudgetVar, LinearAssembler.DefaultBudget)
      tail <- tokens(env, TailVar, RetrievalAssembler.DefaultTail)
      retrieving <- assemblerChoice(env)
      rounds <- toolRounds(env)
      models <- policy(env)
      sweep <- sweepEvery(env)
      expiry <- filesExpiry(env)
      lifecycle <- LifecycleEnv.of(env)
      spend <- Budgets.fromEnv(env, zone, if (serving) Budgets.ServeDefault else None)
      domains <- Claimed.fromEnv(env)
      trusted <- slackIn.fold[Either[String, Vector[Vouching]]](Right(Vector.empty))(team =>
        SlackAccounts
          .realm(team)
          .map(realm => Vector(Vouching(SlackAccounts.Attester, realm)))
          .left
          .map(why => s"the Slack team grit is installed in: $why")
      )
      identities <- Identities.of(trusted, domains).left.map(_.message)
      named <- admins(env)
      capped <- caps(env)
      drive <- DriveAt
      // Serving Drive, its place is shared, believed at member, and Drive cleared for the top.
      driving = env.contains(DriveKeyVar)
      visible <- visibility(
        named,
        slackIn,
        cleared(models) ++
          Option.when(driving)(Cleared(Party.Service(drive), Compartments.Shipped.top)),
        capped ++ Option.when(driving)(Cap(drive.place, Integrity.Member)),
        Option.when(driving)(drive.place).toVector
      )
      plugins <- pluginChoice(env, visible, zone, Formatted)
      deployment <- Deployment
        .of(
          visibility = visible,
          edges = edges,
          worksIn = worksIn,
          reaches = reaches,
          plugins = plugins,
          policy = models,
          offer = Offer(offered, rounds),
          assembly = if (retrieving) Assembly.Retrieval(window, tail) else Assembly.Linear(window),
          placer = placer(env),
          lifecycle = lifecycle,
          budget = spend,
          // grit's own deployment never speaks where it was not addressed; a deployment that
          // does declares its limits (ADR 0022).
          speaking = Speaking.Off,
          sweep = sweep,
          // The reference deployment is grit itself, in a terminal and in Slack alike.
          persona = Persona.Grit,
          identities = identities,
          files = FileArea.Bounds.Default.copy(expiry = expiry),
          formats = Formatted
        )
        .left
        .map(_.message)
    } yield deployment
  }
}
