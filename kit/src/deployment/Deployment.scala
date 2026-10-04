package grit.kit.deployment

import scala.concurrent.duration.{DurationInt, FiniteDuration}

import grit.core.edge.ServedEdge
import grit.core.id.{EdgeName, KnowledgeSourceName, QuestionName, ShadowName}
import grit.core.message.Tokens
import grit.core.model.Policy
import grit.core.period.LifecycleSettings
import grit.core.place.{Reaches, Service, WorksIn}
import grit.core.plugin.Plugin
import grit.core.recipe.{Offering, TurnRecipe}
import grit.core.review.Reviewing
import grit.core.speech.Speaking
import grit.core.spend.Budget
import grit.core.triage.{Bound, Earning, Gate, KnowledgeSources, Reading}
import grit.lifecycle.shadow.ShadowVariant
import grit.lifecycle.triage.TriageQuestions
import grit.turn.TurnLoop

/** What every turn's model is offered, at every place, in at most `rounds` model calls. */
final case class Offer(tools: Offered, rounds: TurnLoop.Budget)

/** Which tools a turn's model is offered. */
enum Offered {

  /** `read`, `list` and `search` (`Coding.readOnly`), and `about`: nothing asks first. */
  case Read

  /** Those and `write`, `edit` and `run` (`Coding`), `propose_model_setting` (`Tuning`) and
    * `probe_pair` (`Probes`), each of which asks first; the last two only in a TUI session
    * (`Audience.operator`).
    */
  case All
}

/** How a turn's window is assembled, within `window` estimated tokens. */
enum Assembly {

  /** The recent turns that fit in `tail`, then the earlier turns a written query recalls. */
  case Retrieval(window: Tokens, tail: Tokens)

  /** The recent turns that fit, and nothing else. */
  case Linear(window: Tokens)
}

/** How a message is placed among its conversation's topics, and how each quiet period is
  * asked whether anyone is waiting on it.
  */
enum Topics {

  /** By Jev; needs `JEV_API_KEY` ([[grit.kit.environment.Secrets]]). */
  case Jev

  /** By the stub classifier, for tests and the gate: no call is made. */
  case Stub

  /** By none, for `reason`: every message after a conversation's first stays where it is. */
  case Off(reason: String)
}

/** A review a deployment declares ([[Deployment.of]]): `reviewing`, and `gate`, the gate of
  * the question set its shadow asks, by which that shadow would draft.
  */
final case class ShadowReview private[deployment] (
    reviewing: Reviewing,
    gate: Gate
)

/** Why [[Deployment.of]] refused. */
enum DeploymentRefusal {

  /** The offer asks a person first, and `edges` cannot answer an ask. */
  case AsksUnanswered(edges: Vector[EdgeName])

  /** Two edges are named `name`. */
  case EdgeRepeated(name: EdgeName)

  /** The sweep, `every`, is under a second. */
  case SweepTooOften(every: FiniteDuration)

  /** It speaks where it was not addressed, but places topics with no classifier, so no draft
    * could be judged: it would pay for drafts and never post one. `topics` says why it has none.
    */
  case SpeaksUnjudged(topics: String)

  /** Speaking's gate reads `reading`, which live triage ([[TriageQuestions.Shipped]]) does not
    * ask, so it would hold every message.
    */
  case SpeechUnread(reading: Reading)

  /** Live triage ([[TriageQuestions.Shipped]]) does not ask [[Earning.Durable]] as a yes/no,
    * so every period it heard would earn a written closing.
    */
  case DurableUnasked

  /** Two shadows are named `name`. */
  case ShadowRepeated(name: ShadowName)

  /** Shadows are declared, but topics are placed with no classifier, so none could be asked.
    * `topics` says why there is none.
    */
  case ShadowsUnasked(topics: String)

  /** The review names `shadow`, which is not declared among the shadows, so no gate says
    * when it would draft.
    */
  case ReviewUngated(shadow: ShadowName)

  /** A review is declared, but speaking is off, so live triage's gate is never reached and no
    * message could be compared.
    */
  case ReviewUnspoken

  /** The recipe offers a service by a gate reading `reading`, which live triage
    * ([[TriageQuestions.Shipped]]) does not ask with the deployment's knowledge sources: the
    * gate would never act.
    */
  case RecipeUnread(reading: Reading)

  /** The recipe offers a service by source, but topics are placed with no classifier, and
    * live triage and a mention's weighing ask that one: every answer would be missing, and the
    * recipe would never act. `topics` says why there is none.
    */
  case RecipeUnweighed(topics: String)

  /** The recipe offers a service by source, but no knowledge source supplies one: no gate
    * would be read, and the recipe would never act.
    */
  case RecipeUnsourced

  /** `source` supplies `service`, which no `worksIn` or `reaches` link offers: offering by it
    * would gate nothing.
    */
  case OffersUnlinked(source: KnowledgeSourceName, service: Service)

  /** The recipe draws a window of `budget` estimated tokens, wider than the assembly's
    * `window`: widening waits until a model's context can be checked against it.
    */
  case Widens(budget: Tokens, window: Tokens)

  def message: String = this match {
    case AsksUnanswered(edges) =>
      s"${edges.map(EdgeName.value).mkString(", ")} cannot answer a tool call that asks first, so the tools offered must be read's"
    case EdgeRepeated(name) => s"two edges are named ${EdgeName.value(name)}"
    case SweepTooOften(every) => s"the sweep, every $every, must be at least a second apart"
    case SpeaksUnjudged(topics) =>
      s"speaking unprompted needs a classifier to judge each draft, and topics are off: $topics"
    case SpeechUnread(reading) =>
      s"speaking's gate reads $reading, which live triage does not ask, so it would hold every message"
    case DurableUnasked =>
      s"live triage does not ask ${QuestionName.value(Earning.Durable)} as a yes/no, so every period it heard would earn a closing"
    case ShadowRepeated(name) => s"two shadows are named ${ShadowName.value(name)}"
    case ShadowsUnasked(topics) =>
      s"a shadow asks its question set of the topics' classifier, and topics are off: $topics"
    case ReviewUngated(shadow) =>
      s"the review names ${ShadowName.value(shadow)}, which is not a declared shadow"
    case ReviewUnspoken =>
      "a review compares a shadow's gate with live triage's, and speaking is off, so live's is never reached"
    case RecipeUnread(reading) =>
      s"the recipe offers a service by $reading, which live triage does not ask, so it would never act"
    case RecipeUnweighed(topics) =>
      s"the recipe offers a service by source, which needs a classifier to weigh each message, and topics are off: $topics"
    case RecipeUnsourced =>
      "the recipe offers a service by source, and no knowledge source supplies a service, so it would never act"
    case OffersUnlinked(source, service) =>
      s"${KnowledgeSourceName.value(source)} supplies ${service.name}, which no worksIn or reaches link offers"
    case Widens(budget, window) =>
      s"the recipe draws a window of ${Tokens.value(budget)} tokens, wider than the assembly's ${Tokens.value(window)}"
  }
}

/** A deployment of grit, declared in code: the edges it serves, which conversations with no
  * directory work in a service one of them hosts ([[WorksIn]]), which conversations'
  * addressed turns also reach a service one of them hosts ([[Reaches]]), the plugins it
  * posts to, the
  * model policy its calls are made under (laid over by the model settings the database
  * keeps), what its turns are offered and how their windows are assembled, how messages are
  * placed among topics, the lifecycle's settings, what it may spend a day, whether and within
  * what it speaks where it was not addressed (ADR 0022), the shadows of triage's question it
  * records beside live triage ([[ShadowVariant]]), the knowledge sources its shadows'
  * question sets ask about ([[KnowledgeSources]]), and its turns' offering by the services
  * they supply, the review of one of them it picks heard messages for ([[ShadowReview]]), how
  * often its engine sweeps, and the `recipe` that shapes each turn by what it answers
  * ([[TurnRecipe]]). The
  * database and the model's keys come from the environment
  * ([[grit.kit.environment.Secrets]]), and each edge's credentials from its own
  * [[ServedEdge.needs]].
  */
final case class Deployment private (
    edges: Vector[ServedEdge],
    worksIn: Vector[WorksIn],
    plugins: Vector[Plugin],
    policy: Policy,
    offer: Offer,
    assembly: Assembly,
    topics: Topics,
    lifecycle: LifecycleSettings,
    budget: Budget,
    speaking: Speaking,
    sweep: FiniteDuration,
    reaches: Vector[Reaches],
    shadows: Vector[ShadowVariant],
    knowledge: KnowledgeSources,
    review: Option[ShadowReview],
    recipe: TurnRecipe
)

object Deployment {

  /** The deployment of these; call it with named arguments. Refused when `offer` asks first
    * ([[Offered.All]]) and an edge cannot answer an ask, when two edges share a name, when
    * `sweep` is under a second, when live triage does not ask what earning reads
    * ([[DeploymentRefusal.DurableUnasked]]: a build's mistake, refused for every deployment
    * of it), or when it speaks (`speaking` not Off) with `topics` Off: the
    * topics' classifier is also the judge of each draft, or by a gate reading a question live
    * triage does not ask ([[DeploymentRefusal.SpeechUnread]]), or when two of `shadows` share a
    * name, or any is declared with `topics` Off: each shadow asks the topics' classifier, Jev
    * (of the shadow's own model when it names one) or the stub, or when `review` names no
    * declared shadow or is declared with `speaking` Off: live's
    * gate is then never reached; or when `recipe` offers a service by source with `topics` Off
    * ([[DeploymentRefusal.RecipeUnweighed]]) or with no source of `knowledge` supplying one
    * ([[DeploymentRefusal.RecipeUnsourced]]), or by a gate reading what live
    * triage does not ask ([[DeploymentRefusal.RecipeUnread]]), a source of `knowledge`
    * supplies a service no `worksIn` or `reaches` link offers
    * ([[DeploymentRefusal.OffersUnlinked]]), or `recipe` draws a window wider than
    * `assembly`'s ([[DeploymentRefusal.Widens]]). `lifecycle` is written over the
    * database's settings on every start, so a change made while grit runs (`/set`, SQL)
    * holds until the next start. What `speaking` spends is counted in `budget` as well as
    * against its own cap ([[grit.core.speech.Limits.spend]]).
    */
  def of(
      edges: Vector[ServedEdge],
      worksIn: Vector[WorksIn],
      plugins: Vector[Plugin],
      policy: Policy,
      offer: Offer,
      assembly: Assembly,
      topics: Topics,
      lifecycle: LifecycleSettings,
      budget: Budget,
      speaking: Speaking,
      sweep: FiniteDuration,
      reaches: Vector[Reaches] = Vector.empty,
      shadows: Vector[ShadowVariant] = Vector.empty,
      knowledge: KnowledgeSources = KnowledgeSources.Empty,
      review: Option[Reviewing] = None,
      recipe: TurnRecipe = TurnRecipe.Shipped
  ): Either[DeploymentRefusal, Deployment] = {
    val names = edges.map(_.name)
    val unanswered = edges.filterNot(_.answersAsks).map(_.name)
    for {
      _ <- names.diff(names.distinct).headOption.map(DeploymentRefusal.EdgeRepeated(_)).toLeft(())
      _ <- Either.cond(
        offer.tools == Offered.Read || unanswered.isEmpty,
        (),
        DeploymentRefusal.AsksUnanswered(unanswered)
      )
      _ <- Either.cond(sweep >= 1.second, (), DeploymentRefusal.SweepTooOften(sweep))
      // Earning reads durable by name: a live set without it would earn every period a closing.
      _ <- earning(TriageQuestions.Shipped)
      _ <- (speaking, topics) match {
        // The judge is the topics' classifier: with none, no draft could ever post.
        case (Speaking.Shadow(_) | Speaking.Within(_), Topics.Off(reason)) =>
          Left(DeploymentRefusal.SpeaksUnjudged(reason))
        case _ => Right(())
      }
      _ <- speaking match {
        // Live triage's answers hold only what it asks: a gate reading more holds everything.
        case Speaking.Shadow(limits) => unread(limits.drafts)
        case Speaking.Within(limits) => unread(limits.drafts)
        case Speaking.Off => Right(())
      }
      shadowNames = shadows.map(_.name)
      _ <- shadowNames
        .diff(shadowNames.distinct)
        .headOption
        .map(DeploymentRefusal.ShadowRepeated(_))
        .toLeft(())
      _ <- topics match {
        // A shadow asks the topics' classifier: with none, it could never be asked.
        case Topics.Off(reason) if shadows.nonEmpty =>
          Left(DeploymentRefusal.ShadowsUnasked(reason))
        case _ => Right(())
      }
      reviewed <- review match {
        case None => Right(None)
        // Live's gate is never reached with speaking off: every decision is Silence.Off.
        case Some(_) if speaking == Speaking.Off => Left(DeploymentRefusal.ReviewUnspoken)
        case Some(r) =>
          shadows
            .find(_.name == r.shadow)
            .map(v => Some(ShadowReview(r, v.questions.speak)))
            .toRight(DeploymentRefusal.ReviewUngated(r.shadow))
      }
      bySource = Vector(recipe.heard.focused, recipe.heard.open, recipe.addressed)
        .exists(_.offering != Offering.All)
      _ <- (bySource, topics) match {
        // Live triage and a mention's weighing ask the topics' classifier: with none, no answer.
        case (true, Topics.Off(reason)) => Left(DeploymentRefusal.RecipeUnweighed(reason))
        case _ => Right(())
      }
      _ <- Either.cond(
        !bySource || knowledge.supplied.nonEmpty,
        (),
        DeploymentRefusal.RecipeUnsourced
      )
      _ <- read(recipe, knowledge, TriageQuestions.Shipped)
      _ <- linked(knowledge, worksIn, reaches)
      assembled = window(assembly)
      _ <- recipe.widens(assembled).map(DeploymentRefusal.Widens(_, assembled)).toLeft(())
    } yield Deployment(
      edges,
      worksIn,
      plugins,
      policy,
      offer,
      assembly,
      topics,
      lifecycle,
      budget,
      speaking,
      sweep,
      reaches,
      shadows,
      knowledge,
      reviewed,
      recipe
    )
  }

  /** Whether `live`, the set live triage asks, asks every reading `recipe`'s gates read with
    * `knowledge`'s sources; [[DeploymentRefusal.RecipeUnread]] naming the first it does not.
    */
  private[deployment] def read(
      recipe: TurnRecipe,
      knowledge: KnowledgeSources,
      live: TriageQuestions
  ): Either[DeploymentRefusal, Unit] =
    live
      .unread(Gate.all(recipe.gates(knowledge)*), knowledge)
      .map(DeploymentRefusal.RecipeUnread(_))
      .toLeft(())

  /** [[DeploymentRefusal.OffersUnlinked]] for the first source of `knowledge` supplying a
    * service none of `worksIn` or `reaches` names.
    */
  private def linked(
      knowledge: KnowledgeSources,
      worksIn: Vector[WorksIn],
      reaches: Vector[Reaches]
  ): Either[DeploymentRefusal, Unit] = {
    val offered = worksIn.map(_.service).toSet ++ reaches.map(_.service)
    knowledge.all
      .flatMap(source =>
        source.supplies.filterNot(offered).map(DeploymentRefusal.OffersUnlinked(source.name, _))
      )
      .headOption
      .toLeft(())
  }

  /** The most estimated tokens `assembly` draws a window within. */
  private def window(assembly: Assembly): Tokens = assembly match {
    case Assembly.Retrieval(window, _) => window
    case Assembly.Linear(window) => window
  }

  /** Whether `live`, the set live triage asks, asks [[Earning.Durable]] as a yes/no;
    * [[DeploymentRefusal.DurableUnasked]] when it does not.
    */
  private[deployment] def earning(live: TriageQuestions): Either[DeploymentRefusal, Unit] =
    live
      .unread(
        Gate.bounds(Bound.AtLeast(Reading.Yes(Earning.Durable), Earning.DurableAt)),
        KnowledgeSources.Empty
      )
      .map(_ => DeploymentRefusal.DurableUnasked)
      .toLeft(())

  private def unread(gate: Gate): Either[DeploymentRefusal, Unit] =
    TriageQuestions.Shipped
      .unread(gate, KnowledgeSources.Empty)
      .map(DeploymentRefusal.SpeechUnread(_))
      .toLeft(())
}
