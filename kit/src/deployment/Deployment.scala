package grit.kit.deployment

import scala.concurrent.duration.{DurationInt, FiniteDuration}

import grit.core.edge.ServedEdge
import grit.core.id.{EdgeName, ShadowName}
import grit.core.message.Tokens
import grit.core.model.Policy
import grit.core.period.LifecycleSettings
import grit.core.place.{Reaches, WorksIn}
import grit.core.plugin.Plugin
import grit.core.speech.Speaking
import grit.core.spend.Budget
import grit.lifecycle.shadow.ShadowVariant
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

  /** Two shadows are named `name`. */
  case ShadowRepeated(name: ShadowName)

  /** Shadows are declared, but topics are placed with no classifier, so none could be asked.
    * `topics` says why there is none.
    */
  case ShadowsUnasked(topics: String)

  def message: String = this match {
    case AsksUnanswered(edges) =>
      s"${edges.map(EdgeName.value).mkString(", ")} cannot answer a tool call that asks first, so the tools offered must be read's"
    case EdgeRepeated(name) => s"two edges are named ${EdgeName.value(name)}"
    case SweepTooOften(every) => s"the sweep, every $every, must be at least a second apart"
    case SpeaksUnjudged(topics) =>
      s"speaking unprompted needs a classifier to judge each draft, and topics are off: $topics"
    case ShadowRepeated(name) => s"two shadows are named ${ShadowName.value(name)}"
    case ShadowsUnasked(topics) =>
      s"a shadow asks the topics' classifier in its own wording, and topics are off: $topics"
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
  * records beside live triage ([[ShadowVariant]]), and how often its engine sweeps. The
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
    shadows: Vector[ShadowVariant]
)

object Deployment {

  /** The deployment of these; call it with named arguments. Refused when `offer` asks first
    * ([[Offered.All]]) and an edge cannot answer an ask, when two edges share a name, when
    * `sweep` is under a second, or when it speaks (`speaking` not Off) with `topics` Off: the
    * topics' classifier is also the judge of each draft, or when two of `shadows` share a
    * name, or any is declared with `topics` Off: each shadow asks the topics' classifier, Jev
    * (of the shadow's own model when it names one) or the stub. `lifecycle` is written over the
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
      shadows: Vector[ShadowVariant] = Vector.empty
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
      _ <- (speaking, topics) match {
        // The judge is the topics' classifier: with none, no draft could ever post.
        case (Speaking.Shadow(_) | Speaking.Within(_), Topics.Off(reason)) =>
          Left(DeploymentRefusal.SpeaksUnjudged(reason))
        case _ => Right(())
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
      shadows
    )
  }
}
