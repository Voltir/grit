package grit.kit.deployment

import scala.concurrent.duration.{DurationInt, FiniteDuration}

import grit.core.document.{Document, DocumentShelf}
import grit.core.edge.ServedEdge
import grit.core.id.{
  AttesterName,
  CorpusName,
  Declarer,
  DocKey,
  EdgeName,
  JobName,
  PluginName,
  QuestionName,
  ScheduleId,
  ShadowName
}
import grit.core.identity.{Identities, Realm}
import grit.core.job.{Declared, Job, Jobs, NotOwn}
import grit.core.message.Tokens
import grit.core.model.Policy
import grit.core.period.LifecycleSettings
import grit.core.persona.Persona
import grit.core.place.{Place, Reaches, Service, WorksIn}
import grit.core.plugin.{Plugin, PluginDocs, PluginReads, Unneeded}
import grit.core.recipe.{Offering, TurnRecipe}
import grit.core.review.Reviewing
import grit.core.speech.Speaking
import grit.core.spend.Budget
import grit.core.store.{StoreError, Tx}
import grit.core.tool.ToolName
import grit.core.triage.{Bound, Corpora, Earning, Gate, Reading}
import grit.core.visibility.{Compartment, Label, Visibility}
import grit.lifecycle.shadow.ShadowVariant
import grit.lifecycle.triage.TriageQuestions
import grit.tools.Names
import grit.turn.{TurnLoop, TurnVerdict}

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

/** A review a deployment declares ([[Deployment.of]]): `reviewing`; `gate`, the gate of the
  * question set its shadow asks, by which that shadow would draft; and `place`, where the one
  * edge that answers it posts its prompts ([[ServedEdge.reviewsAt]]).
  */
final case class ShadowReview private[deployment] (
    reviewing: Reviewing,
    gate: Gate,
    place: Place
)

/** What, beside its [[Visibility]], a deployment holds that names compartments. */
enum Requirer {

  /** The plugin `name`, by its [[Plugin.compartments]]. */
  case ByPlugin(name: PluginName)

  /** The edge `name`, by its [[ServedEdge.compartments]]. */
  case ByEdge(name: EdgeName)

  /** The declared schedule `id`, by its [[Declared.clearance]]. */
  case BySchedule(id: ScheduleId)

  /** As a person reads it in a refusal. */
  def written: String = this match {
    case ByPlugin(name) => s"the plugin ${PluginName.value(name)}"
    case ByEdge(name) => s"the edge ${EdgeName.value(name)}"
    case BySchedule(id) => s"the schedule ${ScheduleId.value(id)}'s clearance"
  }
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

  /** Speaking's gate reads `reading`, which live triage ([[Deployment.triage]]) does not
    * ask, so it would hold every message.
    */
  case SpeechUnread(reading: Reading)

  /** Live triage ([[Deployment.triage]]) does not ask [[Earning.Durable]] as a yes/no,
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
    * ([[Deployment.triage]]) does not ask with the deployment's corpora: the
    * gate would never act.
    */
  case RecipeUnread(reading: Reading)

  /** The recipe offers a service by source, but topics are placed with no classifier, and
    * live triage and a mention's weighing ask that one: every answer would be missing, and the
    * recipe would never act. `topics` says why there is none.
    */
  case RecipeUnweighed(topics: String)

  /** The recipe offers a service by source, but no corpus supplies one: no gate
    * would be read, and the recipe would never act.
    */
  case RecipeUnsourced

  /** `source` supplies `service`, which no `worksIn` or `reaches` link offers: offering by it
    * would gate nothing.
    */
  case OffersUnlinked(source: CorpusName, service: Service)

  /** The recipe draws a window of `budget` estimated tokens, wider than the assembly's
    * `window`: widening waits until a model's context can be checked against it.
    */
  case Widens(budget: Tokens, window: Tokens)

  /** Two plugins are named `name`: they would share one plugin's documents. */
  case PluginRepeated(name: PluginName)

  /** `plugin` needs `needed`, and no plugin of that name is among the deployment's, so its
    * documents would never be posted.
    */
  case PluginUnmet(plugin: PluginName, needed: PluginName)

  /** Two of the tools every turn may be offered, among the plugins' and grit's own, are named
    * `name`.
    */
  case ToolRepeated(name: ToolName)

  /** A tool of `plugin` asks for `dependency`'s service, which `plugin` does not list in its
    * needs.
    */
  case ToolUnneeded(plugin: PluginName, dependency: PluginName)

  /** A tool of `plugin` books `job`, which is not among `plugin`'s jobs. */
  case JobUnowned(plugin: PluginName, job: JobName)

  /** Two of the jobs, every plugin's and the deployment's own, are named `name`: a run names
    * its job by name alone.
    */
  case JobRepeated(name: JobName)

  /** Two declared schedules have the id `id`: two of one declarer's share a key. */
  case ScheduleRepeated(id: ScheduleId)

  /** The declared schedule `id` runs `job`, which is not the job of that name among the
    * deployment's (every plugin's and its own), so its runs would run another's code, or none.
    */
  case ScheduleJobless(id: ScheduleId, job: JobName)

  /** `plugin` declares a schedule of `job`, which is not among `plugin`'s jobs. */
  case ScheduleUnowned(plugin: PluginName, job: JobName)

  /** `by` names `compartment`, which the deployment's visibility does not declare. */
  case CompartmentUndeclared(by: Requirer, compartment: Compartment)

  /** A review is declared, and `edges` post review prompts ([[ServedEdge.reviewsAt]]): none, so
    * nothing picked would be posted, or more than one, so a prompt would be posted twice.
    */
  case ReviewUnposted(edges: Vector[EdgeName])

  /** Its identities trust `attester` for a realm, and no edge served is that attester
    * ([[ServedEdge.attester]]), so that realm's accounts would never be attested.
    */
  case AttesterUnserved(attester: AttesterName)

  /** Two edges served, `by` and `and`, say they are `attester`, so which one's source answers
    * for its realms would be a guess.
    */
  case AttesterTwice(attester: AttesterName, by: EdgeName, and: EdgeName)

  /** A group of its visibility names `realm`, which its identities trust no attester for, so
    * that group would hold none of the realm's full members, ever.
    */
  case RealmUnvouched(realm: Realm)

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
      "the recipe offers a service by source, and no corpus supplies a service, so it would never act"
    case OffersUnlinked(source, service) =>
      s"${CorpusName.value(source)} supplies ${service.name}, which no worksIn or reaches link offers"
    case Widens(budget, window) =>
      s"the recipe draws a window of ${Tokens.value(budget)} tokens, wider than the assembly's ${Tokens.value(window)}"
    case PluginRepeated(name) => s"two plugins are named ${PluginName.value(name)}"
    case PluginUnmet(plugin, needed) =>
      s"${PluginName.value(plugin)} needs ${PluginName.value(needed)}, which is not among the plugins"
    case ToolRepeated(name) => s"two tools are named ${ToolName.value(name)}"
    case ToolUnneeded(plugin, dependency) =>
      s"a tool of ${PluginName.value(plugin)} asks for ${PluginName.value(dependency)}, which it does not list among its needs"
    case JobUnowned(plugin, job) =>
      s"a tool of ${PluginName.value(plugin)} books ${JobName.value(job)}, which is not among its jobs"
    case JobRepeated(name) => s"two jobs are named ${JobName.value(name)}"
    case ScheduleRepeated(id) => s"two declared schedules are ${ScheduleId.value(id)}"
    case ScheduleJobless(id, job) =>
      s"the schedule ${ScheduleId.value(id)} runs ${JobName.value(job)}, which is not the deployment's job of that name"
    case ScheduleUnowned(plugin, job) =>
      s"${PluginName.value(plugin)} declares a schedule of ${JobName.value(job)}, which is not among its jobs"
    case CompartmentUndeclared(by, compartment) =>
      s"${by.written} names the compartment ${Compartment.name(compartment)}, which the deployment's visibility does not declare"
    case ReviewUnposted(edges) =>
      if (edges.isEmpty) "a review is declared, and no edge posts its prompts"
      else
        s"a review is declared, and ${edges.map(EdgeName.value).mkString(", ")} each post its prompts: one must"
    case AttesterUnserved(attester) =>
      s"the identities trust ${AttesterName.value(attester)} to attest a realm, and no edge served is ${AttesterName.value(attester)}"
    case AttesterTwice(attester, by, and) =>
      s"two edges served, ${EdgeName.value(by)} and ${EdgeName.value(and)}, say they are ${AttesterName.value(attester)}"
    case RealmUnvouched(realm) =>
      s"a group names the realm ${realm.namespace}:${realm.within}/, which the identities trust no attester for"
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
  * records beside live triage ([[ShadowVariant]]), the corpora its shadows'
  * question sets ask about ([[Corpora]]), and its turns' offering by the services
  * they supply, the review of one of them it picks heard messages for ([[ShadowReview]]), how
  * often its engine sweeps, the `recipe` that shapes each turn by what it answers
  * ([[TurnRecipe]]), the `persona` grit presents as: the name its turns are told
  * ([[grit.turn.TurnPrompt.called]]) and `about` reports, and the `jobs` and `schedules` it
  * declares itself beside its plugins' (ADR 0029), its `visibility`: who may see what
  * (ADR 0030), and its `identities`: the attester it trusts for each realm, and the email
  * domains it claims (ADR 0032); `allJobs` is every job, its plugins' and its own, by name, as its runs find
  * them. The
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
    persona: Persona,
    reaches: Vector[Reaches],
    shadows: Vector[ShadowVariant],
    knowledge: Corpora,
    review: Option[ShadowReview],
    recipe: TurnRecipe,
    jobs: Vector[Job[?]],
    schedules: Vector[Declared[?]],
    visibility: Visibility,
    identities: Identities,
    allJobs: Jobs
) {

  /** Every schedule declared, each with its declarer: each plugin's, then its own. */
  def declared: Vector[(Declarer, Declared[?])] = Deployment.declared(plugins, schedules)

  /** The set live triage asks for this deployment: [[TriageQuestions.shipped]] of its
    * persona.
    */
  def triage: TriageQuestions = TriageQuestions.shipped(persona)
}

object Deployment {

  /** The deployment of these; call it with named arguments. `lifecycle` is written over the
    * database's settings on every start, so a change made while grit runs (`/set`, SQL) holds
    * until the next start. What `speaking` spends is counted in `budget` as well as against its
    * own cap ([[grit.core.speech.Limits.spend]]). Refused, with one of these when it holds:
    *   - [[DeploymentRefusal.AsksUnanswered]]: `offer` asks first ([[Offered.All]]) and an edge
    *     cannot answer an ask;
    *   - [[DeploymentRefusal.EdgeRepeated]]: two edges share a name;
    *   - [[DeploymentRefusal.SweepTooOften]]: `sweep` is under a second;
    *   - [[DeploymentRefusal.DurableUnasked]]: live triage does not ask what earning reads (a
    *     build's mistake, refused for every deployment of it);
    *   - [[DeploymentRefusal.SpeaksUnjudged]]: it speaks (`speaking` not Off) with `topics`
    *     Off;
    *   - [[DeploymentRefusal.SpeechUnread]]: speaking's gate reads a question live triage does
    *     not ask;
    *   - [[DeploymentRefusal.ShadowRepeated]]: two of `shadows` share a name;
    *   - [[DeploymentRefusal.ShadowsUnasked]]: a shadow is declared with `topics` Off (each
    *     shadow asks the topics' classifier, Jev of the shadow's own model when it names one,
    *     or the stub);
    *   - [[DeploymentRefusal.ReviewUngated]]: `review` names no declared shadow;
    *   - [[DeploymentRefusal.ReviewUnspoken]]: `review` is declared with `speaking` Off;
    *   - [[DeploymentRefusal.ReviewUnposted]]: `review` is declared and not exactly one of
    *     `edges` posts its prompts ([[ServedEdge.reviewsAt]]);
    *   - [[DeploymentRefusal.RecipeUnweighed]]: `recipe` offers a service by source with
    *     `topics` Off;
    *   - [[DeploymentRefusal.RecipeUnsourced]]: `recipe` offers a service by source and no
    *     source of `knowledge` supplies one;
    *   - [[DeploymentRefusal.RecipeUnread]]: `recipe` offers a service by a gate reading what
    *     live triage does not ask;
    *   - [[DeploymentRefusal.OffersUnlinked]]: a source of `knowledge` supplies a service no
    *     `worksIn` or `reaches` link offers;
    *   - [[DeploymentRefusal.Widens]]: `recipe` draws a window wider than `assembly`'s;
    *   - [[DeploymentRefusal.PluginRepeated]]: two of `plugins` share a name;
    *   - [[DeploymentRefusal.PluginUnmet]]: a plugin needs one not among `plugins`;
    *   - [[DeploymentRefusal.ToolRepeated]]: two of the tools a turn may be offered (every
    *     plugin's, and grit's own: [[grit.tools.Names.all]] and the turn's `topic`) share a
    *     name;
    *   - [[DeploymentRefusal.ToolUnneeded]]: a plugin's tool asks for a plugin its own does not
    *     need;
    *   - [[DeploymentRefusal.JobUnowned]]: a plugin's tool books a job not its plugin's;
    *   - [[DeploymentRefusal.JobRepeated]]: two of the jobs, every plugin's and `jobs`, share a
    *     name;
    *   - [[DeploymentRefusal.ScheduleUnowned]]: a plugin declares a schedule of a job not its
    *     own;
    *   - [[DeploymentRefusal.ScheduleRepeated]]: two declared schedules share an id;
    *   - [[DeploymentRefusal.ScheduleJobless]]: a declared schedule holds a job other than the
    *     deployment's job of its name;
    *   - [[DeploymentRefusal.CompartmentUndeclared]]: a plugin's or an edge's `compartments`, or
    *     a declared schedule's `clearance`, names a compartment `visibility` does not declare;
    *   - [[DeploymentRefusal.AttesterTwice]]: two of `edges` say they are one attester
    *     ([[ServedEdge.attester]]);
    *   - [[DeploymentRefusal.AttesterUnserved]]: `identities` trusts an attester for a realm,
    *     and none of `edges` is that attester;
    *   - [[DeploymentRefusal.RealmUnvouched]]: a group of `visibility` names a realm `identities`
    *     trusts no attester for.
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
      persona: Persona,
      reaches: Vector[Reaches] = Vector.empty,
      shadows: Vector[ShadowVariant] = Vector.empty,
      knowledge: Corpora = Corpora.Empty,
      review: Option[Reviewing] = None,
      recipe: TurnRecipe = TurnRecipe.Shipped,
      jobs: Vector[Job[?]] = Vector.empty,
      schedules: Vector[Declared[?]] = Vector.empty,
      visibility: Visibility = Visibility.Shipped,
      identities: Identities = Identities.Shipped
  ): Either[DeploymentRefusal, Deployment] = {
    val names = edges.map(_.name)
    val unanswered = edges.filterNot(_.answersAsks).map(_.name)
    val live = TriageQuestions.shipped(persona)
    val pluginNames = plugins.map(_.name)
    // grit's own, then every plugin's: a name taken twice is the second one's to give up.
    val toolNames =
      Names.all ++ Vector(TurnVerdict.Name) ++ plugins.flatMap(_.tools.map(_.described.name))
    for {
      _ <- names.diff(names.distinct).headOption.map(DeploymentRefusal.EdgeRepeated(_)).toLeft(())
      // One edge per attester: two would leave which source answers to chance.
      _ <- edges
        .flatMap(e => e.attester.map(_ -> e.name))
        .groupBy(_._1)
        .toVector
        .sortBy((a, _) => AttesterName.value(a))
        .collectFirst { case (a, Vector((_, by), (_, and), _*)) =>
          DeploymentRefusal.AttesterTwice(a, by, and)
        }
        .toLeft(())
      // A realm's accounts are attested only by the attester named for it: one not served, never.
      _ <- identities.vouchings
        .map(_.attester)
        .find(a => !edges.exists(_.attester.contains(a)))
        .map(DeploymentRefusal.AttesterUnserved(_))
        .toLeft(())
      // A group taking in a realm's full members holds only those its trusted attester attests.
      _ <- visibility.groups
        .flatMap(_.realms.toVector.sortBy(r => (r.namespace, r.within)))
        .find(!identities.realms.contains(_))
        .map(DeploymentRefusal.RealmUnvouched(_))
        .toLeft(())
      _ <- pluginNames
        .diff(pluginNames.distinct)
        .headOption
        .map(DeploymentRefusal.PluginRepeated(_))
        .toLeft(())
      _ <- plugins
        .flatMap(p =>
          p.needs
            .map(_.name)
            .filterNot(pluginNames.contains)
            .map(DeploymentRefusal.PluginUnmet(p.name, _))
        )
        .headOption
        .toLeft(())
      _ <- toolNames
        .diff(toolNames.distinct)
        .headOption
        .map(DeploymentRefusal.ToolRepeated(_))
        .toLeft(())
      // Binding reads no document (it holds no transaction), so binding over none decides
      // which tools ask for a plugin their own does not need, as the engine's start would.
      _ <- PluginBinding
        .bound(plugins, _ => Unread)
        .fold(
          {
            case u: Unneeded => Left(DeploymentRefusal.ToolUnneeded(u.plugin, u.dependency))
            case n: NotOwn => Left(DeploymentRefusal.JobUnowned(n.plugin, n.job))
          },
          _ => Right(())
        )
      all <- Jobs.of(plugins.flatMap(_.jobs) ++ jobs).left.map(DeploymentRefusal.JobRepeated(_))
      _ <- plugins
        .flatMap(p =>
          p.schedules
            .map(_.job.name)
            .filterNot(n => p.jobs.exists(_.name == n))
            .map(DeploymentRefusal.ScheduleUnowned(p.name, _))
        )
        .headOption
        .toLeft(())
      cleared = declared(plugins, schedules).map((by, s) => (s.id(by), s.clearance))
      _ <- undeclared(visibility, plugins, edges, cleared).toLeft(())
      ids = declared(plugins, schedules).map((by, s) => (s.id(by), s.job))
      _ <- ids
        .map(_._1)
        .diff(ids.map(_._1).distinct)
        .headOption
        .map(DeploymentRefusal.ScheduleRepeated(_))
        .toLeft(())
      // A run finds its job by name: a schedule holding another job of that name would have
      // its parameters read by code that did not write them.
      _ <- ids
        .collectFirst {
          case (id, job) if !all.named(job.name).contains(job) =>
            DeploymentRefusal.ScheduleJobless(id, job.name)
        }
        .toLeft(())
      _ <- Either.cond(
        offer.tools == Offered.Read || unanswered.isEmpty,
        (),
        DeploymentRefusal.AsksUnanswered(unanswered)
      )
      _ <- Either.cond(sweep >= 1.second, (), DeploymentRefusal.SweepTooOften(sweep))
      // Earning reads durable by name: a live set without it would earn every period a closing.
      _ <- earning(live)
      _ <- (speaking, topics) match {
        // The judge is the topics' classifier: with none, no draft could ever post.
        case (Speaking.Shadow(_) | Speaking.Within(_), Topics.Off(reason)) =>
          Left(DeploymentRefusal.SpeaksUnjudged(reason))
        case _ => Right(())
      }
      _ <- speaking match {
        // Live triage's answers hold only what it asks: a gate reading more holds everything.
        case Speaking.Shadow(limits) => unread(live, limits.drafts)
        case Speaking.Within(limits) => unread(live, limits.drafts)
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
          for {
            v <- shadows.find(_.name == r.shadow).toRight(DeploymentRefusal.ReviewUngated(r.shadow))
            // One edge posts the prompts: none would leave every pick unposted, two post each twice.
            at <- edges.flatMap(e => e.reviewsAt.map((e.name, _))) match {
              case Vector((_, place)) => Right(place)
              case several => Left(DeploymentRefusal.ReviewUnposted(several.map(_._1)))
            }
          } yield Some(ShadowReview(r, v.questions.speak, at))
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
      _ <- read(recipe, knowledge, live)
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
      persona,
      reaches,
      shadows,
      knowledge,
      reviewed,
      recipe,
      jobs,
      schedules,
      visibility,
      identities,
      all
    )
  }

  /** Every schedule declared, each with its declarer: each of `plugins`', then `own`. */
  private def declared(
      plugins: Vector[Plugin],
      own: Vector[Declared[?]]
  ): Vector[(Declarer, Declared[?])] =
    plugins.flatMap(p => p.schedules.map(Declarer.Plugin(p.name) -> _)) ++
      own.map(Declarer.Deployment -> _)

  /** [[DeploymentRefusal.CompartmentUndeclared]] for the first compartment one of `plugins` or
    * `edges` names, or one of `cleared`'s clearances holds, that `visibility` does not declare.
    */
  private def undeclared(
      visibility: Visibility,
      plugins: Vector[Plugin],
      edges: Vector[ServedEdge],
      cleared: Vector[(ScheduleId, Label)]
  ): Option[DeploymentRefusal] = {
    val declared = visibility.compartments
    val named =
      plugins.flatMap(p => p.compartments.map((Requirer.ByPlugin(p.name), _))) ++
        edges.flatMap(e => e.compartments.map((Requirer.ByEdge(e.name), _)))
    named
      .collectFirst {
        case (by, c) if !declared.declared.contains(c) =>
          DeploymentRefusal.CompartmentUndeclared(by, c)
      }
      .orElse(
        cleared
          .flatMap((id, label) =>
            declared
              .undeclared(label)
              .map(DeploymentRefusal.CompartmentUndeclared(Requirer.BySchedule(id), _))
          )
          .headOption
      )
  }

  /** Whether `live`, the set live triage asks, asks every reading `recipe`'s gates read with
    * `knowledge`'s sources; [[DeploymentRefusal.RecipeUnread]] naming the first it does not.
    */
  private[deployment] def read(
      recipe: TurnRecipe,
      knowledge: Corpora,
      live: TriageQuestions
  ): Either[DeploymentRefusal, Unit] =
    live
      .unread(Gate.all(recipe.gates(knowledge)*), knowledge)
      .map(DeploymentRefusal.RecipeUnread(_))
      .toLeft(())

  /** No document: what binding a plugin's tools reads, when only their needs are checked. */
  private val Unread: PluginReads = PluginReads(
    new PluginDocs {
      def get(key: String)(using Tx^): Either[StoreError, Option[ujson.Value]] = Right(None)
      def newest(prefix: String, n: Int)(using
          Tx^
      ): Either[StoreError, Vector[(String, ujson.Value)]] = Right(Vector.empty)
    },
    new DocumentShelf {
      def current(key: DocKey, label: Label)(using Tx^): Either[StoreError, Option[Document]] =
        Right(None)
      def newest(n: Int)(using Tx^): Either[StoreError, Vector[Document]] = Right(Vector.empty)
    }
  )

  /** [[DeploymentRefusal.OffersUnlinked]] for the first source of `knowledge` supplying a
    * service none of `worksIn` or `reaches` names.
    */
  private def linked(
      knowledge: Corpora,
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
        Corpora.Empty
      )
      .map(_ => DeploymentRefusal.DurableUnasked)
      .toLeft(())

  private def unread(live: TriageQuestions, gate: Gate): Either[DeploymentRefusal, Unit] =
    live
      .unread(gate, Corpora.Empty)
      .map(DeploymentRefusal.SpeechUnread(_))
      .toLeft(())
}
