package grit.models

import java.net.URI
import java.time.Duration

import grit.core.model.{
  Assignment,
  Effort,
  Grade,
  Ladder,
  ModelId,
  ModelRef,
  Pinned,
  Policy,
  ReasoningReplay,
  Rung,
  Upstream
}
import grit.core.secret.Variable

/** How grit reaches OpenRouter for one role, its key aside ([[OpenRouterProvider]] takes it):
  * `upstream` alone serves its calls, or whichever upstream OpenRouter picks when `None`;
  * `effort` is asked of the model when set; earlier reasoning goes back as `replay` says.
  */
final case class OpenRouterConfig(
    model: String,
    maxTokens: Int,
    endpoint: URI,
    timeout: Duration,
    upstream: Option[Upstream] = None,
    effort: Option[Effort] = None,
    replay: ReasoningReplay = ReasoningReplay.Details
)

object OpenRouterConfig {

  val KeyVar: Variable = Variable("OPENROUTER_API_KEY")

  val Endpoint: URI = URI.create("https://openrouter.ai/api/v1/chat/completions")

  enum Invalid {
    case Missing(variable: String)
    case NotPositive(variable: String)
    case NotAModel(variable: String)
    case NotAnUpstream(variable: String)
    case NotAnEffort(variable: String)

    /** Names the variable, never its value. */
    def message: String = this match {
      case Missing(v) => s"$v is not set"
      case NotPositive(v) => s"$v is not a positive whole number"
      case NotAModel(v) => s"$v is not an OpenRouter model id, such as openai/gpt-oss-120b"
      case NotAnUpstream(v) =>
        s"$v is not one OpenRouter upstream slug, such as open-inference/fp8"
      case NotAnEffort(v) => s"$v is not one of minimal, low, medium, high, xhigh, max"
    }
  }

  /** `policy` with each role's variables ([[ModelRole]]) laid over its ladder for this run; a
    * blank variable is unset. A grade's model variable names the model of the role's rungs at
    * that grade and leaves it open unless its upstream variable names one upstream; its upstream
    * variable alone keeps their model. A grade the ladder has no rung at gets one, after its
    * others, when its model variable is set: the first rung's budget and effort, that pair. A
    * summary or query role that sets neither standard variable follows the turn's standard
    * model and upstream whenever the turn's standard variables set either; a heard role that
    * sets neither follows the summary's, as laid, whenever the summary's or the turn's do. The
    * dream role follows no other: only its own variables lay over its ladder. A budget variable
    * replaces only the standard rungs' budget. `Missing` names the model variable when a grade
    * the ladder has no rung at sets only its upstream or budget. An effort variable replaces the
    * standard rung's effort, and a role that follows the turn's (or the summary's) standard model
    * follows its effort too, unless its own effort variable is set; an effort variable on a
    * ladder with no standard rung changes nothing.
    */
  def policy(env: Map[String, String], policy: Policy): Either[Invalid, Policy] = {
    def set(variable: String): Option[String] = env.get(variable).map(_.trim).filter(_.nonEmpty)
    def names(role: ModelRole) = set(role.modelVar).isDefined || set(role.upstreamVar).isDefined

    def named(role: ModelRole, grade: Grade): Either[Invalid, (Option[ModelId], Option[Upstream])] =
      for {
        model <- set(role.modelVar(grade)) match {
          case None => Right(None)
          case Some(m) =>
            ModelId.of(m).map(Some(_)).toRight(Invalid.NotAModel(role.modelVar(grade)))
        }
        upstream <- set(role.upstreamVar(grade)) match {
          case None => Right(None)
          case Some(u) =>
            Upstream.of(u).map(Some(_)).toRight(Invalid.NotAnUpstream(role.upstreamVar(grade)))
        }
      } yield (model, upstream)

    def laid(own: ModelRef, named: (Option[ModelId], Option[Upstream])): ModelRef = named match {
      case (None, None) => own
      case (Some(m), u) => ModelRef(m, u)
      case (None, Some(u)) => own.copy(upstream = Some(u))
    }

    def budget(role: ModelRole): Either[Invalid, Option[Int]] =
      set(role.maxTokensVar) match {
        case None => Right(None)
        case Some(raw) =>
          raw.toIntOption.filter(_ > 0).map(Some(_)).toRight(Invalid.NotPositive(role.maxTokensVar))
      }

    def effort(role: ModelRole): Either[Invalid, Option[Effort]] =
      set(role.effortVar) match {
        case None => Right(None)
        case Some(e) =>
          Effort.values
            .find(_.toString.equalsIgnoreCase(e))
            .map(Some(_))
            .toRight(Invalid.NotAnEffort(role.effortVar))
      }

    def graded(
        role: ModelRole,
        ladder: Ladder,
        grade: Grade,
        pair: (Option[ModelId], Option[Upstream]),
        max: Option[Int],
        effort: Option[Option[Effort]]
    ): Either[Invalid, Ladder] =
      if (ladder.rungs.exists(_.grade == grade))
        Right(ladder.map { r =>
          if (r.grade != grade) r
          else
            r.copy(assignment =
              r.assignment.copy(
                ref = laid(r.assignment.ref, pair),
                maxTokens = max.getOrElse(r.assignment.maxTokens),
                effort = effort.getOrElse(r.assignment.effort)
              )
            )
        })
      else
        pair match {
          case (Some(m), u) =>
            val first = ladder.first.assignment
            Right(
              ladder :+ Rung(
                grade,
                first.copy(
                  ref = ModelRef(m, u),
                  maxTokens = max.getOrElse(first.maxTokens),
                  effort = effort.getOrElse(first.effort)
                )
              )
            )
          case (None, None) if max.isEmpty => Right(ladder)
          case _ => Left(Invalid.Missing(role.modelVar(grade)))
        }

    def ladder(role: ModelRole, own: Ladder, turn: Option[Assignment]): Either[Invalid, Ladder] =
      for {
        ownEffort <- effort(role)
        laddered <- Grades.foldLeft[Either[Invalid, Ladder]](Right(own)) { (acc, grade) =>
          for {
            l <- acc
            ownPair <- named(role, grade)
            followed = (grade, turn) match {
              case (Grade.Standard, Some(t)) if role != ModelRole.Turn && !names(role) =>
                Some(t)
              case _ => None
            }
            pair = followed.fold(ownPair)(t => (Some(t.ref.model), t.ref.upstream))
            max <- if (grade == Grade.Standard) budget(role) else Right(None)
            // A standard rung's effort: the role's own variable, else the one it follows.
            setEffort = (grade, ownEffort, followed) match {
              case (Grade.Standard, Some(e), _) => Some(Some(e))
              case (Grade.Standard, None, Some(t)) => Some(t.effort)
              case _ => None
            }
            next <- graded(role, l, grade, pair, max, setEffort)
          } yield next
        }
      } yield laddered

    def standard(l: Ladder): Option[Assignment] =
      l.rungs.find(_.grade == Grade.Standard).map(_.assignment)

    for {
      turn <- ladder(ModelRole.Turn, policy.turn, None)
      followed = if (names(ModelRole.Turn)) standard(turn) else None
      summary <- ladder(ModelRole.Summary, policy.summary, followed)
      query <- ladder(ModelRole.Query, policy.query, followed)
      written = if (names(ModelRole.Summary) || names(ModelRole.Turn)) standard(summary) else None
      heard <- ladder(ModelRole.Heard, policy.heard, written)
      dream <- ladder(ModelRole.Dream, policy.dream, None)
    } yield Policy(turn, summary, query, heard, dream)
  }

  /** The grades whose variables are read, the standard first: a rung another grade adds
    * copies the first rung as the standard variables left it.
    */
  private val Grades: Vector[Grade] =
    Grade.Standard +: Grade.values.toVector.filter(_ != Grade.Standard)

  /** A role's configuration: its pinned assignment's model, budget, upstream and effort, its
    * settings' reasoning replay, and a five-minute timeout.
    */
  def of(pinned: Pinned): OpenRouterConfig = {
    val a = pinned.assignment
    OpenRouterConfig(
      ModelId.value(a.ref.model),
      a.maxTokens,
      Endpoint,
      Duration.ofMinutes(5),
      a.ref.upstream,
      a.effort,
      pinned.settings.replay
    )
  }

  /** `role`'s configuration for this run: the [[Seed]]'s policy with `env` laid over it
    * ([[policy]]); `Left` says what is wrong, naming no value. Reads no key.
    */
  def forRole(env: Map[String, String], role: ModelRole): Either[String, OpenRouterConfig] =
    for {
      seed <- Seed.catalog
      p <- policy(env, seed.policy).left.map(_.message)
    } yield of(role.in(seed.withPolicy(p)).first)
}
