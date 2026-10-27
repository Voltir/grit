package grit.projects.method

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import scala.annotation.tailrec

import grit.projects.name.{CheckName, KindName, MethodName, RoleName, StateName}

/** A methodology version (ADR 0049): the kinds of artifact a project of it holds, the states
  * each passes through, the transitions between them, what each role is shown, and the compiled
  * checks its transitions name, each once.
  */
final case class Method private (
    name: MethodName,
    version: Int,
    kinds: Vector[ArtifactKind],
    transitions: Vector[Transition],
    views: Vector[RoleView],
    checks: Vector[TransitionCheck]
) extends caps.Pure {

  /** The transitions an artifact of `kind` in `state` may take, in declared order; none from an
    * end state.
    */
  def from(kind: KindName, state: StateName): Vector[Transition] =
    transitions.filter(t => t.kind == kind && t.from == state)

  /** A SHA-256 digest, 64 lowercase hex digits, of its data and of each check's name and
    * version: equal for equal methods, whatever order their kinds, transitions, role views,
    * states and checks are declared in.
    */
  def digest: String = {
    def field(s: String): String = s"${s.length}:$s"
    def names(ns: Iterable[String]): String = field(ns.toVector.sorted.map(field).mkString)
    val lines =
      kinds.map { k =>
        "kind " + Vector(
          field(KindName.value(k.name)),
          field(StateName.value(k.first)),
          names(k.states.map(StateName.value)),
          names(k.ends.map(StateName.value)),
          field(k.sections.map(field).mkString)
        ).mkString
      } ++ transitions.map { t =>
        "transition " + Vector(
          field(KindName.value(t.kind)),
          field(StateName.value(t.from)),
          field(StateName.value(t.to)),
          names(t.checks.map(CheckName.value)),
          field(t.action.toString)
        ).mkString
      } ++ views.map { v =>
        "view " + field(RoleName.value(v.role)) + names(v.kinds.map(KindName.value)) + field(
          v.depth.toString
        )
      } ++ checks.map(c => "check " + field(CheckName.value(c.name)) + field(c.version.toString))
    val whole = (Vector(field(MethodName.value(name)), field(version.toString)) ++ lines.sorted)
      .mkString("\n")
    MessageDigest
      .getInstance("SHA-256")
      .digest(whole.getBytes(StandardCharsets.UTF_8))
      .map(b => f"${b & 0xff}%02x")
      .mkString
  }
}

object Method {

  /** These as a method, or every reason not ([[MethodRefusal]]): a version under 1; a kind,
    * state, role or check named twice; an end state, a transition or a role view naming a kind
    * or state not declared; a transition from an end state; a check not in `compiled`; a state
    * its kind's first cannot reach.
    */
  def of(
      name: MethodName,
      version: Int,
      kinds: Vector[ArtifactKind],
      transitions: Vector[Transition],
      views: Vector[RoleView],
      compiled: Vector[TransitionCheck]
  ): Either[Vector[MethodRefusal], Method] = {
    import MethodRefusal.*
    val declared = kinds.map(k => k.name -> k).toMap
    val named = transitions.flatMap(_.checks).distinct
    val refusals =
      Option.when(version < 1)(VersionUnderOne(version)).toVector ++
        twice(kinds.map(_.name)).map(KindTwice(_)) ++
        kinds.flatMap { k =>
          twice(k.all).map(StateTwice(k.name, _)) ++
            k.ends.toVector
              .sortBy(StateName.value)
              .filterNot(k.all.contains)
              .map(UndeclaredState(k.name, _))
        } ++
        twice(views.map(_.role)).map(RoleTwice(_)) ++
        twice(compiled.map(_.name)).map(CheckTwice(_)) ++
        transitions.flatMap { t =>
          declared.get(t.kind) match {
            case None => Vector(UndeclaredKind(t.kind))
            case Some(k) =>
              Vector(t.from, t.to).distinct
                .filterNot(k.all.contains)
                .map(UndeclaredState(k.name, _)) ++
                Option.when(k.ends(t.from))(FromEnd(k.name, t.from))
          }
        } ++
        views
          .flatMap(_.kinds.toVector.sortBy(KindName.value))
          .distinct
          .filterNot(declared.contains)
          .map(UndeclaredKind(_)) ++
        named.filterNot(compiled.map(_.name).contains).map(NotCompiled(_)) ++
        kinds.flatMap { k =>
          val reached = reachable(k, transitions)
          k.states.distinct.filterNot(reached).map(Unreachable(k.name, _))
        }
    if (refusals.nonEmpty) Left(refusals.distinct)
    else
      Right(
        new Method(
          name,
          version,
          kinds,
          transitions,
          views,
          compiled.filter(c => named.contains(c.name)).sortBy(c => CheckName.value(c.name))
        )
      )
  }

  /** Each name `names` holds more than once, once, in order of its second. */
  private def twice[N](names: Vector[N]): Vector[N] =
    names.zipWithIndex.collect { case (n, i) if names.take(i).contains(n) => n }.distinct

  /** The states of `kind` its transitions reach from its first. */
  private def reachable(kind: ArtifactKind, transitions: Vector[Transition]): Set[StateName] = {
    val edges = transitions.filter(_.kind == kind.name)
    @tailrec def from(seen: Set[StateName], frontier: Set[StateName]): Set[StateName] = {
      val next = edges.filter(t => frontier(t.from)).map(_.to).toSet -- seen
      if (next.isEmpty) seen else from(seen ++ next, next)
    }
    from(Set(kind.first), Set(kind.first))
  }
}

/** Why some data is not a method. */
enum MethodRefusal extends caps.Pure {

  /** Its version is under 1. */
  case VersionUnderOne(version: Int)

  /** Two kinds share a name. */
  case KindTwice(kind: KindName)

  /** A kind names a state twice. */
  case StateTwice(kind: KindName, state: StateName)

  /** Two role views share a role. */
  case RoleTwice(role: RoleName)

  /** Two compiled checks share a name. */
  case CheckTwice(check: CheckName)

  /** A transition or a role view names a kind not declared. */
  case UndeclaredKind(kind: KindName)

  /** An end state or a transition names a state its kind does not declare. */
  case UndeclaredState(kind: KindName, state: StateName)

  /** A transition leaves an end state. */
  case FromEnd(kind: KindName, state: StateName)

  /** A transition names a check not compiled in. */
  case NotCompiled(check: CheckName)

  /** No transition reaches this state from its kind's first. */
  case Unreachable(kind: KindName, state: StateName)
}
