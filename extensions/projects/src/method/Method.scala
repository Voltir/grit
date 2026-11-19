package grit.projects.method

import scala.annotation.tailrec

import grit.core.id.Sha256
import grit.projects.artifact.{Artifact, ArtifactHead, Body, Project}
import grit.projects.name.{
  ArtifactKey,
  CheckName,
  Handle,
  KindName,
  MethodName,
  Pin,
  ProjectKey,
  RoleName,
  StateName
}

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

  /** `key` moved to `to` by `by` over `artifacts`, its project's, when a transition allows it,
    * its movers include `by` and each of its checks holds, its action done; or why not. The moved
    * artifacts keep their heads' `by`: the mover records itself.
    */
  def move(
      artifacts: Vector[Artifact],
      key: ArtifactKey,
      to: StateName,
      by: Mover
  ): Either[MoveRefusal, Moved] =
    for {
      a <- artifacts.find(_.head.key == key).toRight(MoveRefusal.Missing(key))
      leaving = from(a.head.kind, a.head.state)
      transition <- leaving
        .find(_.to == to)
        .toRight(MoveRefusal.NoTransition(a.head, to, leaving.map(_.to).distinct))
      _ <- Either.cond(
        transition.movers == Movers.Anyone || by == Mover.Person,
        (),
        MoveRefusal.PersonOnly(a.head, to)
      )
      heads = artifacts.map(_.head)
      failed = transition.checks.flatMap { name =>
        checks.find(_.name == name) match {
          case Some(check) => check.holds(a, heads, kinds).left.toOption.map(name -> _)
          case None => Some(name -> "it is not compiled in")
        }
      }
      _ <- Either.cond(failed.isEmpty, (), MoveRefusal.Unheld(a.head, to, failed))
      moved = Artifact(a.head.copy(state = to), a.body)
      done <- transition.action match {
        case TransitionAction.Nothing => Right(Moved(moved, None))
        case TransitionAction.MergeDeltas(kind) =>
          for {
            into <- a.head.cites.distinct
              .filter(_ != key)
              .flatMap(k => artifacts.find(_.head.key == k))
              .filter(_.head.kind == kind) match {
              case Vector(one) => Right(one)
              case Vector() => Left(MoveRefusal.NoInto(a.head, to, kind))
              case several =>
                Left(MoveRefusal.IntoSeveral(a.head, to, kind, several.map(_.head.key)))
            }
            body <- Body
              .merged(into.body, a.body)
              .left
              .map(MoveRefusal.Unmerged(a.head, to, into.head, _))
          } yield Moved(moved, Option.when(body != into.body)(Artifact(into.head, body)))
      }
    } yield done

  /** `project` pinned to this method, each artifact given its kind and state under `remap`, its
    * title, body, links and `by` kept; or every reason not, joined by `; `: `project` pinned to
    * another method, or to this version under another digest (its rules changed: declare a new
    * version); an override in `remap.artifacts` for a key `project` lacks; artifacts that would
    * be of a kind this method does not declare, or in a state their kind does not declare, named
    * by kind or state; what [[Project.of]] refuses; or what [[nested]] refuses. No transition's
    * checks are asked: they guard moves, not the states artifacts are in.
    */
  def migrated(project: Project, remap: Remap): Either[String, Migrated] = {
    val held = project.artifacts.map(_.head.key).toSet
    val remapped = project.artifacts.map { a =>
      val h = a.head
      Artifact(
        h.copy(
          kind = remap.artifacts.getOrElse(h.key, remap.kinds.getOrElse(h.kind, h.kind)),
          state = remap.states.getOrElse((h.kind, h.state), h.state)
        ),
        a.body
      )
    }
    def keys(as: Vector[Artifact]): String =
      as.sortBy(_.head.key.number).map(_.head.called).mkString(", ")
    val ours = s"${MethodName.value(name)} version $version"
    val pinned = project.pin
    val reasons =
      (if (pinned.method != name)
         Vector(
           s"project ${ProjectKey.value(project.key)} holds to method " +
             s"${MethodName.value(pinned.method)}, not ${MethodName.value(name)}"
         )
       else if (pinned.version == version && pinned.digest != digest)
         Vector(Method.changed(project.key, this))
       else Vector()) ++
        remap.artifacts.keys.filterNot(held).toVector.sorted.map(_.value).map { k =>
          s"project ${ProjectKey.value(project.key)} has no artifact $k"
        } ++
        remapped
          .filterNot(a => kinds.exists(_.name == a.head.kind))
          .groupBy(_.head.kind)
          .toVector
          .sortBy((k, _) => KindName.value(k))
          .map((k: KindName, as: Vector[Artifact]) =>
            s"${keys(as)} would be of kind ${KindName.value(k)}, which $ours does not declare"
          ) ++
        remapped
          .filter(a => kinds.exists(k => k.name == a.head.kind && !k.all.contains(a.head.state)))
          .groupBy(a => (a.head.kind, a.head.state))
          .toVector
          .sortBy((ks, _) => (KindName.value(ks._1), StateName.value(ks._2)))
          .map((ks: (KindName, StateName), as: Vector[Artifact]) =>
            s"${keys(as)} would be in state ${StateName.value(ks._2)}, which kind " +
              s"${KindName.value(ks._1)} of $ours does not declare"
          )
    for {
      _ <- Either.cond(reasons.isEmpty, (), reasons.mkString("; "))
      pin <- Pin.of(name, version, digest)
      done <- Project.of(project.key, pin, remapped)
      _ <- nested(done)
    } yield Migrated(
      done,
      project.artifacts
        .zip(remapped)
        .collect {
          case (was, now) if was.head != now.head => now.head.key
        }
        .sorted
    )
  }

  /** `project`, or why not, every reason joined by `; `: an artifact whose kind's nesting refuses
    * its parent, or its having none. A kind this method does not declare is not judged. Reading
    * a stored or exported project judges no nesting, so a project pinned to this method may hold
    * an artifact this refuses.
    */
  def nested(project: Project): Either[String, Project] =
    project.artifacts.sortBy(_.head.key.number).flatMap(misplaced(project, _)) match {
      case Vector() => Right(project)
      case some => Left(some.mkString("; "))
    }

  /** Why `a` may not sit where it does in `project` under its kind's nesting, or nothing. */
  private[projects] def misplaced(project: Project, a: Artifact): Option[String] = {
    val parent = a.head.parent.flatMap(p => project.artifacts.find(_.head.key == p))
    def kindOf(h: ArtifactHead): String = s"${h.called}, of kind ${KindName.value(h.kind)}"
    val where = parent.fold("sit at the project's root")(p => s"sit under ${kindOf(p.head)}")
    kinds
      .find(_.name == a.head.kind)
      .map(_.nesting)
      .flatMap {
        case Nesting.Anywhere => None
        case Nesting.Root => parent.map(_ => "it may sit only at the project's root")
        case Nesting.Under(under) =>
          Option.when(!parent.exists(p => under.contains(p.head.kind)))(
            s"it may sit under ${under.toVector.map(KindName.value).sorted.mkString(" or ")}"
          )
      }
      .map(why => s"${kindOf(a.head)}, cannot $where: $why")
  }

  /** The digest of its data and of each check's name and version: equal for equal methods,
    * whatever order their kinds, transitions, role views, states and checks are declared in. A
    * view carrying nothing or covering nothing, a transition anyone makes, or a kind that may sit
    * anywhere, adds nothing to the digest.
    */
  def digest: Sha256 = {
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
        ).mkString + (k.nesting match {
          case Nesting.Anywhere => ""
          case Nesting.Root => field("root")
          case Nesting.Under(under) => field("under") + names(under.map(KindName.value))
        })
      } ++ transitions.map { t =>
        "transition " + Vector(
          field(KindName.value(t.kind)),
          field(StateName.value(t.from)),
          field(StateName.value(t.to)),
          names(t.checks.map(CheckName.value)),
          field(t.action.digested)
        ).mkString + (t.movers match {
          case Movers.Anyone => ""
          case Movers.Person => field("people")
        })
      } ++ views.map { v =>
        "view " + field(RoleName.value(v.role)) + names(v.kinds.map(KindName.value)) + field(
          v.depth.toString
        ) + (if (v.carried.isEmpty) "" else names(v.carried.map(Handle.value))) +
          (if (v.covers == Cover.Under) field("covers") else "")
      } ++ checks.map(c => "check " + field(CheckName.value(c.name)) + field(c.version.toString))
    Sha256.text(
      (Vector(field(MethodName.value(name)), field(version.toString)) ++ lines.sorted)
        .mkString("\n")
    )
  }
}

object Method {

  /** Why `project`, pinned to `method`'s name and version under another digest, may neither move
    * nor migrate to that version.
    */
  private[projects] def changed(project: ProjectKey, method: Method): String =
    s"project ${ProjectKey.value(project)} holds to method ${MethodName.value(method.name)} " +
      s"version ${method.version}, whose rules have changed since the project started; " +
      "declare a new version of the method rather than changing this one"

  /** These as a method, or every reason not ([[MethodRefusal]]): a version under 1; a kind,
    * state, role or check named twice; an end state, a transition (its action's kind among them),
    * a role view or a kind's nesting naming a kind or state not declared; a transition from an end state; a check
    * not in `compiled`; a state its kind's first cannot reach.
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
        checksTwice(compiled) ++
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
        (transitions.collect { case Transition(_, _, _, _, TransitionAction.MergeDeltas(into), _) =>
          into
        } ++ views.flatMap(_.kinds.toVector.sortBy(KindName.value)) ++
          kinds.flatMap(_.nesting match {
            case Nesting.Under(under) => under.toVector.sortBy(KindName.value)
            case Nesting.Anywhere | Nesting.Root => Vector.empty
          })).distinct
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

  /** Each name two of `compiled` share, once, in order of its second. */
  private[projects] def checksTwice(
      compiled: Vector[TransitionCheck]
  ): Vector[MethodRefusal.CheckTwice] =
    twice(compiled.map(_.name)).map(MethodRefusal.CheckTwice(_))

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

/** An allowed move: the artifact in its new state, and the one its transition's action changed,
  * as changed, if any.
  */
final case class Moved(artifact: Artifact, changed: Option[Artifact]) extends caps.Pure

/** How a project's artifacts are renamed onto another method version, each looked up by what the
  * artifact is before: `artifacts` gives one artifact its new kind, over what `kinds` gives every
  * artifact of a kind; `states` gives a state its new name, keyed by the artifact's kind before,
  * whatever kind it is given. A kind or state no entry names keeps its name.
  */
final case class Remap(
    kinds: Map[KindName, KindName] = Map.empty,
    states: Map[(KindName, StateName), StateName] = Map.empty,
    artifacts: Map[ArtifactKey, KindName] = Map.empty
) extends caps.Pure

/** What [[Method.migrated]] leaves: the project pinned to the method, every artifact in it; and
  * `renamed`, the keys of those whose kind or state changed, in key order.
  */
final case class Migrated(project: Project, renamed: Vector[ArtifactKey]) extends caps.Pure

/** Why an artifact may not move. */
enum MoveRefusal extends caps.Pure {

  /** The project holds no artifact of this key. */
  case Missing(key: ArtifactKey)

  /** No transition of its kind leads from its state to `to`; `may`, the states one leads to. */
  case NoTransition(head: ArtifactHead, to: StateName, may: Vector[StateName])

  /** These of its transition's checks did not hold, each with what failed. */
  case Unheld(head: ArtifactHead, to: StateName, failed: Vector[(CheckName, String)])

  /** Its transition merges deltas into the one `wanted` its head cites, and it cites none. */
  case NoInto(head: ArtifactHead, to: StateName, wanted: KindName)

  /** Its transition merges deltas into the one `wanted` its head cites, and it cites each of
    * `cited`, several.
    */
  case IntoSeveral(head: ArtifactHead, to: StateName, wanted: KindName, cited: Vector[ArtifactKey])

  /** Its deltas merged into `into` are no body: `why`, a stored form over [[Body.StoredMost]]. */
  case Unmerged(head: ArtifactHead, to: StateName, into: ArtifactHead, why: String)

  /** Its transition is made only by [[Mover.Person]] ([[Movers.Person]]), and its mover is not. */
  case PersonOnly(head: ArtifactHead, to: StateName)
}

/** Who is making a move, as the plugin reads its caller: a person, or any other mover (an agent,
  * or a caller not believed a person).
  */
enum Mover extends caps.Pure {
  case Person, Other
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

  /** A transition, a role view or a kind's nesting names a kind not declared. */
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
