package grit.projects.plugin

import scala.concurrent.duration.*

import grit.core.document.{DocLabel, DocWeight, DocumentTerms}
import grit.core.edge.{WatchFor, Watched}
import grit.core.id.{DocKey, PluginName}
import grit.core.plugin.{DocumentPosting, Documents, Plugin, PluginCommand}
import grit.projects.artifact.ArtifactJson
import grit.projects.method.{Example, Method, MethodBench}
import grit.projects.name.{ArtifactKey, KindName, MethodName, StateName}

/** The projects plugin (ADR 0049): projects of `methods`, new ones under the latest version of
  * `default` unless another is named, kept under [[Projects.Terms]], and changed only by its
  * commands: `project_new`, `project_keep` (never a state, and refused when it would put the
  * brief of a claimed artifact over [[grit.core.document.DocRef.BriefChars]]), `project_move` (only as the
  * project's pinned method allows, refused once that method's digest has changed),
  * `project_batch` (keeps and moves of one project, all or none, or a dry run of them),
  * `project_claim` and `project_release` (a claim on an artifact, one of the plugin's claims and
  * never a document, so the artifact keeps its version; a claim keeps the artifact's brief for its
  * holder as a document of its own ([[grit.core.document.DocRef.brief]], at most
  * [[grit.core.document.DocRef.BriefChars]],
  * none when over), which a release withdraws, claimed still or not), `project_withdraw` (an artifact no
  * other names, or with what is under it, ended citers rewritten; its history kept as of an
  * instant), `project_migrate` (a project moved to another version of its method, every artifact
  * kept again, renamed as asked), and the reading `project_show`, `project_history`, `project_list`, `project_find`
  * (ended artifacts only when asked), `project_stale` (what no live work names, untouched for
  * weeks), `project_export`, `project_claims` and `project_brief`. Each write records its writer
  * as the artifact's `by`, and a claim its claimer. Which of them a turn is offered, and what a
  * turn may not do with them, is `turns`'s.
  */
final class Projects private (methods: Vector[Method], default: MethodName, turns: TurnCommands)
    extends Plugin {
  val name: PluginName = Projects.Name
  val version: Int = 1
  override val documents: Option[Documents] = Some(Projects.Kept)
  override val commands: Vector[PluginCommand[?]] =
    new ProjectCommands(methods, default, turns).all
}

object Projects {

  /** `projects`: the plugin's name, which keys its documents. */
  val Name: PluginName =
    // A literal PluginName.of accepts.
    PluginName.of("projects").fold(e => sys.error(e), identity)

  /** Retention 180 days; the last 20 versions of each live key kept whatever their age; at most
    * 20,000 current documents, a write of a new key past them refused.
    */
  val Terms: DocumentTerms =
    // Literals DocLabel.of and DocumentTerms.of accept.
    DocLabel
      .of("project artifacts")
      .flatMap(
        DocumentTerms.of(
          _,
          DocWeight.Unscaled,
          180.days,
          bound = 20_000,
          keepLast = 20,
          full = DocumentTerms.Full.Refuse
        )
      )
      .fold(e => sys.error(e), identity)

  /** What a write refused past `bound` current documents answers. */
  def full(bound: Int): String =
    s"this deployment keeps at most $bound project documents and holds that many already, " +
      "so nothing was kept"

  /** The plugin of these methods, each with its worked examples, its turns offered as `turns`
    * says, or why not: two of one name and version, `default` not among them, a method failing
    * its examples ([[MethodBench]]), or a state `turns` guards that no method's kind of that name
    * has.
    */
  def of(
      methods: Vector[(Method, Vector[Example])],
      default: MethodName,
      turns: TurnCommands = TurnCommands.Unguarded
  ): Either[ProjectsRefusal, Projects] = {
    val declared = methods.map(_._1)
    declared.groupBy(m => (m.name, m.version)).collectFirst {
      case ((name, version), same) if same.size > 1 => ProjectsRefusal.Twice(name, version)
    } match {
      case Some(twice) => Left(twice)
      case None if !declared.exists(_.name == default) => Left(ProjectsRefusal.NoDefault(default))
      case None =>
        val unbenched: Vector[ProjectsRefusal] = methods.flatMap {
          (m: Method, examples: Vector[Example]) =>
            val failed = MethodBench.failures(m, examples)
            if (failed.isEmpty) None else Some(ProjectsRefusal.Unbenched(m.name, m.version, failed))
        }
        val unguardable = turns match {
          case TurnCommands.Shipped(guarded) =>
            guarded.toVector
              .filterNot((kind, state) =>
                declared.exists(_.kinds.exists(k => k.name == kind && k.all.contains(state)))
              )
              .map(ProjectsRefusal.Unguardable(_, _))
          case TurnCommands.Proposing(kind) =>
            Vector(ProjectsRefusal.NoKind(kind)).filterNot(_ =>
              declared.exists(_.kinds.exists(_.name == kind))
            )
        }
        (unbenched ++ unguardable).headOption.toLeft(new Projects(declared, default, turns))
    }
  }

  /** A watch for the artifact `key` to be in one of `states`, or why not: no states, or a key
    * no document's may be.
    */
  def reaching(key: ArtifactKey, states: Set[StateName]): Either[String, WatchFor.Holding] =
    DocKey.of(key.value).flatMap(WatchFor.Holding.of(Name, _, states.map(ArtifactJson.stateRef)))

  /** The states a watch from [[reaching]] was met in: those of its refs that name one. */
  def reached(held: Watched.Held): Set[StateName] = held.refs.flatMap(ArtifactJson.stateOf)

  /** Why a turn's move of `key`, of `kind`, to `state` is refused, when [[TurnCommands]] guards
    * that state: the one wording, which an edge refusing a person's such move builds on.
    */
  def guardedMove(key: ArtifactKey, kind: KindName, state: StateName): String =
    s"A turn cannot move ${key.value} (${KindName.value(kind)}) to ${StateName.value(state)}: " +
      "only a person can, and not by asking a turn."

  private object Kept extends Documents {
    val terms: DocumentTerms = Terms
    val posting: Option[DocumentPosting] = None
  }
}

/** Why some methods are no projects plugin. */
enum ProjectsRefusal extends caps.Pure {

  /** No method is named the default. */
  case NoDefault(default: MethodName)

  /** Two methods share a name and a version. */
  case Twice(method: MethodName, version: Int)

  /** A method's worked examples came out otherwise than expected: each such, with its first such
    * move.
    */
  case Unbenched(method: MethodName, version: Int, failed: Vector[MethodBench.Failed])

  /** A guarded state no method's kind `kind` has, so its guard would hold nothing. */
  case Unguardable(kind: KindName, state: StateName)

  /** A kind to propose that no method declares. */
  case NoKind(kind: KindName)
}
