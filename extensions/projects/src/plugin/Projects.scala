package grit.projects.plugin

import scala.concurrent.duration.*

import grit.core.document.{DocLabel, DocWeight, DocumentTerms}
import grit.core.id.PluginName
import grit.core.plugin.{DocumentPosting, Documents, Plugin, PluginCommand}
import grit.projects.method.Method
import grit.projects.name.MethodName

/** The projects plugin (ADR 0049): projects of `methods`, new ones under the latest version of
  * `default` unless another is named, kept under [[Projects.Terms]], and changed only by its
  * commands: `project_new`, `project_keep` (never a state), `project_move` (only as the
  * project's pinned method allows, refused once that method's digest has changed), and the
  * reading `project_show`, `project_list`, `project_find` and `project_export`. Each write
  * records its writer as the artifact's `by`.
  */
final class Projects private (methods: Vector[Method], default: MethodName) extends Plugin {
  val name: PluginName = Projects.Name
  val version: Int = 1
  override val documents: Option[Documents] = Some(Projects.Kept)
  override val commands: Vector[PluginCommand[?]] = new ProjectCommands(methods, default).all
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

  /** What ends every command's `does` line, telling its artifacts from claude.ai's. */
  val NotClaudeAi: String =
    "An artifact here is a project artifact, a versioned record of a grit project, not a " +
      "claude.ai Artifact."

  /** What a write refused past `bound` current documents answers. */
  def full(bound: Int): String =
    s"this deployment keeps at most $bound project documents and holds that many already, " +
      "so nothing was kept"

  /** The plugin, or why not: `default` not among `methods`, or two of one name and version. */
  def of(methods: Vector[Method], default: MethodName): Either[ProjectsRefusal, Projects] =
    methods.groupBy(m => (m.name, m.version)).collectFirst {
      case ((name, version), same) if same.size > 1 => ProjectsRefusal.Twice(name, version)
    } match {
      case Some(twice) => Left(twice)
      case None if !methods.exists(_.name == default) => Left(ProjectsRefusal.NoDefault(default))
      case None => Right(new Projects(methods, default))
    }

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
}
