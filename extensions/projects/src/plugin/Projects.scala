package grit.projects.plugin

import scala.concurrent.duration.*

import grit.core.document.{DocLabel, DocWeight, DocumentTerms}
import grit.core.edge.{WatchFor, Watched}
import grit.core.id.{DocKey, PluginName}
import grit.core.plugin.{DocumentPosting, Documents, Plugin, PluginCommand}
import grit.core.visibility.Label
import grit.projects.artifact.ArtifactJson
import grit.projects.method.{Method, MethodRefusal, TransitionCheck}
import grit.projects.name.{ArtifactKey, MethodName, StateName}

/** The projects plugin (ADR 0049): projects of the methods declared through `project_declare`
  * (each version a document of its own, never withdrawn, and read by every command from the store
  * in its own transaction, its checks found among `checks`), new ones under the latest version of
  * `default` unless another is named, kept under [[Projects.Terms]], and changed only by its
  * commands: `project_new`, `project_keep` (never a state, and refused when it would put the
  * brief of a claimed artifact over [[grit.core.document.DocRef.BriefChars]]), `project_move` (only as the
  * project's pinned method allows, refused when that version is not declared or its digest is not
  * the pin's),
  * `project_batch` (keeps and moves of one project, all or none, or a dry run of them),
  * `project_claim` and `project_release` (a claim on an artifact, one of the plugin's claims and
  * never a document, so the artifact keeps its version; a claim keeps the artifact's brief for its
  * holder as a document of its own ([[grit.core.document.DocRef.brief]], at most
  * [[grit.core.document.DocRef.BriefChars]],
  * none when over), kept again by every command that changes what it shows, which a release
  * withdraws, claimed still or not), `project_withdraw` (an artifact no
  * other names, or with what is under it, ended citers rewritten; its history kept as of an
  * instant), `project_sweep` (a project's old ended artifacts of the kinds named, withdrawn at
  * once unless claimed or cited by live work, ended citers rewritten; or a dry run of it),
  * `project_migrate` (a project moved to another version of its method, the artifacts it
  * renames kept again, the rest left as they are), `project_declare` (a method version, given
  * directly by a caller believed at `Authority`, accepted once its worked examples replay),
  * and the reading `project_show`, `project_history`, `project_list`, `project_find`
  * (ended artifacts only when asked), `project_stale` (what no live work names, untouched for
  * weeks), `project_export`, `project_claims`, `project_brief` and `project_methods`. Each write records its writer
  * as the artifact's `by`, and a claim its claimer. Which of them a turn is offered is `turns`'s;
  * who may make each move, its method's.
  */
final class Projects private (
    checks: Vector[TransitionCheck],
    default: MethodName,
    turns: TurnCommands,
    methodsAt: Label
) extends Plugin {
  val name: PluginName = Projects.Name
  val version: Int = 1
  override val documents: Option[Documents] = Some(Projects.Kept)
  override val commands: Vector[PluginCommand[?]] =
    new ProjectCommands(checks, default, turns, methodsAt).all
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

  /** The plugin over these compiled checks; its methods are those declared through
    * `project_declare`, new projects under the latest version of `default`. Each method version
    * is kept and read at `methodsAt` alone, so it has one digest whoever reads it: give a label
    * every caller of its commands reads. A declaration from a room that writes at another label
    * is refused; a caller who cannot read `methodsAt` finds no method declared.
    */
  def of(
      checks: Vector[TransitionCheck],
      default: MethodName,
      turns: TurnCommands = TurnCommands.Shipped,
      methodsAt: Label = Label.Public
  ): Either[MethodRefusal.CheckTwice, Projects] =
    Method.checksTwice(checks).headOption.toLeft(new Projects(checks, default, turns, methodsAt))

  /** A watch for the artifact `key` to be in one of `states`, or why not: no states, or a key
    * no document's may be.
    */
  def reaching(key: ArtifactKey, states: Set[StateName]): Either[String, WatchFor.Holding] =
    DocKey.of(key.value).flatMap(WatchFor.Holding.of(Name, _, states.map(ArtifactJson.stateRef)))

  /** The states a watch from [[reaching]] was met in: those of its refs that name one. */
  def reached(held: Watched.Held): Set[StateName] = held.refs.flatMap(ArtifactJson.stateOf)

  private object Kept extends Documents {
    val terms: DocumentTerms = Terms
    val posting: Option[DocumentPosting] = None
  }
}
