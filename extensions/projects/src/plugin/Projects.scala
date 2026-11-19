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
import grit.std.either.Proven

/** The projects plugin (ADR 0049): projects held to versions of methods declared through
  * `project_declare`, kept under [[Projects.Terms]] and changed only by its commands, each of
  * which says what it does in its tool's description. Each write records its writer as the
  * artifact's `by`, and a claim its claimer; a claim keeps its artifact's brief for its holder as
  * a document of its own ([[grit.core.document.DocRef.brief]]), kept again by every command that
  * changes what the brief shows. Which commands a turn is offered is `turns`'s; who may make each
  * move, its method's.
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
  val Name: PluginName = PluginName.literal("projects")

  /** Retention 180 days; the last 20 versions of each live key kept whatever their age; at most
    * 20,000 current documents, a write of a new key past them refused.
    */
  val Terms: DocumentTerms =
    // 180 days is a minute or more, 20,000 at least 1 and 20 at least 0: DocumentTerms.of
    // takes them.
    Proven.right(
      DocumentTerms.of(
        DocLabel.literal("project artifacts"),
        DocWeight.Unscaled,
        180.days,
        bound = 20_000,
        keepLast = 20,
        full = DocumentTerms.Full.Refuse
      )
    )

  /** The words `project_declare`'s answer holds when the version it was given is declared
    * already under the same digest, and so it kept nothing; no other answer of it holds them.
    */
  val DeclaredAlready: String = "declared already, under the same digest; nothing changed"

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

  /** A watch for the artifact kept under `stored` to be in one of `states`, or why not: no
    * states. `stored` is the key it was first kept under: its id for an artifact made with one,
    * its name before for one a re-key gave an id; a watch under any other key never fires.
    */
  def reaching(stored: DocKey, states: Set[StateName]): Either[String, WatchFor.Holding] =
    WatchFor.Holding.of(Name, stored, states.map(ArtifactJson.stateRef))

  /** The states a watch from [[reaching]] was met in: those of its refs that name one. */
  def reached(held: Watched.Held): Set[StateName] = held.refs.flatMap(ArtifactJson.stateOf)

  private object Kept extends Documents {
    val terms: DocumentTerms = Terms
    val posting: Option[DocumentPosting] = None
  }
}
