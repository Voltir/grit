package grit.core.plugin

import java.time.Instant

import grit.core.claim.Claims
import grit.core.document.{DocumentKeeper, DocumentTerms}
import grit.core.id.{PluginName, TurnRef}
import grit.core.identity.Principal
import grit.core.need.PluginReads
import grit.core.place.Place
import grit.core.store.{StoreError, Tx}
import grit.core.tool.ToolSpec
import grit.core.visibility.Integrity

/** A command a person gives a plugin through an edge (ADR 0048), run with no model and no turn
  * in one transaction and answered at once. `spec` names it and reads its arguments as a tool's
  * does.
  */
sealed trait PluginCommand[A <: caps.Pure] extends caps.Pure {
  def spec: ToolSpec[A]
}

/** A command that only reads its plugin's documents, in a read-only transaction: a write through
  * its transaction is refused by the database, which its caller sees as
  * [[grit.core.edge.CommandError.Store]] holding a `DatabaseError`.
  */
trait ReadingCommand[A <: caps.Pure] extends PluginCommand[A] {

  /** What `args`, given by `by` at `at`, come to over `own`. */
  def run(args: A, by: CommandBy, own: PluginReads, at: Instant)(using
      Tx^
  ): Either[StoreError, CommandOutcome]
}

/** A command that may keep its plugin's documents. A deployment is refused one whose plugin keeps
  * none.
  */
trait KeepingCommand[A <: caps.Pure] extends PluginCommand[A] {

  /** As [[ReadingCommand.run]], keeping through `own`, which believes each document at no more
    * than its asker's words are believed, met with their room's cap ([[KeepingCommand.run]]), and
    * claiming through `claims`, its plugin's: committed only when it answers
    * [[CommandOutcome.Done]].
    */
  def run(args: A, by: CommandBy, own: DocumentKeeper, claims: Claims, at: Instant)(using
      Tx^
  ): Either[StoreError, CommandOutcome]
}

object KeepingCommand {

  /** `command` run on `args` for `by` at `at`, keeping through `keeper`, each document believed
    * at no more than `believed` met with `room`'s declared cap
    * ([[grit.core.visibility.Visibility.capOf]], ADR 0036), and claiming through `claims`. Its
    * caller commits only on [[CommandOutcome.Done]].
    */
  def run[A <: caps.Pure](
      command: KeepingCommand[A],
      args: A,
      by: CommandBy,
      keeper: DocumentKeeper,
      claims: Claims,
      room: Place,
      believed: Integrity,
      at: Instant
  )(using tx: Tx^): Either[StoreError, CommandOutcome] = {
    // As an inbound entry is believed: no more than the room's cap.
    val most = Tx.visibility(tx).capOf(room).fold(believed)(believed.meet)
    command.run(args, by, DocumentKeeper.believedAtMost(keeper, most), claims, at)
  }
}

/** Who a command runs for: a person, asking through an edge ([[Direct]]) or through a turn
  * answering them ([[Turn]]).
  */
enum CommandBy extends caps.Pure {
  case Direct(person: Principal.Person)
  case Turn(person: Principal.Person, turn: TurnRef)

  def person: Principal.Person
}

/** A plugin's command as a deployment runs it: a reading one over its plugin's reads, or a
  * keeping one under its plugin's terms.
  */
enum Commanded extends caps.Pure {
  case Reading(plugin: PluginName, command: ReadingCommand[?])
  case Keeping(plugin: PluginName, terms: DocumentTerms, command: KeepingCommand[?])

  def command: PluginCommand[?]
}

object Commanded {

  /** Every command of `plugins`, in their order; a keeping one of a plugin that keeps no
    * documents, which a deployment refuses ([[grit.core.plugin.Plugin]]), left out.
    */
  def of(plugins: Vector[Plugin]): Vector[Commanded] =
    plugins.flatMap(p =>
      p.commands.flatMap {
        case r: ReadingCommand[?] => Vector(Reading(p.name, r))
        case k: KeepingCommand[?] => p.documents.toVector.map(d => Keeping(p.name, d.terms, k))
      }
    )
}

/** What a command answers its asker, to them alone. */
enum CommandOutcome extends caps.Pure {

  /** It did what it was asked; `text` says what. */
  case Done(text: String)

  /** It did not, and kept nothing; `why` is in words the asker can act on. */
  case Refused(why: String)
}
