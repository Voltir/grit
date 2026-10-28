package grit.core.plugin

import java.time.Instant

import grit.core.claim.Claims
import grit.core.document.DocumentKeeper
import grit.core.identity.Principal
import grit.core.need.PluginReads
import grit.core.store.{StoreError, Tx}
import grit.core.tool.ToolSpec

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
  def run(args: A, by: Principal.Person, own: PluginReads, at: Instant)(using
      Tx^
  ): Either[StoreError, CommandOutcome]
}

/** A command that may keep its plugin's documents. A deployment is refused one whose plugin keeps
  * none.
  */
trait KeepingCommand[A <: caps.Pure] extends PluginCommand[A] {

  /** As [[ReadingCommand.run]], keeping through `own`, which believes each document at no more
    * than its edge gave, met with its room's cap, and claiming through `claims`, its plugin's:
    * committed only when it answers [[CommandOutcome.Done]].
    */
  def run(args: A, by: Principal.Person, own: DocumentKeeper, claims: Claims, at: Instant)(using
      Tx^
  ): Either[StoreError, CommandOutcome]
}

/** What a command answers its asker, to them alone. */
enum CommandOutcome extends caps.Pure {

  /** It did what it was asked; `text` says what. */
  case Done(text: String)

  /** It did not, and kept nothing; `why` is in words the asker can act on. */
  case Refused(why: String)
}
