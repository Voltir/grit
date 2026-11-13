package grit.core.edge

import java.time.Instant

import grit.core.id.PluginName
import grit.core.identity.Account
import grit.core.need.PluginReads
import grit.core.plugin.{CommandBy, CommandOutcome, Plugin, PluginCommand, ReadingCommand}
import grit.core.store.{StoreError, Tx}
import grit.core.tool.{Args, Field, ToolName, ToolSpec}
import grit.core.visibility.Integrity

import utest.*

/** What an edge's routes rely on of [[PluginCommands]]: each command's own outcome, and each
  * error a run is refused with. Run by the scripted fake in core and by the SQL commands in
  * grit.dbos, both offering [[PluginCommandsContract.Commands]]' commands. It leaves
  * [[CommandError.Store]], which only a store's own failure gives, to grit.app's live plugin
  * suite, and [[CommandError.Unavailable]], a database that cannot be reached, untested.
  */
abstract class PluginCommandsContract extends TestSuite {

  /** `body` lent [[PluginCommandsContract.Commands]]' commands, over a store where
    * [[EdgeContracts.Reader]] is linked to a person and [[EdgeContracts.Stranger]] is not.
    */
  protected def commanding[A](body: PluginCommands^ => A): A

  import EdgeContracts.*
  import PluginCommandsContract.*

  private def run(name: ToolName, args: ujson.Value, by: Account = Reader) =
    commanding(_.run(by, Room, Integrity.Member, name, args, At))

  val tests = Tests {
    test("every command is offered by its name, with whether it only reads") {
      commanding(_.offered.map(l => (l.schema.name, l.reads))) ==>
        Vector(("echo", true), ("refuse", true))
    }

    test("a command's own outcome is answered, done or refused") {
      (run(Echo, ujson.Obj("text" -> "hi")), run(Refuse, ujson.Obj("text" -> "hi"))) ==>
        (Right(CommandOutcome.Done("hi")), Right(CommandOutcome.Refused("not hi")))
    }

    test("a name no command has is unknown") {
      run(ToolName("absent"), ujson.Obj()) ==> Left(CommandError.Unknown(ToolName("absent")))
    }

    test("arguments a command does not read are unread, naming it") {
      run(Echo, ujson.Obj("words" -> "hi")) ==> Left(
        CommandError.Unread(Echo, "There is no argument `words`; the arguments there are `text`.")
      )
    }

    test("an account no person is linked to is refused") {
      run(Echo, ujson.Obj("text" -> "hi"), Stranger) ==> Left(CommandError.NotVouched(Stranger))
    }
  }
}

object PluginCommandsContract {

  val Echo: ToolName = ToolName("echo")
  val Refuse: ToolName = ToolName("refuse")

  final case class Text(text: String) extends caps.Pure

  /** What `echo` and `refuse` read, so a fake reads them as the commands do. */
  val text: Args[Text] = Args.of((text = Field.text("What to say."))).map(a => Text(a.text))

  /** `echo` answers its `text` done; `refuse` refuses it as `not <text>`; both only read. */
  object Commands extends Plugin {
    val name: PluginName = PluginName.of("contracted").fold(e => throw new Error(e), n => n)
    val version: Int = 1
    override val commands: Vector[PluginCommand[?]] = Vector(
      answering(Echo, t => CommandOutcome.Done(t)),
      answering(Refuse, t => CommandOutcome.Refused(s"not $t"))
    )
  }

  private def answering(n: ToolName, outcome: String -> CommandOutcome): ReadingCommand[Text] =
    new ReadingCommand[Text] {
      val spec: ToolSpec[Text] = ToolSpec(n, "Answers its text.", text)
      def run(args: Text, by: CommandBy, own: PluginReads, at: Instant)(using
          Tx^
      ): Either[StoreError, CommandOutcome] = Right(outcome(args.text))
    }
}
