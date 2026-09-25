package grit.app.chat

import grit.app.chat.ChatScreen.Msg
import grit.app.look.Theme

/** The chat screen's slash commands: the one table the palette lists, the help dialog
  * describes, and a submitted `/` draft runs. A draft that starts with `/` is a command,
  * never a message to the model.
  */
object Commands {

  /** A slash command: `name` as typed, `about` as the palette and the help list it. */
  enum Command(val name: String, val about: String) {
    case SetTheme extends Command("/theme", "switch the colour theme")
    case Panel extends Command("/panel", "show or hide the turn panel")
    case Summaries extends Command("/summaries", "show or hide turn summaries")
    case Help extends Command("/help", "commands and keys")
    case Quit extends Command("/quit", "leave grit")

    /** The arguments this command offers as a second list; empty when it takes none. */
    def choices: Vector[String] = this match {
      case SetTheme => Theme.all.map(_.key)
      case Panel | Summaries | Help | Quit => Vector.empty
    }

    /** What the command does with `argument` (empty for none): the screen's message, or
      * why it cannot run.
      */
    def run(argument: String): Either[String, Msg] = this match {
      case SetTheme =>
        if (argument.isEmpty) Left(s"$name takes one of ${choices.mkString(", ")}")
        else Theme.named(argument).map(Msg.SetTheme(_)).toRight(s"no theme $argument")
      case Panel => bare(Msg.TogglePanel, argument)
      case Summaries => bare(Msg.ToggleSummaries, argument)
      case Help => bare(Msg.OpenHelp, argument)
      case Quit => bare(Msg.Quit, argument)
    }

    private def bare(msg: Msg, argument: String): Either[String, Msg] =
      if (argument.isEmpty) Right(msg) else Left(s"$name takes no argument")
  }

  val all: Vector[Command] = Command.values.toVector

  /** The command `name` names, exactly. */
  def named(name: String): Option[Command] = all.find(_.name == name)

  /** `line` run: its first word names the command, the rest is its argument. */
  def run(line: String): Either[String, Msg] = {
    val (name, argument) = split(line)
    named(name).toRight(s"no command $name").flatMap(_.run(argument))
  }

  /** What the palette lists over `draft`, and the query that filters it: the commands
    * while a name is being typed, then a command's choices once its name and a space are.
    * None when there is nothing to offer: a draft that is not a command, or a command
    * that takes no argument followed by one.
    */
  def listing(draft: String): Option[Listing] =
    if (!draft.startsWith("/")) None
    else if (!draft.contains(' ')) Some(Listing(all.map(row), draft))
    else {
      val (name, argument) = split(draft)
      named(name).filter(_.choices.nonEmpty).map(c => Listing(c.choices, argument))
    }

  /** A palette's items, filtered by prefix against `query`. */
  final case class Listing(items: Vector[String], query: String)

  /** What choosing `item` from the list over `draft` does. */
  enum Picked {

    /** The prompt becomes `draft`, and the palette lists what follows it. */
    case Fill(draft: String)

    /** The command `line` runs. */
    case Run(line: String)
  }

  def pick(draft: String, item: String): Picked =
    if (!draft.contains(' ')) {
      val name = item.takeWhile(_ != ' ')
      named(name) match {
        case Some(c) if c.choices.nonEmpty => Picked.Fill(s"$name ")
        case _ => Picked.Run(name)
      }
    } else Picked.Run(s"${split(draft)._1} $item")

  /** A command's row in the palette: its name, then what it does. */
  private def row(c: Command): String = s"${c.name.padTo(NameCols, ' ')}${c.about}"

  /** The width of the names' column: the longest name and two spaces. */
  private val NameCols = all.map(_.name.length).maxOption.getOrElse(0) + 2

  private def split(line: String): (String, String) = {
    val (name, rest) = line.trim.span(_ != ' ')
    (name, rest.trim)
  }
}
