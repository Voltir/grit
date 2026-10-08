package grit.core.admin

import grit.core.identity.Account
import grit.core.visibility.{Compartment, Label, Level}

/** What a person asks of grit's visibility by command, as [[Command.read]] reads it. Each runs
  * where it was asked: its room is that conversation's room.
  */
enum Command {

  /** This room's label in force, and where it comes from. */
  case ShowLabel

  /** This room's label set to `label`. */
  case SetLabel(label: Label)

  /** This room's label set back to its default, the one it takes with none set. */
  case Unlabel

  /** The asker's clearance, or, with `of`, that person's. */
  case Clearance(of: Option[Account])

  /** `person` cleared for `compartment`. */
  case Clear(person: Account, compartment: Compartment)

  /** `person` no longer cleared for `compartment` through grit. */
  case Remove(person: Account, compartment: Compartment)

  /** The compartments the asker can name. */
  case Compartments

  /** This room quiet (`on`): nothing posted there unasked; or not. */
  case Quiet(on: Boolean)

  /** [[Command.Usage]]. */
  case Help
}

object Command {

  /** `words`, what follows the command's name, as a [[Command]]: `label`, `label <level>
    * [compartment…]`, `unlabel`, `clearance [person]`, `clear <person> for <compartment>`,
    * `remove <person> from <compartment>`, `compartments`, `quiet`, `speak`, `help` (also for
    * nothing). The command's words and a level's name are read in any case, a compartment's
    * name as [[Compartment.of]] reads it. `person` reads a word naming a person into their
    * account (an edge's mention syntax), `None` when it names none. Why not, as one line a
    * person reads naming the word it stopped at, then [[Usage]]. Whether a compartment is
    * declared is decided when it runs.
    */
  def read(words: String, person: String => Option[Account]): Either[String, Command] = {
    val said = words.trim.split("\\s+").toVector.filter(_.nonEmpty)
    def done(command: Command, used: Int): Either[String, Command] =
      said.drop(used).headOption match {
        case Some(extra) => Left(s"$extra: nothing goes after ${said.take(used).mkString(" ")}")
        case None => Right(command)
      }
    def named(at: Int, what: String): Either[String, String] =
      said.lift(at).toRight(s"${said.take(at).lastOption.mkString}: $what goes next")
    def keyword(at: Int, expected: String): Either[String, Unit] =
      named(at, expected).flatMap { word =>
        Either.cond(word.toLowerCase == expected, (), s"$word: $expected goes here")
      }
    def account(at: Int): Either[String, Account] =
      named(at, "a person").flatMap(word => person(word).toRight(s"$word: names no person"))
    def compartment(at: Int): Either[String, Compartment] =
      named(at, "a compartment").flatMap(Compartment.of)
    def membership(
        link: String,
        make: (Account, Compartment) => Command
    ): Either[String, Command] =
      for {
        who <- account(1)
        _ <- keyword(2, link)
        c <- compartment(3)
        command <- done(make(who, c), 4)
      } yield command
    val read = said.headOption.fold[Either[String, Command]](Right(Help)) { first =>
      first.toLowerCase match {
        case "help" => done(Help, 1)
        case "label" =>
          said.lift(1) match {
            case None => Right(ShowLabel)
            case Some(word) =>
              for {
                level <- Level.values
                  .find(name(_) == word.toLowerCase)
                  .toRight(s"$word: not a level; the levels are $levels")
                cs <- said
                  .drop(2)
                  .foldLeft[Either[String, Vector[Compartment]]](Right(Vector.empty)) { (read, c) =>
                    read.flatMap(cs => Compartment.of(c).map(cs :+ _))
                  }
              } yield SetLabel(Label.at(level, cs*))
          }
        case "unlabel" => done(Unlabel, 1)
        case "clearance" =>
          if (said.size == 1) Right(Clearance(None))
          else account(1).flatMap(who => done(Clearance(Some(who)), 2))
        case "clear" => membership("for", Clear(_, _))
        case "remove" => membership("from", Remove(_, _))
        case "compartments" => done(Compartments, 1)
        case "quiet" => done(Quiet(true), 1)
        case "speak" => done(Quiet(false), 1)
        case _ => Left(s"$first: no such command")
      }
    }
    read.left.map(line => s"$line\n$Usage")
  }

  /** The commands, one per line, as `help` shows them, and the levels. */
  val Usage: String =
    Vector(
      "label: this room's label",
      "label <level> [compartment…]: set this room's label",
      "unlabel: set this room's label back to its default",
      "clearance [person]: your clearance, or another person's",
      "clear <person> for <compartment>: clear a person for a compartment",
      "remove <person> from <compartment>: remove a person from a compartment",
      "compartments: the compartments you can name",
      "quiet: post nothing here unasked",
      "speak: post here unasked again",
      "help: these commands",
      s"The levels: $levels."
    ).mkString("\n")

  /** A level's name, as a label's written form spells it. */
  private def name(level: Level): String = Label.written(Label.at(level))

  private def levels: String =
    Level.values.toVector.map(name) match {
      case most :+ last if most.nonEmpty => s"${most.mkString(", ")} and $last"
      case one => one.mkString
    }
}
