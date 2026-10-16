package grit.core.admin

import grit.core.identity.Account
import grit.core.model.Grade
import grit.core.place.Place
import grit.core.visibility.{Compartment, Label, Level}

/** What a person asks of grit's visibility, or of a room's baseline, by command, as [[Command.read]] reads it. Each runs
  * where it was asked: its room is that conversation's room, but for [[At]]'s.
  */
enum Command {

  /** `labelling` run for `place` instead of the room it is asked in, when `place` lies within
    * a place a service shares with grit ([[grit.core.visibility.Visibility.invites]]);
    * otherwise refused ([[Refusal.NotShared]]).
    */
  case At(place: Place, labelling: Command.Labelling)

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

  /** This room's baseline, and what it does. */
  case ShowBaseline

  /** This room's baseline set to `to`; `None` clears it. */
  case SetBaseline(to: Option[Grade])

  /** [[Command.Usage]]. */
  case Help
}

object Command {

  /** What [[At]] runs for a place: its label shown ([[ShowLabel]]), set ([[SetLabel]]), or set
    * back to its default ([[Unlabel]]).
    */
  enum Labelling {
    case Show
    case Set(label: Label)
    case Unset
  }

  /** `words`, what follows the command's name, as a [[Command]]: `label`, `label <level>
    * [compartment…]`, `unlabel`, `clearance [person]`, `clear <person> for <compartment>`,
    * `remove <person> from <compartment>`, `compartments`, `quiet`, `speak`, `baseline`,
    * `baseline <light|standard|strong|none>`, `help` (also for nothing). The command's words, a
    * level's name and a grade's are read in any case, a compartment's
    * name as [[Compartment.of]] reads it. `person` reads a word naming a person into their
    * account (an edge's mention syntax), `None` when it names none. `label at <place>`,
    * `label <level> [compartment…] at <place>` and `unlabel at <place>` are [[At]] `place`: a
    * word `places` names, else a place as [[Place.read]] reads it; `at` read in any case. Why
    * not, as one line a person reads naming the word it stopped at, then [[Usage]]. Whether a
    * compartment is declared, and whether a place is shared with grit, is decided when it runs.
    */
  def read(
      words: String,
      person: String => Option[Account],
      places: PlaceNames
  ): Either[String, Command] = {
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
    def isAt(at: Int): Boolean = said.lift(at).exists(_.toLowerCase == AtWord)
    // `at <place>` from `at` on, ending the words: `labelling` run there.
    def at(from: Int, labelling: Labelling): Either[String, Command] =
      named(from + 1, "a place")
        .flatMap(word =>
          places.place(word).orElse(Place.read(word).toOption).toRight(s"$word: names no place")
        )
        .flatMap(place => done(Command.At(place, labelling), from + 2))
    val read = said.headOption.fold[Either[String, Command]](Right(Help)) { first =>
      first.toLowerCase match {
        case "help" => done(Help, 1)
        case "label" =>
          said.lift(1) match {
            case None => Right(ShowLabel)
            case Some(_) if isAt(1) => at(1, Labelling.Show)
            case Some(word) =>
              val placed = said.indices.find(i => i > 1 && isAt(i))
              for {
                level <- Level.values
                  .find(name(_) == word.toLowerCase)
                  .toRight(s"$word: not a level; the levels are $levels")
                cs <- said
                  .slice(2, placed.getOrElse(said.size))
                  .foldLeft[Either[String, Vector[Compartment]]](Right(Vector.empty)) { (read, c) =>
                    read.flatMap(cs => Compartment.of(c).map(cs :+ _))
                  }
                label = Label.at(level, cs*)
                command <- placed.fold(Right(SetLabel(label)))(at(_, Labelling.Set(label)))
              } yield command
          }
        case "unlabel" => if (isAt(1)) at(1, Labelling.Unset) else done(Unlabel, 1)
        case "clearance" =>
          if (said.size == 1) Right(Clearance(None))
          else account(1).flatMap(who => done(Clearance(Some(who)), 2))
        case "clear" => membership("for", Clear(_, _))
        case "remove" => membership("from", Remove(_, _))
        case "compartments" => done(Compartments, 1)
        case "quiet" => done(Quiet(true), 1)
        case "speak" => done(Quiet(false), 1)
        case "baseline" =>
          said.lift(1) match {
            case None => Right(ShowBaseline)
            case Some(word) =>
              val to = word.toLowerCase match {
                case "none" => Right(None)
                case w =>
                  Grade.values
                    .find(_.word == w)
                    .map(Some(_))
                    .toRight(s"$word: not a baseline; the baselines are $baselines")
              }
              to.flatMap(g => done(SetBaseline(g), 2))
          }
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
      "label at <link>: the label of a folder another app shares with grit, named by its link",
      "label <level> [compartment…] at <link>: set that folder's label",
      "unlabel at <link>: set that folder's label back to its default",
      "clearance [person]: your clearance, or another person's",
      "clear <person> for <compartment>: clear a person for a compartment",
      "remove <person> from <compartment>: remove a person from a compartment",
      "compartments: the compartments you can name",
      "quiet: post nothing here unasked",
      "speak: post here unasked again",
      "baseline: this room's baseline",
      s"baseline <$baselines>: set this room's baseline, the least grade its turns get",
      "help: these commands",
      s"The levels: $levels."
    ).mkString("\n")

  /** The word that names, after a label command, the place it runs for; never a compartment's
    * name ([[Compartment.of]]).
    */
  private val AtWord: String = "at"

  /** A level's name, as a label's written form spells it. */
  private def name(level: Level): String = Label.written(Label.at(level))

  private def levels: String = and(Level.values.toVector.map(name))

  /** What `baseline` takes: each grade's word, then `none`. */
  private def baselines: String = (Grade.values.toVector.map(_.word) :+ "none").mkString("|")

  /** `words` as a person lists them: `a`, `a and b`, `a, b and c`. Here, not in [[Answer]],
    * whose `Help` reads [[Usage]] as it is initialised.
    */
  private[admin] def and(words: Vector[String]): String =
    words match {
      case most :+ last if most.nonEmpty => s"${most.mkString(", ")} and $last"
      case one => one.mkString
    }
}
