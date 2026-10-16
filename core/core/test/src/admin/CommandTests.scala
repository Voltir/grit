package grit.core.admin

import grit.core.identity.{Account, TestAccounts}
import grit.core.model.Grade
import grit.core.place.{Namespace, Place}
import grit.core.visibility.TestLabels.{compartment, place}
import grit.core.visibility.{Label, Level}

import utest.*

object CommandTests extends TestSuite {

  private val bo = TestAccounts.account("slack:T1/U-bo")

  /** `<@U-bo>` names bo, as an edge's mention syntax would; any other word names no one. */
  private def person(word: String): Option[Account] = Option.when(word == "<@U-bo>")(bo)

  /** `shared` is the one link a person names a shared place by here. */
  private val shared = Place.under(Namespace.Service, Vector("files", "shared"))
  private val places: PlaceNames = new PlaceNames {
    def place(word: String): Option[Place] = Option.when(word == "https://files/shared")(shared)
  }

  private def read(words: String): Either[String, Command] = Command.read(words, person, places)

  /** The line a refusal opens with, before the usage every refusal ends with. */
  private def refusal(words: String): Either[String, String] =
    read(words) match {
      case Right(command) => Left(s"read as $command")
      case Left(text) =>
        text.linesIterator.toVector match {
          case first +: rest if rest.mkString("\n") == Command.Usage => Right(first)
          case other => Left(s"no usage after the refusal's line: $other")
        }
    }

  val tests = Tests {
    test("each command is read from the words after the command's name") {
      Vector(
        read("label"),
        read("label confidential trial acme"),
        read("unlabel"),
        read("clearance"),
        read("clearance <@U-bo>"),
        read("clear <@U-bo> for trial"),
        read("remove <@U-bo> from trial"),
        read("compartments"),
        read("quiet"),
        read("speak"),
        read("help")
      ) ==> Vector(
        Right(Command.ShowLabel),
        Right(
          Command.SetLabel(Label.at(Level.Confidential, compartment("trial"), compartment("acme")))
        ),
        Right(Command.Unlabel),
        Right(Command.Clearance(None)),
        Right(Command.Clearance(Some(bo))),
        Right(Command.Clear(bo, compartment("trial"))),
        Right(Command.Remove(bo, compartment("trial"))),
        Right(Command.Compartments),
        Right(Command.Quiet(true)),
        Right(Command.Quiet(false)),
        Right(Command.Help)
      )
    }

    test(
      "baseline shows this room's baseline; with a grade's word, in any case, sets it, and with none clears it"
    ) {
      Vector(
        read("baseline"),
        read("baseline light"),
        read("BASELINE Standard"),
        read("baseline strong"),
        read("baseline none")
      ) ==> Vector(
        Right(Command.ShowBaseline),
        Right(Command.SetBaseline(Some(Grade.Light))),
        Right(Command.SetBaseline(Some(Grade.Standard))),
        Right(Command.SetBaseline(Some(Grade.Strong))),
        Right(Command.SetBaseline(None))
      )
    }

    test("baseline refuses a word that names no grade, naming the choices, and any word after") {
      Vector(refusal("baseline mighty"), refusal("baseline strong now")) ==> Vector(
        Right("mighty: not a baseline; the baselines are light|standard|strong|none"),
        Right("now: nothing goes after baseline strong")
      )
    }

    test("nothing, or only spaces, asks for help") {
      (read(""), read("   ")) ==> (Right(Command.Help), Right(Command.Help))
    }

    test(
      "a level and the keywords are read in any case; a compartment's name only in lower case"
    ) {
      (
        read("  LABEL   Internal  "),
        read("Clear <@U-bo> FOR trial"),
        refusal("label internal Trial")
      ) ==> (
        Right(Command.SetLabel(Label.at(Level.Internal))),
        Right(Command.Clear(bo, compartment("trial"))),
        Right(
          "Trial is not a compartment's name: lowercase letters, digits and -, 1 to 32 of them"
        )
      )
    }

    test(
      "label at, label … at and unlabel at name a place, by a word the places name or as grit writes one; at after any other command is refused"
    ) {
      (
        Vector(
          read("label at https://files/shared"),
          read("LABEL internal trial AT https://files/shared"),
          read("label confidential at service:files/other"),
          read("unlabel At https://files/shared")
        ),
        Vector(
          refusal("label at"),
          refusal("label internal at elsewhere"),
          refusal("label internal at https://files/shared now"),
          refusal("unlabel at"),
          refusal("quiet at https://files/shared"),
          refusal("baseline at https://files/shared")
        )
      ) ==> (
        Vector(
          Right(Command.At(shared, Command.Labelling.Show)),
          Right(
            Command.At(
              shared,
              Command.Labelling.Set(Label.at(Level.Internal, compartment("trial")))
            )
          ),
          Right(
            Command.At(
              place("service:files/other"),
              Command.Labelling.Set(Label.at(Level.Confidential))
            )
          ),
          Right(Command.At(shared, Command.Labelling.Unset))
        ),
        Vector(
          Right("at: a place goes next"),
          Right("elsewhere: names no place"),
          Right("now: nothing goes after label internal at https://files/shared"),
          Right("at: a place goes next"),
          Right("at: nothing goes after quiet"),
          Right("at: not a baseline; the baselines are light|standard|strong|none")
        )
      )
    }

    test("a refusal is one line naming the word it stopped at, then the usage") {
      Vector(
        refusal("relabel"),
        refusal("label top"),
        refusal("label internal public"),
        refusal("unlabel now"),
        refusal("clearance someone"),
        refusal("clearance <@U-bo> <@U-bo>"),
        refusal("clear"),
        refusal("clear <@U-bo>"),
        refusal("clear <@U-bo> from trial"),
        refusal("clear <@U-bo> for"),
        refusal("clear someone for trial"),
        refusal("clear <@U-bo> for trial acme"),
        refusal("remove <@U-bo> for trial"),
        refusal("quiet please")
      ) ==> Vector(
        Right("relabel: no such command"),
        Right("top: not a level; the levels are public, internal, confidential and restricted"),
        Right("public is a level's name, never a compartment's"),
        Right("now: nothing goes after unlabel"),
        Right("someone: names no person"),
        Right("<@U-bo>: nothing goes after clearance <@U-bo>"),
        Right("clear: a person goes next"),
        Right("<@U-bo>: for goes next"),
        Right("from: for goes here"),
        Right("for: a compartment goes next"),
        Right("someone: names no person"),
        Right("acme: nothing goes after clear <@U-bo> for trial"),
        Right("for: from goes here"),
        Right("please: nothing goes after quiet")
      )
    }

    test("help lists every command, and the levels") {
      Command.Usage ==>
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
          "baseline <light|standard|strong|none>: set this room's baseline, the least grade its turns get",
          "help: these commands",
          "The levels: public, internal, confidential and restricted."
        ).mkString("\n")
    }
  }
}
