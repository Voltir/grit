package grit.slack.edge

import utest.*

/** [[SlackCommand.of]]: the names Slack lets an app register. */
object SlackCommandTests extends TestSuite {

  private val Why = "a slash command is / then 1 to 32 of a-z, 0-9, - and _"

  val tests = Tests {
    test("a command is / then its name, trimmed") {
      (SlackCommand.of(" /grit ").map(_.name), SlackCommand.of("/a-b_9").map(_.name)) ==>
        (Right("/grit"), Right("/a-b_9"))
    }

    test("a name of 32 is a command, and one of 33 is not") {
      (SlackCommand.of("/" + "a" * 32).isRight, SlackCommand.of("/" + "a" * 33)) ==>
        (true, Left(s"${"/" + "a" * 33}: $Why"))
    }

    test("no slash, a capital, a space, or no name is no command, saying why") {
      Vector("grit", "/Grit", "/gr it", "/").map(SlackCommand.of) ==>
        Vector("grit", "/Grit", "/gr it", "/").map(raw => Left(s"$raw: $Why"))
    }
  }
}
