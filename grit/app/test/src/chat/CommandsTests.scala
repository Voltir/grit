package grit.app.chat

import utest.*

/** The slash-command table, read as drafts: what is a command, and what is escaped. */
object CommandsTests extends TestSuite {

  val tests = Tests {
    test("a draft starting with one / is a command; with // it is not") {
      assert(
        Commands.isCommand("/theme"),
        Commands.isCommand("/"),
        !Commands.isCommand("//theme"),
        !Commands.isCommand("hello /theme"),
        !Commands.isCommand("")
      )
    }

    test("a // draft stands for a message with one slash taken off") {
      Commands.escaped("//etc") ==> Some("/etc")
      Commands.escaped("//") ==> Some("/")
      Commands.escaped("/etc") ==> None
      Commands.escaped("etc") ==> None
    }

    test("the palette offers nothing over a // draft") {
      Commands.listing("//") ==> None
      Commands.listing("//th") ==> None
      assert(Commands.listing("/th").nonEmpty)
    }
  }
}
