package grit.app.chat

import scala.concurrent.duration.DurationInt

import grit.app.config.Lifecycle

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

    test("/done signals, and takes no argument") {
      Commands.run("/done") ==> Right(ChatScreen.Msg.Signal)
      Commands.run("/done now") ==> Left("/done takes no argument")
    }

    test("/set alone asks for the settings; with a name and a value, changes one") {
      Commands.run("/set") ==> Right(ChatScreen.Msg.Settings(None))
      Commands.run("/set idle 3m") ==>
        Right(ChatScreen.Msg.Settings(Some(Lifecycle.Change.Idle(3.minutes))))
      Commands.run("/set idle") ==> Left("idle takes a duration, as 30s or 3m")
      Commands.pick("/set ", "grace") ==> Commands.Picked.Fill("/set grace ")
    }

    test("the palette offers nothing over a // draft") {
      Commands.listing("//") ==> None
      Commands.listing("//th") ==> None
      assert(Commands.listing("/th").nonEmpty)
    }
  }
}
