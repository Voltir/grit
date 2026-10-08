package grit.slack.event

import utest.*

/** [[Events.command]] over slash commands' payloads shaped as Slack's docs give them. */
object CommandedTests extends TestSuite {
  import Payloads.*

  val tests = Tests {
    test("a command is read with its name, words, asker, channel, team and response url") {
      Events.command(command(s"clearance <@$Bot|grit>")) ==> Right(
        Commanded(
          TeamId(Team),
          ChannelId("C123ABC456"),
          UserId(Ana),
          "/grit",
          s"clearance <@$Bot|grit>",
          ResponseUrl(Hook)
        )
      )
    }

    test("a command asked in a direct message is direct, and one in a channel is not") {
      (
        Events.command(command("label", channel = AnasDm)).map(_.direct),
        Events.command(command("label")).map(_.direct)
      ) ==> (Right(true), Right(false))
    }

    test("a command without text has no words") {
      val bare = ujson.read(command(""))
      bare.obj.remove("text")
      Events.command(bare.render()).map(_.text) ==> Right("")
    }

    test("a command lacking a field it needs, or not JSON, is not read, saying why") {
      val unanswerable = ujson.read(command("help"))
      unanswerable.obj.remove("response_url")
      (
        Events.command(unanswerable.render()),
        Events.command("command=/grit").left.map(_.startsWith("not JSON"))
      ) ==>
        (Left("a slash command without response_url"), Left(true))
    }
  }
}
