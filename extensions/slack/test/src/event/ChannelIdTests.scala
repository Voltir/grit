package grit.slack.event

import utest.*

/** [[ChannelId.read]] over ids as Slack's channel details show them. */
object ChannelIdTests extends TestSuite {
  val tests = Tests {
    test("a channel's id is read, trimmed: a public or private one's (C…), an older private one's (G…)") {
      Vector(" C123ABC456 ", "G0123ABCD").map(ChannelId.read) ==>
        Vector(Some(ChannelId("C123ABC456")), Some(ChannelId("G0123ABCD")))
    }

    test("a direct message's id, a channel's name, and lower case are not a channel's id") {
      Vector("D0123ABCD", "#general", "c123abc456", "").map(ChannelId.read) ==>
        Vector(None, None, None, None)
    }
  }
}
