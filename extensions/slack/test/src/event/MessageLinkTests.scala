package grit.slack.event

import utest.*

/** [[MessageLink.read]] over links as Slack's "Copy link" writes them. */
object MessageLinkTests extends TestSuite {

  val tests = Tests {
    test("a root's link names its channel, and its own ts as the thread") {
      MessageLink.read("https://acme.slack.com/archives/C0C5U2FPAL8/p1790782262102319") ==>
        Some(MessageLink(ChannelId("C0C5U2FPAL8"), Ts("1790782262.102319")))
    }

    test(
      "a reply's link names the thread it is in, from thread_ts, wherever it sits in the query"
    ) {
      MessageLink.read(
        " https://acme.slack.com/archives/C0C5U2FPAL8/p1790782264739569?thread_ts=1790782262.102319&cid=C0C5U2FPAL8 "
      ) ==> Some(MessageLink(ChannelId("C0C5U2FPAL8"), Ts("1790782262.102319")))
      MessageLink.read(
        "https://acme.slack.com/archives/C0C5U2FPAL8/p1790782264739569?cid=C0C5U2FPAL8&thread_ts=1790782262.102319"
      ) ==> Some(MessageLink(ChannelId("C0C5U2FPAL8"), Ts("1790782262.102319")))
    }

    test("a link to another host, or with no p-ts, or with a thread_ts that is no ts, is None") {
      Vector(
        "https://acme.slack.com.evil.example/archives/C0C5U2FPAL8/p1790782262102319",
        "https://evil.example/archives/C0C5U2FPAL8/p1790782262102319",
        "http://acme.slack.com/archives/C0C5U2FPAL8/p1790782262102319",
        "https://acme.slack.com/archives/C0C5U2FPAL8",
        "https://acme.slack.com/archives/C0C5U2FPAL8/p179078226210231",
        "https://acme.slack.com/archives/C0C5U2FPAL8/p1790782264739569?thread_ts=yesterday",
        "#probably-not-skynet",
        ""
      ).map(MessageLink.read) ==> Vector.fill(8)(None)
    }
  }
}
