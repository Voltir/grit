package grit.slack.event

import grit.core.id.Href

import utest.*

/** [[MessageLink.read]] over links as Slack's "Copy link" writes them, and [[MessageLink.to]]. */
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

    test(
      "to writes a root's and a reply's link as Slack does, which read reads back, the workspace with a slash or without"
    ) {
      val c = ChannelId("C0C5U2FPAL8")
      val root = Ts("1790782262.102319")
      val reply = Ts("1790782264.739569")
      val links = Vector("https://acme.slack.com/", "https://acme.slack.com").flatMap(Href.of).map {
        workspace =>
          (
            MessageLink.to(workspace, c, root, root).map(Href.value),
            MessageLink.to(workspace, c, root, reply).map(Href.value)
          )
      }
      // As Slack's "Copy link" and chat.getPermalink write them (the reply's above).
      val written = (
        Some("https://acme.slack.com/archives/C0C5U2FPAL8/p1790782262102319"),
        Some(
          "https://acme.slack.com/archives/C0C5U2FPAL8/p1790782264739569?thread_ts=1790782262.102319&cid=C0C5U2FPAL8"
        )
      )
      links ==> Vector(written, written)
      Vector(written._1, written._2).flatten.map(MessageLink.read) ==>
        Vector(Some(MessageLink(c, root)), Some(MessageLink(c, root)))
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
