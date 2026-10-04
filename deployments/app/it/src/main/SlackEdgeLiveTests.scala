package grit.app.main

import grit.core.edge.EdgeStores
import grit.dbos.engine.{Engine, LiveEngine}
import grit.dbos.sql.TestPostgres
import grit.prose.markdown.Markdown
import grit.slack.client.{FakeSlack, Self}
import grit.slack.edge.SlackEdge
import grit.slack.event.{Payloads, TeamId, Ts, UserId}
import grit.slack.text.RichText
import grit.turn.Turn

import utest.*

/** Against Postgres and DBOS, with a fake Slack: the Slack edge over a real engine, across a
  * restart.
  */
object SlackEdgeLiveTests extends TestSuite {
  import LiveTurn.*
  import Payloads.*

  private val self = Self(TeamId(Team), UserId(Bot))

  private def edge(engine: Engine^, slack: FakeSlack): SlackEdge^{engine, slack} =
    new SlackEdge(
      slack,
      self,
      EdgeStores(
        engine.inbox,
        engine.principals,
        engine.deliveries,
        engine.acknowledgements,
        engine.reviews,
        engine.jot,
        engine
      ),
      Set.empty,
      None,
      _ => ()
    )

  val tests = Tests {
    test(
      "a mention recorded by one engine is answered once, under its author's name, by the next, in its thread"
    ) {
      val config = TestPostgres.freshDatabase("slack_edge")
      val slack = new FakeSlack
      val first = LiveEngine.open(config, Turn.Epoch)
      try {
        launch(first, first.entries, new CountingProvider)
        val _ = slack.listen(edge(first, slack).receive)
        slack.deliver(mention("1.0")) ==> true
      } finally first.close()
      slack.posts ==> Vector.empty

      val second = LiveEngine.open(config, Turn.Epoch)
      val (passes, last) =
        try {
          launch(second, second.entries, new CountingProvider)
          val restarted = edge(second, slack)
          val deadline = System.nanoTime() + 60_000_000_000L
          var passes = 0
          while (slack.posts.isEmpty && System.nanoTime() < deadline) {
            val _ = restarted.deliver()
            passes += 1
            Thread.sleep(200)
          }
          (passes, restarted.deliver())
        } finally second.close()
      assert(passes >= 1)
      last ==> Right(0)
      val expected = RichText.render(
        Markdown.parse("stub reply to: Ana Lima wrote:\nis it everything a river should be?")
      )
      slack.posts.map(p => (p.thread, p.post.fallback)) ==> Vector(
        (Ts("1.0"), expected.head.fallback)
      )
      slack.reactions ==> Set.empty
    }
  }
}
