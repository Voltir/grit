package grit.app.main

import grit.core.id.SourceId
import grit.core.identity.Account
import grit.core.message.{AssistantBlock, Message}
import grit.core.store.{Origin, Payload}
import grit.core.visibility.{Label, Level, Subject}
import grit.dbos.engine.LiveEngine
import grit.dbos.sql.{DbConfig, TestPostgres}
import grit.kit.deployment.Offered
import grit.kit.environment.Secrets
import grit.kit.run.Launch
import grit.turn.Turn

import utest.*

/** The reference deployment's clearances (ADR 0037) in stub mode, against a real Postgres. */
object ClearedLiveTests extends TestSuite {

  private def right[E, A](e: Either[E, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why.toString), identity)

  /** `env` with `config`'s database. */
  private def on(config: DbConfig, env: Map[String, String]): Map[String, String] =
    env ++ Map(
      DbConfig.UrlVar -> config.jdbcUrl,
      DbConfig.UserVar -> config.user,
      DbConfig.PasswordVar -> config.password
    )

  val tests = Tests {
    test(
      "in stub mode, a confidential conversation's turn is answered by the stub, whose pair the reference deployment clears"
    ) {
      val config = TestPostgres.freshDatabase("app_cleared_stub")
      // As the chat declares it: stub mode, no edge served.
      val deployment = right(
        Main.deployment(
          Map.empty,
          Offered.Read,
          Vector.empty,
          Vector.empty,
          java.time.ZoneOffset.UTC
        )
      )
      val secrets =
        Secrets.of(on(config, Map.empty), deployment).fold(r => sys.error(r.message), identity)
      val origin = Origin.Task("cleared", "secret")
      val engine = LiveEngine.open(config, Turn.Epoch, visibility = deployment.visibility)
      val replies =
        try {
          val launched = Main.ownEngine(engine, deployment, secrets, Launch.Run.Served)
          val c = right(
            launched.jot.write(Subject.Public)(
              launched.conversations
                .findOrCreate(origin, Account.Local, Label.at(Level.Confidential))
            )
          )
          val turn = right(
            launched.inbox.ingest(origin, SourceId("secret"), Message.User("hello"), Account.Local)
          )
          right(launched.inbox.startTurn(turn))
          val _ = launched.awaitTurn(turn)
          right(launched.db.read(Subject.Conversation(c.id))(launched.entries.list(c.id)))
            .collect { case e if e.turnSeq == turn.turnSeq => e.payload }
            .collect { case Payload.Message(Message.Assistant(blocks, _, _, _, _)) =>
              blocks.collect { case AssistantBlock.Text(t) => t }.mkString
            }
        } finally engine.close()
      // The stub's reply quotes the window it was sent; its opening is what it says.
      replies.map(r => if (r.startsWith("stub reply to:")) "a stub reply" else r) ==>
        Vector("a stub reply")
    }
  }
}
