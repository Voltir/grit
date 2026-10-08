package grit.dbos.engine

import grit.core.admin.{Answer, Change, Command}
import grit.core.identity.{Standing, Vouched}
import grit.core.store.Origin
import grit.core.visibility.Label
import grit.dbos.sql.{DbConfig, LiveDb, SqlConversationStore, TestPostgres}

import utest.*

/** What the engine's administration keeps, read back as the labels in force: a label set in
  * force from the next transaction, a room never heard before included.
  */
object AdministrationLiveTests extends TestSuite {
  import grit.core.admin.AdministrationContract.*

  /** A database of its own under [[Declared]], [[Members]] vouched full members, and its
    * engine; closed after `body`.
    */
  private def withEngine[A](name: String)(body: (DbConfig, Engine^) => A): A = {
    val c = TestPostgres.freshDatabase(name)
    val engine = LiveEngine.open(c, "test", visibility = Declared)
    try {
      val voucher = engine.voucher(Set(T1), Set.empty)
      Members.foreach { a =>
        LiveDb
          .under(c, Declared)(voucher.vouch(Vouched(a, Standing.Full(None))))
          .fold(e => throw new java.lang.AssertionError(s"vouching: $e"), identity)
      }
      body(c, engine)
    } finally engine.close()
  }

  private def label(c: DbConfig, id: grit.core.id.ConversationId): Option[Label] =
    LiveDb
      .under(c, Declared)(new SqlConversationStore().get(id))
      .fold(e => throw new java.lang.AssertionError(s"reading: $e"), _.map(_.label))

  val tests = Tests {
    test(
      "a label set is in force from the next transaction: a conversation begun after takes it, one begun before keeps its own, and a room never heard before is labelled too"
    ) {
      withEngine("administration_in_force") { (c, engine) =>
        def begun(channel: String, thread: String) =
          engine
            .conversation(Origin.Slack("T1", channel, thread), mia)
            .fold(e => throw new java.lang.AssertionError(s"a conversation: $e"), identity)
        val before = begun("C-heard", "1.0")
        val heard = Origin.Slack("T1", "C-heard", "1.0").room
        val unheard = Origin.Slack("T1", "C-unheard", "1.0").room
        val set = Vector(
          engine.administration.run(ada, heard, Command.SetLabel(internalTrial), t(1)),
          engine.administration.run(ada, unheard, Command.SetLabel(confidentialTrial), t(2))
        )
        val after = begun("C-heard", "2.0")
        val first = begun("C-unheard", "1.0")
        (set, label(c, before), label(c, after), label(c, first)) ==> (
          Vector(
            Right(
              Answer.relabelled(
                new Change.Relabel(heard, Label.Public, Change.To.Set(internalTrial))
              )
            ),
            Right(
              Answer.relabelled(
                new Change.Relabel(unheard, Label.Public, Change.To.Set(confidentialTrial))
              )
            )
          ),
          Some(Label.Public),
          Some(internalTrial),
          Some(confidentialTrial)
        )
      }
    }
  }
}
