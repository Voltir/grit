package grit.dbos.engine

import grit.core.admin.{Answer, Change, Command}
import grit.core.identity.{Standing, Vouched}
import grit.core.store.Origin
import grit.core.visibility.Label
import grit.dbos.sql.{DbConfig, LiveDb, SqlConversationStore, TestPostgres}

import utest.*

/** What the engine's administration keeps, read back as a conversation's label: a label set
  * for a room never heard, which has no place until the label makes one.
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
      "a label set through grit for a room never heard is the label of its first conversation"
    ) {
      withEngine("administration_in_force") { (c, engine) =>
        val origin = Origin.Slack("T1", "C-unheard", "1.0")
        val set =
          engine.administration.run(ada, origin.room, Command.SetLabel(confidentialTrial), t(1))
        val first = engine
          .conversation(origin, mia)
          .fold(e => throw new java.lang.AssertionError(s"a conversation: $e"), identity)
        (set, label(c, first)) ==> (
          Right(
            Answer.relabelled(
              new Change.Relabel(origin.room, Label.Public, Change.To.Set(confidentialTrial))
            )
          ),
          Some(confidentialTrial)
        )
      }
    }
  }
}
