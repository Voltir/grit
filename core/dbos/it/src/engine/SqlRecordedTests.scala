package grit.dbos.engine

import java.util.concurrent.atomic.AtomicInteger

import grit.core.admin.Administration
import grit.core.admin.AdministrationContract.{Declared, T1, ada, mia}
import grit.core.edge.{Joins, RecordedContract}
import grit.core.identity.{Standing, Vouched}
import grit.core.inbox.Inbox
import grit.core.store.Origin
import grit.core.visibility.{Label, Subject}
import grit.dbos.sql.{LiveDb, TestPostgres}

/** The recorded contract, kept by the engine's administration, joins and inbox over one
  * Postgres. Each test gets a database of its own, so no test sees another's rooms.
  */
object SqlRecordedTests extends RecordedContract {

  private val made = new AtomicInteger

  protected def withStores[A](
      body: (Administration, Joins, Inbox, Origin => Option[Label]) => A
  ): A = {
    val c = TestPostgres.freshDatabase(s"sql_recorded_${made.incrementAndGet()}")
    val engine = LiveEngine.open(c, "test", visibility = Declared)
    try {
      val voucher = engine.voucher(Set(T1), Set.empty)
      Vector(mia, ada).foreach { a =>
        LiveDb
          .under(c, Declared)(voucher.vouch(Vouched(a, Standing.Full(None))))
          .fold(e => throw new java.lang.AssertionError(s"vouching: $e"), identity)
      }
      body(
        engine.administration,
        engine.joins,
        engine.inbox,
        origin =>
          engine.db
            .read(Subject.Public)(engine.conversations.find(origin))
            .fold(e => throw new java.lang.AssertionError(s"reading: $e"), _.map(_.label))
      )
    } finally engine.close()
  }
}
