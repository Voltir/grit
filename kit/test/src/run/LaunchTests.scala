package grit.kit.run

import grit.core.period.LifecycleSettings
import grit.core.place.Weight
import grit.core.store.{InMemoryLifecycleStore, Jot, StoreError, Tx}
import grit.core.visibility.Subject
import grit.dbos.sql.TestTx

import utest.*

/** What [[Launch]] makes of a deployment's declared lifecycle settings. */
object LaunchTests extends TestSuite {

  private object FakeJot extends Jot {
    def write[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using
        TestTx.fake
      )
  }

  private def weighted(w: Double): LifecycleSettings = {
    val d = LifecycleSettings.Default
    Weight
      .of(w)
      .flatMap(LifecycleSettings.of(d.windows, d.balance, d.settle, d.resolveAt, d.asks, _))
      .fold(why => sys.error(why), identity)
  }

  val tests = Tests {
    test("a restart with a changed declaration puts the new settings in force") {
      val store = new InMemoryLifecycleStore
      val first = weighted(2)
      val changed = weighted(3)
      Launch.declare(store, FakeJot, first) ==> Right(first)
      Launch.declare(store, FakeJot, changed) ==> Right(changed)
      FakeJot.write(Subject.Public)(store.current()) ==> Right(changed)
    }
  }
}
