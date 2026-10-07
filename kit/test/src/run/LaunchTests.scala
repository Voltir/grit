package grit.kit.run

import grit.core.period.LifecycleSettings
import grit.core.place.{Locality, Scope, Weight}
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

  private def scoped(scope: Scope): LifecycleSettings = {
    val d = LifecycleSettings.Default
    LifecycleSettings
      .of(d.windows, d.balance, d.settle, d.resolveAt, d.asks, Locality(scope, Weight.Default))
      .fold(why => sys.error(why), identity)
  }

  val tests = Tests {
    test("a restart with a changed declaration puts the new settings in force") {
      val store = new InMemoryLifecycleStore
      val first = scoped(Scope.Everywhere)
      val changed = scoped(Scope.Room)
      Launch.declare(store, FakeJot, first) ==> Right(first)
      Launch.declare(store, FakeJot, changed) ==> Right(changed)
      FakeJot.write(Subject.Public)(store.current()) ==> Right(changed)
    }
  }
}
