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

  /** The administering tools `d` is launched with, by name. */
  private def administering(d: grit.kit.deployment.Deployment): Vector[String] = {
    val admin: grit.core.admin.Administration^ = grit.core.admin.InMemoryAdministration.none()
    val clock: grit.core.clock.Clock^ = grit.core.clock.Clock.system()
    Launch.administering(d, admin, clock).map(t => grit.core.tool.ToolName.value(t.name))
  }

  val tests = Tests {
    test(
      "the admin tools are offered under Offered.Read, and not while a served edge is Unanswered"
    ) {
      import grit.core.edge.ServedEdge.Asks
      import grit.kit.deployment.Deployments.{accepted, edge, of}
      val answered =
        accepted(edges = Vector(edge("slack", Asks.Answered), edge("github", Asks.Never)))
      val unanswered =
        of(edges = Vector(edge("slack", Asks.Answered), edge("tui", Asks.Unanswered)))
          .fold(r => sys.error(r.message), identity)
      (answered.offer.tools, administering(answered), administering(unanswered)) ==> (
        grit.kit.deployment.Offered.Read,
        Vector("label_room", "quiet_room"),
        Vector.empty
      )
    }

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
