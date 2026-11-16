package grit.kit.run

import grit.core.period.LifecycleSettings
import grit.core.place.Weight
import grit.core.store.{InMemoryLifecycleStore, TestJot}
import grit.core.visibility.Subject

import utest.*

/** What [[Launch]] makes of a deployment's declared lifecycle settings. */
object LaunchTests extends TestSuite {

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

    test("a served edge's renders reach the turn tooling by its name; one that says none is not") {
      import grit.core.edge.ServedEdge.Asks
      import grit.kit.deployment.Deployments.{accepted, edge}
      val d = accepted(edges =
        Vector(edge("slack", Asks.Answered, shows = Some("bold")), edge("github", Asks.Never))
      )
      Launch.renders(d) ==> Map(grit.core.id.EdgeName("slack") -> "bold")
    }

    test("a restart with a changed declaration puts the new settings in force") {
      val store = new InMemoryLifecycleStore
      val first = weighted(2)
      val changed = weighted(3)
      Launch.declare(store, TestJot(), first) ==> Right(first)
      Launch.declare(store, TestJot(), changed) ==> Right(changed)
      TestJot().write(Subject.Public)(store.current()) ==> Right(changed)
    }
  }
}
