package grit.projects.plugin

import grit.projects.method.Method
import grit.projects.paper.{ElephantGoldfishTests, OpenSpecTests}

import utest.*

object StaleTests extends TestSuite {

  import CommandFixtures.*

  private val day: Long = 24L * 60 * 60

  /** Keeps `p/<local>` of `kind`, holding `body`, on day `d` after [[at]], with `more`. */
  private def keptOn(
      projects: Projects,
      store: Store,
      d: Int,
      local: String,
      kind: String,
      body: String,
      more: (String, ujson.Value)*
  ): Unit =
    done(
      askAt(at.plusSeconds(day * d))(
        projects,
        store,
        "project_keep",
        (Vector[(String, ujson.Value)](
          "key" -> s"p/$local",
          "kind" -> kind,
          "title" -> s"Title $local",
          "body" -> body
        ) ++ more)*
      )
    )

  private def movedOn(projects: Projects, store: Store, d: Int, local: String, to: String): Unit =
    done(
      askAt(at.plusSeconds(day * d))(
        projects,
        store,
        "project_move",
        "key" -> s"p/$local",
        "state" -> to
      )
    )

  /** What `project_stale` answers on day `d` for `weeks`. */
  private def staleOn(projects: Projects, store: Store, d: Int, weeks: Int): String =
    done(
      askAt(at.plusSeconds(day * d))(
        projects,
        store,
        "project_stale",
        "project" -> "p",
        "weeks" -> weeks
      )
    )

  val tests: Tests = Tests {

    test(
      "project_stale lists the artifacts not ended untouched past its weeks that none not ended names, oldest first"
    ) {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      keptOn(projects, store, 0, "e", "note", "Cited by d alone.")
      keptOn(projects, store, 0, "b", "note", "Cited by c.")
      keptOn(projects, store, 1, "d", "note", "See [e](artifact:p/e).")
      movedOn(projects, store, 1, "d", "closed")
      keptOn(projects, store, 2, "a", "note", "Alone.")
      keptOn(projects, store, 30, "c", "note", "See [b](artifact:p/b).")
      staleOn(projects, store, 35, 2) ==> "p/e (note, open): Title e\np/a (note, open): Title a"
    }

    test(
      "under grit's, OpenSpec's and elephant-goldfish's methods, an ended child leaves its parent stale, and a cascade withdraws both"
    ) {
      val under: Vector[(Method, String, String, String)] = Vector(
        (method(), "note", "open", "closed"),
        (OpenSpecTests.openSpec, "task", "open", "done"),
        (ElephantGoldfishTests.model, "decision", "proposed", "rejected")
      )
      val answers = under.map { (m, kind, _, end) =>
        val projects = Projects
          .of(Vector(m -> Vector.empty), m.name)
          .fold(e => throw new java.lang.AssertionError(e.toString), identity)
        val store = Store()
        started(projects, store)
        keptOn(projects, store, 0, "x", kind, "The parent.")
        keptOn(projects, store, 0, "y", kind, "The child.", "parent" -> "p/x")
        movedOn(projects, store, 0, "y", end)
        val stale = staleOn(projects, store, 35, 2)
        val withdrawn = done(
          askAt(at.plusSeconds(day * 35))(
            projects,
            store,
            "project_withdraw",
            "key" -> "p/x",
            "cascade" -> true
          )
        ).linesIterator.toVector.headOption
        (stale, withdrawn)
      }
      answers ==> under.map((_, kind, first, _) =>
        (s"p/x ($kind, $first): Title x", Some("Withdrawn: p/x, p/y"))
      )
    }
  }
}
