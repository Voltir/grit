package grit.core.edge

import grit.core.durable.Probes

import utest.*

/** A route is made only by [[Edges.authorize]]: outside `grit.core.edge` no route can be
  * built, so a tool runs a repository's request only in the run its conversation's origin names.
  */
object RouteVisibilityTests extends TestSuite {

  private val prelude =
    """package outside
      |import grit.core.edge.*
      |import grit.core.place.{Repository, WorkName}
      |""".stripMargin

  private val matched =
    "def f(r: Route) = r match { case Route.Repository(repo, work) => repo.place; case _ => () }"
  private val built =
    """val r = for { repo <- Repository.of("grit"); work <- WorkName.of("w1") }
      |  yield Route.Repository(repo, work)""".stripMargin

  private val made =
    """def f(repo: Repository, work: WorkName) = new Route.Repository(repo, work)"""

  private lazy val compiled = Probes.batch(prelude, List(matched, built, made))

  private def probe(body: String): List[String] = compiled.errors(body)

  val tests = Tests {
    test("two compiles answer every probe: the typer's refusals, then the control") {
      compiled.runs ==> 2
    }

    test("control: a repository route is matched outside its package") {
      probe(matched) ==> Nil
    }

    test("a repository route cannot be built outside its package") {
      // Its constructor private, the companion's apply is too: the compiler finds no apply.
      assert(
        probe(built).exists(
          _.contains("object Repository in object Route does not take parameters")
        ),
        probe(made).exists(_.contains("can only be accessed from package grit.core.edge"))
      )
    }
  }
}
