package grit.edge.desk

import grit.core.durable.Probes

import utest.*

/** What separation checking rejects of a [[Server]]'s tools and the log it is handed, pinned by
  * compiling probe sources against the edge with its own flags: tools that hold the server's
  * log are refused, so a tool reaches the log only through what [[Tools.run]] lends it.
  *
  * Expires: delete once separation checking is no longer experimental, as `SeparationTests`.
  */
object ServerSeparationTests extends TestSuite {

  private val prelude =
    """package probe
      |import grit.core.edge.{Desk, Desks, EdgeRefusal, Route, ServedEdge, ToolRequest}
      |import grit.core.place.Place
      |import grit.core.tool.{Outcome, ToolSet}
      |import grit.edge.desk.{Server, Tools}
      |""".stripMargin

  private val lent =
    """def f(desk: Desk^, log: String => Unit): Server^{desk, log} = {
      |  val tools = new Tools {
      |    def run(route: Route, q: ToolRequest, said: String => Unit): Outcome = {
      |      said("why")
      |      Outcome.Failed("try again")
      |    }
      |  }
      |  new Server(desk, tools, run => run(), log)
      |}
      |""".stripMargin

  private val held =
    """def f(desk: Desk^, log: String => Unit): Server^{desk, log} = {
      |  val tools = new Tools {
      |    def run(route: Route, q: ToolRequest, said: String => Unit): Outcome = {
      |      log("why")
      |      Outcome.Failed("try again")
      |    }
      |  }
      |  new Server(desk, tools, run => run(), log)
      |}
      |""".stripMargin

  /** [[Server.open]] serving tools that log through what each run lends them. */
  private val openLent =
    """def g(desks: Desks^, place: Place, log: String => Unit)
      |    : Either[EdgeRefusal, ServedEdge.Open^{desks, log, caps.any}] =
      |  Server.open(desks, place, log) { desk =>
      |    val tools = new Tools {
      |      def run(route: Route, q: ToolRequest, said: String => Unit): Outcome = {
      |        said("why")
      |        Outcome.Failed("try again")
      |      }
      |    }
      |    Right(new Server.Advertised(ToolSet.Empty, tools))
      |  }
      |""".stripMargin

  /** [[Server.open]] serving tools that hold the log `open` is handed. */
  private val openHeld =
    """def g(desks: Desks^, place: Place, log: String => Unit)
      |    : Either[EdgeRefusal, ServedEdge.Open^{desks, log, caps.any}] =
      |  Server.open(desks, place, log) { desk =>
      |    val tools = new Tools {
      |      def run(route: Route, q: ToolRequest, said: String => Unit): Outcome = {
      |        log("why")
      |        Outcome.Failed("try again")
      |      }
      |    }
      |    Right(new Server.Advertised(ToolSet.Empty, tools))
      |  }
      |""".stripMargin

  private lazy val compiled = Probes.batch(prelude, List(lent, held, openLent, openHeld))

  val tests = Tests {
    test("the probe environment is set") {
      assert(Probes.options.contains("-language:experimental.separationChecking"))
    }

    test("tools that log through what each run lends them are served with the edge's log") {
      assert(compiled.errors(lent).isEmpty)
    }

    test("tools that hold the log the server is handed are refused") {
      val errs = compiled.errors(held)
      assert(errs.nonEmpty, errs.forall(_.contains("Separation failure")))
    }

    test("tools served through open that log through what each run lends them compile") {
      assert(compiled.errors(openLent).isEmpty)
    }

    test("tools served through open that hold the log open is handed are refused") {
      val errs = compiled.errors(openHeld)
      assert(errs.nonEmpty, errs.forall(_.contains("Separation failure")))
    }
  }
}
