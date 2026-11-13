package grit.edge.http

import grit.core.durable.Probes

import utest.*

/** What capture and separation checking reject of a [[Listener]], pinned by compiling probe
  * sources against the edge with its own flags: an `answer` that holds the log the listener is
  * handed is refused, so it reaches the log only through what each request lends it; and a
  * Listener cannot be typed with fewer captures than its `answer` and `log` reach.
  */
object ListenerSeparationTests extends TestSuite {

  private val prelude =
    """package probe
      |import grit.core.edge.EdgeRefusal
      |import grit.edge.http.{Listen, Listener, Response, Status}
      |""".stripMargin

  private val lent =
    """def f(at: Listen, log: String => Unit): Either[EdgeRefusal, Listener^{log}] =
      |  Listener.open(at, (r, said) => { said("why"); Response.text(Status.Ok, "") }, log)
      |""".stripMargin

  private val held =
    """def f(at: Listen, log: String => Unit): Either[EdgeRefusal, Listener^{log}] =
      |  Listener.open(at, (r, said) => { log("why"); Response.text(Status.Ok, "") }, log)
      |""".stripMargin

  private val keptWide =
    """def f(at: Listen, reach: String => Unit, log: String => Unit): Either[EdgeRefusal, Listener^{reach, log}] =
      |  Listener.open(at, (r, said) => { reach("why"); Response.text(Status.Ok, "") }, log)
      |""".stripMargin

  private val keptNarrow =
    """def f(at: Listen, reach: String => Unit, log: String => Unit): Either[EdgeRefusal, Listener^{log}] =
      |  Listener.open(at, (r, said) => { reach("why"); Response.text(Status.Ok, "") }, log)
      |""".stripMargin

  private lazy val compiled = Probes.batch(prelude, List(lent, held, keptWide, keptNarrow))

  val tests = Tests {
    test("the probe environment is set") {
      assert(Probes.options.contains("-language:experimental.separationChecking"))
    }

    test("an answer that logs through what each request lends it is listened with the log") {
      assert(compiled.errors(lent).isEmpty)
    }

    test("an answer that holds the log the listener is handed is refused") {
      val errs = compiled.errors(held)
      assert(errs.nonEmpty, errs.forall(_.contains("Separation failure")))
    }

    test("a listener typed with every capability its answer and log reach compiles") {
      assert(compiled.errors(keptWide).isEmpty)
    }

    test("a listener typed without a capability its answer reaches is refused") {
      val errs = compiled.errors(keptNarrow)
      assert(errs.nonEmpty, errs.exists(_.contains("reach")))
    }
  }
}
