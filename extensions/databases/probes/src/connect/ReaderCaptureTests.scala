package grit.databases.connect

import grit.core.durable.Probes

import utest.*

/** That a [[DatabaseReader]] is a capability, pinned by compiling probe sources against the
  * module with its own flags: a pure function may not hold one, so nothing pure (a tool's
  * description, a request's outcome) can carry a connection to the database.
  */
object ReaderCaptureTests extends TestSuite {

  private val prelude =
    """package probe
      |import grit.databases.connect.DatabaseReader
      |""".stripMargin

  private val lent =
    """def f(reader: DatabaseReader): () ->{reader} Unit = () => reader.close()
      |""".stripMargin

  private val held =
    """def f(reader: DatabaseReader): () -> Unit = () => reader.close()
      |""".stripMargin

  private lazy val compiled = Probes.batch(prelude, List(lent, held))

  val tests = Tests {
    test("the probe environment is set") {
      assert(Probes.options.contains("-language:experimental.captureChecking"))
    }

    test("a function that names the reader it holds compiles") {
      assert(compiled.errors(lent).isEmpty)
    }

    test("a pure function holding a reader is refused") {
      val errs = compiled.errors(held)
      assert(errs.nonEmpty, errs.exists(_.contains("reader")))
    }
  }
}
