package grit.commands.declare

import grit.core.durable.Probes

import utest.*

/** What capture checking rejects of what a deployment declares to the commands edge, pinned by
  * compiling probe sources against the module with its own flags: an own command's `run` is a
  * pure function, so it reaches the store only through the `ran` each request lends it, never
  * through the edge's stores or its listener captured where it was declared.
  */
object DeclaredPurityTests extends TestSuite {

  private val prelude =
    """package probe
      |import grit.commands.declare.OwnCommand
      |import grit.core.edge.EdgeStores
      |import grit.core.plugin.CommandOutcome
      |import grit.core.tool.ToolName
      |import grit.core.visibility.Integrity
      |import grit.edge.http.Listener
      |""".stripMargin

  private val lent =
    """def f() =
      |  OwnCommand.added(ToolName("mine"), Integrity.Member, (ran, args) => ran(ToolName("theirs"), args))
      |""".stripMargin

  private val holdingStores =
    """def f(stores: EdgeStores^) =
      |  OwnCommand.added(
      |    ToolName("mine"),
      |    Integrity.Member,
      |    (ran, args) => Right(CommandOutcome.Done(stores.commands.offered.size.toString))
      |  )
      |""".stripMargin

  private val holdingListener =
    """def f(listener: Listener^) =
      |  OwnCommand.guarding(
      |    ToolName("mine"),
      |    Integrity.Member,
      |    (ran, args) => { listener.close(); ran(ToolName("mine"), args) }
      |  )
      |""".stripMargin

  private lazy val compiled = Probes.batch(prelude, List(lent, holdingStores, holdingListener))

  val tests = Tests {
    test("the probe environment is set") {
      assert(Probes.options.contains("-language:experimental.captureChecking"))
    }

    test("an own command that runs through what it is lent compiles") {
      assert(compiled.errors(lent).isEmpty)
    }

    test("an own command holding the edge's stores is refused") {
      val errs = compiled.errors(holdingStores)
      assert(errs.nonEmpty, errs.exists(_.contains("cannot flow into capture set {}")))
    }

    test("an own command holding the edge's listener is refused") {
      val errs = compiled.errors(holdingListener)
      assert(errs.nonEmpty, errs.exists(_.contains("cannot flow into capture set {}")))
    }
  }
}
