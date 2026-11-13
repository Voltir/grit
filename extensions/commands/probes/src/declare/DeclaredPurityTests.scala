package grit.commands.declare

import grit.core.durable.Probes

import utest.*

/** What capture checking rejects of what a deployment declares to the commands edge, pinned by
  * compiling probe sources against the module with its own flags: an own command's `run` and a
  * holding watch's functions are pure, so an own command reaches the store only through the `ran`
  * each request lends it, and neither through the edge's stores or its listener captured where it
  * was declared.
  */
object DeclaredPurityTests extends TestSuite {

  private val prelude =
    """package probe
      |import grit.commands.declare.{HoldingWatch, OwnCommand}
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

  private val watchReading =
    """def f() = HoldingWatch(_ => None, (h, held) => s"held at ${held.version}")
      |""".stripMargin

  private val watchHoldingStores =
    """def f(stores: EdgeStores^) =
      |  HoldingWatch(_ => None, (h, held) => s"held among ${stores.commands.offered.size}")
      |""".stripMargin

  private lazy val compiled = Probes.batch(
    prelude,
    List(lent, holdingStores, holdingListener, watchReading, watchHoldingStores)
  )

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

    test("a holding watch that reads only what it is given compiles") {
      assert(compiled.errors(watchReading).isEmpty)
    }

    test("a holding watch holding the edge's stores is refused") {
      val errs = compiled.errors(watchHoldingStores)
      assert(errs.nonEmpty, errs.exists(_.contains("cannot flow into capture set {}")))
    }
  }
}
