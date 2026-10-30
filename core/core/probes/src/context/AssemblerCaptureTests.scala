package grit.core.context

import grit.core.durable.Probes

import utest.*

/** An assembler makes no call: every model and classifier call a window needs is the turn's,
  * made between the assembler's reads and recorded apart from them, so one that holds a
  * capability, or hands the turn a query writer's outcome that holds one, does not compile.
  */
object AssemblerCaptureTests extends TestSuite {

  private val prelude =
    """package outside
      |import grit.core.context.*
      |import grit.core.message.Message
      |import grit.core.provider.ModelRequest
      |import grit.core.store.Db
      |trait Provider extends caps.SharedCapability { def call(): String }
      |trait Store { def count(): Int }
      |""".stripMargin

  /* An assembler whose `field` it draws its window with; its type left to inference, so only
   * the assembler's own self type can refuse what it holds. */
  private def assembler(field: String, use: String): String =
    s"""def make($field): Unit = {
       |    val made = new ContextAssembler {
       |      def prepare(request: AssemblyRequest)(using Db^): Either[AssemblyError, Prepared] =
       |        Right(Prepared.Drawn(Querying.Drawn(Vector(AssemblyNote.FellBack($use)))))
       |      def search(request: AssemblyRequest, written: Querying.Written)(using Db^): Either[AssemblyError, Results] =
       |        Right(Results.empty)
       |      def judgeText(request: AssemblyRequest, retrieved: Vector[Retrieved], chars: Int)(using Db^): Either[AssemblyError, JudgeText] =
       |        Right(JudgeText("", Vector.empty))
       |      def assemble(request: AssemblyRequest, packing: Packing)(using Db^): Either[AssemblyError, Projection] =
       |        Right(Projection.Boundary(Window(Vector.empty), grit.core.message.Tokens.Zero))
       |    }
       |    val _ = made
       |  }""".stripMargin

  private val storing = assembler("s: Store", "s.count().toString")
  private val calling = assembler("p: Provider^", "p.call()")
  private val asking =
    // Its type left to inference, so only `Asking`'s own field type can refuse `p`.
    """def ask(p: Provider^): Unit = {
      |    val asked = Prepared.Asking(ModelRequest("q", Vector.empty), reply => Querying.Drawn(Vector(AssemblyNote.FellBack(p.call()))))
      |    val _ = asked
      |  }""".stripMargin
  /* A window drawn with no word on what is carried would be a boundary on every turn: the
   * request has no default for it. */
  private def requesting(args: String): String =
    s"""def request(t: grit.core.id.TurnRef): Unit = {
       |    val r = AssemblyRequest($args)
       |    val _ = r
       |  }""".stripMargin
  private val carrying = requesting("t, Width.Deployed, CarriedWindow.Empty")
  private val forgetting = requesting("t, Width.Deployed")
  private lazy val compiled = Probes.batch(prelude, List(storing, calling, asking))
  // Its own batch: a typer error stops a run before the capture checker reaches the others.
  private lazy val requested = Probes.batch(prelude, List(carrying, forgetting))

  val tests = Tests {
    test("one compile answers every probe") {
      assert(compiled.runs == 1)
    }

    test("control: an assembler holding only stores compiles") {
      compiled.errors(storing) ==> Nil
    }

    test("control: a request naming its carried window compiles") {
      requested.errors(carrying) ==> Nil
    }

    test("a request that names no carried window is rejected") {
      val errs = requested.errors(forgetting)
      assert(errs.exists(_.contains("missing argument for parameter carried")))
    }

    test("an assembler that calls a capability it holds is rejected") {
      val errs = compiled.errors(calling)
      assert(
        errs.exists(e =>
          e.contains("Reference `p` is not included in the allowed capture set {}") &&
            e.contains("self type")
        )
      )
    }

    test("a query writer's outcome that calls a capability is rejected") {
      val errs = compiled.errors(asking)
      assert(
        errs.exists(e =>
          e.contains("Note that capability `p` cannot flow into capture set {}") &&
            e.contains("Querying")
        )
      )
    }
  }
}
