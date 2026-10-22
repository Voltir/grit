package grit.prose.form

import grit.core.durable.Probes

import utest.*

/** What the compiler rejects of a table built outside prose: only [[Block.table]] builds one,
  * so every table's rows are as wide as its columns and a renderer need not fill or cut them.
  * Pinned by compiling probe sources against prose, as a package outside it.
  */
object TableProbeTests extends TestSuite {

  private val prelude =
    """package probe
      |import grit.prose.form.{Block, CellKind, Column, Text}
      |
      |def columns: Vector[Column[Nothing]] = Vector(Column(Text.plain("a"), CellKind.Words))
      |""".stripMargin

  private val built =
    """def t: Block[Nothing] = Block.Table(columns, Vector(Vector.empty))
      |""".stripMargin

  private val copied =
    """def f(t: Block.Table[Nothing]): Block[Nothing] = t.copy(rows = Vector(Vector.empty))
      |""".stripMargin

  private val made =
    """def t: Block[Nothing] = Block.table(columns, Vector(Vector.empty))
      |""".stripMargin

  private lazy val compiled = Probes.batch(prelude, List(built, copied, made))

  val tests = Tests {
    test("a table built by its constructor outside prose is rejected") {
      compiled.errors(built) ==> List("object Table in object Block does not take parameters")
    }

    test("a table copied with other rows outside prose is rejected") {
      val errs = compiled.errors(copied)
      assert(
        errs.size == 1,
        errs.forall(_.contains("private[prose] method copy can only be accessed"))
      )
    }

    test("a table made by Block.table is accepted") {
      compiled.errors(made) ==> Nil
    }
  }
}
