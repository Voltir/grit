package grit.core.prompt

import grit.core.durable.Probes

import utest.*

/** What the compiler rejects about [[Fenced]]: a part of a fence rendered on its own, which
  * would show a model material outside any fence, or passed where a fence is wanted. Typer
  * errors, compiled against core; the control must compile.
  */
object FencedProbeTests extends TestSuite {

  private val prelude =
    """package grit.probe
      |import grit.core.prompt.*
      |""".stripMargin

  private val control = "def s: String = Fenced.in(Tag.Document, \"x\").rendered"
  private val materialRendered = "def s = Fenced.Part.Material(\"x\").rendered"
  private val wordsRendered = "def s = Fenced.Part.Words(\"x\").rendered"
  private val partAsFence =
    "def f(x: Fenced): String = x.rendered\ndef s = f(Fenced.Part.Material(\"x\"))"

  private val rendered = List(materialRendered, wordsRendered)

  private lazy val compiled = Probes.batch(prelude, control :: partAsFence :: rendered)

  val tests = Tests {
    test("the control compiles: a fence renders") {
      compiled.errors(control) ==> Nil
    }

    test("a part has no rendering of its own") {
      rendered.filterNot(p =>
        compiled.errors(p).exists(_.contains("value rendered is not a member"))
      ) ==> Nil
    }

    test("a part is not a fence") {
      compiled
        .errors(partAsFence)
        .exists(e =>
          e.contains("Found:") && e.contains("Required: grit.core.prompt.Fenced")
        ) ==> true
    }
  }
}
