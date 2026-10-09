package grit.outline.render

import grit.outline.model.{Defn, Kind, Lines}
import grit.outline.trace.Traced

import utest.*

object OneLineTests extends TestSuite {

  private def defn(
      kind: Kind,
      name: String,
      fullName: String,
      lines: Lines,
      signature: String,
      parents: Vector[String],
      members: Vector[Defn] = Vector.empty,
      isAbstract: Boolean = false
  ): Defn =
    Defn(
      kind = kind,
      name = name,
      fullName = fullName,
      file = "p/S.scala",
      lines = lines,
      doc = None,
      signature = signature,
      body = None,
      members = members,
      refs = Vector.empty,
      parents = parents,
      isPrivate = false,
      isAbstract = isAbstract
    )

  private val posed = defn(
    Kind.Trait,
    "Posed",
    "p.Posed",
    Lines(1, 3),
    "trait Posed",
    Vector.empty,
    isAbstract = true
  )

  private val text = defn(
    Kind.CaseClass,
    "Text",
    "p.Text",
    Lines(23, 23),
    "/** The text. */ final case class Text(request: ModelRequest) extends Posed[Int]",
    Vector("p.Posed")
  )

  private val companion = defn(
    Kind.Object,
    "Companion",
    "p.Companion",
    Lines(20, 30),
    "object Companion",
    Vector.empty,
    Vector(text)
  )

  def tests = Tests {
    test(
      "a trait's one-liner lists each implementor as its bare constructor, without modifiers, extends clause or doc"
    ) {
      assert(
        Render.oneLine(
          posed,
          Vector(companion)
        ) == "→ 1-3 trait Posed: Text(request: ModelRequest) :23"
      )
    }

    test("the library footer names no synthetic entry such as scala.<repeated>") {
      val answer = Render.show(
        named = Vector(posed),
        traced = Traced(Vector.empty, Vector("scala.<repeated>", "scala.Int"), Vector.empty),
        companions = Vector.empty,
        stale = Set.empty,
        bodies = Set.empty,
        withPrivate = false,
        cap = 80000
      )
      assert(answer.contains("scala.Int"))
      assert(!answer.contains("<repeated>"))
    }
  }
}
