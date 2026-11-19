package grit.projects.method

import grit.projects.artifact.{Artifact, ArtifactHead}
import grit.projects.method.MethodFixtures.*
import grit.projects.name.{CheckName, KindName}
import grit.projects.render.ProjectGen

import utest.*

object MethodJsonTests extends TestSuite {

  /** A note kind, open then closed, its one transition naming `checks`, written by hand. */
  private def declared(checks: String*): ujson.Obj = ujson.Obj(
    "name" -> "grit",
    "version" -> 1,
    "kinds" -> ujson.Arr(
      ujson.Obj("name" -> "note", "first" -> "open", "states" -> ujson.Arr("closed"))
    ),
    "transitions" -> ujson.Arr(
      ujson.Obj(
        "kind" -> "note",
        "from" -> "open",
        "to" -> "closed",
        "checks" -> ujson.Arr.from(checks.map(ujson.Str(_)))
      )
    ),
    "views" -> ujson.Arr(
      ujson.Obj("role" -> "builder", "kinds" -> ujson.Arr("note"), "depth" -> 1)
    ),
    "checks" -> ujson.Arr.from(checks.map(c => ujson.Obj("name" -> c, "version" -> 1)))
  )

  /** `sections-present` as compiled at `v`, deciding nothing. */
  private def sectionsAt(v: Int): TransitionCheck = new TransitionCheck {
    val name: CheckName = check("sections-present")
    val version: Int = v
    def holds(
        judged: Artifact,
        heads: Vector[ArtifactHead],
        kinds: Vector[ArtifactKind]
    ): Either[String, Unit] =
      Right(())
  }

  val tests = Tests {
    test(
      "a generated method and its examples, written and read back, are equal under an equal digest"
    ) {
      (0 until 50).foreach { seed =>
        val Declared(method, examples) = ProjectGen.method(seed)
        val text = ujson.write(MethodJson.written(method, examples))
        val back = MethodJson.read(ujson.read(text), TransitionChecks.all)
        assert(
          back.map(d => (d.method.digest, d)) == Right((method.digest, Declared(method, examples)))
        )
      }
    }
    test(
      "an absent optional field reads as its default: no states, ends, sections, checks, action, " +
        "carried, and anyone moves"
    ) {
      val note = ArtifactKind(kind("note"), state("open"), Vector.empty, Set.empty, Vector.empty)
      val memo =
        ArtifactKind(kind("memo"), state("draft"), Vector(state("shut")), Set.empty, Vector.empty)
      val json = ujson.Obj(
        "name" -> "grit",
        "version" -> 2,
        "kinds" -> ujson.Arr(
          ujson.Obj("name" -> "note", "first" -> "open"),
          ujson.Obj("name" -> "memo", "first" -> "draft", "states" -> ujson.Arr("shut"))
        ),
        "transitions" -> ujson.Arr(ujson.Obj("kind" -> "memo", "from" -> "draft", "to" -> "shut")),
        "views" -> ujson.Arr(
          ujson.Obj("role" -> "builder", "kinds" -> ujson.Arr("note"), "depth" -> 0)
        )
      )
      val expected = Method.of(
        method,
        2,
        Vector(note, memo),
        Vector(
          Transition(memo.name, state("draft"), state("shut"), Vector(), TransitionAction.Nothing)
        ),
        Vector(right(RoleView.of(role("builder"), Set(note.name), 0))),
        TransitionChecks.all
      )
      val declared = Declared(right(expected.left.map(_.mkString("; "))), Vector.empty)
      assert(MethodJson.read(json, TransitionChecks.all) == Right(declared))
    }
    test("checks not compiled, or compiled at another version, give every reason at once") {
      assert(
        MethodJson.read(declared("children-ended", "sections-present"), Vector.empty) == Left(
          Vector("check children-ended is not compiled", "check sections-present is not compiled")
        )
      )
      assert(
        MethodJson.read(declared("sections-present"), Vector(sectionsAt(2))) == Left(
          Vector("check sections-present is compiled at version 2, not 1")
        )
      )
    }
    test("fields missing or malformed give every reason at once") {
      val json = declared("sections-present")
      json("kinds")(0).obj.remove("first")
      json("transitions")(0).obj.remove("to")
      json("views")(0)("depth") = "deep"
      json("examples") = ujson.Arr(ujson.Obj("name" -> "one", "moves" -> ujson.Arr()))
      assert(
        MethodJson.read(json, TransitionChecks.all) == Left(
          Vector(
            "kind 1 names no first",
            "transition 1 names no to",
            "view 1's depth is not a number",
            "example 1 names no start"
          )
        )
      )
    }
    test("an inconsistent method gives every reason Method.of gives, in words") {
      val json = declared()
      json("transitions")(0)("to") = "shut"
      json("kinds") = ujson.Arr(json("kinds")(0), json("kinds")(0))
      assert(
        MethodJson.read(json, TransitionChecks.all) == Left(
          Vector(
            "kind note is declared twice",
            "kind note declares no state shut",
            "state closed of kind note is reached by no transition from its first"
          )
        )
      )
    }

    test(
      "a kind's nesting is written only when not anywhere, and read back under an equal digest"
    ) {
      val m = right(
        MethodJson.read(declared(), TransitionChecks.all).left.map(_.mkString("; "))
      ).method
      def nested(nesting: Nesting) = right(
        Method
          .of(
            m.name,
            m.version,
            m.kinds.map(_.copy(nesting = nesting)),
            m.transitions,
            m.views,
            TransitionChecks.all
          )
          .left
          .map(_.mkString("; "))
      )
      def round(method: Method) = {
        val json = MethodJson.written(method, Vector.empty)
        val back = MethodJson.read(json, TransitionChecks.all).map(_.method.digest)
        (json("kinds")(0).obj.get("nesting"), back == Right(method.digest))
      }
      val under = right(Nesting.under(m.kinds.map(_.name).toSet))
      (round(m), round(nested(Nesting.Root)), round(nested(under))) ==> (
        (None, true),
        (Some(ujson.Str("root")), true),
        (
          Some(
            ujson.Obj("under" -> ujson.Arr.from(m.kinds.map(k => KindName.value(k.name)).sorted))
          ),
          true
        )
      )
    }
    test("a view's covers is written only when true, and read back under an equal digest") {
      val m = right(
        MethodJson.read(declared(), TransitionChecks.all).left.map(_.mkString("; "))
      ).method
      val covering = right(
        Method
          .of(
            m.name,
            m.version,
            m.kinds,
            m.transitions,
            m.views.map(v =>
              right(RoleView.of(v.role, v.kinds, v.depth, v.carried, covers = true))
            ),
            TransitionChecks.all
          )
          .left
          .map(_.mkString("; "))
      )
      def round(method: Method) = {
        val json = MethodJson.written(method, Vector.empty)
        val back = MethodJson.read(json, TransitionChecks.all).map(_.method.digest)
        (json("views")(0).obj.get("covers"), back == Right(method.digest))
      }
      (round(m), round(covering)) ==> ((None, true), (Some(ujson.True), true))
    }
    test("a nesting neither anywhere, root nor under kinds is refused, naming the kind") {
      val json = declared()
      json("kinds")(0)("nesting") = "inside"
      MethodJson.read(json, TransitionChecks.all).left.map(_.exists(_.contains("nesting"))) ==>
        Left(true)
    }
    test("movers and a move's mover are written only when not their default") {
      val m = right(
        MethodJson.read(declared(), TransitionChecks.all).left.map(_.mkString("; "))
      ).method
      val people = right(
        Method
          .of(
            m.name,
            m.version,
            m.kinds,
            m.transitions.map(_.copy(movers = Movers.People)),
            m.views,
            TransitionChecks.all
          )
          .left
          .map(_.mkString("; "))
      )
      val open = head("p/a", note, "open")
      def example(by: Mover) = Example(
        "e",
        project(artifact(open, "A note.")),
        Vector(Example.Move(open.key, state("closed"), Example.Expect.RefusedPersonOnly, by))
      )
      def written(method: Method, by: Mover) = {
        val json = MethodJson.written(method, Vector(example(by)))
        (json("transitions")(0).obj.get("movers"), json("examples")(0)("moves")(0).obj.get("by"))
      }
      (written(m, Mover.Agent), written(people, Mover.Person)) ==> (
        (None, None),
        (Some(ujson.Str("people")), Some(ujson.Str("person")))
      )
    }
  }
}
