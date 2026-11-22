package grit.projects.method

import grit.core.id.AgentType
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
    test(
      "a worked example of the old form, its artifacts keyed by name, reads as its form with ids, which is what is written"
    ) {
      // A declared method's examples are left out of its digest, so a version declared before
      // ids keeps its stored examples of the old form, and is read back through them for good.
      val digest =
        right(
          MethodJson.read(declared(), TransitionChecks.all).left.map(_.mkString)
        ).method.digest.hex
      val pin = s""""pin":{"method":"grit","version":1,"digest":"$digest"}"""
      def body(text: String) =
        s"""{"v":1,"notes":[],"blocks":[{"type":"para","text":[{"t":"$text"}]}]}"""
      def example(artifacts: String, moved: String) = ujson.read(
        s"""{"name":"a note closes","start":{"key":"p",$pin,"artifacts":[$artifacts]},""" +
          s""""moves":[{"key":"$moved","to":"closed","expect":{"kind":"allowed"}}]}"""
      )
      val old = example(
        s"""{"key":"p/b","head":{"kind":"note","state":"open","title":"B","parent":null,"cites":[],"by":null},"body":${body(
            "b"
          )}},""" +
          s"""{"key":"p/a","head":{"kind":"note","state":"open","title":"A","parent":"p/b","cites":["p/b"],"by":null},"body":${body(
              "a"
            )}}""",
        "p/a"
      )
      val withIds = example(
        s"""{"head":{"id":"p/1","handle":"b","kind":"note","state":"open","title":"B","parent":null,"rank":"n","cites":[],"by":null},"body":${body(
            "b"
          )}},""" +
          s"""{"head":{"id":"p/2","handle":"a","kind":"note","state":"open","title":"A","parent":"p/1","rank":"n","cites":["p/1"],"by":null},"body":${body(
              "a"
            )}}""",
        "p/2"
      )
      val read = (e: ujson.Value) =>
        MethodJson.read(
          ujson.Obj.from(declared().value ++ Seq("examples" -> ujson.Arr(e))),
          TransitionChecks.all
        )
      val fromOld = read(old)
      (
        fromOld == read(withIds),
        fromOld.map(d => MethodJson.written(d.method, d.examples).value.get("examples"))
      ) ==> (true, Right(Some(ujson.Arr(withIds))))
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
              right(RoleView.of(v.role, v.kinds, v.depth, v.carried, covers = Cover.Under))
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
    test("a view's holders are written only when not anyone, and read back under an equal digest") {
      val m = right(
        MethodJson.read(declared(), TransitionChecks.all).left.map(_.mkString("; "))
      ).method
      val coders = Holders.Agents
        .of(Set("coder-opus", "coder-haiku").map(t => right(AgentType.of(t))))
        .getOrElse(Holders.Anyone)
      def held(holders: Holders) = right(
        Method
          .of(
            m.name,
            m.version,
            m.kinds,
            m.transitions,
            m.views.map(v =>
              right(RoleView.of(v.role, v.kinds, v.depth, v.carried, v.covers, holders))
            ),
            TransitionChecks.all
          )
          .left
          .map(_.mkString("; "))
      )
      def round(method: Method) = {
        val json = MethodJson.written(method, Vector.empty)
        val back = MethodJson.read(json, TransitionChecks.all).map(_.method)
        (
          json("views")(0).obj.get("holders"),
          back.map(_.views.map(_.holders)),
          back.map(_.digest) == Right(method.digest)
        )
      }
      // Pinned: the stored form a method's file holds.
      Vector(Holders.Anyone, Holders.Session, coders).map(h => round(held(h))) ==> Vector(
        (None, Right(Vector(Holders.Anyone)), true),
        (Some(ujson.Str("session")), Right(Vector(Holders.Session)), true),
        (
          Some(ujson.Obj("agents" -> ujson.Arr("coder-haiku", "coder-opus"))),
          Right(Vector(coders)),
          true
        )
      )
    }
    test("holders neither a session nor agents of types kept whole are refused, naming the view") {
      def read(holders: ujson.Value) = {
        val json = declared()
        json("views")(0)("holders") = holders
        MethodJson.read(json, TransitionChecks.all).map(_ => ())
      }
      (
        read(ujson.Str("people")),
        read(ujson.Obj("agents" -> ujson.Arr())),
        read(ujson.Obj("agents" -> ujson.Arr("Explore")))
      ) ==> ((
        Left(Vector("""view 1's holders is neither session nor {"agents": [agent types]}""")),
        Left(Vector("view 1's agents names no agent type")),
        Left(
          Vector(
            "view 1's agents: an agent's type is kept whole only as a slug of at most 40 " +
              "characters that is not eight hex digits, alone or after a '-': Explore"
          )
        )
      ))
    }
    test(
      "a view's claimed-in is written only when it names a kind, and read back under an equal digest"
    ) {
      def method(claimedIn: Option[ujson.Value]) = {
        val json = declared()
        claimedIn.foreach(c => json("views")(0)("claimed-in") = c)
        right(MethodJson.read(json, TransitionChecks.all).left.map(_.mkString("; "))).method
      }
      def round(m: Method) = {
        val json = MethodJson.written(m, Vector.empty)
        (
          json("views")(0).obj.get("claimed-in"),
          MethodJson.read(json, TransitionChecks.all).map(_.method.digest) == Right(m.digest)
        )
      }
      val claimed = ujson.Obj("note" -> ujson.Arr("closed", "open"))
      val (plain, empty, named) =
        (
          method(None),
          method(Some(ujson.Obj())),
          method(Some(ujson.Obj("note" -> ujson.Arr("open", "closed"))))
        )
      // Pinned: the stored form a method's file holds, its states in order.
      (
        round(plain),
        round(empty),
        round(named),
        plain.digest == empty.digest,
        plain.digest == named.digest
      ) ==> (
        (None, true),
        (None, true),
        (Some(claimed), true),
        true,
        false
      )
    }
    test(
      "a view's claimed-in is refused for a kind it does not show, no state, or a state its kind lacks, naming the view"
    ) {
      def read(claimedIn: ujson.Value) = {
        val json = declared()
        json("views")(0)("claimed-in") = claimedIn
        MethodJson.read(json, TransitionChecks.all).map(_ => ())
      }
      (
        read(ujson.Arr("note")),
        read(ujson.Obj("plan" -> ujson.Arr("open"))),
        read(ujson.Obj("note" -> ujson.Arr())),
        read(ujson.Obj("note" -> ujson.Arr("shut")))
      ) ==> ((
        Left(Vector("view 1's claimed-in is not an object of kinds, each naming its states")),
        Left(Vector("view 1: a role view is claimed in kind plan, which it does not show")),
        Left(Vector("view 1: a role view is claimed in no state of kind note")),
        Left(
          Vector("role builder is claimed in state shut of kind note, which declares no such state")
        )
      ))
    }
    test("a nesting neither anywhere, root nor under kinds is refused, naming the kind") {
      val json = declared()
      json("kinds")(0)("nesting") = "inside"
      MethodJson.read(json, TransitionChecks.all).map(_ => ()) ==>
        Left(Vector("""kind 1's nesting is neither anywhere, root nor {"under": [kinds]}"""))
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
            m.transitions.map(_.copy(movers = Movers.Person)),
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
      (written(m, Mover.Other), written(people, Mover.Person)) ==> (
        (None, None),
        (Some(ujson.Str("people")), Some(ujson.Str("person")))
      )
    }
  }
}
