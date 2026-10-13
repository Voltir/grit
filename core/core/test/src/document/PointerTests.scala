package grit.core.document

import java.time.Instant

import grit.core.id.{DocKey, DocumentVersion, PluginName}
import grit.core.place.Place
import grit.core.tool.{ToolName, Writes}
import grit.core.visibility.Label

import utest.*

object PointerTests extends TestSuite {

  private val tool = ToolName("github_get_file_contents")

  private val pointer =
    Pointer
      .of(
        tool,
        "owner" -> ujson.Str("o"),
        "repo" -> ujson.Str("r"),
        "path" -> ujson.Str("p")
      )
      .fold(sys.error, identity)

  private def at(path: String): Option[Pointer] =
    Pointer.of(tool, "path" -> ujson.Str(path)).toOption

  private def version(n: Long): DocumentVersion =
    DocumentVersion.of(n).getOrElse(sys.error(s"version $n"))

  /** Version `n`, pointing as `pointer` says. */
  private def document(n: Long, pointer: Option[Pointer]): Document =
    Document(
      version(n),
      PluginName.of("cards").fold(sys.error, identity),
      DocKey.of(s"k$n").fold(sys.error, identity),
      Label.Public,
      Place.Everywhere,
      DocText.of(s"card $n").fold(sys.error, identity),
      ujson.Obj(),
      pointer,
      Instant.EPOCH,
      Placement(0, Instant.EPOCH),
      None
    )

  private def call(path: String, succeeded: Boolean = true, named: ToolName = tool) =
    Pointer.Call(named, ujson.Obj("path" -> ujson.Str(path), "ref" -> ujson.Str("main")), succeeded)

  val tests = Tests {
    test(
      "followedIn: each version whose pointer a call followed, once each, in the documents' order"
    ) {
      val documents = Vector(
        document(1, at("a")),
        document(2, None),
        document(3, at("b")),
        document(4, at("c"))
      )
      val calls = Vector(call("b"), call("a"), call("a"), call("c", named = ToolName("other")))
      Pointer.followedIn(documents, calls) ==> Vector(version(1), version(3))
    }

    test("followedIn: a call whose result was an error follows nothing") {
      val documents = Vector(document(1, at("a")), document(2, at("b")))
      Pointer.followedIn(documents, Vector(call("a", succeeded = false), call("b"))) ==>
        Vector(version(2))
    }

    test(
      "a call naming the tool with every pointed argument equal, and another, follows the pointer"
    ) {
      val call = ujson.Obj(
        "owner" -> ujson.Str("o"),
        "repo" -> ujson.Str("r"),
        "path" -> ujson.Str("p"),
        "ref" -> ujson.Str("main")
      )
      pointer.followedBy(tool, call) ==> true
    }

    test("a call differing in one pointed argument, or without it, does not follow the pointer") {
      val differing = ujson.Obj(
        "owner" -> ujson.Str("o"),
        "repo" -> ujson.Str("r"),
        "path" -> ujson.Str("q")
      )
      val without = ujson.Obj("owner" -> ujson.Str("o"), "repo" -> ujson.Str("r"))
      (pointer.followedBy(tool, differing), pointer.followedBy(tool, without)) ==> (false, false)
    }

    test("a call whose arguments are not an object does not follow the pointer") {
      pointer.followedBy(tool, ujson.Arr()) ==> false
      pointer.followedBy(tool, ujson.Str("owner")) ==> false
    }

    test("a pointer refuses two arguments of one name") {
      Pointer
        .of(tool, "path" -> ujson.Str("p"), "path" -> ujson.Str("q"))
        .map(_.shown) ==> Left("a pointer names argument path twice")
    }

    test("a pointer refuses an argument naming the destination, since it names a read") {
      Pointer
        .of(tool, Writes.Field -> ujson.Str("x"))
        .map(_.shown) ==> Left("a pointer names no destination (\"to\"): it reads")
    }

    test(
      "a pointer accepts arguments of exactly 1000 characters of compact JSON and refuses 1001"
    ) {
      val exact = ujson.Obj("k" -> ujson.Str("x" * 992))
      val over = ujson.Obj("k" -> ujson.Str("x" * 993))
      ujson.write(exact).length ==> 1000
      ujson.write(over).length ==> 1001
      Pointer.of(tool, "k" -> ujson.Str("x" * 992)).map(_ => ()) ==> Right(())
      Pointer.of(tool, "k" -> ujson.Str("x" * 993)).map(_ => ()) ==> Left(
        "a pointer's arguments are over 1000 characters of JSON"
      )
    }

    test(
      "shown is the read line with its arguments in compact JSON, keys sorted, and equal in either key order"
    ) {
      val reordered = Pointer
        .of(
          tool,
          "repo" -> ujson.Str("r"),
          "path" -> ujson.Str("p"),
          "owner" -> ujson.Str("o")
        )
        .fold(sys.error, identity)
      pointer.shown ==> """Read it with github_get_file_contents {"owner":"o","path":"p","repo":"r"}"""
      reordered.shown ==> pointer.shown
      reordered ==> pointer
    }

    test(
      "a pointer is unchanged by a change to its arguments as read, or to a value it was made from"
    ) {
      val at = ujson.Obj("ref" -> ujson.Str("main"))
      val made = Pointer.of(tool, "path" -> ujson.Str("p"), "at" -> at).fold(sys.error, identity)
      val shown = made.shown
      at("ref") = ujson.Str("dev")
      made.arguments("path") = ujson.Str("q")
      val fresh = Pointer
        .of(tool, "path" -> ujson.Str("p"), "at" -> ujson.Obj("ref" -> ujson.Str("main")))
        .fold(sys.error, identity)
      (made.shown, made) ==> (shown, fresh)
    }

    test(
      "an argument's nested objects are kept with their keys sorted at every depth: built in either order, equal pointers with one shown line"
    ) {
      val inner = Pointer.of(
        tool,
        "at" -> ujson.Obj(
          "b" -> ujson.Num(1),
          "a" -> ujson.Obj(
            "d" -> ujson.Num(1),
            "c" -> ujson.Arr(ujson.Obj("f" -> ujson.Num(2), "e" -> ujson.Num(3)))
          )
        )
      )
      val outer = Pointer.of(
        tool,
        "at" -> ujson.Obj(
          "a" -> ujson.Obj(
            "c" -> ujson.Arr(ujson.Obj("e" -> ujson.Num(3), "f" -> ujson.Num(2))),
            "d" -> ujson.Num(1)
          ),
          "b" -> ujson.Num(1)
        )
      )
      (inner.map(_.shown), outer.map(_.shown), inner == outer) ==> (
        Right(
          """Read it with github_get_file_contents {"at":{"a":{"c":[{"e":3,"f":2}],"d":1},"b":1}}"""
        ),
        Right(
          """Read it with github_get_file_contents {"at":{"a":{"c":[{"e":3,"f":2}],"d":1},"b":1}}"""
        ),
        true
      )
    }
  }
}
