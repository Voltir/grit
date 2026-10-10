package grit.core.document

import grit.core.tool.{ToolName, Writes}

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

  val tests = Tests {
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

    test("a call differing in one pointed argument does not follow the pointer") {
      val call = ujson.Obj(
        "owner" -> ujson.Str("o"),
        "repo" -> ujson.Str("r"),
        "path" -> ujson.Str("q")
      )
      pointer.followedBy(tool, call) ==> false
    }

    test("a call whose arguments are not an object does not follow the pointer") {
      pointer.followedBy(tool, ujson.Arr()) ==> false
      pointer.followedBy(tool, ujson.Str("owner")) ==> false
    }

    test("a call of the unprefixed name does not follow the pointer, with equal arguments") {
      val call = ujson.Obj(
        "owner" -> ujson.Str("o"),
        "repo" -> ujson.Str("r"),
        "path" -> ujson.Str("p")
      )
      pointer.followedBy(ToolName("get_file_contents"), call) ==> false
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
  }
}
