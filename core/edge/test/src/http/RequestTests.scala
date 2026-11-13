package grit.edge.http

import utest.*

/** [[Request.of]]: how a request line's target, its headers and its method are read. */
object RequestTests extends TestSuite {

  private def read(target: String, headers: Vector[(String, String)] = Vector.empty): Request =
    Request
      .of(Verb.Get, target, headers, "")
      .fold(why => throw new java.lang.AssertionError(why), r => r)

  val tests = Tests {
    test("the path is split at each / and each segment decoded; . and .. are kept as sent") {
      (
        read("/").path,
        read("/a/b%20c/").path,
        read("/a%2Fb/+").path,
        read("/./../x").path,
        read("/caf%C3%A9").path
      ) ==> (
        Vector.empty,
        Vector("a", "b c", ""),
        Vector("a/b", "+"),
        Vector(".", "..", "x"),
        Vector("café")
      )
    }

    test("the query is decoded as a form, a name matched exactly, every value kept in order") {
      val r = read("/x?a=1&b=two+words&a=%C3%A9&flag&&c=&A=upper")
      (
        r.query("a"),
        r.queryValues("a"),
        r.query("b"),
        r.query("flag"),
        r.query("c"),
        r.queryValues("A"),
        r.query("missing"),
        r.path
      ) ==> (
        Some("1"),
        Vector("1", "é"),
        Some("two words"),
        Some(""),
        Some(""),
        Vector("upper"),
        None,
        Vector("x")
      )
    }

    test("every query parameter's name is listed once, in the order first sent") {
      read("/x?b=1&a=2&b=3&flag&A=4").queryNames ==> Vector("b", "a", "flag", "A")
    }

    test("a header is found by its name in any case, every value kept in order") {
      val r = read("/", Vector("Content-Type" -> "text/plain", "X-Trace" -> "1", "x-trace" -> "2"))
      (
        r.header("content-type"),
        r.header("CONTENT-TYPE"),
        r.headerValues("X-TRACE"),
        r.header("Accept")
      ) ==>
        (Some("text/plain"), Some("text/plain"), Vector("1", "2"), None)
    }

    test(
      "a target not starting with /, or with an escape that is not two hex digits of UTF-8, is refused"
    ) {
      val refused =
        Vector("http://example.org/x", "*", "", "x/y", "/a%zz", "/a%2", "/a%C3", "/?q=%FF", "/?%=1")
      refused.filter(t => Request.of(Verb.Get, t, Vector.empty, "").isRight) ==> Vector.empty
      Request.of(Verb.Get, "*", Vector.empty, "") ==> Left(
        "the target must be a path starting with /"
      )
      Request.of(Verb.Get, "/a%zz", Vector.empty, "") ==> Left(
        "an escape must be % and two hex digits"
      )
      Request.of(Verb.Get, "/a%C3", Vector.empty, "") ==> Left("the escapes must decode as UTF-8")
    }

    test("a method is its named verb only when written exactly so") {
      Vector("GET", "HEAD", "POST", "PUT", "PATCH", "DELETE", "OPTIONS", "get", "PROPFIND").map(
        Verb.of
      ) ==>
        Vector(
          Verb.Get,
          Verb.Head,
          Verb.Post,
          Verb.Put,
          Verb.Patch,
          Verb.Delete,
          Verb.Options,
          Verb.Other("get"),
          Verb.Other("PROPFIND")
        )
      Verb.of("PROPFIND").name ==> "PROPFIND"
    }
  }
}
