package grit.core.place

import grit.core.store.Origin

import utest.*

object PlaceTests extends TestSuite {

  private def dir(path: String): Directory =
    Directory.of(path).fold(e => throw new java.lang.AssertionError(e), identity)

  private def place(text: String): Place =
    Place.read(text).fold(e => throw new java.lang.AssertionError(e), identity)

  val tests = Tests {
    test("a place under fs is its directory again; a place elsewhere is none") {
      val dir = Directory.of("/home/nick/api").getOrElse(throw new java.lang.AssertionError())
      val root = Directory.of("/").getOrElse(throw new java.lang.AssertionError())
      (Place.of(dir).directory, Place.of(root).directory) ==> (Some(dir), Some(root))
      Place.under(Namespace.Slack, Vector("acme", "dev")).directory ==> None
      Place.Everywhere.directory ==> None
    }

    test("a place's written form reads back to the same place, in every namespace") {
      val places = Vector(
        Place.of(dir("/home/nick/Projects/grit")),
        Place.of(dir("/")),
        Place.under(Namespace.Slack, Vector("acme", "#grit-dev", "1712.3")),
        Place.under(Namespace.Task, Vector("m0", "main")),
        Place.Everywhere
      )
      places.map(_.written) ==> Vector(
        "fs:/home/nick/Projects/grit",
        "fs:/",
        "slack:acme/#grit-dev/1712.3",
        "task:m0/main",
        "everywhere"
      )
      places.map(p => Place.read(p.written)) ==> places.map(Right(_))
      place("fs:home/nick") ==> place("fs:/home/nick")
    }

    test("a place is within its ancestors and itself, never within a sibling sharing a prefix") {
      val grit = place("fs:/home/nick/Projects/grit")
      assert(
        grit.within(grit),
        grit.within(place("fs:/home/nick")),
        grit.within(Place.Everywhere),
        !place("fs:/home/nick/Projects/grit2").within(grit),
        !place("fs:/home/nick").within(grit),
        !place("slack:home/nick/Projects/grit").within(grit)
      )
    }

    test("a directory is absolute and normalized") {
      Directory.of("home/nick") ==> Left("not an absolute path: home/nick")
      Directory.of("/home/../etc") ==> Left("not a normalized path (..): /home/../etc")
      Directory.of("/home//nick") ==> Left("not a normalized path (an empty segment): /home//nick")
      Directory.of("/home/nick/") ==> Left("not a normalized path (an empty segment): /home/nick/")
      Directory.of("/").map(Directory.value) ==> Right("/")
    }

    test("a place that does not read names why") {
      Place.read("home/nick") ==>
        Left("no namespace in home/nick: write it as fs:/a/path, slack:team/channel or task:name")
      Place.read("git:grit") ==> Left("no namespace git: fs, slack, task")
    }

    test("each origin's place, a field holding / split like the rest") {
      Origin.Tui(dir("/home/nick/api"), "default").place.written ==> "fs:/home/nick/api"
      Origin.Slack("acme", "C01", "1712.3").place.written ==> "slack:acme/C01/1712.3"
      Origin.Task("eval", "recent-fact/plain").place ==>
        Place.under(Namespace.Task, Vector("eval", "recent-fact", "plain"))
    }

    test("a scope holds the places within any of its prefixes; none holds nothing") {
      val scope = Scope.read("fs:/home/nick/Projects slack:acme").fold(e => sys.error(e), identity)
      assert(
        scope.holds(place("fs:/home/nick/Projects/api")),
        scope.holds(place("slack:acme/C01/1.2")),
        !scope.holds(place("fs:/tmp/x")),
        Scope.Everywhere.holds(place("task:m0/main")),
        !Scope.Off.holds(place("fs:/"))
      )
      scope.written ==> "fs:/home/nick/Projects slack:acme"
      Vector("none", "everywhere").map(Scope.read(_).map(_.written)) ==>
        Vector(Right("none"), Right("everywhere"))
      Scope.read("fs:/a bad") ==>
        Left("no namespace in bad: write it as fs:/a/path, slack:team/channel or task:name")
    }

    test("a weight is at least 1") {
      Weight.of(0.5) ==> Left("a weight is a number of at least 1, not 0.5")
      Weight.of(Double.NaN) ==> Left("a weight is a number of at least 1, not NaN")
      Weight.of(1.0).map(Weight.value) ==> Right(1.0)
    }
  }
}
