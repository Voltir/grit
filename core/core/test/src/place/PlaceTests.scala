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
        Place.under(Namespace.Service, Vector("github")),
        Place.Everywhere
      )
      places.map(_.written) ==> Vector(
        "fs:/home/nick/Projects/grit",
        "fs:/",
        "slack:acme/#grit-dev/1712.3",
        "task:m0/main",
        "service:github",
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
      Place.read("git:grit") ==> Left("no namespace git: fs, slack, task, service")
    }

    test("each origin's place, a field holding / split like the rest") {
      Origin.Tui(dir("/home/nick/api"), "default").place.written ==> "fs:/home/nick/api"
      Origin.Slack("acme", "C01", "1712.3").place.written ==> "slack:acme/C01/1712.3"
      Origin.Task("eval", "recent-fact/plain").place ==>
        Place.under(Namespace.Task, Vector("eval", "recent-fact", "plain"))
    }

    test("a scope holds the places within any of its prefixes; none holds nothing") {
      val scope = Scope.read("fs:/home/nick/Projects slack:acme").fold(e => sys.error(e), identity)
      val room = place("fs:/home/nick/Projects/api")
      assert(
        scope.holds(room, place("fs:/home/nick/Projects/api")),
        scope.holds(room, place("slack:acme/C01/1.2")),
        !scope.holds(room, place("fs:/tmp/x")),
        Scope.Everywhere.holds(room, place("task:m0/main")),
        !Scope.Off.holds(room, place("fs:/home/nick/Projects/api"))
      )
      scope.written ==> "fs:/home/nick/Projects slack:acme"
      Vector("none", "everywhere").map(Scope.read(_).map(_.written)) ==>
        Vector(Right("none"), Right("everywhere"))
      Scope.read("fs:/a bad") ==>
        Left("no namespace in bad: write it as fs:/a/path, slack:team/channel or task:name")
    }

    test("a scope reads and writes room beside places, in the order written") {
      Scope.read("room fs:/home/nick") ==> Right(
        Scope(Vector(Prefix.Room, Prefix.At(place("fs:/home/nick"))))
      )
      Scope.read("slack:acme room").map(_.written) ==> Right("slack:acme room")
      Scope.Room.written ==> "room"
    }

    test(
      "with room, a place within the conversation's own room is held, and one outside it is not"
    ) {
      val channel = Origin.Slack("acme", "C01", "1.0").room
      assert(
        Scope.Room.holds(channel, place("slack:acme/C01/2.0")),
        !Scope.Room.holds(channel, place("slack:acme/C02/2.0")),
        !Scope.Room.holds(channel, place("fs:/home/nick"))
      )
    }

    test("a room: a thread's channel, a session's directory, a task's name") {
      Origin.Slack("acme", "C01", "1712.3").room.written ==> "slack:acme/C01"
      Origin.Tui(dir("/home/nick/api"), "default").room.written ==> "fs:/home/nick/api"
      Origin.Task("nightly", "7").room.written ==> "task:nightly"
    }

    test("a service is named by a lowercase letter, then lowercase letters, digits or _") {
      Service.of("git_hub2").map(_.place.written) ==> Right("service:git_hub2")
      Vector("GitHub", "2git", "git-hub", "").map(Service.of) ==> Vector(
        Left("a service's name is a lowercase letter, then lowercase letters, digits or _: GitHub"),
        Left("a service's name is a lowercase letter, then lowercase letters, digits or _: 2git"),
        Left(
          "a service's name is a lowercase letter, then lowercase letters, digits or _: git-hub"
        ),
        Left("a service's name is a lowercase letter, then lowercase letters, digits or _: ")
      )
    }

    test("a place is a service only as service:{a service's name}, never below one") {
      val github = Service.of("github").fold(e => throw new java.lang.AssertionError(e), identity)
      place("service:github").service ==> Some(github)
      Vector("service:GitHub", "service:github/issues", "fs:/github", "slack:github")
        .map(place(_).service) ==> Vector(None, None, None, None)
    }

    test("a conversation works in the service of the first link holding its place") {
      def service(name: String): Service =
        Service.of(name).fold(e => throw new java.lang.AssertionError(e), identity)
      val links = Vector(
        WorksIn(place("slack:acme/C01"), service("tracker")),
        WorksIn(place("slack:acme"), service("github"))
      )
      Vector("slack:acme/C01/1.2", "slack:acme/C02/1.2", "slack:other/C01/1.2", "fs:/home/nick")
        .map(p => WorksIn.of(links, place(p))) ==>
        Vector(Some(service("tracker")), Some(service("github")), None, None)
    }

    test(
      "a conversation reaches the service of every link holding its place, each once, first linked first"
    ) {
      def service(name: String): Service =
        Service.of(name).fold(e => throw new java.lang.AssertionError(e), identity)
      val links = Vector(
        Reaches(place("slack:acme/C01"), service("slack")),
        Reaches(place("slack:acme"), service("tracker")),
        Reaches(place("slack:"), service("slack"))
      )
      Vector("slack:acme/C01/1.2", "slack:acme/C02/1.2", "fs:/home/nick")
        .map(p => Reaches.of(links, place(p))) ==>
        Vector(
          Vector(service("slack"), service("tracker")),
          Vector(service("tracker"), service("slack")),
          Vector.empty
        )
    }

    test("a weight is at least 1") {
      Weight.of(0.5) ==> Left("a weight is a number of at least 1, not 0.5")
      Weight.of(Double.NaN) ==> Left("a weight is a number of at least 1, not NaN")
      Weight.of(1.0).map(Weight.value) ==> Right(1.0)
    }
  }
}
