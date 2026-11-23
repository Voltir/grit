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

    test(
      "a direct message's thread reads back to itself and is direct; a channel is not, nor a place holding direct below its head"
    ) {
      val thread = place("direct:slack/T/U/1712.3")
      (
        thread.written,
        thread.direct,
        place("direct:slack/T/U").direct,
        place("slack:T/C").direct,
        place("slack:T/direct/1712.3").direct
      ) ==>
        ("direct:slack/T/U/1712.3", true, true, false, false)
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
    test(
      "a directory holding a control character is refused; a stored place holding one is its directory"
    ) {
      (
        Directory.of("/work/a\nb"),
        Directory.of("/work/\u007f"),
        Directory.of("/work/\u0085b"),
        place("fs:/work/a\nb").directory.map(Directory.value)
      ) ==> (
        Left("holds a control character (U+000A): /work/a?b"),
        Left("holds a control character (U+007F): /work/?"),
        Left("holds a control character (U+0085): /work/?b"),
        Some("/work/a\nb")
      )
    }

    test("a place that does not read names why") {
      Place.read("home/nick") ==>
        Left("no namespace in home/nick: write it as fs:/a/path, slack:team/channel or task:name")
      Place.read("git:grit") ==>
        Left("no namespace git: fs, slack, task, service, direct, machine, repository")
    }

    test("each origin's place, a field holding / split like the rest") {
      Origin.Tui(dir("/home/nick/api"), "default").place.written ==> "fs:/home/nick/api"
      Origin.Slack("acme", "C01", "1712.3").place.written ==> "slack:acme/C01/1712.3"
      Origin.Task("eval", "recent-fact/plain").place ==>
        Place.under(Namespace.Task, Vector("eval", "recent-fact", "plain"))
    }

    test("a machine's directory reads back to itself and is no directory of grit's own machine") {
      Place.read("machine:desktop/home/a").map(p => (p.written, p.directory)) ==>
        Right(("machine:desktop/home/a", None))
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

    test("a repository and a run of work in it read back from their written form") {
      Vector("repository:grit", "repository:grit/fix-typo").map(Place.read(_).map(_.written)) ==>
        Vector(Right("repository:grit"), Right("repository:grit/fix-typo"))
    }

    test("a repository is named as a service is, and its place is repository:{name}") {
      Repository.of("my_app2").map(r => (r.name, r.place.written)) ==>
        Right(("my_app2", "repository:my_app2"))
      Vector("MyApp", "2app", "my-app", "").map(Repository.of) ==> Vector(
        Left(
          "a repository's name is a lowercase letter, then lowercase letters, digits or _: MyApp"
        ),
        Left(
          "a repository's name is a lowercase letter, then lowercase letters, digits or _: 2app"
        ),
        Left(
          "a repository's name is a lowercase letter, then lowercase letters, digits or _: my-app"
        ),
        Left("a repository's name is a lowercase letter, then lowercase letters, digits or _: ")
      )
    }

    test("a place names the repository it is or lies within; any other place none") {
      val grit = Repository.of("grit").fold(e => throw new java.lang.AssertionError(e), identity)
      Vector("repository:grit", "repository:grit/fix-typo", "repository:grit/fix-typo/more")
        .map(place(_).repository) ==> Vector(Some(grit), Some(grit), Some(grit))
      Vector("repository:Grit/fix", "service:grit", "fs:/repository/grit", "everywhere")
        .map(place(_).repository) ==> Vector(None, None, None, None)
    }

    test(
      "a run of work's name is a lowercase letter, then at most 39 lowercase letters, digits or -"
    ) {
      val longest = "a" + "b" * 39
      Vector("fix-typo", "w3f2a", longest).map(WorkName.of(_).map(WorkName.value)) ==>
        Vector(Right("fix-typo"), Right("w3f2a"), Right(longest))
      Vector(longest + "c", "3f2a", "-a", "Fix", "a/b", "a.b", "a_b", "").map(WorkName.of) ==>
        Vector(
          Left(s"a run of work's name is over 40 characters: ${longest}c"),
          Left("a run of work's name starts with a digit: 3f2a"),
          Left("a run of work's name starts with '-': -a"),
          Left("a run of work's name holds 'F', not a lowercase letter, a digit or '-': Fix"),
          Left("a run of work's name holds '/', not a lowercase letter, a digit or '-': a/b"),
          Left("a run of work's name holds '.', not a lowercase letter, a digit or '-': a.b"),
          Left("a run of work's name holds '_', not a lowercase letter, a digit or '-': a_b"),
          Left("a run of work's name is empty")
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
      // The first link, not the most specific: a broader one listed first wins.
      WorksIn.of(links.reverse, place("slack:acme/C01/1.2")) ==> Some(service("github"))
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
