package grit.claudecode

import utest.*

/** scripts/gate-stamp over a record the real scripts/check wrote, in [[GateFixtures]]' checkout,
  * with its fake `gh`: what it posts, each refusal, and the read-back.
  */
object GateStampTests extends TestSuite {

  /** `work` holding a doc change, gated by a bare passing run, and on origin beside `main`. */
  private def gated(
      g: GateFixtures,
      args: Seq[String] = Seq(),
      more: Map[String, String] = Map()
  ): String = {
    val head = g.change("README.md" -> "a doc\n")
    val done = g.check(args, more)
    if (done.exit != 0) throw new java.lang.AssertionError(s"the gate failed: ${done.out}")
    g.remote("main", g.sha("main"))
    g.remote("work", head)
    head
  }

  /** The post the fake gh is given for `sha`, with `description`. */
  private def post(sha: String, description: String): String =
    s"|api -X POST repos/someone/somerepo/statuses/$sha -f state=success -f context=grit/gate " +
      s"-f description=$description"

  /** A stamp of HEAD refused: its exit, its one line, and whether gh was called. */
  private def refused(g: GateFixtures, args: Seq[String] = Seq()): (Int, Vector[String], Int) = {
    val done = g.stamp(args)
    (done.exit, done.out, g.calls("gh").size)
  }

  val tests = Tests {
    test(
      "a gated, clean HEAD on origin is stamped once, as gh's own login, its description the run and the base"
    ) {
      GateFixtures { g =>
        val head = gated(g)
        val done = g.stamp(Seq(), Map("GITHUB_TOKEN" -> "planted", "GH_TOKEN" -> "planted"))
        val description = s"check, base ${g.short("main")}"
        (done.exit, done.out, g.calls("gh")) ==> ((
          0,
          Vector(
            s"gate-stamp: posted grit/gate success on ${g.short(head)} ($description) to someone/somerepo"
          ),
          Vector(post(head, description))
        ))
      }
    }

    test("a gate run with --it or --it-reached is named so in the description") {
      GateFixtures { g =>
        val all = gated(g, Seq("--it"))
        val _ = g.stamp(Seq())
        val _ = g.change("core/dbos/src/A.scala" -> "object A\n")
        val _ = g.check(Seq("--it-reached"))
        val reached = g.sha("HEAD")
        g.remote("work", reached)
        val _ = g.stamp(Seq(reached))
        g.calls("gh") ==> Vector(
          post(all, s"check --it, base ${g.short("main")}"),
          post(reached, s"check --it-reached, base ${g.short("main")}")
        )
      }
    }

    test("a SHA that HEAD is not is refused") {
      GateFixtures { g =>
        val head = gated(g)
        refused(g, Seq(g.sha("main"))) ==>
          ((1, Vector(s"gate-stamp: HEAD is ${g.short(head)}, not ${g.short("main")}"), 0))
      }
    }

    test("a tree with a change or an untracked file now is refused") {
      GateFixtures { g =>
        val _ = gated(g)
        val _ = g.machine.write(g.dir.resolve("stray.txt"), "untracked\n")
        refused(g) ==>
          ((
            1,
            Vector("gate-stamp: the tree is not clean (git status --porcelain prints 1 line)"),
            0
          ))
      }
    }

    test("no record is refused, naming the gate that writes one") {
      GateFixtures { g =>
        val head = g.change("README.md" -> "a doc\n")
        g.remote("main", g.sha("main"))
        g.remote("work", head)
        refused(g) ==> ((
          1,
          Vector(
            "gate-stamp: no gate record in out/check/passed: run scripts/check here, " +
              "which writes one when a whole gate passes"
          ),
          0
        ))
      }
    }

    test("a record naming another commit is refused") {
      GateFixtures { g =>
        val old = gated(g)
        val head = g.change("README.md" -> "another doc\n")
        g.remote("work", head)
        refused(g) ==> ((
          1,
          Vector(s"gate-stamp: the gate's record names ${g.short(old)}, not ${g.short(head)}"),
          0
        ))
      }
    }

    test("a record whose HEAD moved while the gate ran is refused") {
      GateFixtures { g =>
        val old = gated(g, more = Map("FAKE_LAW_COMMITS" -> "1"))
        val head = g.sha("HEAD")
        g.remote("work", head)
        refused(g) ==> ((
          1,
          Vector(
            s"gate-stamp: HEAD moved while the gate ran (${g.short(old)} to ${g.short(head)})"
          ),
          0
        ))
      }
    }

    test("a record of a tree not clean when the gate began, or when it ended, is refused") {
      GateFixtures { g =>
        val head = g.change("README.md" -> "a doc\n")
        val _ = g.machine.write(g.dir.resolve("left-behind"), "untracked\n")
        val _ = g.check(Seq())
        val _ = g.sh("rm left-behind")
        g.remote("main", g.sha("main"))
        g.remote("work", head)
        val began = refused(g)
        val _ = g.check(Seq(), Map("FAKE_LAW_DIRTIES" -> "1"))
        val _ = g.sh("rm left-behind")
        val ended = refused(g)
        (began, ended) ==> ((
          (1, Vector("gate-stamp: the tree was not clean when the gate began"), 0),
          (1, Vector("gate-stamp: the tree was not clean when the gate ended"), 0)
        ))
      }
    }

    test("a gate with no base is refused") {
      GateFixtures { g =>
        val _ = gated(g, more = Map("CHECK_BASE" -> ""))
        refused(g) ==> ((1, Vector("gate-stamp: the gate ran with no base: set CHECK_BASE"), 0))
      }
    }

    test("a base that is HEAD itself is refused as no proper ancestor") {
      GateFixtures { g =>
        val head = gated(g, more = Map("CHECK_BASE" -> "HEAD"))
        refused(g) ==> ((
          1,
          Vector(
            s"gate-stamp: the gate's base ${g.short(head)} is not a proper ancestor of ${g.short(head)}"
          ),
          0
        ))
      }
    }

    test("a base origin/main does not hold is refused") {
      GateFixtures { g =>
        val _ = gated(g)
        val _ = g.sh("git update-ref -d refs/remotes/origin/main")
        refused(g) ==> ((
          1,
          Vector(s"gate-stamp: the gate's base ${g.short("main")} is not on origin/main"),
          0
        ))
      }
    }

    test("a gate that skipped the integration suites its change reaches is refused") {
      GateFixtures { g =>
        val head = g.change("core/dbos/src/A.scala" -> "object A\n")
        val _ = g.check(Seq())
        g.remote("main", g.sha("main"))
        g.remote("work", head)
        refused(g) ==> ((
          1,
          Vector(
            "gate-stamp: the change reaches integration suites and the gate skipped them: " +
              "run scripts/check --it-reached"
          ),
          0
        ))
      }
    }

    test("a commit no branch of origin holds is refused: the branch is pushed first") {
      GateFixtures { g =>
        val _ = gated(g)
        val _ = g.sh("git update-ref -d refs/remotes/origin/work")
        refused(g) ==>
          ((1, Vector("gate-stamp: origin does not have it; push the branch first"), 0))
      }
    }

    test(
      "an origin not on GitHub is refused, and its scp, ssh and https forms each name the repository"
    ) {
      GateFixtures { g =>
        val head = gated(g)
        def url(u: String) = g.sh(s"git remote set-url origin '$u'")
        val _ = url("https://example.invalid/someone/somerepo.git")
        val elsewhere = refused(g)
        val _ = url("ssh://git@github.com/someone/somerepo.git")
        val ssh = g.stamp(Seq()).exit
        val _ = url("https://github.com/someone/somerepo")
        val https = g.stamp(Seq()).exit
        val description = s"check, base ${g.short("main")}"
        (elsewhere, ssh, https, g.calls("gh")) ==> ((
          (
            1,
            Vector(
              "gate-stamp: origin (https://example.invalid/someone/somerepo.git) is not a GitHub repository"
            ),
            0
          ),
          0,
          0,
          Vector(post(head, description), post(head, description))
        ))
      }
    }

    test("gh refusing the post is said in its words, and the record kept stamps on a rerun") {
      GateFixtures { g =>
        val head = gated(g)
        val failed = g.stamp(Seq(), Map("FAKE_GH_FAILS" -> "HTTP 401: Bad credentials"))
        val again = g.stamp(Seq())
        (failed.exit, failed.out, again.exit, g.calls("gh").lastOption) ==> ((
          1,
          Vector("gate-stamp: gh refused the post: HTTP 401: Bad credentials"),
          0,
          Some(post(head, s"check, base ${g.short("main")}"))
        ))
      }
    }

    test("--check says gh's refusal in its words, and that origin is not set when it is not") {
      GateFixtures { g =>
        val head = g.change("README.md" -> "a doc\n")
        val failed = g.stamp(Seq("--check", head), Map("FAKE_GH_FAILS" -> "HTTP 404: Not Found"))
        val _ = g.sh("git remote remove origin")
        val unset = g.stamp(Seq("--check", head))
        ((failed.exit, failed.out), (unset.exit, unset.out)) ==> ((
          (1, Vector("gate-stamp: gh refused the read: HTTP 404: Not Found")),
          (1, Vector("gate-stamp: origin is not set"))
        ))
      }
    }

    test(
      "--check reads a success and its description, and names a commit with no stamp or another state"
    ) {
      GateFixtures { g =>
        val head = g.change("README.md" -> "a doc\n")
        def status(json: String) = g.machine.write(g.machine.home.resolve("gh-status.json"), json)
        def read() = { val d = g.stamp(Seq("--check", head)); (d.exit, d.out) }
        val none = read()
        val _ = status(
          """{"statuses":[{"context":"other","state":"success","description":"x"},""" +
            """{"context":"grit/gate","state":"success","description":"check, base abc1234"}]}"""
        )
        val stamped = read()
        val _ = status("""{"statuses":[{"context":"grit/gate","state":"failure"}]}""")
        val failed = read()
        val short = g.short(head)
        (none, stamped, failed, g.calls("gh").distinct) ==> ((
          (1, Vector(s"gate-stamp: $short carries no grit/gate stamp")),
          (0, Vector(s"gate-stamp: $short carries grit/gate success (check, base abc1234)")),
          (1, Vector(s"gate-stamp: $short carries grit/gate failure, not success")),
          Vector(s"|api repos/someone/somerepo/commits/$head/status")
        ))
      }
    }
  }
}
