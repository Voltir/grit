package grit.core.session

import java.time.Instant

import grit.core.id.{Harness, SessionId, TestPrincipalIds}
import grit.core.identity.Principal
import grit.core.place.{Namespace, Place}
import grit.core.session.SessionDigest.Window
import grit.core.visibility.Label

import utest.*

/** A session's digest, rendered from what it did lately, and cut to fit. */
object SessionDigestTests extends TestSuite {

  private def ok[A](e: Either[String, A]): A = e.fold(sys.error, identity)

  private val started = Instant.parse("2026-10-29T09:00:00Z")
  private val last = Instant.parse("2026-10-29T10:40:30Z")

  private def session(summary: Option[String], full: Boolean): Session =
    Session(
      ok(SessionId.of(ok(Harness.of("claude-code")), "s1")),
      None,
      Principal.Person(TestPrincipalIds.stored("ann"), Set.empty),
      Label.Public,
      Place.under(Namespace.Fs, Vector("/w")),
      started,
      None,
      last,
      summary.map(s => ok(Session.Summary.of(s))),
      ok(Session.Counters.read(143, 12, 2, 2048 * 1024)),
      full,
      None
    )

  private def lately(summary: Option[String] = Some("built the digest"), full: Boolean = false) =
    Session.Lately(
      session(summary, full),
      Vector(
        Session.Lately.Commit(last, "1a2b3c4d5e", Some("core: a digest")),
        Session.Lately.Commit(started, "9f8e7d6c5b", None)
      ),
      0,
      Vector("e1.scala", "e2.scala").map(Session.Lately.Touched(_, None)),
      Vector("Ledger.post", "r2.scala").map(Session.Lately.Touched(_, None))
    )

  private val Head: Vector[String] = Vector(
    "## This session so far",
    "Started 2026-10-29 09:00 UTC, last active 2026-10-29 10:40 UTC: 143 tool calls, 12 edits, " +
      "2 commits, 2048 KB read."
  )
  private val Commits: Vector[String] =
    Vector("Commits, newest first:", "- 1a2b3c4 core: a digest", "- 9f8e7d6")
  private val Edited: Vector[String] = Vector("Edited, newest first:", "- e1.scala", "- e2.scala")
  private val Read: Vector[String] = Vector("Read, newest first:", "- Ledger.post", "- r2.scala")

  private def lines(parts: Vector[String]*): String = parts.flatten.mkString("\n")

  val tests = Tests {
    test("a digest shows its times and counters, then commits, edited and read, newest first") {
      SessionDigest.of(lately(), Window.HoldsLastSaid, 2500) ==> lines(Head, Commits, Edited, Read)
    }

    test(
      "commits its agents made are one line after the counters; a path under another worktree shows under its last segment"
    ) {
      val fixer = ok(Place.read("fs:/home/n/grit/.claude/worktrees/fixer"))
      val more = lately().copy(
        byAgents = 3,
        edited = Vector(Session.Lately.Touched("core/src/A.scala", Some(fixer))),
        read = Vector(Session.Lately.Touched("Ledger.post", None))
      )
      (
        SessionDigest.of(more, Window.HoldsLastSaid, 2500),
        SessionDigest.of(lately().copy(byAgents = 1), Window.HoldsLastSaid, 2500)
      ) ==> (
        lines(
          Head,
          Vector("Its agents made 3 commits."),
          Commits,
          Vector("Edited, newest first:", "- fixer: core/src/A.scala"),
          Vector("Read, newest first:", "- Ledger.post")
        ),
        lines(Head, Vector("Its agents made 1 commit."), Commits, Edited, Read)
      )
    }

    test("what it last said shows only when the window lacks it, at most Said characters") {
      val long = "x" * 600
      (
        SessionDigest.of(lately(), Window.LacksLastSaid, 2500),
        SessionDigest.of(lately(Some(long)), Window.LacksLastSaid, 2500),
        SessionDigest.of(lately(None), Window.LacksLastSaid, 2500)
      ) ==> (
        lines(Head, Vector("Last said: built the digest"), Commits, Edited, Read),
        lines(Head, Vector("Last said: " + "x" * 499 + "…"), Commits, Edited, Read),
        lines(Head, Commits, Edited, Read)
      )
    }

    test("a full session's digest says its log is full, after its counters") {
      SessionDigest.of(lately(None, full = true), Window.HoldsLastSaid, 2500) ==>
        lines(
          Head,
          Vector("Its log is full: nothing it did after it filled is shown."),
          Commits,
          Edited,
          Read
        )
    }

    test(
      "over chars, the oldest read goes first, then edited, then commits, then the said is cut"
    ) {
      val said = Vector("Last said: built the digest")
      def at(chars: Int) = SessionDigest.of(lately(), Window.LacksLastSaid, chars)
      val whole = lines(Head, said, Commits, Edited, Read)
      val noRead = lines(Head, said, Commits, Edited)
      val oneEdit = lines(Head, said, Commits, Edited.take(2))
      val oneCommit = lines(Head, said, Commits.take(2))
      val bare = lines(Head)
      (
        at(whole.length),
        at(whole.length - 1),
        at(noRead.length),
        at(noRead.length - 1),
        at(oneCommit.length),
        at(oneCommit.length - 1),
        at(bare.length + 17),
        at(bare.length + 5),
        at(10),
        at(0)
      ) ==> (
        whole,
        lines(Head, said, Commits, Edited, Read.take(2)),
        noRead,
        oneEdit,
        oneCommit,
        lines(Head, said),
        lines(Head, Vector("Last said: buil…")),
        bare,
        "## This s…",
        ""
      )
    }
  }
}
