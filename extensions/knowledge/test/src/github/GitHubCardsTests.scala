package grit.knowledge.github

import java.time.{LocalTime, ZoneId}

import grit.core.id.{CorpusName, ScheduleKey}
import grit.core.job.SlotRule
import grit.core.place.Service
import grit.core.tool.ToolName
import grit.core.triage.Corpus
import grit.knowledge.card.Scope
import grit.knowledge.sync.SyncSchedule

import utest.*

object GitHubCardsTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val service = right(Service.of("github"))
  private val corpus =
    Corpus(right(CorpusName.of("repo")), "the team's repository", service.place, Some(service))
  private val rule = SlotRule.Daily(LocalTime.of(3, 0), ZoneId.of("UTC"))
  private def sync(key: String): SyncSchedule = right(
    SyncSchedule.of(right(ScheduleKey.of(key)), rule, 5)
  )

  private def cards(
      corpus: Corpus = corpus,
      server: String = "github",
      owner: String = "octocat",
      repo: String = "Hello-World",
      ref: String = "main",
      maxUnits: Int = 100,
      syncs: Vector[SyncSchedule] = Vector(sync("nightly"))
  ): Either[String, GitHubCards] =
    GitHubCards.of(
      corpus,
      server,
      owner,
      repo,
      ref,
      Vector("docs"),
      Set("md"),
      maxUnits,
      None,
      service.place,
      syncs
    )

  val tests = Tests {
    test(
      "a card's pointer names the read tool as offered, and no ref, so a read at any ref follows"
    ) {
      val pointer = right(right(cards()).pointer("docs/testing.md"))
      assert(pointer.tool == ToolName("github_get_file_contents"))
      assert(
        pointer.arguments == ujson.Obj(
          "owner" -> "octocat",
          "path" -> "docs/testing.md",
          "repo" -> "Hello-World"
        )
      )
      val call = ujson.Obj(
        "owner" -> "octocat",
        "repo" -> "Hello-World",
        "path" -> "docs/testing.md",
        "ref" -> "refs/heads/other"
      )
      assert(pointer.followedBy(ToolName("github_get_file_contents"), call))
    }

    test("a unit is read at the source's ref") {
      assert(
        right(cards()).reading("docs") == ujson.Obj(
          "owner" -> "octocat",
          "repo" -> "Hello-World",
          "path" -> "docs",
          "ref" -> "main"
        )
      )
    }

    test("of refuses a corpus supplying no service, and a server making no tool name") {
      assert(
        cards(corpus = corpus.copy(supplies = None)) == Left("the corpus repo supplies no service")
      )
      assert(
        cards(server = "GitHub") == Left(
          "a tool name is a lowercase letter, then lowercase letters, digits or _: " +
            "GitHub_get_file_contents"
        )
      )
    }

    test("of refuses a blank owner, repo or ref") {
      assert(cards(owner = " ") == Left("a repository's owner is not blank"))
      assert(cards(repo = "") == Left("a repository's name is not blank"))
      assert(cards(ref = "\t") == Left("a repository's ref is not blank"))
    }

    test("of refuses maxUnits under 1, no syncs, and two syncs of one key") {
      assert(cards(maxUnits = 0) == Left("maxUnits is at least 1, not 0"))
      assert(cards(syncs = Vector.empty) == Left("a source has at least one sync schedule"))
      assert(
        cards(syncs = Vector(sync("nightly"), sync("hourly"), sync("nightly"))) ==
          Left("a source's sync schedules have one key each: nightly is two")
      )
      assert(cards(syncs = Vector(sync("nightly"), sync("hourly"))).isRight)
    }

    test(
      "of trims include's slashes and extensions' dots, and its scope digests the two and the form"
    ) {
      def of(include: Vector[String], extensions: Set[String]) = right(
        GitHubCards.of(
          corpus,
          "github",
          "octocat",
          "Hello-World",
          "main",
          include,
          extensions,
          100,
          None,
          service.place,
          Vector(sync("nightly"))
        )
      )
      val trimmed = of(Vector("/docs/"), Set(".md"))
      assert(
        trimmed.include == Vector("docs"),
        trimmed.extensions == Set("md"),
        trimmed.scope == Scope.of(Set("include docs", "extension md", "directory form 2")),
        of(Vector("docs"), Set("md", "txt")).scope ==
          Scope.of(Set("include docs", "extension md", "extension txt", "directory form 2"))
      )
    }

    test("a sync schedule refuses perRun under 1") {
      val key = right(ScheduleKey.of("nightly"))
      assert(SyncSchedule.of(key, rule, 0) == Left("perRun is at least 1, not 0"))
      assert(SyncSchedule.of(key, rule, 1).map(_.perRun) == Right(1))
    }
  }
}
