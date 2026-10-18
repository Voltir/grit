package grit.knowledge.ref

import java.time.LocalTime
import java.time.ZoneId

import grit.core.document.{DocRef, Pointer}
import grit.core.id.{ConversationId, CorpusName, DocKey, ScheduleKey}
import grit.core.job.SlotRule
import grit.core.place.Service
import grit.core.tool.ToolName
import grit.core.triage.Corpus
import grit.core.visibility.Label
import grit.knowledge.drive.DriveCardsTests
import grit.knowledge.github.GitHubCards
import grit.knowledge.sync.SyncSchedule

import utest.*

object RefTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val hex = "ab" * 32

  /** The other link `text` names. */
  private def url(text: String): Ref =
    Ref.Url(Href.of(text).getOrElse(throw new java.lang.AssertionError(text)))

  /** The repository `octocat/Hello-World` as a card source, its server named `github`. */
  private val repository: GitHubCards = {
    val service = right(Service.of("github"))
    val corpus =
      Corpus(right(CorpusName.of("repo")), "the repository", service.place, Some(service))
    val rule = SlotRule.Daily(LocalTime.of(3, 0), ZoneId.of("UTC"))
    right(
      GitHubCards.of(
        corpus,
        "github",
        "octocat",
        "Hello-World",
        "main",
        Vector("docs"),
        Set("md"),
        "documentation",
        100,
        None,
        service.place,
        Vector(right(SyncSchedule.of(right(ScheduleKey.of("nightly")), rule, 5)))
      )
    )
  }

  val tests = Tests {
    test("a Drive document's, sheet's, deck's and file's links name the Drive file of their id") {
      val links = Vector(
        "https://docs.google.com/document/d/1AbC-x_9/edit#heading=h.1",
        "https://docs.google.com/spreadsheets/d/1AbC-x_9/edit?gid=0",
        "https://docs.google.com/presentation/d/1AbC-x_9",
        "https://drive.google.com/file/d/1AbC-x_9/view?usp=sharing",
        "https://drive.google.com/open?id=1AbC-x_9",
        "HTTPS://Docs.Google.com/document/u/0/d/1AbC-x_9/"
      )
      links.map(Ref.ofLink) ==> links.map(_ => Some(Ref.Drive("1AbC-x_9")))
    }

    test("a GitHub link names its repository's path past the ref, and a bare repository its root") {
      Ref.ofLink("https://github.com/octocat/Hello-World/blob/main/docs/testing.md#L3") ==>
        Some(Ref.GitHub("octocat", "Hello-World", "docs/testing.md"))
      Ref.ofLink("https://GitHub.com/octocat/Hello-World/tree/main/docs/") ==>
        Some(Ref.GitHub("octocat", "Hello-World", "docs"))
      Ref.ofLink("https://github.com/octocat/Hello-World") ==>
        Some(Ref.GitHub("octocat", "Hello-World", "/"))
      Ref.ofLink("https://github.com/octocat/Hello-World/pull/7") ==>
        Some(url("https://github.com/octocat/Hello-World/pull/7"))
    }

    test("another link is a Url, its scheme and host lower-cased and its fragment dropped") {
      Ref.ofLink("HTTPS://Example.COM/Path/A?q=B#Part") ==> Some(
        url("https://example.com/Path/A?q=B")
      )
      Ref.ofLink("https://drive.google.com/drive/folders/1F") ==>
        Some(url("https://drive.google.com/drive/folders/1F"))
    }

    test("text with no scheme and host is no link") {
      Vector("example.com/a", "mailto:a@example.com", "", "https://", "not a link").map(
        Ref.ofLink
      ) ==>
        Vector(None, None, None, None, None)
    }

    test("a card's pointer reads as the ref a link to the same unit gives") {
      val drive = right(DriveCardsTests.cards(Vector(Label.Public)).pointer("1AbC-x_9"))
      val file = right(repository.pointer("docs/testing.md"))
      val root = right(repository.pointer("/"))
      Vector(drive, file, root).map(Ref.ofPointer) ==> Vector(
        Some(Ref.Drive("1AbC-x_9")),
        Some(Ref.GitHub("octocat", "Hello-World", "docs/testing.md")),
        Some(Ref.GitHub("octocat", "Hello-World", "/"))
      )
      Vector(drive, file, root).map(Ref.ofPointer) ==> Vector(
        "https://drive.google.com/file/d/1AbC-x_9/view",
        "https://github.com/octocat/Hello-World/blob/main/docs/testing.md",
        "https://github.com/octocat/Hello-World"
      ).map(Ref.ofLink)
    }

    test("a pointer of another tool reads as no ref") {
      val other = right(Pointer.of(right(ToolName.of("drive_list")), "file" -> ujson.Str("1A")))
      Ref.ofPointer(other) ==> None
    }

    test("every kind of ref reads back as written") {
      val refs = Vector(
        Ref.Strand(ConversationId("c-1")),
        Ref.Topic(TopicKey.card(CardKey.of(right(CorpusName.of("repo")), "docs/a.md"))),
        Ref.Drive("1AbC"),
        Ref.GitHub("octocat", "Hello-World", "/"),
        Ref.GitHub("octocat", "Hello-World", "docs/a/b.md"),
        url("https://example.com/a")
      )
      refs.map(r => right(Ref.written(r)).kind) ==>
        Vector("strand", "topic", "drive", "github", "github", "url")
      refs.map(r => Ref.read(right(Ref.written(r)))) ==> refs.map(Some(_))
    }

    test("a link's reference reads back normalised, and one holding no link as None") {
      Ref.read(right(DocRef.of("url", "HTTPS://Example.COM/a#f"))) ==> Some(
        url("https://example.com/a")
      )
      Ref.read(right(DocRef.of("url", "example.com/a"))) ==> None
    }

    test("a reference of a kind knowledge does not keep reads as None") {
      Ref.read(right(DocRef.of("mention", "u-1"))) ==> None
      Ref.read(right(DocRef.of("topic", "standing:c/1"))) ==> None
    }

    test("a ref over what a reference holds is refused, naming why") {
      assert(Ref.written(url("https://example.com/" + "a" * 600)).isLeft)
    }

    test("a topic key is opened by a strand or a card and read back from its document's key") {
      val strand = right(TopicKey.strand(ConversationId("c-1")))
      val card = TopicKey.card(right(CardKey.parse(hex)))
      DocKey.value(TopicKey.doc(strand)) ==> "topic:strand-c-1"
      DocKey.value(TopicKey.doc(card)) ==> s"topic:card-$hex"
      Vector(strand, card).map(k => TopicKey.read(TopicKey.doc(k))) ==> Vector(
        Some(strand),
        Some(card)
      )
    }

    test("a key of another plugin, or a card's key that is not one, is no topic's") {
      Vector("standing:c/1", "topic:card-abc", "topic:strand-", "topic:other-x")
        .map(k => TopicKey.read(right(DocKey.of(k)))) ==> Vector(None, None, None, None)
    }

    test("a strand whose topic key would be over a document key's 200 characters opens none") {
      assert(TopicKey.strand(ConversationId("c" * 190)).isLeft)
    }
  }
}
