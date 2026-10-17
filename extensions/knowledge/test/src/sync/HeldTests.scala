package grit.knowledge.sync

import java.time.Instant

import grit.core.document.{Document, Placement, Pointer}
import grit.core.id.{CorpusName, DocKey, DocumentVersion, PluginName}
import grit.core.place.{Place, Service}
import grit.core.tool.ToolName
import grit.core.visibility.Integrity
import grit.core.visibility.{Label, Level}
import grit.knowledge.card.{Card, CardSummary, Scope, UnitVersion}

import utest.*

object HeldTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val name = right(CorpusName.of("repo"))
  private val version = right(UnitVersion.of("3068d045f21e723190d01fed918ed7e6651bf59a"))
  private val label = Label.at(Level.Internal)
  private val at = right(Service.of("github")).place
  private val summary = right(CardSummary.of("What the file holds.", Vector.empty))
  private val pointer = right(
    Pointer.of(right(ToolName.of("github_get_file_contents")), "path" -> ujson.Str("docs/a.md"))
  )
  private val card = Card.file(name, "docs/a.md", version, summary, pointer)

  private def document(key: DocKey = card.docKey, data: ujson.Value = card.data): Document =
    Document(
      DocumentVersion.of(1).getOrElse(throw new java.lang.AssertionError("no version")),
      right(PluginName.of("knowledge")),
      key,
      label,
      Integrity.Member,
      at,
      card.text,
      data,
      Some(pointer),
      Instant.EPOCH,
      Placement(0, Instant.EPOCH),
      None
    )

  val tests = Tests {
    test("a card's document reads back as held at its own label, path and version") {
      assert(
        Held.read(name, at, document()) == Right(
          Some(Held.of(name, "docs/a.md", label, version, None))
        )
      )
    }

    test("a card kept at another place than its source's reads back as held elsewhere") {
      val channel = right(Place.read("slack:acme/#docs"))
      assert(
        Held.read(name, channel, document()).map(_.map(_.elsewhere)) == Right(Some(true)),
        Held.read(name, at, document()).map(_.map(_.elsewhere)) == Right(Some(false))
      )
    }

    test("a card changed at an instant reads back at that instant, its title aside") {
      val changed = UnitVersion.Changed(Instant.parse("2026-10-14T09:30:00Z"))
      val file = Card.file(name, "docs/b.md", changed, summary, pointer, Some("B"))
      assert(
        Held.read(name, at, document(file.docKey, file.data)) ==
          Right(Some(Held.of(name, "docs/b.md", label, changed, None)))
      )
    }

    test("a file's card reads back with the listing it was shown in") {
      val file = Card.file(name, "file-1", version, summary, pointer, within = Some("folder-3"))
      assert(
        Held.read(name, at, document(file.docKey, file.data)) ==
          Right(Some(Held.of(name, "file-1", label, version, None, Some("folder-3"))))
      )
    }
    test("a directory's card reads back with the scope it was made under") {
      val scope = Scope.of(Set("include docs"))
      val dir = Card.directory(name, "docs", version, scope, summary, pointer)
      assert(
        Held.read(name, at, document(dir.docKey, dir.data)) ==
          Right(Some(Held.of(name, "docs", label, version, Some(scope))))
      )
    }

    test("another source's card is not held") {
      assert(Held.read(right(CorpusName.of("other")), at, document()) == Right(None))
    }

    test("a document that is no card is reported, naming its key") {
      val k = DocKey.value(card.docKey)
      val noPath = ujson.Obj("kind" -> "file", "version" -> UnitVersion.value(version))
      val badVersion = ujson.Obj("kind" -> "file", "path" -> "docs/a.md", "version" -> "v1")
      assert(
        Held.read(name, at, document(key = right(DocKey.of("notes")))) ==
          Left("the document notes is no card: its key is not 64 lowercase hex digits"),
        Held.read(name, at, document(data = noPath)) ==
          Left(s"the document $k is no card: its data holds no string path"),
        Held.read(name, at, document(data = badVersion)) ==
          Left(s"the document $k is no card: its data's version, v1, is no git SHA or instant"),
        Held.read(
          name,
          at,
          document(data = ujson.Obj.from(card.data.value.toSeq :+ ("scope" -> ujson.Num(7))))
        ) ==
          Left(s"the document $k is no card: its data's scope, 7, is no scope")
      )
    }
  }
}
