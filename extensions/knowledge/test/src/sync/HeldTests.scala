package grit.knowledge.sync

import java.time.Instant

import grit.core.document.{Document, Placement, Pointer}
import grit.core.id.{CorpusName, DocKey, DocumentVersion, PluginName}
import grit.core.place.Service
import grit.core.tool.ToolName
import grit.core.visibility.{Label, Level}
import grit.knowledge.card.{Card, CardSummary, Scope, UnitVersion}

import utest.*

object HeldTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val name = right(CorpusName.of("repo"))
  private val version = right(UnitVersion.of("3068d045f21e723190d01fed918ed7e6651bf59a"))
  private val label = Label.at(Level.Internal)
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
      right(Service.of("github")).place,
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
        Held.read(name, document()) == Right(Some(Held.of(name, "docs/a.md", label, version, None)))
      )
    }

    test("a directory's card reads back with the scope it was made under") {
      val scope = Scope.of(Set("include docs"))
      val dir = Card.directory(name, "docs", version, scope, summary, pointer)
      assert(
        Held.read(name, document(dir.docKey, dir.data)) ==
          Right(Some(Held.of(name, "docs", label, version, Some(scope))))
      )
    }

    test("another source's card is not held") {
      assert(Held.read(right(CorpusName.of("other")), document()) == Right(None))
    }

    test("a document that is no card is reported, naming its key") {
      val k = DocKey.value(card.docKey)
      val noPath = ujson.Obj("kind" -> "file", "version" -> UnitVersion.value(version))
      val badVersion = ujson.Obj("kind" -> "file", "path" -> "docs/a.md", "version" -> "v1")
      assert(
        Held.read(name, document(key = right(DocKey.of("notes")))) ==
          Left("the document notes is no card: its key is not 64 lowercase hex digits"),
        Held.read(name, document(data = noPath)) ==
          Left(s"the document $k is no card: its data holds no string path"),
        Held.read(name, document(data = badVersion)) ==
          Left(s"the document $k is no card: its data's version, v1, is no git SHA"),
        Held.read(
          name,
          document(data = ujson.Obj.from(card.data.value.toSeq :+ ("scope" -> ujson.Num(7))))
        ) ==
          Left(s"the document $k is no card: its data's scope, 7, is no scope")
      )
    }
  }
}
