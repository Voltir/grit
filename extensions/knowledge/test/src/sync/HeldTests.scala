package grit.knowledge.sync

import java.time.Instant

import grit.core.document.{Document, Placement, Pointer}
import grit.core.id.{CorpusName, DocKey, DocumentVersionId, PluginName}
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
      DocumentVersionId.of(1).getOrElse(throw new java.lang.AssertionError("no version")),
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
        Held.read(name, at, document()) ==
          Held.Read.Ours(Held.of(name, "docs/a.md", label, version, None))
      )
    }

    test("a card kept at another place than its source's reads back as held elsewhere") {
      val channel = right(Place.read("slack:acme/#docs"))
      def elsewhere(place: Place) = Held.read(name, place, document()) match {
        case Held.Read.Ours(h) => Some(h.elsewhere)
        case _ => None
      }
      (elsewhere(channel), elsewhere(at)) ==> (Some(true), Some(false))
    }

    test("a card changed at an instant reads back at that instant, its title aside") {
      val changed = UnitVersion.Changed(Instant.parse("2026-10-14T09:30:00Z"))
      val file = Card.file(name, "docs/b.md", changed, summary, pointer, Some("B"))
      assert(
        Held.read(name, at, document(file.docKey, file.data)) ==
          Held.Read.Ours(Held.of(name, "docs/b.md", label, changed, None))
      )
    }

    test("a file's card reads back with the listing it was shown in") {
      val file = Card.file(name, "file-1", version, summary, pointer, within = Some("folder-3"))
      assert(
        Held.read(name, at, document(file.docKey, file.data)) ==
          Held.Read.Ours(Held.of(name, "file-1", label, version, None, Some("folder-3")))
      )
    }
    test("a directory's card reads back with the scope it was made under") {
      val scope = Scope.of(Set("include docs"))
      val dir = Card.directory(name, "docs", version, scope, summary, pointer)
      assert(
        Held.read(name, at, document(dir.docKey, dir.data)) ==
          Held.Read.Ours(Held.of(name, "docs", label, version, Some(scope)))
      )
    }

    test(
      "a document whose key is no card key, whose data holds no path, or whose key is not its source's for that path is another's, whatever else it holds"
    ) {
      val undated = ujson.Obj("kind" -> "file", "path" -> "docs/a.md", "version" -> "2026-10-23")
      val noPath = ujson.Obj("kind" -> "file", "version" -> "2026-10-23")
      val other = right(DocKey.of(f"${7}%064x"))
      (
        Held.read(name, at, document(key = right(DocKey.of("notes")), data = undated)),
        Held.read(name, at, document(data = noPath)),
        Held.read(name, at, document(key = other, data = undated)),
        Held.read(right(CorpusName.of("other")), at, document(data = undated))
      ) ==> (Held.Read.Theirs, Held.Read.Theirs, Held.Read.Theirs, Held.Read.Theirs)
    }

    test("its source's card whose version, scope or within does not read is damaged, saying why") {
      def withField(field: String, value: ujson.Value) =
        Held.read(
          name,
          at,
          document(data = ujson.Obj.from(card.data.value.toSeq :+ (field -> value)))
        )
      def damaged(why: String) = Held.Read.Damaged(card.key, "docs/a.md", label, why)
      (
        withField("version", ujson.Str("2026-10-23")),
        withField("version", ujson.Num(7)),
        withField("scope", ujson.Num(7)),
        withField("within", ujson.Num(7))
      ) ==> (
        damaged("its version, 2026-10-23, is no git SHA or instant"),
        damaged("its version, 7, is no git SHA or instant"),
        damaged("its scope, 7, is no scope"),
        damaged("its within, 7, is no string")
      )
    }
  }
}
