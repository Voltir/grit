package grit.projects.artifact

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.document.{
  DocLabel,
  DocRef,
  DocText,
  DocWeight,
  Document,
  DocumentTerms,
  InMemoryDocuments
}
import grit.core.id.{DocKey, PluginName}
import grit.core.place.Place
import grit.core.store.Tx
import grit.core.visibility.{Integrity, Label}
import grit.dbos.sql.TestTx
import grit.projects.name.{ArtifactKey, KindName, ProjectKey, StateName}
import grit.projects.render.ProjectGen
import grit.prose.form.{Block, Doc, Span, Text}
import grit.prose.json.ProseJson

import utest.*

object ArtifactJsonTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val plugin: PluginName = right(PluginName.of("projects"))
  private val terms: DocumentTerms =
    right(DocLabel.of("projects").flatMap(DocumentTerms.of(_, DocWeight.Unscaled, 1.day, 100)))
  private val at: Instant = Instant.parse("2026-10-27T12:00:00Z")

  private val p: ProjectKey = right(ProjectKey.of("p"))
  private def key(local: String): ArtifactKey = right(ArtifactKey.of(p, local))

  private def head(local: String, cites: Vector[ArtifactKey] = Vector.empty): ArtifactHead =
    ArtifactHead(
      key(local),
      right(KindName.of("note")),
      right(StateName.of("open")),
      "A title",
      None,
      cites,
      Some("nick")
    )

  /** `text`, `data` and `refs` kept under `under` in a fresh store, and read back. */
  private def kept(under: String, text: DocText, data: ujson.Value, refs: Set[DocRef]): Document = {
    val documents = new InMemoryDocuments
    val k = right(DocKey.of(under))
    inTx {
      val written = documents
        .keeper(plugin, terms)
        .write(k, Label.Public, Integrity.Member, Place.Everywhere, text, data, None, at, refs)
        .fold(e => throw new java.lang.AssertionError(e.toString), identity)
      documents
        .shelf(plugin)
        .current(k, written.kept)
        .fold(e => throw new java.lang.AssertionError(e.toString), identity)
        .getOrElse(throw new java.lang.AssertionError(s"nothing kept under $under"))
    }
  }

  private def inTx[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)

  private def stored(blocks: Vector[Block[ArtifactKey]]): ujson.Value =
    ProseJson.write(Doc(blocks), k => ujson.Str(k.value))

  val tests: Tests = Tests {

    test("an artifact survives a write and a read, head, body and links") {
      (0 until 50).foreach { seed =>
        ProjectGen.project(seed).artifacts.foreach { a =>
          val (text, data, refs) = right(ArtifactJson.write(a))
          assert(Artifact.of(kept(a.head.key.value, text, data, refs)) == Right(a))
        }
      }
      val a = Artifact(
        head("a", Vector(key("b"))).copy(parent = Some(key("p0"))),
        right(Body.read("See [c](artifact:p/c)."))
      )
      val (_, _, refs) = right(ArtifactJson.write(a))
      val expected = Set(
        "project" -> "p",
        "kind" -> "note",
        "state" -> "open",
        "parent" -> "p/p0",
        "cites" -> "p/b",
        "cites" -> "p/c"
      ).map((kind, ref) => right(DocRef.of(kind, ref)))
      assert(refs == expected)
    }

    test("a document whose key or data is not an artifact's is not one, saying why") {
      val a = Artifact(head("a"), right(Body.read("Some text.")))
      val (text, data, refs) = right(ArtifactJson.write(a))
      val noKey = Artifact.of(kept("p", text, data, refs))
      assert(noKey.left.exists(_.contains("key")))
      val noState = ujson.Obj(
        "head" -> ujson.Obj.from(data("head").obj.toSeq.filterNot(_._1 == "state")),
        "body" -> data("body")
      )
      val stateless = Artifact.of(kept("p/a", text, noState, refs))
      assert(stateless.left.exists(_.contains("state")))
    }

    test("a head past 4 KiB beside its body is refused, naming its size") {
      val a = Artifact(head("a").copy(title = "x" * 5000), right(Body.read("Some text.")))
      val written = ArtifactJson.write(a)
      assert(written.left.exists(_.contains(s"over ${ArtifactJson.HeadMost}")))
    }

    test("an artifact citing more than a document's refs hold is refused, naming how many") {
      val cites = Vector.tabulate(300)(i => key(s"a$i"))
      val a = Artifact(head("a", cites), right(Body.read("Some text.")))
      val written = ArtifactJson.write(a)
      assert(written.left.exists(_.contains(s"over ${DocRef.Most}")))
    }

    test("a stored body that is empty or past its most is refused") {
      val empty = Body.fromStored(stored(Vector.empty))
      assert(empty.left.exists(_.contains("empty")))
      val words = Block.Paragraph(Text.of(Vector(Span.plain("word " * 7000))))
      val large = Body.fromStored(stored(Vector(words)))
      assert(large.left.exists(_.contains(s"over ${Body.StoredMost}")))
    }

    test("an artifact whose stored body is empty is reported, not read") {
      val a = Artifact(head("a"), right(Body.read("Some text.")))
      val (text, data, refs) = right(ArtifactJson.write(a))
      val emptied = ujson.Obj("head" -> data("head"), "body" -> stored(Vector.empty))
      val read = Artifact.of(kept("p/a", text, emptied, refs))
      assert(read.left.exists(_.contains("empty")))
    }
  }
}
