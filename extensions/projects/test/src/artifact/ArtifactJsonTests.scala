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
import grit.core.edge.Watched
import grit.core.id.{DocKey, DocumentVersionId, PluginName}
import grit.core.place.Place
import grit.core.store.Tx
import grit.core.visibility.{Integrity, Label}
import grit.dbos.sql.TestTx
import grit.projects.method.MethodFixtures
import grit.projects.name.{ArtifactKey, KindName, MethodName, Pin, ProjectKey, StateName}
import grit.projects.plugin.Projects
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
  private def key(local: String): ArtifactKey = MethodFixtures.key(s"p/$local")

  private def head(local: String, cites: Vector[ArtifactKey] = Vector.empty): ArtifactHead =
    ArtifactHead(
      key(local),
      MethodFixtures.handleOf(key(local)),
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

  /** `project` written as data, through its text, and read back. */
  private def throughData(project: Project): Either[String, Project] =
    ArtifactJson.projectFrom(ujson.read(ujson.write(ArtifactJson.projectData(project))))

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
        right(Body.read("See [c](artifact:p/c).", MethodFixtures.ids))
      )
      val (_, _, refs) = right(ArtifactJson.write(a))
      val expected = Set(
        "project" -> "p",
        "kind" -> "note",
        "state" -> "open",
        "parent" -> "p/32",
        "cites" -> "p/2",
        "cites" -> "p/3"
      ).map((kind, ref) => right(DocRef.of(kind, ref)))
      assert(refs == expected)
    }

    test(
      "a document is an artifact by its head, whatever its key, and one whose head is not an artifact's is not one, saying why"
    ) {
      val a = Artifact(head("a"), right(Body.read("Some text.", MethodFixtures.ids)))
      val (text, data, refs) = right(ArtifactJson.write(a))
      def without(field: String) = ujson.Obj(
        "head" -> ujson.Obj.from(data("head").obj.toSeq.filterNot(_._1 == field)),
        "body" -> data("body")
      )
      assert(
        Artifact.of(kept("p/old-name", text, data, refs)) == Right(a),
        Artifact.of(kept("p/1", text, without("id"), refs)) == Left("its head holds no id"),
        Artifact.of(kept("p/1", text, without("state"), refs)) == Left("its head holds no state")
      )
    }

    test("a head past 4 KiB beside its body is refused, naming its size") {
      val a = Artifact(
        head("a").copy(title = "x" * 5000),
        right(Body.read("Some text.", MethodFixtures.ids))
      )
      val written = ArtifactJson.write(a)
      assert(
        written.map(_ => ()) == Left(
          "its head is 5119 bytes, over 4096: shorten its title or cite fewer"
        )
      )
    }

    test("an artifact citing more than a document's refs hold is refused, naming how many") {
      val cites = Vector.tabulate(300)(i => key(s"${i + 100}"))
      val a = Artifact(head("a", cites), right(Body.read("Some text.", MethodFixtures.ids)))
      val written = ArtifactJson.write(a)
      assert(written.map(_ => ()) == Left("it would hold 303 references, over 256: cite fewer"))
    }

    test("a stored body past its most is refused, naming its size") {
      val why = Body
        .fromStored(stored(Vector(Block.Paragraph(Text.of(Vector(Span.plain("word " * 7000)))))))
        .map(_ => ())
      assert(why == Left("a body's stored form is 35063 bytes, over 28672"))
    }

    test("an artifact whose stored body is empty is reported, not read") {
      val a = Artifact(head("a"), right(Body.read("Some text.", MethodFixtures.ids)))
      val (text, data, refs) = right(ArtifactJson.write(a))
      val emptied = ujson.Obj("head" -> data("head"), "body" -> stored(Vector.empty))
      assert(Artifact.of(kept("p/a", text, emptied, refs)) == Left("a body is empty"))
    }
    test(
      "a watch for an artifact's states is met by the ref its write holds, read back as its state"
    ) {
      val review = right(StateName.of("review"))
      val merged = right(StateName.of("merged"))
      val (_, _, refs) =
        right(
          ArtifactJson.write(
            Artifact(head("a").copy(state = review), right(Body.read("x", MethodFixtures.ids)))
          )
        )
      val watch = right(Projects.reaching(right(DocKey.of("p/1")), Set(review, merged)))
      val met = watch.refs.intersect(refs)
      val version = DocumentVersionId.of(1L).getOrElse(throw new java.lang.AssertionError("id"))
      assert(
        (watch.plugin, watch.key) == (Projects.Name, right(DocKey.of("p/1"))),
        Projects.reached(Watched.Held(version, met, at)) == Set(review)
      )
    }

    // A pin of the stored form: projects' documents are kept 180 days, and each is read back
    // through these keys.
    test("an artifact is stored as its head and its body, under these keys") {
      val a = Artifact(
        head("a", Vector(key("b"))).copy(parent = Some(key("p0"))),
        right(Body.read("See [b](artifact:p/b).", MethodFixtures.ids))
      )
      val (text, data, _) = right(ArtifactJson.write(a))
      val rendered = data.render()
      val shown = DocText.value(text)
      assert(
        rendered ==
          """{"head":{"id":"p/1","handle":"a","kind":"note","state":"open","title":"A title",""" +
          """"parent":"p/32","cites":["p/2"],"by":"nick"},"body":{"v":1,"notes":["p/2"],""" +
          """"blocks":""" +
          """[{"type":"para","text":[{"t":"See "},{"t":"b","n":[0]},{"t":"."}]}]}}"""
      )
      assert(shown == "A title\n\nSee b.")
    }
    test("a project written as data reads back equal: its key, pin, heads and bodies") {
      (0 until 50).foreach { seed =>
        val project = ProjectGen.project(seed)
        assert(throughData(project) == Right(project))
      }
    }
    test("data whose artifacts are another project's is not that project, saying why") {
      val pin = right(Pin.read(right(MethodName.of("grit")), 1, "0" * 64))
      val body = right(Body.read("Text.", MethodFixtures.ids))
      val project = right(
        Project.of(p, pin, Vector(Artifact(head("a"), body), Artifact(head("b"), body)))
      )
      val data = ArtifactJson.projectData(project)
      data("key") = "elsewhere"
      ArtifactJson.projectFrom(data) ==>
        Left("p/1 is not of project elsewhere; p/2 is not of project elsewhere")
    }
    test("a project's data with no key is refused as holding none, naming no head") {
      val pin = right(Pin.read(right(MethodName.of("grit")), 1, "0" * 64))
      val data = ArtifactJson.projectData(right(Project.of(p, pin, Vector.empty)))
      data.obj.remove("key")
      ArtifactJson.projectFrom(data) ==> Left("it holds no key")
    }
  }
}
