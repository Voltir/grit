package grit.knowledge.plugin

import java.time.{Instant, LocalDate}

import grit.core.document.{DocText, Document, Placement}
import grit.core.id.{ConversationId, DocKey, DocumentVersion, PluginName}
import grit.core.place.Place
import grit.core.stitch.{Link, StrandDigest}
import grit.core.visibility.{Integrity, Label}
import grit.knowledge.ref.TopicKey
import grit.knowledge.strand.StrandDocument
import grit.knowledge.topic.{Lineage, Passage, Topic}
import grit.prose.markdown.Markdown

import utest.*

object LeavesTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val room = right(Place.read("slack:T/C1"))
  private val digest = StrandDigest.of(Vector.empty)

  private def doc(plugin: String, key: DocKey, data: ujson.Value = ujson.Obj()): Document =
    Document(
      right(DocumentVersion.of(1).toRight("no version")),
      right(PluginName.of(plugin)),
      key,
      Label.Public,
      Integrity.Member,
      Place.Everywhere,
      right(DocText.of("x")),
      data,
      None,
      Instant.EPOCH,
      Placement(0, Instant.EPOCH),
      None
    )

  /** The topic opened by `root`, a passage of `passages` each written from the strands of its roots. */
  private def topic(root: String, passages: Vector[String]*): Document = {
    val t = Topic.of(
      Markdown.parse("A lead."),
      passages.toVector.map(roots =>
        Passage(
          LocalDate.parse("2026-10-12"),
          Lineage.of(roots.map(r => Lineage.Tag.Strand(ConversationId(r), room, digest))),
          Markdown.parse("A passage.")
        )
      )
    )
    doc("topics", TopicKey.doc(right(TopicKey.strand(ConversationId(root)))), Topic.write(t))
  }

  private def standing(c: String): Document =
    doc("standing", right(DocKey.of(s"standing:$c/3")))

  val tests = Tests {
    test("a topic's roots are the strands its passages were written from, each once") {
      val t = topic("a", Vector("a", "b"), Vector("a"))
      Leaves.roots(Vector(t, standing("a"))) ==> Vector(
        t.key -> Vector(ConversationId("a"), ConversationId("b"))
      )
    }

    test(
      "a topic's own leaves are its strands' documents and their conversations' Standing ones, no others"
    ) {
      val t = topic("a", Vector("a"))
      val window = Vector(
        t,
        doc("strands", StrandDocument.key(ConversationId("a"))),
        doc("strands", StrandDocument.key(ConversationId("z"))),
        standing("a-follower"),
        standing("z")
      )
      val links = Vector(Link(ConversationId("a-follower"), ConversationId("a")))
      Leaves.pairs(window, links) ==> Vector(
        Leaves.Own(t.key, StrandDocument.key(ConversationId("a")), Leaves.Kind.Strand),
        Leaves.Own(t.key, right(DocKey.of("standing:a-follower/3")), Leaves.Kind.Standing)
      )
    }

    test("a topic's root no conversation follows has its own Standing document as a leaf") {
      val t = topic("a", Vector("a"))
      Leaves.pairs(Vector(t, standing("a")), Vector.empty) ==>
        Vector(Leaves.Own(t.key, right(DocKey.of("standing:a/3")), Leaves.Kind.Standing))
    }
  }
}
