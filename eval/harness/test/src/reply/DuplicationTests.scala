package grit.eval.harness.reply

import java.time.{Instant, LocalDate}

import grit.core.document.{DocText, Document, Placement}
import grit.core.id.{ConversationId, DocKey, DocumentVersionId, PluginName, WorkflowId}
import grit.core.place.Place
import grit.core.stitch.{Link, StrandDigest}
import grit.core.visibility.{Integrity, Label}
import grit.knowledge.ref.TopicKey
import grit.knowledge.strand.StrandDocument
import grit.knowledge.topic.{Lineage, Passage, Topic}
import grit.prose.markdown.Markdown

import utest.*

object DuplicationTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val room = right(Place.read("slack:T/C1"))

  private def doc(plugin: String, key: DocKey, data: ujson.Value = ujson.Obj()): Document =
    Document(
      right(DocumentVersionId.of(1).toRight("no version")),
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

  /** The topic opened by the strand rooted at `root`, written from it alone. */
  private def topic(root: String): Document = {
    val t = Topic.of(
      Markdown.parse("A lead."),
      Vector(
        Passage(
          LocalDate.parse("2026-10-12"),
          Lineage.of(
            Vector(Lineage.Tag.Strand(ConversationId(root), room, StrandDigest.of(Vector.empty)))
          ),
          Markdown.parse("A passage.")
        )
      )
    )
    doc("topics", TopicKey.doc(right(TopicKey.strand(ConversationId(root)))), Topic.write(t))
  }

  private def strand(root: String) = doc("strands", StrandDocument.key(ConversationId(root)))

  private def standing(c: String) = doc("standing", right(DocKey.of(s"standing:$c/1")))

  private def w(n: Int) = WorkflowId(s"turn-$n")

  val tests = Tests {
    test(
      "a window holding a topic and its strand's document counts, one holding another strand's does not"
    ) {
      val d = Duplication.of(
        Vector(
          w(1) -> Right(Vector(topic("a"), strand("a"))),
          w(2) -> Right(Vector(topic("a"), strand("b"))),
          w(3) -> Right(Vector(strand("a"))),
          w(4) -> Left("unread")
        ),
        Vector.empty
      )
      d ==> Duplicated(3, 2, 1, 0, Vector(w(1)), Vector(w(4) -> "unread"))
    }

    test("a Standing document of a member conversation counts") {
      val links = Vector(Link(ConversationId("f"), ConversationId("a")))
      Duplication.of(
        Vector(
          w(1) -> Right(Vector(topic("a"), standing("f"))),
          w(2) -> Right(Vector(standing("f")))
        ),
        links
      ) ==> Duplicated(2, 1, 0, 1, Vector(w(1)), Vector.empty)
    }

    test("the roots asked for are each topic's strands', each once") {
      Duplication.roots(Vector(topic("a"), topic("a"), strand("b"))) ==>
        Vector(ConversationId("a"))
    }
  }
}
