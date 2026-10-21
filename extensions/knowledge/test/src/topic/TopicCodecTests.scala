package grit.knowledge.topic

import java.time.LocalDate

import grit.core.document.DocText
import grit.core.id.{ConversationId, DocumentVersionId}
import grit.core.place.Place
import grit.core.stitch.StrandDigest
import grit.knowledge.ref.TopicKey
import grit.prose.form.{Block, Doc, Mark, Span, Text}

import utest.*

object TopicCodecTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private def version(n: Long): DocumentVersionId =
    DocumentVersionId.of(n).getOrElse(throw new java.lang.AssertionError(n))

  private val key: TopicKey = right(TopicKey.strand(ConversationId("c-1")))
  private val other: TopicKey = right(TopicKey.strand(ConversationId("c-2")))

  private val strand = Lineage.Tag.Strand(
    ConversationId("c-1"),
    right(Place.read("slack:acme/#docs")),
    right(StrandDigest.read("0123456789abcdef"))
  )
  private val from =
    Lineage.of(Vector(strand, Lineage.Tag.Card(version(7)), Lineage.Tag.Topic(version(9))))

  private val c1 = right(ClaimId.of("c1"))
  private val c2 = right(ClaimId.of("c2"))

  private val firm =
    Claim(c1, Some(right(ClaimKey.of("release-date"))), Certainty.Firm, Vector.empty, from)
  private val later = Claim(
    c2,
    None,
    Certainty.Likely,
    Vector(
      Relation(Relation.Kind.Supersedes, ClaimRef(key, c1)),
      Relation(Relation.Kind.Contradicts, ClaimRef(other, c1))
    ),
    Lineage.of(Vector(Lineage.Tag.Card(version(7))))
  )

  private def para[A <: caps.Pure](spans: Span[A]*): Block[A] =
    Block.Paragraph(Text(spans.toVector))

  private val lead: Doc[TopicNote] = Doc(
    Vector(
      para(
        Span.plain("The release "),
        Span("ships Friday", Set(Mark.Strong), Vector(TopicNote.Claimed(firm))),
        Span.plain(", see "),
        Span("the docs", Set.empty, Vector(TopicNote.Link(other)))
      )
    )
  )

  private val monday = Passage(
    LocalDate.of(2026, 10, 12),
    from,
    Doc(Vector(para(Span.plain("Planned for Friday."))))
  )
  private val tuesday = Passage(
    LocalDate.of(2026, 10, 13),
    Lineage.of(Vector(Lineage.Tag.Card(version(7)))),
    Doc(Vector(para(Span("Moved to Monday", Set.empty, Vector(TopicNote.Claimed(later))))))
  )

  private val topic = Topic.of(lead, Vector(monday, tuesday))

  val tests = Tests {
    test("a topic reads back as written") {
      Topic.read(key, Topic.write(topic)) ==> Right(topic)
    }

    test("an unknown version is refused, naming it") {
      val v = Topic.write(topic)
      v("v") = 2
      val read = Topic.read(key, v)
      read ==> Left("a topic of version 2, which this reader does not know")
    }

    test("a topic with no lead is refused, naming it") {
      val v = Topic.write(topic)
      v.obj.remove("lead")
      val read = Topic.read(key, v)
      read ==> Left("a topic with no lead")
    }

    test("a relation to a claim of its own topic that it does not hold is refused, naming it") {
      val dangling = Claim(
        c2,
        None,
        Certainty.Doubtful,
        Vector(Relation(Relation.Kind.Confirms, ClaimRef(key, right(ClaimId.of("c5"))))),
        from
      )
      val t = Topic.of(
        Doc(Vector(para(Span("x", Set.empty, Vector(TopicNote.Claimed(dangling)))))),
        Vector.empty
      )
      val read = Topic.read(key, Topic.write(t))
      assert(read.left.exists(_.contains("c5")))
    }

    test("a relation to another topic's claim is kept unchecked") {
      val elsewhere = Topic.of(
        Doc(
          Vector(
            para(
              Span(
                "x",
                Set.empty,
                Vector(TopicNote.Claimed(later.copy(relations = later.relations.drop(1))))
              )
            )
          )
        ),
        Vector.empty
      )
      Topic.read(key, Topic.write(elsewhere)) ==> Right(elsewhere)
    }

    test("two different claims of one id are refused, naming it") {
      val twin = Claim(c1, None, Certainty.Doubtful, Vector.empty, from)
      val t = Topic.of(
        lead,
        Vector(
          Passage(
            monday.day,
            from,
            Doc(Vector(para(Span("y", Set.empty, Vector(TopicNote.Claimed(twin))))))
          )
        )
      )
      val read = Topic.read(key, Topic.write(t))
      read ==> Left("two different claims of id c1")
    }

    test("a topic keeps its newest passages, oldest first, past its most dropping the oldest") {
      val days = (1 to Topic.Passages + 2).toVector.map(d =>
        Passage(LocalDate.of(2026, 1, 1).plusDays(d.toLong), from, Doc.empty)
      )
      val kept = Topic.of(lead, days.reverse).passages
      kept ==> days.drop(2)
    }

    test(
      "a topics run's parameters read back as written, asideFor among them; ones kept before asideFor read it as AsideFor"
    ) {
      import scala.concurrent.duration.*
      val p = TopicParams(3, 2.days, 4.days, Some(LocalDate.parse("2026-10-12")), None, 3.days)
      val stored = ujson.Obj("perRun" -> 3, "recent" -> "PT48H", "cardRecent" -> "PT96H")
      (
        TopicParams.read(TopicParams.write(p)),
        TopicParams.read(stored).map(_.asideFor),
        TopicParams.AsideFor
      ) ==> (Right(p), Right(TopicParams.AsideFor), 7.days)
    }

    test("a lineage names each tag once, in the order first named") {
      val card = Lineage.Tag.Card(version(7))
      Lineage.of(Vector(card, strand, card)).tags ==> Vector(card, strand)
    }

    test("a claim's id is c and a number, its key lower-case words joined by -") {
      Vector("c1", "c12", "c0", "C1", "x1", "c").map(ClaimId.of(_).isRight) ==>
        Vector(true, true, false, false, false, false)
      Vector("release-date", "q3", "Release", "a--b", "-a", "a b").map(ClaimKey.of(_).isRight) ==>
        Vector(true, true, false, false, false, false)
    }

    test("its text is the lead, then each passage newest first under its day, notes lowered") {
      DocText.value(right(Topic.text(topic))) ==>
        s"""The release **ships Friday**, see [the docs](${TopicKey.value(other)})
          |
          |## 2026-10-13
          |
          |Moved to Monday
          |
          |## 2026-10-12
          |
          |Planned for Friday.""".stripMargin
    }

    test("a topic with no text has none") {
      assert(Topic.text(Topic.of(Doc.empty, Vector.empty)).isLeft)
    }
  }
}
