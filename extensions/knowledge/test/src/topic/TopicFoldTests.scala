package grit.knowledge.topic

import java.time.LocalDate

import grit.core.id.{ConversationId, DocumentVersionId}
import grit.core.place.Place
import grit.core.stitch.StrandDigest
import grit.knowledge.ref.TopicKey
import grit.prose.form.{Block, Doc, Span, Text}

import utest.*

/** [[Topic.folded]]: one topic's passages and claims moved into another's. */
object TopicFoldTests extends TestSuite {
  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private def version(n: Long): DocumentVersionId =
    DocumentVersionId.of(n).getOrElse(throw new java.lang.AssertionError(n))

  private def id(n: Int): ClaimId = right(ClaimId.of(s"c$n"))

  private val into: TopicKey = right(TopicKey.strand(ConversationId("c-1")))
  private val from: TopicKey = right(TopicKey.strand(ConversationId("c-2")))
  private val elsewhere: TopicKey = right(TopicKey.strand(ConversationId("c-3")))
  private val was = version(41)

  private def strand(root: String) = Lineage.Tag.Strand(
    ConversationId(root),
    right(Place.read("slack:acme/#docs")),
    right(StrandDigest.read("0123456789abcdef"))
  )
  private val ofInto = Lineage.of(Vector(strand("c-1")))
  private val ofFrom = Lineage.of(Vector(strand("c-2")))

  private def doc(words: String, notes: TopicNote*): Doc[TopicNote] =
    Doc(Vector(Block.Paragraph(Text(Vector(Span(words, Set.empty, notes.toVector))))))

  private def claim(n: Int, bears: ClaimRef*): TopicNote =
    TopicNote.Claimed(
      Claim(
        id(n),
        None,
        Certainty.Firm,
        bears.toVector.map(Relation(Relation.Kind.Supersedes, _)),
        ofInto
      )
    )

  private def day(d: Int): LocalDate = LocalDate.of(2026, 11, d)

  private def claims(t: Topic): Vector[Claim] =
    (t.lead.notes ++ t.passages.flatMap(_.body.notes)).collect { case TopicNote.Claimed(c) =>
      c
    }.distinct

  val tests: Tests = Tests {
    test(
      "a fold keeps the kept topic's lead, and each passage under its own day, a moved one naming the topic it came from"
    ) {
      val a = Topic.of(doc("A's lead"), Vector(Passage(day(10), ofInto, doc("A on the 10th"))))
      val b = Topic.of(doc("B's lead"), Vector(Passage(day(12), ofFrom, doc("B on the 12th"))))
      val t = Topic.folded(into, a, from, b, was)
      (t.lead, t.passages) ==> (
        doc("A's lead"),
        Vector(
          Passage(day(10), ofInto, doc("A on the 10th")),
          Passage(
            day(12),
            Lineage.of(Vector(strand("c-2"), Lineage.Tag.Topic(was))),
            doc("B on the 12th")
          )
        )
      )
    }

    test(
      "on a day both hold, the passage is the kept topic's body then the folded one's, their lineages joined"
    ) {
      val a = Topic.of(doc("A's lead"), Vector(Passage(day(10), ofInto, doc("A on the 10th"))))
      val b = Topic.of(doc("B's lead"), Vector(Passage(day(10), ofFrom, doc("B on the 10th"))))
      Topic.folded(into, a, from, b, was).passages ==> Vector(
        Passage(
          day(10),
          Lineage.of(Vector(strand("c-1"), strand("c-2"), Lineage.Tag.Topic(was))),
          Doc(doc("A on the 10th").blocks ++ doc("B on the 10th").blocks)
        )
      )
    }

    test(
      "a folded topic's claims are numbered past the kept one's, relations to them follow, and it reads back"
    ) {
      // A holds c1 and c3; B holds c1 and c2, its c2 bearing on its own c1 and on a third topic's.
      val a = Topic.of(
        doc("A's lead", claim(1)),
        Vector(Passage(day(10), ofInto, doc("A", claim(3), claim(4, ClaimRef(from, id(2))))))
      )
      val b = Topic.of(
        doc("B's lead"),
        Vector(
          Passage(day(11), ofFrom, doc("B one", claim(1))),
          Passage(
            day(12),
            ofFrom,
            doc("B two", claim(2, ClaimRef(from, id(1)), ClaimRef(elsewhere, id(1))))
          )
        )
      )
      val t = Topic.folded(into, a, from, b, was)
      val bearing = claims(t).map(c => ClaimId.value(c.id) -> c.relations.map(_.to)).toMap
      (
        claims(t).map(c => ClaimId.value(c.id)).sorted,
        bearing.get("c4"),
        bearing.get("c6"),
        Topic.read(into, Topic.write(t))
      ) ==> (
        Vector("c1", "c3", "c4", "c5", "c6"),
        Some(Vector(ClaimRef(into, id(6)))),
        Some(Vector(ClaimRef(into, id(5)), ClaimRef(elsewhere, id(1)))),
        Right(t)
      )
    }

    test("a relation to a claim of the folded topic's lead, which no passage holds, is dropped") {
      val a = Topic.of(doc("A's lead"), Vector(Passage(day(10), ofInto, doc("A", claim(1)))))
      val b = Topic.of(
        doc("B's lead", claim(1)),
        Vector(Passage(day(11), ofFrom, doc("B", claim(2, ClaimRef(from, id(1))))))
      )
      val t = Topic.folded(into, a, from, b, was)
      (
        claims(t).map(c => ClaimId.value(c.id) -> c.relations),
        Topic.read(into, Topic.write(t)).isRight
      ) ==> (Vector("c1" -> Vector.empty, "c3" -> Vector.empty), true)
    }

    test("a fold past Passages drops the oldest passages, whichever topic held them") {
      // B holds the 1st to the 20th day, A the 21st to the 40th: 40 days, the 10 oldest dropped.
      def nth(d: Int) = day(1).plusDays(d.toLong)
      val a = Topic.of(
        doc("A's lead"),
        (21 to 40).toVector.map(d => Passage(nth(d), ofInto, doc(s"A $d")))
      )
      val b = Topic.of(
        doc("B's lead"),
        (1 to 20).toVector.map(d => Passage(nth(d), ofFrom, doc(s"B $d")))
      )
      val t = Topic.folded(into, a, from, b, was)
      (t.passages.size, t.passages.map(_.day).headOption, t.passages.map(_.day).lastOption) ==> (
        Topic.Passages,
        Some(nth(41 - Topic.Passages)),
        Some(nth(40))
      )
    }

    // A topic written from the strands of `roots`, a passage each.
    def written(roots: String*): Topic = Topic.of(
      doc("lead"),
      roots.toVector.zipWithIndex.map((r, i) =>
        Passage(day(i + 1), Lineage.of(Vector(strand(r))), doc(r))
      )
    )
    def keyed(n: Int): TopicKey = right(TopicKey.strand(ConversationId(s"t-$n")))

    test(
      "a scan offers to merge the topics of fewer than two discussions, oldest written first, at most merges"
    ) {
      val topics = Vector(
        (keyed(1), version(30), written("s-1")),
        (keyed(2), version(10), written("s-2", "s-3")),
        (keyed(3), version(20), written("s-4", "s-4")),
        (keyed(4), version(5), written()),
        (keyed(5), version(40), written("s-5"))
      )
      (
        TopicScan.single(topics, Vector.empty, 8),
        TopicScan.single(topics, Vector.empty, 2)
      ) ==> (
        Vector(
          keyed(4) -> version(5),
          keyed(3) -> version(20),
          keyed(1) -> version(30),
          keyed(5) -> version(40)
        ),
        Vector(keyed(4) -> version(5), keyed(3) -> version(20))
      )
    }

    test("a topic set aside at its version is not offered to merge; one set aside at another is") {
      val at = java.time.Instant.parse("2026-11-19T00:00:00Z")
      val topics =
        Vector((keyed(1), version(30), written("s-1")), (keyed(2), version(31), written("s-2")))
      val aside = Vector(
        Aside.Item(Lineage.Tag.Topic(version(30)), at),
        Aside.Item(Lineage.Tag.Topic(version(12)), at)
      )
      TopicScan.single(topics, aside, 8) ==> Vector(keyed(2) -> version(31))
    }

    test(
      "a scan offers to merge no meeting without another of its day, and no undated one, so the subjects past them are offered"
    ) {
      def on(d: Int) =
        TestMeetings.meeting(Meeting.State.Planned(Meeting.When(day(d), None)), Vector.empty, "m")
      val undated = TestMeetings.meeting(Meeting.State.Proposed(None), Vector.empty, "m")
      val topics = Vector(
        (keyed(1), version(1), on(1)),
        (keyed(2), version(2), on(2)),
        (keyed(3), version(3), undated),
        (keyed(4), version(4), undated),
        (keyed(5), version(5), on(1)),
        (keyed(6), version(6), written("s-1")),
        (keyed(7), version(7), written("s-2"))
      )
      TopicScan.single(topics, Vector.empty, 4) ==> Vector(
        keyed(1) -> version(1),
        keyed(5) -> version(5),
        keyed(6) -> version(6),
        keyed(7) -> version(7)
      )
    }

    test(
      "a scan offers to merge no meeting whose only others of its day start at another time; one with no start pairs with either"
    ) {
      def at(d: Int, h: Option[Int]) = TestMeetings.meeting(
        Meeting.State.Planned(Meeting.When(day(d), h.map(java.time.LocalTime.of(_, 0)))),
        Vector.empty,
        "m"
      )
      val topics = Vector(
        (keyed(1), version(1), at(1, Some(9))),
        (keyed(2), version(2), at(1, Some(14))),
        (keyed(3), version(3), at(2, Some(9))),
        (keyed(4), version(4), at(2, None))
      )
      TopicScan.single(topics, Vector.empty, 8) ==>
        Vector(keyed(3) -> version(3), keyed(4) -> version(4))
    }
  }
}
