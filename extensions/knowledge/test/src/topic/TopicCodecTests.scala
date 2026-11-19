package grit.knowledge.topic

import java.time.LocalDate

import grit.core.document.DocText
import grit.core.durable.Journaled
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

    test("recent under a day plus settle is refused") {
      import scala.concurrent.duration.*
      val p = TopicParams(3, 2.days, 4.days, None, None, sorts = 5)
      def recent(d: FiniteDuration) = {
        val v = TopicParams.write(p)
        v("recent") = java.time.Duration.ofSeconds(d.toSeconds).toString
        TopicParams.read(v).map(_.recent)
      }
      (
        TopicParams.read(TopicParams.write(p)),
        recent(1.day + 29.minutes).isLeft,
        recent(1.day + 30.minutes)
      ) ==>
        (Right(p), true, Right(1.day + 30.minutes))
    }

    test("v2 reads as a subject") {
      val v2 = Topic.write(topic)
      v2("v") = 2
      v2.obj.remove("kind")
      Topic.read(key, v2).map(_.kind) ==> Right(Topic.Kind.Subject)
    }

    test("a meeting round-trips") {
      val held = Meeting.of(Meeting.State.Held(LocalDate.of(2026, 11, 16)), Vector("Ana"))
      val planned = Meeting.of(
        Meeting.State.Planned(
          Meeting.When(LocalDate.of(2026, 11, 18), Some(java.time.LocalTime.of(9, 30)))
        ),
        Vector("Ana", "Ben")
      )
      Vector(Meeting.Opened, held, planned)
        .map(m => Topic.of(lead, Vector(monday), Topic.Kind.Meeting(m)))
        .foreach(t => Topic.read(key, Topic.write(t)) ==> Right(t))
    }

    test("a meeting's text opens with its head line") {
      val m = Meeting.of(
        Meeting.State.Planned(
          Meeting.When(LocalDate.of(2026, 11, 16), Some(java.time.LocalTime.of(9, 30)))
        ),
        Vector("Ana", "Ben", "Cy")
      )
      DocText
        .value(right(Topic.text(Topic.of(lead, Vector(monday), Topic.Kind.Meeting(m)))))
        .linesIterator
        .take(3)
        .toVector ==> Vector(
        "Meeting, planned for Monday 2026-11-16 at 09:30, with Ana, Ben and Cy.",
        "",
        s"The release **ships Friday**, see [the docs](${TopicKey.value(other)})"
      )
    }

    test("of leaves out a 13th name") {
      val names = (1 to 13).toVector.map(n => s"Person $n")
      Meeting.of(Meeting.State.Proposed(None), names :+ " " :+ ("x" * 81)).who ==> names.take(12)
    }

    val day = LocalDate.of(2026, 11, 19)
    def reply(fields: (String, ujson.Value)*) = Some(ujson.Obj.from(fields))

    test("refined drops a day 3 days back") {
      Meeting.refined(
        Meeting.Opened,
        reply("state" -> "planned", "day" -> "2026-11-16", "who" -> ujson.Arr("Ana")),
        day,
        90
      ) ==> Meeting.of(Meeting.State.Proposed(None), Vector("Ana"))
      Meeting
        .refined(Meeting.Opened, reply("state" -> "planned", "day" -> "2026-11-17"), day, 90)
        .state ==> Meeting.State.Planned(Meeting.When(LocalDate.of(2026, 11, 17), None))
    }

    test("held stays held") {
      val held = Meeting.of(Meeting.State.Held(LocalDate.of(2026, 11, 18)), Vector("Ana"))
      Meeting.refined(
        held,
        reply("state" -> "proposed", "day" -> "2026-11-25", "who" -> ujson.Arr("Ben")),
        day,
        90
      ) ==> held
    }

    test("a held on a later day becomes planned") {
      Meeting
        .refined(Meeting.Opened, reply("state" -> "held", "day" -> "2026-11-20"), day, 90)
        .state ==> Meeting.State.Planned(Meeting.When(LocalDate.of(2026, 11, 20), None))
    }

    test("refined: each reply's meeting, a bad or empty field leaving the one it had") {
      val ana = Meeting.of(Meeting.State.Proposed(None), Vector("Ana"))
      def on(d: Int) = LocalDate.of(2026, 11, d)
      Vector(
        // planned with no day is proposed
        reply("state" -> "planned"),
        // a day past `ahead` (5) is dropped
        reply("state" -> "planned", "day" -> "2026-11-25"),
        // held with no day is held on the run's day
        reply("state" -> "held"),
        // a bad day is dropped, the names kept
        reply("day" -> "next week", "start" -> "09:30", "who" -> ujson.Arr("Ben")),
        // no names is no change to them
        reply("state" -> "planned", "day" -> "2026-11-20", "who" -> ujson.Arr())
      ).map(Meeting.refined(ana, _, day, 5)) ==> Vector(
        ana,
        ana,
        Meeting.of(Meeting.State.Held(day), Vector("Ana")),
        Meeting.of(Meeting.State.Proposed(None), Vector("Ben")),
        Meeting.of(Meeting.State.Planned(Meeting.When(on(20), None)), Vector("Ana"))
      )
    }

    test("a topic is version 3, each doc stored prose, a claim on two spans written once") {
      val twice = Topic.of(
        Doc(
          Vector(
            para(
              Span("ships", Set.empty, Vector(TopicNote.Claimed(firm))),
              Span(" Friday", Set(Mark.Strong), Vector(TopicNote.Claimed(firm)))
            )
          )
        ),
        Vector(monday)
      )
      val v = Topic.write(twice)
      val lead = v("lead").objOpt
      val notes = lead.flatMap(_.get("notes")).flatMap(_.arrOpt).map(_.map(_.obj.keys.toVector))
      (v("v"), lead.flatMap(_.get("v")), notes) ==> (
        ujson.Num(3),
        Some(ujson.Num(1)),
        Some(Vector(Vector("claim")))
      )
      Topic.read(key, v) ==> Right(twice)
    }

    // A topic of the first version, as kept before prose was stored flat: never read again.
    test("a topic of version 1 is refused, naming its version") {
      val v1 = ujson.read(
        """{"v":1,"lead":[{"paragraph":[{"text":"The release"}]}],"passages":[]}"""
      )
      Topic.read(key, v1) ==> Left(
        Topic.Unread.Refused("a topic of version 1, which this reader does not know")
      )
    }

    // A topic a later reader kept: told apart from a refused one, so a run leaves it be.
    test("a topic of a later version is not refused but read as a later reader's") {
      val v3 = Topic.write(topic)
      v3("v") = 4
      Topic.read(key, v3) ==> Left(Topic.Unread.Later(4))
    }

    test("a topic with no lead is refused, naming it") {
      val v = Topic.write(topic)
      v.obj.remove("lead")
      val read = Topic.read(key, v)
      read ==> Left(Topic.Unread.Refused("a topic with no lead"))
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
      read ==> Left(Topic.Unread.Refused("claim c2 bears on c5, which this topic does not hold"))
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
      read ==> Left(Topic.Unread.Refused("two different claims of id c1"))
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

    test(
      "a topics run's parameters read back as written, keepFor among them; ones kept before keepFor read it as KeepFor, 90 days"
    ) {
      import scala.concurrent.duration.*
      val p = TopicParams(3, 2.days, 4.days, None, None, 7.days, 30.days)
      val stored = ujson.Obj("perRun" -> 3, "recent" -> "PT48H", "cardRecent" -> "PT96H")
      (
        TopicParams.read(TopicParams.write(p)),
        TopicParams.read(stored).map(_.keepFor),
        TopicParams.KeepFor
      ) ==> (Right(p), Right(TopicParams.KeepFor), 90.days)
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

    test(
      "a scan's unreadable and later counts round-trip, and a scan recorded without them reads none"
    ) {
      val codec = summon[Journaled[TopicScan.Scanned]]
      def scan(unreadable: Int, later: Int): TopicScan.Scanned = TopicScan.Scanned(
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector(grit.core.stitch.Strand.Id(grit.core.id.ConversationId("c1"), None) -> Vector.empty),
        0,
        0,
        0,
        Vector.empty,
        unreadable,
        later,
        Vector.empty
      )
      codec.decode(codec.encode(scan(1, 2))) ==> Right(scan(1, 2))
      // A scan journaled with neither count, as one that counted none is.
      val recorded = """{"attached":[],"strands":[],"cards":[],"topics":[],""" +
        """"near":[{"root":"c1","topics":[]}],"withdrew":0,"unchanged":0,"left":0,"linked":[]}"""
      codec.decode(recorded) ==> Right(scan(0, 0))
    }

    test(
      "a scan's topics to merge round-trip by key and version, and a scan recorded without them reads none"
    ) {
      val codec = summon[Journaled[TopicScan.Scanned]]
      def scan(single: Vector[(TopicKey, DocumentVersionId)]): TopicScan.Scanned =
        TopicScan.Scanned(
          Vector.empty,
          Vector.empty,
          Vector.empty,
          Vector.empty,
          Vector.empty,
          0,
          0,
          0,
          Vector.empty,
          0,
          0,
          single
        )
      val recorded = """{"attached":[],"strands":[],"cards":[],"topics":[],""" +
        """"near":[],"withdrew":0,"unchanged":0,"left":0,"linked":[]}"""
      (
        codec.decode(codec.encode(scan(Vector(key -> version(3), other -> version(5))))),
        codec.decode(recorded)
      ) ==> (Right(scan(Vector(key -> version(3), other -> version(5)))), Right(scan(Vector.empty)))
    }

    test(
      "a topics run's parameters read back as written, merges among them; ones kept before merges read it as Merges, 8"
    ) {
      import scala.concurrent.duration.*
      val p = TopicParams(3, 2.days, 4.days, None, None, 7.days, 30.days, 2)
      val stored = ujson.Obj("perRun" -> 3, "recent" -> "PT48H", "cardRecent" -> "PT96H")
      (
        TopicParams.read(TopicParams.write(p)),
        TopicParams.read(stored).map(_.merges),
        TopicParams.Merges
      ) ==> (Right(p), Right(TopicParams.Merges), 8)
    }

    test("a topics run's parameters with sorts over Sorts, 32, are refused, naming sorts") {
      val stored =
        ujson.Obj("perRun" -> 3, "recent" -> "PT48H", "cardRecent" -> "PT96H", "sorts" -> 33)
      (TopicParams.read(stored).left.map(_.contains("hold sorts")), TopicParams.Sorts) ==>
        (Left(true), 32)
    }

    test("a topics run's parameters with merges over Merges, 8, are refused, naming merges") {
      val stored =
        ujson.Obj("perRun" -> 3, "recent" -> "PT48H", "cardRecent" -> "PT96H", "merges" -> 9)
      (TopicParams.read(stored).left.map(_.contains("hold merges")), TopicParams.Merges) ==>
        (Left(true), 8)
    }
  }
}
