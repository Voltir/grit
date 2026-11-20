package grit.knowledge.topic

import java.time.{LocalDate, ZoneId, ZoneOffset}

import grit.core.document.DocText
import grit.core.id.{ConversationId, DocumentVersionId}
import grit.core.identity.TestAccounts
import grit.core.place.Place
import grit.core.stitch.{Strand, StrandDigest}
import grit.core.store.InMemoryPrincipals
import grit.core.visibility.Label
import grit.knowledge.ref.TopicKey

import utest.*

/** [[PersonCard]]: a person's card, its facts by code, its data and its text. */
object PersonCardTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val Day = LocalDate.parse("2026-11-10")

  private def version(n: Long): DocumentVersionId =
    DocumentVersionId.of(n).getOrElse(throw new java.lang.AssertionError(n))

  private def person(slack: String) =
    InMemoryPrincipals.person(TestAccounts.account(s"slack:T/$slack"))

  private val dana = person("U-dana")
  private val bo = person("U-bo")
  private val cy = person("U-cy")

  private def name(n: String): Strand.Name =
    Strand.Name.of(n).getOrElse(throw new java.lang.AssertionError(n))

  /** The strand of `root` in `room`, its last message said `day`. */
  private def strand(root: String, room: String = "C1", day: LocalDate = Day): Strand.Digested = {
    val at = day.atStartOfDay(ZoneOffset.UTC).toInstant.plusSeconds(3600)
    Strand.Digested(
      right(Place.read(s"slack:T/$room")),
      Label.Public,
      ConversationId(root),
      Vector(ConversationId(root)),
      2,
      40,
      at,
      at,
      right(StrandDigest.read("0123456789abcdef")),
      at
    )
  }

  private def spoke(p: grit.core.id.PrincipalId, n: Option[String], at: LocalDate = Day) =
    Strand.Spoke(
      p,
      1,
      at.atStartOfDay(ZoneOffset.UTC).toInstant.plusSeconds(3600),
      n.map(name),
      None
    )

  private def topic(
      lead: String,
      from: Vector[Strand.Digested],
      kind: Topic.Kind = Topic.Kind.Subject
  ) =
    Topic.of(
      TopicWrite.prose(lead),
      Vector(
        Passage(
          Day,
          Lineage.of(from.map(s => Lineage.Tag.Strand(s.root, s.room, s.digest, s.id.first))),
          TopicWrite.prose("what was said")
        )
      ),
      kind
    )

  private def key(n: String): TopicKey = right(TopicKey.strand(ConversationId(n)))

  private val Facts0 = PersonCard.Facts(Vector.empty, Vector.empty, 1, 1, Day)

  private def card(written: LocalDate = Day, seen: Vector[Strand.Digested] = Vector(strand("r1"))) =
    PersonCard.written(
      None,
      right(PersonKey.of(dana)),
      name("Dana Ito"),
      Some(ZoneId.of("Europe/London")),
      Facts0,
      Vector.empty,
      seen,
      written
    )

  val tests = Tests {
    test("a card is due after 7 days and 5 fresh strands, not before") {
      Vector(
        PersonCard.due(None, 4, Day),
        PersonCard.due(None, 5, Day),
        PersonCard.due(Some(card(Day.minusDays(6))), 5, Day),
        PersonCard.due(Some(card(Day.minusDays(7))), 4, Day),
        PersonCard.due(Some(card(Day.minusDays(7))), 5, Day)
      ) ==> Vector(false, true, false, false, true)
    }

    test("a paragraph is cut to 300 characters at a word; a blank one is none") {
      val long = Vector.fill(80)("word").mkString(" ")
      card().told(Some(long)).paragraph ==> Some(Vector.fill(60)("word").mkString(" ") + "…")
      card().told(Some("  ")).paragraph ==> None
    }

    test("no paragraph said keeps the paragraph before") {
      card().told(Some("Appears to run the releases.")).told(None).paragraph ==>
        Some("Appears to run the releases.")
    }

    test("a strand from a 33rd room is not read into the card") {
      val first = card(seen = (1 to 32).toVector.map(i => strand(s"r$i", room = s"C$i")))
      val late = strand("r33", room = "C33")
      val c = PersonCard.written(
        Some(first),
        first.key,
        first.name,
        first.zone,
        Facts0,
        Vector.empty,
        Vector(late, strand("r1", room = "C1")),
        Day
      )
      (
        c.from.size,
        c.from.contains(late.room),
        c.fresh(Vector(late)),
        c.fresh(Vector(strand("r1")))
      ) ==>
        (32, false, 1, 0)
    }

    test("data reads back to the same card") {
      val c = PersonCard
        .written(
          None,
          right(PersonKey.of(dana)),
          name("Dana Ito"),
          Some(ZoneId.of("Europe/London")),
          PersonCard.Facts(Vector(key("a") -> version(3)), Vector(name("Bo")), 2, 3, Day),
          Vector(PersonCard.Quote(key("a"), version(3), "The release train.", Some(Day))),
          Vector(strand("r1")),
          Day
        )
        .told(Some("Appears to run the releases."))
      PersonCard.read(c.key, c.write) ==> Right(c)
    }

    test("a card's text is its name's line, its paragraph, then its facts' lines") {
      val c = PersonCard
        .written(
          None,
          right(PersonKey.of(dana)),
          name("Dana Ito"),
          Some(ZoneId.of("Europe/London")),
          PersonCard.Facts(Vector.empty, Vector(name("Bo"), name("Cy")), 2, 3, Day),
          Vector(
            PersonCard.Quote(key("a"), version(3), "The release train.", None),
            PersonCard.Quote(key("b"), version(4), "The pricing review.", Some(Day))
          ),
          Vector(strand("r1")),
          Day
        )
        .told(Some("Appears to run the releases."))
      c.text.map(DocText.value) ==> Right(
        """Dana Ito, as grit sees them from the team's talk
          |Appears to run the releases.
          |Talks about: The release train. (meetings: The pricing review. on 2026-11-10)
          |Works most with: Bo, Cy
          |Active in 2 rooms on 3 days lately; last spoke 2026-11-10. Time zone: Europe/London.""".stripMargin
      )
    }

    test("a card quoting a topic no longer current is re-rendered without it") {
      val c = PersonCard.written(
        None,
        right(PersonKey.of(dana)),
        name("Dana Ito"),
        None,
        Facts0,
        Vector(
          PersonCard.Quote(key("a"), version(3), "The release train.", None),
          PersonCard.Quote(key("b"), version(4), "The pricing review.", None)
        ),
        Vector(strand("r1")),
        Day
      )
      val left = c.without(Set(version(3)))
      (left.quotes.map(_.said), left.text.map(DocText.value(_).contains("pricing"))) ==>
        (Vector("The release train."), Right(false))
    }

    test("a card whose person last spoke more than 90 days before has lapsed; 90 days has not") {
      (card().lapsed(Day.plusDays(90)), card().lapsed(Day.plusDays(91))) ==> (false, true)
    }

    test(
      "a person's topics are those whose lineage holds a strand they spoke in, newest first, at most 8"
    ) {
      val theirs = (1 to 10).toVector.map(i => strand(s"r$i"))
      val other = strand("elsewhere")
      val said = theirs.map(s => s -> Vector(spoke(dana, Some("Dana")))) :+
        (other -> Vector(spoke(bo, Some("Bo"))))
      // Newest first, as a scan reads the floor: t10 down to t1, then one of a strand not theirs.
      val topics = theirs.reverse.zipWithIndex.map((s, i) =>
        (key(s"t${10 - i}"), version(20L - i), topic(s"Topic ${10 - i}.", Vector(s)))
      ) :+ (key("x"), version(1), topic("Not theirs.", Vector(other)))
      PersonCard.Facts.of(dana, said, topics, ZoneOffset.UTC).map(_.topics.map(_._1)) ==>
        Some((3 to 10).reverse.toVector.map(i => key(s"t$i")))
    }

    test(
      "a person's peers are those they share most strands with, ties by id, named; rooms and days by the strands they spoke in"
    ) {
      val a = strand("a", room = "C1", day = Day.minusDays(2))
      val b = strand("b", room = "C2", day = Day.minusDays(1))
      val c = strand("c", room = "C2", day = Day)
      val said = Vector(
        a -> Vector(
          spoke(dana, Some("Dana"), Day.minusDays(2)),
          spoke(cy, Some("Cy")),
          spoke(bo, Some("Bo"))
        ),
        b -> Vector(spoke(dana, Some("Dana"), Day.minusDays(1)), spoke(cy, Some("Cy"))),
        c -> Vector(spoke(dana, Some("Dana"), Day.minusDays(1)), spoke(person("U-nameless"), None))
      )
      PersonCard.Facts.of(dana, said, Vector.empty, ZoneOffset.UTC) ==>
        Some(PersonCard.Facts(Vector.empty, Vector(name("Cy"), name("Bo")), 2, 2, Day.minusDays(1)))
    }

    test("a card's key is person: then its id; a topic's key is no card's") {
      val k = right(PersonKey.of(dana))
      (
        grit.core.id.DocKey.value(PersonKey.doc(k)),
        PersonKey.read(PersonKey.doc(k)),
        PersonKey.read(TopicKey.doc(key("a")))
      ) ==> ("person:slack:T/U-dana", Some(k), None)
    }
  }
}
