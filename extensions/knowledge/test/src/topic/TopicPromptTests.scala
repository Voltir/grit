package grit.knowledge.topic

import java.time.{Instant, LocalDate}

import scala.concurrent.duration.*

import grit.core.document.{DocLabel, DocWeight, DocumentTerms, InMemoryDocuments}
import grit.core.id.{ConversationId, DocKey, DocumentVersion, PluginName}
import grit.core.identity.TestAccounts
import grit.core.place.Place
import grit.core.review.Reviews
import grit.core.stitch.StrandDigest
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{Clearance, Label, Recorded}
import grit.dbos.sql.TestTx
import grit.knowledge.ref.TopicKey
import grit.prose.form.{Block, Doc, Span, Text}
import grit.prose.markdown.Markdown

import utest.*

/** A topics plugin's reviews read as topic prompts, over documents in memory, and the prompt a
  * topic shows.
  */
object TopicPromptTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private def ok[A](e: Either[StoreError, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val Plugin: PluginName = right(PluginName.of("topics"))

  private val Terms: DocumentTerms =
    right(DocLabel.of("topic").flatMap(DocumentTerms.of(_, DocWeight.Unscaled, 30.days, 500)))

  private val Day: LocalDate = LocalDate.parse("2026-10-12")

  private val At: Instant = Instant.parse("2026-10-12T05:00:00Z")

  private val Room: Place = right(Place.read("slack:T/CROOM"))

  private val Review: Place = right(Place.read("slack:T/CREVIEW"))

  private val Rater = TestAccounts.account("slack:T/U1")

  private val Digest: StrandDigest = right(StrandDigest.read("0123456789abcdef"))

  private val Old = ConversationId("c-old")
  private val First = ConversationId("c-first")
  private val Second = ConversationId("c-second")

  private val Said = "Ana traced the flaky deploy to the shared database."

  /** The lead, its first words a link to another topic. */
  private val Lead: Doc[TopicNote] = Doc(
    Vector(
      Block.Paragraph(
        Text(
          Vector(
            Span(
              "The deploy pipeline",
              Set.empty,
              Vector(TopicNote.Link(right(TopicKey.strand(Old))))
            ),
            Span.plain(" and what breaks it.")
          )
        )
      )
    )
  )

  private def strand(root: ConversationId): Lineage.Tag = Lineage.Tag.Strand(root, Room, Digest)

  /** A topic opened by `Old` a day before [[Day]], which grew on [[Day]] from `Second` and
    * `First`, in that order, and a card.
    */
  private val Grown: Topic = Topic.of(
    Lead,
    Vector(
      Passage(
        Day.minusDays(1),
        Lineage.of(Vector(strand(Old))),
        Doc(Vector(Block.Paragraph(Text.plain("Older."))))
      ),
      Passage(
        Day,
        Lineage.of(
          Vector(
            strand(Second),
            Lineage.Tag.Card(right(DocumentVersion.of(7).toRight("no version"))),
            strand(First),
            strand(Second)
          )
        ),
        Doc(Vector(Block.Paragraph(Text.plain(Said))))
      )
    )
  )

  private val Reading: Tx = TestTx.fake(
    Clearance.of(Label.Public),
    recorded =
      Recorded(Map(Review -> Recorded.Kept(None, Some(Label.Public), false, None)), Map.empty)
  )

  /** Documents holding `topic` under `Old`'s key, its day's review `today`, put up for review on
    * [[Day]].
    */
  private def putUp(topic: Topic, today: Option[TopicWrite.Today]): InMemoryDocuments = {
    given Tx = Reading
    val documents = new InMemoryDocuments
    val keeper = documents.keeper(Plugin, Terms)
    val kept = ok(
      TopicWrite.kept(
        keeper,
        right(TopicKey.strand(Old)),
        Label.Public,
        TopicWrite.Held(topic, Vector.empty, today),
        At
      )
    )
    kept.version.foreach(v => ok(keeper.forReview(v, Day, At)))
    documents
  }

  private def prompts(documents: InMemoryDocuments): Reviews[TopicPrompt, TopicVerdict] =
    TopicPrompt.reviews(documents.reviews.of(Plugin))

  private val Today = TopicWrite.Today(Day, opened = false, cards = Vector("Deploy runbook"))

  val tests = Tests {
    test(
      "a topic put up is offered as its prompt: not opened that day, its lead plain, that day's strands by root once each, and its cards"
    ) {
      given Tx = Reading
      val offered = ok(prompts(putUp(Grown, Some(Today))).unposted(Review))
      offered.map(p =>
        (DocKey.value(p.review.key), p.opened, p.lead, p.strands, p.cards)
      ) ==> Vector(
        (
          TopicKey.value(right(TopicKey.strand(Old))),
          false,
          Doc(
            Vector(
              Block.Paragraph(
                Text(Vector(Span.plain("The deploy pipeline"), Span.plain(" and what breaks it.")))
              )
            )
          ),
          Vector(Second, First),
          Vector("Deploy runbook")
        )
      )
    }

    test("a topic whose document holds no day's review is never offered") {
      given Tx = Reading
      ok(prompts(putUp(Grown, None)).unposted(Review)) ==> Vector.empty
    }

    test("each verdict is kept as its word: right, split, merge, noise") {
      given Tx = Reading
      val documents = putUp(Grown, Some(Today))
      val reviews = prompts(documents)
      val p = ok(reviews.unposted(Review)).headOption
        .getOrElse(throw new java.lang.AssertionError("none offered"))
      ok(reviews.posted(p, "CREVIEW/1.0", At)) ==> true
      TopicVerdict.values.toVector.map { v =>
        ok(reviews.reacted("CREVIEW/1.0", Rater, v, At))
        ok(documents.reviews.labels(Plugin, Day)).map(_.verdict.word)
      } ==> Vector(Vector("right"), Vector("split"), Vector("merge"), Vector("noise"))
    }

    test(
      "its prompt asks the question, says the topic grew or is new, shows its lead, links each thread that links, and names its cards, never the day's passage"
    ) {
      given Tx = Reading
      val p = ok(prompts(putUp(Grown, Some(Today))).unposted(Review)).headOption
        .getOrElse(throw new java.lang.AssertionError("none offered"))
      val link: ConversationId => Option[String] =
        c => Option.when(c == First)("https://example.slack.com/archives/C1/p1")
      val grew = Markdown.write(TopicPrompt.doc(p, link))
      val opened = Markdown.write(TopicPrompt.doc(p.copy(opened = true), link))
      (grew, opened.linesIterator.next(), grew.contains(Said)) ==> (
        Vector(
          "**Is this one topic, told right?** It grew today.",
          "",
          "The deploy pipeline and what breaks it.",
          "",
          "From today's threads: thread 1, [thread 2](https://example.slack.com/archives/C1/p1)",
          "",
          "Cards: Deploy runbook"
        ).mkString("\n"),
        "**Is this one topic, told right?** New today.",
        false
      )
    }
  }
}
