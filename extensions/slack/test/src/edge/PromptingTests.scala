package grit.slack.edge

import java.time.{Instant, LocalDate}

import scala.collection.immutable.VectorMap
import scala.concurrent.duration.*

import grit.core.admin.InMemoryAdministration
import grit.core.document.{DocLabel, DocText, DocWeight, DocumentTerms, InMemoryDocuments, Written}
import grit.core.edge.{
  Attesting,
  EdgeStores,
  InMemoryAcknowledgements,
  InMemoryDeliveries,
  InMemoryEdges,
  InMemoryJoins,
  InMemoryOrigins
}
import grit.core.id.{ConversationId, DocKey, PluginName}
import grit.core.identity.TestAccounts
import grit.core.inbox.InMemoryInbox
import grit.core.place.Place
import grit.core.review.{DocumentReview, InMemoryReviews, ReviewWord}
import grit.core.store.{InMemoryVoucher, Jot, Origin, StoreError, Tx}
import grit.core.visibility.{Clearance, Integrity, Label, Level, Recorded, Subject}
import grit.dbos.sql.TestTx
import grit.prose.form.{Block, Doc, Mark, Span, Text}
import grit.slack.client.{FakeSlack, Self, Tag}
import grit.slack.event.{ChannelId, Listed, Payloads, TeamId, Ts, UserId}
import grit.slack.text.RichText

import utest.*

/** [[SlackEdge]] prompting for a review beside triage's: a plugin's documents put up for
  * review, posted at the review's place with their prose and four reactions, and the verdicts
  * its rater gives them, over in-memory stores and a fake Slack.
  */
object PromptingTests extends TestSuite {
  import Payloads.*

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private def ok[A](e: Either[StoreError, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why.toString), identity)

  private val Plugin: PluginName = right(PluginName.of("topics"))

  private val Terms: DocumentTerms =
    right(DocLabel.of("topic").flatMap(DocumentTerms.of(_, DocWeight.Unscaled, 30.days, 500)))

  private val Day: LocalDate = LocalDate.parse("2026-10-12")
  private val At: Instant = Instant.parse("2026-10-12T05:00:00Z")

  private val Internal: Label = Label.at(Level.Internal)
  private val Confidential: Label = Label.at(Level.Confidential)

  private val C = ChannelId("C123ABC456")

  /** The review's place: a private channel grit's bot is in. */
  private val Here = ChannelId("C0REVIEW1")
  private val ReviewPlace: Place = Origin.channel(Team, "C0REVIEW1")

  private val Nick = "U0NICK001"

  private val Thread = "1515449522.000016"

  private def word(w: String): ReviewWord = right(ReviewWord.of(w))

  /** The four reactions, as a deployment declares them, each the word it keeps. */
  private val Verdicts: VectorMap[String, ReviewWord] = VectorMap(
    "white_check_mark" -> word("right"),
    "twisted_rightwards_arrows" -> word("split"),
    "link" -> word("merge"),
    "wastebasket" -> word("noise")
  )

  /** A document's prompt: its key in bold, then its thread, the conversation its data names,
    * linked where it links.
    */
  private def shown(d: DocumentReview, link: ConversationId => Option[String]): Doc[Nothing] = {
    val thread = d.document.data.objOpt
      .flatMap(_.get("thread"))
      .flatMap(_.strOpt)
      .map(ConversationId(_))
    Doc(
      Vector(
        Block.Paragraph(
          Text(
            Vector(Span(DocKey.value(d.key), Set(Mark.Strong))) ++ thread.toVector.map(c =>
              link(c).fold(Span.plain(" (unlinked)"))(href =>
                Span(" (thread)", Set(Mark.Link(href)))
              )
            )
          )
        )
      )
    )
  }

  private val Documents: Prompting.OverDocuments = over =>
    Prompting[DocumentReview, ReviewWord](
      over(Plugin),
      d => DocKey.value(d.key),
      _ => Prompting.Of.NoMessage,
      d => Some(d.label),
      (d, _, link) => shown(d, link),
      Verdicts
    )

  /** A review place recorded at `placed`, quiet when `quiet`, its edge's transactions opened at
    * `cleared`; recorded public in each transaction after the first [[fallAfter]].
    */
  private final class World(
      placed: Label = Internal,
      quiet: Boolean = false,
      cleared: Label = Confidential
  ) {
    val slack = new FakeSlack
    slack.channelNames = Map(C -> "standup", Here -> "debug")
    slack.privateChannels = Set(Here)
    val inbox: InMemoryInbox = InMemoryInbox.fresh()
    val documents = new InMemoryDocuments

    // Read and set by the test, on its one thread, around the edge calls it makes. It counts the
    // pass's transactions, not its reads of the place: a pass that opens one more before it posts
    // moves the fall, and each test setting it names the transactions it counts.
    @caps.unsafe.untrackedCaptures
    var fallAfter: Option[Int] = None
    // Counted by the jot's transactions, made by the edge on the test's thread.
    @caps.unsafe.untrackedCaptures
    var opened = 0

    def recorded: Recorded = {
      val label = if (fallAfter.exists(opened > _)) Label.Public else placed
      Recorded(Map(ReviewPlace -> Recorded.Kept(None, Some(label), quiet, None)), Map.empty)
    }

    def tx: Tx = TestTx.fake(Clearance.of(cleared), recorded = recorded)

    // Its transactions read the world's own state, which the test changes on its one thread.
    val jot: Jot = caps.unsafe.unsafeAssumePure(new Jot {
      def write[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]) = {
        opened += 1
        body(using tx)
      }
    })

    // Appended to by the edge's log on the test's thread, read by the test once it returns.
    @caps.unsafe.untrackedCaptures
    var logged = Vector.empty[String]

    private val review =
      right(SlackReview.of(ReviewPlace, UserId(Nick))).also(Vector(Documents))

    val edge: SlackEdge^ = new SlackEdge(
      slack,
      Self(TeamId(Team), UserId(Bot)),
      EdgeStores(
        inbox,
        InMemoryAdministration.none(),
        InMemoryJoins.none(),
        inbox.principals,
        new InMemoryDeliveries,
        inbox.ledger,
        new InMemoryAcknowledgements,
        InMemoryReviews.over(inbox),
        new InMemoryOrigins(inbox.conversations),
        documents.reviews.of,
        jot,
        new InMemoryEdges,
        new Attesting(InMemoryVoucher.none(), jot, _ => ())
      ),
      Some(review),
      ReplyNote.Off,
      grit.core.clock.Clock.system(),
      s => logged = logged :+ s
    )
    val _ = slack.listen(edge.receive, _ => ())

    /** The conversation of the thread `thread` in `channel` of `team`, its root known to Slack. */
    def conversation(team: String, channel: ChannelId, thread: String): ConversationId = {
      slack.histories = slack.histories.updated(
        channel,
        slack.histories.getOrElse(channel, Vector.empty) :+
          Listed(Ts(thread), None, Some(UserId(Ana)), false, None, "hm")
      )
      ok(
        inbox.conversations.findOrCreate(
          Origin.Slack(team, ChannelId.value(channel), thread),
          TestAccounts.account(s"slack:$team/$Ana"),
          Label.Public
        )(using TestTx.fake)
      ).id
    }

    /** The document `key` at `label`, naming `thread`, put up for review on [[Day]]. */
    def put(key: String, label: Label, thread: Option[ConversationId] = None): Unit = {
      // Written by a public writer, whose floor keeps it at `label` itself, and put up by one
      // that reads it.
      val keeper = documents.keeper(Plugin, Terms)
      ok(
        keeper.write(
          right(DocKey.of(key)),
          label,
          Integrity.Authority,
          Place.Everywhere,
          right(DocText.of(s"the $key")),
          ujson.Obj.from(thread.map(c => "thread" -> ujson.Str(ConversationId.value(c))).toSeq),
          None,
          At
        )(using TestTx.fake(Clearance.of(Label.Public)))
      ) match {
        case Written.Versioned(v, _, _) =>
          ok(keeper.forReview(v, Day, At)(using TestTx.fake(Clearance.of(Confidential)))) ==> true
        case Written.Unchanged(_, _) => throw new java.lang.AssertionError("unchanged")
      }
    }

    /** The keys of the documents waiting to be posted at the review's place. */
    def unposted: Vector[String] =
      ok(documents.reviews.of(Plugin).unposted(ReviewPlace)(using tx)).map(r => DocKey.value(r.key))

    /** Each word given on the documents' reviews standing. */
    def words: Vector[String] =
      ok(documents.reviews.labels(Plugin, Day)(using tx)).map(_.verdict.word)

    /** The ts of `key`'s prompt. */
    def promptOf(key: String): String =
      slack.posts
        .find(_.tag == Tag.Prompt(key))
        .fold(throw new java.lang.AssertionError(s"no prompt for $key"))(p => Ts.value(p.ts))
  }

  private def thread(href: Option[String]): Doc[Nothing] =
    Doc(
      Vector(
        Block.Paragraph(
          Text(
            Vector(
              Span("topic:a", Set(Mark.Strong)),
              href.fold(Span.plain(" (unlinked)"))(h => Span(" (thread)", Set(Mark.Link(h))))
            )
          )
        )
      )
    )

  private val Link = s"https://fake.slack.com/archives/C123ABC456/p1515449522000016"

  val tests = Tests {
    test(
      "a document put up is prompted at the top of the place, its thread linked, wearing its four reactions, then kept as posted"
    ) {
      val w = new World
      w.put("topic:a", Internal, Some(w.conversation(Team, C, Thread)))
      val first = w.edge.prompt()
      val ts = Ts(w.promptOf("topic:a"))
      (
        first,
        w.slack.posts.map(p => (p.channel, p.thread == p.ts, Vector(p.post), p.tag)),
        w.slack.reactions,
        w.unposted,
        w.edge.prompt()
      ) ==> (
        Right(1),
        Vector((Here, true, RichText.render(thread(Some(Link))), Tag.Prompt("topic:a"))),
        Verdicts.keySet.map(e => (Here, ts, e)),
        Vector.empty,
        Right(0)
      )
    }

    test("a thread of another workspace's Slack is shown unlinked") {
      val w = new World
      w.put("topic:a", Internal, Some(w.conversation("T0OTHER01", C, Thread)))
      val _ = w.edge.prompt()
      w.slack.posts.map(_.post) ==> RichText.render(thread(None))
    }

    test(
      "each of its four reactions keeps its word as the verdict, and removing it withdraws it"
    ) {
      val w = new World
      w.put("topic:a", Internal)
      val _ = w.edge.prompt()
      val ts = w.promptOf("topic:a")
      Verdicts.keys.toVector.map { emoji =>
        val added = w.slack.deliver(reaction(ts, emoji, user = Nick, channel = "C0REVIEW1"))
        val kept = w.words
        val removed =
          w.slack.deliver(reaction(ts, emoji, added = false, user = Nick, channel = "C0REVIEW1"))
        (added, kept, removed, w.words)
      } ==> Verdicts.values.toVector.map(v => (true, Vector(v.word), true, Vector.empty))
    }

    test("another person's reaction, or one on another message, is ignored") {
      val w = new World
      w.put("topic:a", Internal)
      val _ = w.edge.prompt()
      val ts = w.promptOf("topic:a")
      Vector(
        reaction(ts, "link", user = Ana, channel = "C0REVIEW1"),
        reaction("9.0", "link", user = Nick, channel = "C0REVIEW1")
      ).map(w.slack.deliver) -> w.words ==> (Vector(true, true) -> Vector.empty)
    }

    test("a document above the place's label is never offered, nor posted") {
      val w = new World
      w.put("topic:a", Confidential)
      // Never offered, so not even dropped: nothing said.
      (w.edge.prompt(), w.slack.posts, w.logged) ==> (Right(0), Vector.empty, Vector.empty)
    }

    test(
      "a document whose place falls below it before it is posted is not posted, and is dropped, which is said"
    ) {
      val w = new World
      w.put("topic:a", Internal)
      // A pass's first three transactions (its look at the place, triage's unposted read and
      // this review's) see the place as it was; its look before posting sees it fallen.
      w.fallAfter = Some(3)
      (w.edge.prompt(), w.slack.posts.size, w.unposted, w.logged) ==> (
        Right(0),
        0,
        Vector.empty,
        Vector(
          "slack: the review prompt of topic:a: the review's place is no longer written at a label dominating it, so it is dropped"
        )
      )
    }

    test(
      "a document whose place falls below it while it is posted is not kept as posted, and is dropped, which is said"
    ) {
      val w = new World
      w.put("topic:a", Internal)
      // As above, but its look before posting sees the place as it was too, and its keep fallen.
      w.fallAfter = Some(4)
      (w.edge.prompt(), w.slack.posts.size, w.unposted, w.logged) ==> (
        Right(0),
        1,
        Vector.empty,
        Vector(
          "slack: the review prompt of topic:a: the review's place is no longer written at a label dominating it, so it is dropped"
        )
      )
    }

    test("a review's place grit may not write to is said once, however many passes") {
      val w = new World(quiet = true)
      w.put("topic:a", Internal)
      (w.edge.prompt(), w.edge.prompt(), w.slack.posts, w.logged) ==> (
        Right(0),
        Right(0),
        Vector.empty,
        Vector(
          s"slack: the review's place, ${ReviewPlace.written}, is not labelled for grit to write to, so no prompt is posted there"
        )
      )
    }
  }
}
