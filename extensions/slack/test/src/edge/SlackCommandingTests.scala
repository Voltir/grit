package grit.slack.edge

import java.time.{Instant, ZoneOffset}

import grit.core.admin.{
  Administration,
  AdministrationContract,
  Answer,
  Change,
  Command,
  InMemoryAdministration,
  Refusal
}
import grit.core.edge.{
  Attesting,
  EdgeStores,
  InMemoryAcknowledgements,
  InMemoryDeliveries,
  InMemoryEdges
}
import grit.core.identity.{Account, Realm, TestAccounts}
import grit.core.inbox.InMemoryInbox
import grit.core.place.Place
import grit.core.review.InMemoryReviews
import grit.core.spend.Budget
import grit.core.store.{InMemoryVoucher, Jot, Origin, StoreError, Tx}
import grit.core.visibility.TestLabels.{compartment, group}
import grit.core.visibility.{
  Compartments,
  Grant,
  Group,
  Label,
  Level,
  RoomLabels,
  Steward,
  Subject,
  Visibility
}
import grit.dbos.sql.TestTx
import grit.slack.client.{FakeSlack, Self, SlackError}
import grit.slack.event.{Payloads, ResponseUrl, TeamId, UserId}

import utest.*

/** The Slack edge answering its slash command, over a fake Slack and the in-memory
  * administration, whose people are whom the edge's attestation vouches.
  */
object SlackCommandingTests extends TestSuite {
  import Payloads.*

  private object FakeJot extends Jot {
    def write[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using
        TestTx.fake
      )
  }

  private val Grit: SlackCommand =
    SlackCommand.of("/grit").fold(e => throw new java.lang.AssertionError(e), identity)

  private val Ours: Realm =
    SlackAccounts.realm(TeamId(Team)).fold(e => throw new java.lang.AssertionError(e), identity)

  private def of(user: String): Account = TestAccounts.account(s"slack:$Team/$user")

  /** An administrator, declared by account. */
  private val Ada = "U0ADA0001"

  /** A member of grit's team, in no declared group. */
  private val Ben = "U0BEN0001"

  private val trial = compartment("trial")

  /** grit's team's full members cleared internal; [[Ada]] administers; trial's own group
    * granted internal·trial and its steward.
    */
  private val Declared: Visibility =
    (for {
      compartments <- Compartments.of(Vector(trial)).left.map(_.toString)
      v <- Visibility
        .of(
          compartments,
          RoomLabels.Public,
          Vector(
            Group(group("staff"), Set.empty, Set(Ours)),
            Group(group("admins"), Set(of(Ada))),
            Group(group("trial"), Set.empty)
          ),
          Vector(
            Grant(group("staff"), Label.at(Level.Internal)),
            Grant(group("trial"), Label.at(Level.Internal, trial))
          ),
          administrators = Some(group("admins")),
          stewards = Vector(Steward(trial, group("trial")))
        )
        .left
        .map(_.toString)
    } yield v).fold(e => throw new java.lang.AssertionError(e), identity)

  private val Now = Instant.parse("2026-10-08T09:00:00Z")

  private object Stopped extends grit.core.clock.Clock {
    def now(): Instant = Now
    def millis(): Long = 0L
    def sleep(duration: scala.concurrent.duration.FiniteDuration): Unit = ()
  }

  /** The room of channel C123ABC456. */
  private val Channel: Place = Origin.channel(Team, "C123ABC456")

  /** The edge over a fake Slack knowing [[Ada]], [[Ben]] and Ana, and an administration under
    * [[Declared]] whose voucher the edge's attestation writes; a database that fails when
    * `failing`.
    */
  private final class World(failing: Boolean = false) {
    val slack = new FakeSlack
    slack.names = Map(
      UserId(Ana) -> Some("Ana Lima"),
      UserId(Ada) -> Some("Ada"),
      UserId(Ben) -> Some("Ben")
    )
    val voucher = new InMemoryVoucher(Set(Ours), Set.empty, Declared)
    val admin = new InMemoryAdministration(Declared, voucher)
    private val down: Administration = new Administration {
      def run(by: Account, room: Place, command: Command, at: Instant) =
        Left(StoreError.DatabaseError("gone"))
    }
    private val inbox: InMemoryInbox = InMemoryInbox.fresh(Budget(ZoneOffset.UTC, None))

    /** What the edge said, in order. */
    @caps.unsafe.untrackedCaptures
    var logged = Vector.empty[String]

    val edge: SlackEdge^ =
      new SlackEdge(
        slack,
        Self(TeamId(Team), UserId(Bot)),
        EdgeStores(
          inbox,
          if (failing) down else admin,
          inbox.principals,
          new InMemoryDeliveries,
          new InMemoryAcknowledgements,
          InMemoryReviews.over(inbox),
          FakeJot,
          new InMemoryEdges,
          new Attesting(voucher, FakeJot, _ => ())
        ),
        Set.empty,
        None,
        Stopped,
        s => logged = logged :+ s
      )

    /** What Slack was asked to show, in order: each answer's words. */
    def answers: Vector[String] = slack.responses.map((url, text) => {
      assert(url == ResponseUrl(Hook))
      text
    })
  }

  val tests = Tests {
    test(
      "a change is run now as its attested asker in its channel's room, kept, and answered to them alone"
    ) {
      val w = World()
      w.edge.command(Grit)(command("label confidential trial", user = Ada))
      val change =
        new Change.Relabel(
          Channel,
          Label.Public,
          Change.To.Set(Label.at(Level.Confidential, trial))
        )
      (w.answers, w.admin.kept) ==> (
        Vector(Answer.relabelled(change).text),
        Vector(AdministrationContract.Kept(of(Ada), Now, change))
      )
    }

    test("a mention in its words names that user's account in the command's team") {
      val w = World()
      w.edge.command(Grit)(command(s"clear <@$Ben|ben> for trial", user = Ada))
      w.admin.kept.map(_.change) ==> Vector(Change.Clear(of(Ben), trial))
    }

    test("asked in a direct message, a command runs in its asker's own direct room") {
      val w = World()
      w.edge.command(Grit)(command("quiet", channel = AnasDm))
      w.answers ==> Vector(Answer.Refused(Refusal.InDirectMessage).text)
    }

    test("words read wrong are answered with why and the commands, and nothing is run") {
      val w = World()
      w.edge.command(Grit)(command("frobnicate", user = Ada))
      (w.answers, w.admin.kept) ==>
        (Vector(s"frobnicate: no such command\n${Command.Usage}"), Vector.empty)
    }

    test("another command is ignored, and said") {
      val w = World()
      w.edge.command(Grit)(command("label", name = "/other"))
      (w.answers, w.logged) ==>
        (Vector.empty, Vector("slack: a slash command not grit's, /other, ignored"))
    }

    test(
      "an asker Slack cannot answer for and never has is told nothing was run, and nothing is kept"
    ) {
      val w = World()
      w.slack.unasked = Some(SlackError.Unreachable("down"))
      w.edge.command(Grit)(command("label confidential trial", user = Ada))
      (w.answers, w.admin.kept) ==> (Vector(SlackEdge.NotAttested), Vector.empty)
    }

    test("a database that failed is answered that nothing was run, and said") {
      val w = World(failing = true)
      w.edge.command(Grit)(command("label", user = Ada))
      (w.answers, w.logged) ==> (
        Vector(SlackEdge.NotRun),
        Vector("slack: /grit not run: DatabaseError(gone)")
      )
    }

    test("an answer Slack will not take is said") {
      val w = World()
      w.slack.expired = Set(ResponseUrl(Hook))
      w.edge.command(Grit)(command("help"))
      w.logged ==> Vector("slack: the answer to /grit not shown: Refused(expired_url)")
    }

    test("a payload grit cannot read is said, and nothing answered") {
      val w = World()
      w.edge.command(Grit)("{}")
      (w.answers, w.logged) ==>
        (
          Vector.empty,
          Vector("slack: a slash command grit cannot read: a slash command without command")
        )
    }
  }
}
