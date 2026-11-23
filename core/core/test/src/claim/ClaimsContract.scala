package grit.core.claim

import java.time.Instant

import grit.core.id.{ClaimHolder, DocKey, PluginName, SessionId}
import grit.core.identity.{Account, Principal, TestAccounts}
import grit.core.place.{Namespace, Place}
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{Clearance, Label, TestLabels}

import utest.*

/** The contract every plugin's [[Claims]] keeps, run against the in-memory fake in core and the
  * SQL store in grit.dbos. Tests share the store, so each names its own plugin.
  */
abstract class ClaimsContract extends TestSuite {

  /** `plugin`'s claims in the store under test, each holder in `worked` having worked a session
    * whose last event is at its time, and each session in `sessions` recorded as it says, all kept
    * where every transaction here reads them.
    */
  protected def claims(
      plugin: PluginName,
      worked: Map[ClaimHolder, Instant],
      sessions: Map[SessionId, ClaimSessions.Working] = Map.empty
  ): Claims

  private def claims(plugin: PluginName): Claims = claims(plugin, Map.empty)

  /** A session begun by `by`, working as `as`, its last event at `last`. */
  private def working(
      as: String,
      last: Instant,
      parent: Option[String] = None,
      ended: Boolean = false,
      by: Principal.Person = ann
  ): ClaimSessions.Working =
    ClaimSessions.Working(Some(holder(as)), last, parent.map(session), ended, by)

  /** The person `account` is linked to, seen by the store under test. */
  protected def person(account: Account): Principal.Person

  /** Runs `body` in one transaction opened at `clearance`, committed when it returns. */
  protected def opened[A](clearance: Clearance)(body: (Tx^) ?=> A): A

  private val Low: Clearance = Clearance.of(Label.Public)

  private val High: Clearance = Clearance.of(TestLabels.Trial)

  private def ok[A](e: Either[String | StoreError, A]): A =
    e.fold(err => throw new java.lang.AssertionError(s"$err"), identity)

  private def plugin(s: String): PluginName = ok(PluginName.of(s))

  private def key(s: String): DocKey = ok(DocKey.of(s))

  private def holder(s: String): ClaimHolder = ok(ClaimHolder.of(s))

  private def session(s: String): SessionId = ok(SessionId.read(s"claude-code:$s"))

  private val builder: ClaimRole = ok(ClaimRole.of("builder"))

  private val at: Instant = Instant.parse("2026-03-01T00:00:00Z")

  /** When a claim made at `at` that no session works lapses. */
  private val lapse: Instant = at.plusMillis(Claims.Lease.toMillis)

  private val hour: Instant = at.plusSeconds(3600)

  private lazy val ann: Principal.Person = person(TestAccounts.account("slack:T1/U-ann"))

  private lazy val bob: Principal.Person = person(TestAccounts.account("slack:T1/U-bob"))

  private def claim(
      c: Claims,
      subject: String,
      by: String,
      label: Label = Label.Public,
      clearance: Clearance = Low,
      when: Instant = at,
      role: ClaimRole = builder,
      working: Option[SessionId] = None
  ): Claimed =
    opened(clearance)(ok(c.claim(key(subject), holder(by), role, working, ann, label, when)))

  private def release(
      c: Claims,
      subject: String,
      label: Label = Label.Public,
      clearance: Clearance = Low
  ): Option[Claim] =
    opened(clearance)(ok(c.release(key(subject), label)))

  private def live(c: Claims, when: Instant): Vector[Claim] =
    opened(Low)(ok(c.live(when)))

  private def made(
      subject: String,
      by: String,
      label: Label = Label.Public,
      when: Instant = at,
      active: Option[Instant] = None,
      working: Option[SessionId] = None
  ): Claim =
    Claim(key(subject), holder(by), builder, ann, when, label, working, active.getOrElse(when))

  val tests = Tests {
    test("a claim made is the claim given, kept at its label") {
      val c = claims(plugin("claims-made"))
      assert(claim(c, "p/a", "s1") == Claimed.Made(made("p/a", "s1")))
    }

    test("a second claim on a subject is refused naming the first") {
      val c = claims(plugin("claims-taken"))
      claim(c, "p/a", "s1")
      assert(claim(c, "p/a", "s2") == Claimed.Taken(Some(made("p/a", "s1"))))
    }

    test("a holder holds one claim at a label, and is refused a second naming it") {
      val c = claims(plugin("claims-holding"))
      claim(c, "p/a", "s1")
      assert(claim(c, "p/b", "s1") == Claimed.Holding(Some(made("p/a", "s1"))))
    }

    test("a claim conflicting on both its subject and its holder is Taken") {
      val c = claims(plugin("claims-both"))
      claim(c, "p/a", "s1")
      claim(c, "p/b", "s2")
      assert(claim(c, "p/a", "s2") == Claimed.Taken(Some(made("p/a", "s1"))))
    }

    test("a release deletes the claim: its subject and holder are free, and a second finds none") {
      val c = claims(plugin("claims-release"))
      claim(c, "p/a", "s1")
      val freed = release(c, "p/a")
      val again = claim(c, "p/b", "s1")
      val second = release(c, "p/a")
      assert(
        freed == Some(made("p/a", "s1")),
        again == Claimed.Made(made("p/b", "s1")),
        second == None
      )
    }

    test("claims at two labels are apart: neither refuses nor releases the other") {
      val c = claims(plugin("claims-labels"))
      claim(c, "p/a", "s1", TestLabels.Trial, High)
      val low = claim(c, "p/a", "s1")
      val freedLow = release(c, "p/a", Label.Public, High)
      val freedHigh = release(c, "p/a", TestLabels.Trial, High)
      assert(
        low == Claimed.Made(made("p/a", "s1")),
        freedLow == Some(made("p/a", "s1")),
        freedHigh == Some(made("p/a", "s1", TestLabels.Trial))
      )
    }

    test("a claim this transaction does not read refuses without naming it, and is not released") {
      val c = claims(plugin("claims-unread"))
      claim(c, "p/a", "s1", TestLabels.Trial, High)
      val refused = claim(c, "p/a", "s2", TestLabels.Trial)
      val held = claim(c, "p/b", "s1", TestLabels.Trial)
      val unreleased = release(c, "p/a", TestLabels.Trial)
      val released = release(c, "p/a", TestLabels.Trial, High)
      assert(
        refused == Claimed.Taken(None),
        held == Claimed.Holding(None),
        unreleased == None,
        released == Some(made("p/a", "s1", TestLabels.Trial))
      )
    }

    test("a claim is kept at its label joined with the transaction's floor") {
      val c = claims(plugin("claims-floor"))
      val room = Clearance.inRoom(
        Place.under(Namespace.Slack, Vector("T", "CFLOOR")),
        TestLabels.Trial,
        TestLabels.Trial
      )
      val kept = claim(c, "p/a", "s1", Label.Public, room)
      assert(kept == Claimed.Made(made("p/a", "s1", TestLabels.Trial)))
    }

    test(
      "a claim no session works lapses its lease after it is made, and another holder takes it"
    ) {
      val c = claims(plugin("claims-lapse"))
      claim(c, "p/a", "lapse-1")
      val early = claim(c, "p/a", "lapse-2", when = lapse.minusMillis(1))
      val taken = claim(c, "p/a", "lapse-2", when = lapse)
      assert(
        early == Claimed.Taken(Some(made("p/a", "lapse-1"))),
        taken == Claimed.Made(made("p/a", "lapse-2", when = lapse))
      )
    }

    test("a session working as its holder keeps a claim live for a lease after its last event") {
      val c = claims(plugin("claims-worked"), Map(holder("worked-1") -> hour))
      claim(c, "p/a", "worked-1")
      val kept = claim(c, "p/a", "worked-2", when = lapse)
      val after = hour.plusMillis(Claims.Lease.toMillis)
      val taken = claim(c, "p/a", "worked-2", when = after)
      assert(
        kept == Claimed.Taken(Some(made("p/a", "worked-1", active = Some(hour)))),
        taken == Claimed.Made(made("p/a", "worked-2", when = after))
      )
    }

    test("a re-claim by its holder renews it: one claim, its role the new one, first made kept") {
      val c = claims(plugin("claims-renew"), Map(holder("renew-1") -> hour))
      val reviewer = ok(ClaimRole.of("reviewer"))
      claim(c, "p/a", "renew-1")
      val renewed = claim(c, "p/a", "renew-1", when = hour, role = reviewer)
      val one = made("p/a", "renew-1", active = Some(hour)).copy(role = reviewer)
      assert(renewed == Claimed.Made(one), live(c, hour) == Vector(one))
    }

    test("held answers a live claim as it is, and leaves it as it was") {
      val c = claims(plugin("claims-held"))
      claim(c, "p/a", "held-1")
      val read = live(c, at)
      val held = read.map(r => opened(Low)(ok(c.held(r))))
      assert(held == Vector(Some(made("p/a", "held-1"))), live(c, at) == read)
    }

    test("held answers none for a claim released since") {
      val c = claims(plugin("claims-held-gone"))
      claim(c, "p/a", "gone-1")
      val read = live(c, at)
      release(c, "p/a")
      assert(read.map(r => opened(Low)(ok(c.held(r)))) == Vector(None))
    }

    test("held answers none for a claim released and made again since, and the new one") {
      val c = claims(plugin("claims-held-again"))
      claim(c, "p/a", "again-1")
      val first = live(c, at)
      release(c, "p/a")
      claim(c, "p/a", "again-1", when = hour)
      val second = live(c, hour)
      assert(
        first.map(r => opened(Low)(ok(c.held(r)))) == Vector(None),
        second.map(r => opened(Low)(ok(c.held(r)))) == Vector(
          Some(made("p/a", "again-1", when = hour))
        )
      )
    }

    test("held answers a claim renewed since under the role it has now") {
      val c = claims(plugin("claims-held-role"))
      val reviewer = ok(ClaimRole.of("reviewer"))
      claim(c, "p/a", "role-1")
      val read = live(c, at)
      claim(c, "p/a", "role-1", role = reviewer)
      assert(
        read.map(r => opened(Low)(ok(c.held(r)))) ==
          Vector(Some(made("p/a", "role-1").copy(role = reviewer)))
      )
    }

    test("held answers none for a claim this transaction does not read") {
      val c = claims(plugin("claims-held-unread"))
      claim(c, "p/a", "unread-1", TestLabels.Trial, High)
      val read = opened(High)(ok(c.live(at)))
      assert(read.map(r => opened(Low)(ok(c.held(r)))) == Vector(None))
    }

    test("a claim unreleased at its label frees its holder once lapsed") {
      val c = claims(plugin("claims-relabel"))
      claim(c, "p/a", "relabel-1")
      val missed = release(c, "p/a", TestLabels.Trial, High)
      val holding = claim(c, "p/b", "relabel-1", when = lapse.minusMillis(1))
      val freed = claim(c, "p/b", "relabel-1", when = lapse)
      assert(
        missed == None,
        holding == Claimed.Holding(Some(made("p/a", "relabel-1"))),
        freed == Claimed.Made(made("p/b", "relabel-1", when = lapse))
      )
    }

    test("live lists the claims not lapsed at its time, by subject") {
      val c = claims(plugin("claims-live"))
      claim(c, "p/b", "live-1")
      claim(c, "p/a", "live-2", when = hour)
      assert(
        live(c, hour) == Vector(made("p/a", "live-2", when = hour), made("p/b", "live-1")),
        live(c, lapse) == Vector(made("p/a", "live-2", when = hour))
      )
    }

    test("on answers its subject's live claim at each label it reads, oldest first") {
      val c = claims(plugin("claims-on"))
      // The older claim is at the higher label and by the later holder, so that neither a
      // label's order nor a holder's is the order asked for.
      claim(c, "p/a", "on-2", TestLabels.Trial, High)
      claim(c, "p/a", "on-1", when = hour)
      claim(c, "p/b", "on-3")
      def on(clearance: Clearance, when: Instant) = opened(clearance)(ok(c.on(key("p/a"), when)))
      val older = made("p/a", "on-2", TestLabels.Trial)
      val newer = made("p/a", "on-1", when = hour)
      assert(
        on(High, hour) == Vector(older, newer),
        on(Low, hour) == Vector(newer),
        on(High, lapse) == Vector(newer),
        opened(High)(ok(c.on(key("p/c"), hour))) == Vector.empty
      )
    }

    test("a lock keeps no claim: its subject is claimed after it, in the same transaction") {
      val c = claims(plugin("claims-lock"))
      val claimed = opened(Low) {
        ok(c.lock(key("p#lock")))
        ok(c.claim(key("p#lock"), holder("lock-1"), builder, None, ann, Label.Public, at))
      }
      (claimed, live(c, at)) ==> (
        Claimed.Made(made("p#lock", "lock-1")),
        Vector(
          made("p#lock", "lock-1")
        )
      )
    }

    test(
      "a claim's session keeps it live a lease after its own last event, whatever another session of its holder does"
    ) {
      val c = claims(
        plugin("claims-own-lease"),
        Map(holder("own-1") -> hour),
        Map(session("own-a") -> working("own-1", at))
      )
      claim(c, "p/a", "own-1", working = Some(session("own-a")))
      val taken = claim(c, "p/a", "own-2", when = lapse)
      assert(taken == Claimed.Made(made("p/a", "own-2", when = lapse)))
    }

    test("an agent's event keeps the claim its parent session works live a lease after it") {
      val c = claims(
        plugin("claims-agent-renews"),
        Map.empty,
        Map(
          session("parent-a") -> working("agent-1", at),
          session("parent-a/x") -> working("agent-2", hour, parent = Some("parent-a"))
        )
      )
      claim(c, "p/a", "agent-1", working = Some(session("parent-a")))
      val taken = claim(c, "p/a", "agent-3", when = lapse)
      assert(
        taken == Claimed.Taken(
          Some(made("p/a", "agent-1", active = Some(hour), working = Some(session("parent-a"))))
        )
      )
    }

    test("an agent's event keeps its parent session's claim no longer than a lease after it") {
      val c = claims(
        plugin("claims-agent-lapses"),
        Map.empty,
        Map(
          session("lapsing-a") -> working("lapsing-1", at),
          session("lapsing-a/x") -> working("lapsing-2", hour, parent = Some("lapsing-a"))
        )
      )
      claim(c, "p/a", "lapsing-1", working = Some(session("lapsing-a")))
      val after = hour.plusMillis(Claims.Lease.toMillis)
      val taken = claim(c, "p/a", "lapsing-3", when = after)
      assert(taken == Claimed.Made(made("p/a", "lapsing-3", when = after)))
    }

    test("an ended session's agents keep the claim it works from lapsing no longer") {
      val c = claims(
        plugin("claims-agent-ended"),
        Map.empty,
        Map(
          session("ended-a") -> working("ended-1", at, ended = true),
          session("ended-a/x") -> working("ended-2", hour, parent = Some("ended-a"))
        )
      )
      claim(c, "p/a", "ended-1", working = Some(session("ended-a")))
      val taken = claim(c, "p/a", "ended-3", when = lapse)
      assert(taken == Claimed.Made(made("p/a", "ended-3", when = lapse)))
    }

    test("an agent another person began keeps no claim its parent session works from lapsing") {
      val c = claims(
        plugin("claims-agent-stranger"),
        Map.empty,
        Map(
          session("strange-a") -> working("strange-1", at),
          session("strange-a/x") ->
            working("strange-2", hour, parent = Some("strange-a"), by = bob)
        )
      )
      claim(c, "p/a", "strange-1", working = Some(session("strange-a")))
      val taken = claim(c, "p/a", "strange-3", when = lapse)
      assert(taken == Claimed.Made(made("p/a", "strange-3", when = lapse)))
    }

    test("an agent's own agent keeps no claim of the session above its parent from lapsing") {
      val c = claims(
        plugin("claims-agent-nested"),
        Map.empty,
        Map(
          session("nested-a") -> working("nested-1", at),
          session("nested-a/x") -> working("nested-2", at, parent = Some("nested-a")),
          session("nested-a/x/y") -> working("nested-3", hour, parent = Some("nested-a/x"))
        )
      )
      claim(c, "p/a", "nested-1", working = Some(session("nested-a")))
      val taken = claim(c, "p/a", "nested-4", when = lapse)
      assert(taken == Claimed.Made(made("p/a", "nested-4", when = lapse)))
    }

    test("a claim no session works is kept by no agent of its holder's sessions") {
      val c = claims(
        plugin("claims-agent-unworked"),
        Map.empty,
        Map(
          session("unworked-a") -> working("unworked-1", at),
          session("unworked-a/x") -> working("unworked-2", hour, parent = Some("unworked-a"))
        )
      )
      claim(c, "p/a", "unworked-1")
      val taken = claim(c, "p/a", "unworked-3", when = lapse)
      assert(taken == Claimed.Made(made("p/a", "unworked-3", when = lapse)))
    }

    test("a claim made with a session is worked by it, and its lease is that session's") {
      val c = claims(
        plugin("claims-sessioned"),
        Map.empty,
        Map(session("worked-a") -> working("sessioned-1", hour))
      )
      val made1 = claim(c, "p/a", "sessioned-1", working = Some(session("worked-a")))
      assert(
        made1 == Claimed.Made(
          made("p/a", "sessioned-1", active = Some(hour), working = Some(session("worked-a")))
        )
      )
    }

    test("a second session claiming its holder's claim is answered HeldBy, changing nothing") {
      val c = claims(plugin("claims-held-by"))
      claim(c, "p/a", "held-1", working = Some(session("held-a")))
      val second = claim(c, "p/a", "held-1", working = Some(session("held-b")))
      val first = made("p/a", "held-1", working = Some(session("held-a")))
      assert(second == Claimed.HeldBy(first), live(c, at) == Vector(first))
    }

    test("a claim no session works takes the session that claims it again") {
      val c = claims(plugin("claims-taken-by"))
      claim(c, "p/a", "take-1")
      val again = claim(c, "p/a", "take-1", working = Some(session("take-a")))
      val now = made("p/a", "take-1", working = Some(session("take-a")))
      assert(again == Claimed.Made(now), live(c, at) == Vector(now))
    }

    test("a re-claim naming no session keeps the session its claim has") {
      val c = claims(plugin("claims-keeps-session"))
      claim(c, "p/a", "keep-1", working = Some(session("keep-a")))
      val again = claim(c, "p/a", "keep-1")
      assert(
        again == Claimed.Made(made("p/a", "keep-1", working = Some(session("keep-a"))))
      )
    }

    test(
      "a claim moved goes to the new holder's session as its role, made anew, and frees the old holder"
    ) {
      val c = claims(plugin("claims-moved"))
      val reviewer = ok(ClaimRole.of("reviewer"))
      claim(c, "p/a", "move-1", working = Some(session("move-a")))
      val first = made("p/a", "move-1", working = Some(session("move-a")))
      val moved =
        opened(Low)(ok(c.moved(first, holder("move-2"), session("move-b"), reviewer, hour)))
      val now = made("p/a", "move-2", when = hour, working = Some(session("move-b")))
        .copy(role = reviewer)
      assert(
        moved == Claimed.Made(now),
        live(c, hour) == Vector(now),
        claim(c, "p/b", "move-1", when = hour) == Claimed.Made(made("p/b", "move-1", when = hour))
      )
    }

    test("a move to a holder working another subject is Holding, moving nothing") {
      val c = claims(plugin("claims-move-holding"))
      claim(c, "p/a", "mh-1")
      claim(c, "p/b", "mh-2")
      val moved = opened(Low)(
        ok(c.moved(made("p/a", "mh-1"), holder("mh-2"), session("mh-b"), builder, at))
      )
      assert(
        moved == Claimed.Holding(Some(made("p/b", "mh-2"))),
        live(c, at) == Vector(made("p/a", "mh-1"), made("p/b", "mh-2"))
      )
    }

    test("a move of a claim released since is Gone") {
      val c = claims(plugin("claims-move-gone"))
      claim(c, "p/a", "mg-1")
      release(c, "p/a")
      val moved = opened(Low)(
        ok(c.moved(made("p/a", "mg-1"), holder("mg-2"), session("mg-b"), builder, at))
      )
      assert(moved == ClaimMoved.Gone, live(c, at) == Vector.empty)
    }

    test("a move of a claim released since is Gone, though its target holder holds another") {
      val c = claims(plugin("claims-move-gone-holding"))
      claim(c, "p/a", "mgh-1")
      claim(c, "p/b", "mgh-2")
      release(c, "p/a")
      val moved = opened(Low)(
        ok(c.moved(made("p/a", "mgh-1"), holder("mgh-2"), session("mgh-b"), builder, at))
      )
      assert(moved == ClaimMoved.Gone, live(c, at) == Vector(made("p/b", "mgh-2")))
    }

    test(
      "a move of a claim lapsed since is made, to another holder or to its own holder's other session"
    ) {
      val c = claims(plugin("claims-move-lapsed"))
      claim(c, "p/a", "ml-1", working = Some(session("ml-a")))
      claim(c, "p/b", "ml-3", working = Some(session("ml-c")))
      val away = opened(Low)(
        ok(
          c.moved(
            made("p/a", "ml-1", working = Some(session("ml-a"))),
            holder("ml-2"),
            session("ml-b"),
            builder,
            lapse
          )
        )
      )
      val own = opened(Low)(
        ok(
          c.moved(
            made("p/b", "ml-3", working = Some(session("ml-c"))),
            holder("ml-3"),
            session("ml-d"),
            builder,
            lapse
          )
        )
      )
      assert(
        away == Claimed.Made(made("p/a", "ml-2", when = lapse, working = Some(session("ml-b")))),
        own == Claimed.Made(made("p/b", "ml-3", when = lapse, working = Some(session("ml-d"))))
      )
    }

    test("a move to a holder whose other claim has lapsed deletes that claim first, and is made") {
      val c = claims(plugin("claims-move-past"))
      claim(c, "p/b", "mp-2")
      claim(c, "p/a", "mp-1", when = hour)
      val moved = opened(Low)(
        ok(
          c.moved(made("p/a", "mp-1", when = hour), holder("mp-2"), session("mp-b"), builder, lapse)
        )
      )
      val now = made("p/a", "mp-2", when = lapse, working = Some(session("mp-b")))
      assert(moved == Claimed.Made(now), live(c, at) == Vector(now))
    }

    test("a move to a session working as another holder is Elsewhere, moving nothing") {
      val c = claims(
        plugin("claims-move-elsewhere"),
        Map.empty,
        Map(session("me-b") -> working("me-3", at))
      )
      claim(c, "p/a", "me-1")
      val moved = opened(Low)(
        ok(c.moved(made("p/a", "me-1"), holder("me-2"), session("me-b"), builder, at))
      )
      assert(
        moved == Claimed.Elsewhere(session("me-b"), holder("me-3")),
        live(c, at) == Vector(made("p/a", "me-1"))
      )
    }

    test("a session working as another holder is answered Elsewhere, claiming nothing") {
      val c = claims(
        plugin("claims-elsewhere"),
        Map.empty,
        Map(session("away-a") -> working("away-2", at))
      )
      val refused = claim(c, "p/a", "away-1", working = Some(session("away-a")))
      assert(
        refused == Claimed.Elsewhere(session("away-a"), holder("away-2")),
        live(c, at) == Vector.empty
      )
    }
  }
}
