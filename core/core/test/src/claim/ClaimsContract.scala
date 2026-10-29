package grit.core.claim

import java.time.Instant

import grit.core.id.{ClaimHolder, DocKey, PluginName}
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
    * whose last event is at its time, kept where every transaction here reads it.
    */
  protected def claims(plugin: PluginName, worked: Map[ClaimHolder, Instant]): Claims

  private def claims(plugin: PluginName): Claims = claims(plugin, Map.empty)

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

  private val builder: ClaimRole = ok(ClaimRole.of("builder"))

  private val at: Instant = Instant.parse("2026-03-01T00:00:00Z")

  /** When a claim made at `at` that no session works lapses. */
  private val lapse: Instant = at.plusMillis(Claims.Lease.toMillis)

  private val hour: Instant = at.plusSeconds(3600)

  private lazy val ann: Principal.Person = person(TestAccounts.account("slack:T1/U-ann"))

  private def claim(
      c: Claims,
      subject: String,
      by: String,
      label: Label = Label.Public,
      clearance: Clearance = Low,
      when: Instant = at,
      role: ClaimRole = builder
  ): Claimed =
    opened(clearance)(ok(c.claim(key(subject), holder(by), role, ann, label, when)))

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
      active: Option[Instant] = None
  ): Claim =
    Claim(key(subject), holder(by), builder, ann, when, label, active.getOrElse(when))

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

    test("a claim its subject's label left behind frees its holder once lapsed") {
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
  }
}
