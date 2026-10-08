package grit.dbos.engine

import grit.core.id.{AttesterName, PrincipalIds}
import grit.core.identity.{
  Account,
  Domain,
  Identities,
  Realm,
  Standing,
  TestAccounts,
  Vouched,
  Vouching
}
import grit.core.store.{Linking, StoreError}
import grit.core.visibility.Label
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}

import utest.*

/** What a deployment no longer trusts, ended at its start by `Engine.untrust`, against a real
  * Postgres.
  */
object UntrustLiveTests extends TestSuite {
  import Vouchings.*

  /** A fresh database an engine has started on under [[Vouchings.Seen]]. */
  private def fresh(suite: String): DbConfig = {
    val c = TestPostgres.freshDatabase(suite)
    LiveEngine.open(c, "test", visibility = Seen).close()
    c
  }

  /** `account` vouched `standing` by a voucher of every realm the suites name, claiming
    * example.com.
    */
  private def attested(account: Account, standing: Standing)(using in: DbConfig): Unit = {
    val engine = LiveEngine.open(in, "test", visibility = Seen)
    val voucher =
      try engine.voucher(Set(T1, T2, T3, Wild, R), Claimed)
      finally engine.close()
    LiveDb
      .transaction(in)(voucher.vouch(Vouched(account, standing)))
      .fold(e => throw new java.lang.AssertionError(s"vouching: $e"), _ => ())
  }

  /** Identities trusting the attester `test` for `realms`, claiming `domains`. */
  private def trusting(realms: Set[Realm], domains: Set[Domain]): Identities =
    Identities
      .of(realms.toVector.map(Vouching(AttesterName("test"), _)), domains)
      .fold(e => throw new java.lang.AssertionError(e.message), identity)

  /** What an engine started on `in` under [[Vouchings.Seen]] ends under `identities`. */
  private def untrusted(
      identities: Identities
  )(using in: DbConfig): Either[StoreError, Vector[Linking]] = {
    val engine = LiveEngine.open(in, "test", visibility = Seen)
    try engine.untrust(identities)
    finally engine.close()
  }

  /** Every attestation: account, email (empty for none), membership, when, and the transaction
    * that wrote it, in the accounts' order.
    */
  private def attestations()(using DbConfig): Vector[Vector[String]] =
    rows(
      """SELECT account, coalesce(email, ''), member::text, seen_at::text, xmin::text
        |  FROM grit.attestations ORDER BY account""".stripMargin
    )

  private def personOf(address: String)(using DbConfig) =
    rows("SELECT principal_id FROM grit.emails WHERE email = ?", address).flatten.headOption
      .fold(throw new java.lang.AssertionError(s"no person for $address"))(PrincipalIds.stored)

  val tests = Tests {
    test(
      "identities that no longer trust a realm end its attestations, email and membership, keeping when each was said, and leave another realm's"
    ) {
      given DbConfig = fresh("untrust_realm")
      val a = TestAccounts.account("slack:T1/U-dropped")
      val b = TestAccounts.account("slack:T1/U-dropped-b")
      val c = TestAccounts.account("slack:T2/U-kept")
      attested(a, full("dropped@example.com"))
      attested(b, Standing.Full(None))
      attested(c, full("kept@example.com"))
      val pA = personOf("dropped@example.com")
      val before = attestations()
      val ended = untrusted(trusting(Set(T2), Claimed))
      val after = attestations()
      (
        ended,
        after.map(_.take(4)),
        before.map(_(3)),
        after.map(_(4)).zip(before.map(_(4))).map(_ == _)
      ) ==> (
        Right(
          Vector(
            Linking.Unlinked(a, pA, Internal, Label.Public),
            Linking.Standing(a, member = false, Internal, Label.Public),
            Linking.Standing(b, member = false, Internal, Label.Public)
          )
        ),
        Vector(
          Vector(Account.written(a), "", "false", before(0)(3)),
          Vector(Account.written(b), "", "false", before(1)(3)),
          Vector(Account.written(c), "kept@example.com", "true", before(2)(3))
        ),
        after.map(_(3)),
        Vector(false, false, true)
      )
    }

    test(
      "identities that no longer claim a domain end the attestations holding its addresses, and their membership"
    ) {
      given DbConfig = fresh("untrust_domain")
      val d = TestAccounts.account("slack:T3/U-unclaimed-now")
      val e = TestAccounts.account("slack:T3/U-no-email")
      attested(d, full("d@example.com"))
      attested(e, Standing.Full(None))
      val pD = personOf("d@example.com")
      val elsewhere = Domain.of("example.org").fold(sys.error, identity)
      val ended = untrusted(trusting(Set(T1, T2, T3, Wild, R), Set(elsewhere)))
      (ended, attestations().map(_.take(3))) ==> (
        Right(
          Vector(
            Linking.Unlinked(d, pD, Label.Public, Label.Public),
            Linking.Standing(d, member = false, Label.Public, Label.Public)
          )
        ),
        Vector(
          Vector(Account.written(e), "", "true"),
          Vector(Account.written(d), "", "false")
        )
      )
    }

    test("a second start under the same identities ends nothing and writes no row") {
      given DbConfig = fresh("untrust_again")
      val a = TestAccounts.account("slack:T1/U-again")
      val c = TestAccounts.account("slack:T2/U-again-kept")
      attested(a, full("again@example.com"))
      attested(c, Standing.Full(None))
      val identities = trusting(Set(T2), Claimed)
      val first = untrusted(identities).map(_.size)
      val once = attestations()
      (first, untrusted(identities), attestations()) ==> (Right(2), Right(Vector()), once)
    }

    test("an engine's open changes no attestation") {
      given DbConfig = fresh("untrust_open")
      val a = TestAccounts.account("slack:T9/U-untrusted-anywhere")
      val b = TestAccounts.account("slack:T1/U-opened")
      // T9 is no realm the voucher holds: arrange its attestation as an older deployment left it.
      LiveDb
        .transaction(summon[DbConfig])(grit.dbos.sql.SqlIdentities.enroll(Set(a)))
        .fold(e => sys.error(s"$e"), identity)
      val _ = rows(
        """INSERT INTO grit.attestations (account, email, member, seen_at)
          |VALUES (?, NULL, true, now()) RETURNING account""".stripMargin,
        Account.written(a)
      )
      attested(b, full("opened@example.com"))
      val before = attestations()
      LiveEngine.open(summon[DbConfig], "test", visibility = Seen).close()
      attestations() ==> before
    }
  }
}
