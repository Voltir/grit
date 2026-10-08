package grit.kit.run

import grit.core.id.{AttesterName, TestPrincipalIds}
import grit.core.identity.{Domain, Identities, Realm, TestAccounts, Vouching}
import grit.core.store.{Linking, StoreError}
import grit.core.visibility.Label

import utest.*

/** What a deployment's start does with its identities before anything serves. */
object TrustingTests extends TestSuite {

  private val realm = Realm.of("slack", "T0123").fold(sys.error, identity)
  private val domain = Domain.of("example.com").fold(sys.error, identity)

  private def identities(realms: Vector[Realm], domains: Set[Domain]): Identities =
    Identities
      .of(realms.map(Vouching(AttesterName("slack"), _)), domains)
      .fold(e => sys.error(e.message), identity)

  private val account = TestAccounts.account("slack:T0123/U1")
  private val ended: Vector[Linking] = Vector(
    Linking.Unlinked(account, TestPrincipalIds.stored("p1"), Label.Public, Label.Public),
    Linking.Standing(account, member = false, Label.Public, Label.Public)
  )

  /** What [[Kit.trusted]] said at info and at warn, untrusting with `untrust`, and its result. */
  private def started(
      under: Identities,
      untrust: Either[StoreError, Vector[Linking]] = Right(Vector.empty)
  ): (Vector[String], Vector[String], Either[KitFailure, Unit]) = {
    val infos = Vector.newBuilder[String]
    val warns = Vector.newBuilder[String]
    val result = Kit.trusted(_ => untrust, under, infos += _, warns += _)
    (infos.result(), warns.result(), result)
  }

  val tests = Tests {
    test("realms trusted with no domain claimed warn at every start that nothing links by email") {
      started(identities(Vector(realm), Set.empty))._2 ==> Vector(
        "identity: realms are trusted (slack:T0123/) but no email domain is claimed, so no " +
          "account is linked to another by email; each is its own person, and membership still " +
          "counts. Claim the deployment's domains (GRIT_CLAIMED_DOMAINS in the reference " +
          "deployment) to link them."
      )
    }

    test("a claimed domain, or no realm trusted, warns of nothing") {
      (
        started(identities(Vector(realm), Set(domain)))._2,
        started(identities(Vector.empty, Set.empty))._2
      ) ==> (Vector(), Vector())
    }

    test("each attestation the start ends is logged at info, in its own line") {
      started(identities(Vector(realm), Set(domain)), Right(ended))._1 ==>
        ended.map(l => s"identity: ${l.message}")
    }

    test("a database that fails the start is a store failure, naming what was being done") {
      started(identities(Vector(realm), Set(domain)), Left(StoreError.DatabaseError("down")))._3 ==>
        Left(
          KitFailure.Store(
            "identity: what the deployment no longer trusts was not ended: DatabaseError(down)"
          )
        )
    }
  }
}
