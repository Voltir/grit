package grit.dbos.engine

import scala.util.Using

import grit.core.identity.{Account, Domain, Email, Realm, Standing, TestAccounts}
import grit.core.store.Tx
import grit.core.visibility.{
  Compartments,
  Grant,
  Group,
  Label,
  Level,
  RoomLabels,
  TestLabels,
  Visibility
}
import grit.dbos.sql.{DbConfig, LiveDb, SqlIdentities}

/** What the suites of trusted realms' attestations share: their realms, the claimed domain, the
  * visibility they clear people under, and reads of the rows they arrange.
  */
private[engine] object Vouchings {

  def realm(namespace: String, within: String): Realm =
    Realm.of(namespace, within).fold(e => throw new java.lang.AssertionError(e), identity)

  def email(text: String): Email =
    Email.of(text).fold(e => throw new java.lang.AssertionError(e), identity)

  val T1 = realm("slack", "T1")
  val T2 = realm("slack", "T2")
  val T3 = realm("slack", "T3")
  val Wild = realm("slack", "T_")
  val R = realm("test", "R")

  val Claimed: Set[Domain] =
    Set(Domain.of("example.com").fold(e => throw new java.lang.AssertionError(e), identity))

  val Internal = Label.at(Level.Internal)
  val Confidential = Label.at(Level.Confidential)

  /** Two accounts of realm T2 that a group names. */
  val across = TestAccounts.account("slack:T2/U-across")
  val named = TestAccounts.account("slack:T2/U-m1-b")

  /** Every full member of realm T1 is staff, cleared Internal; the accounts of T2 that `named`
    * lists are cleared Confidential. Every room public.
    */
  val Seen: Visibility =
    (for {
      compartments <- Compartments.of(Vector.empty).left.map(_.toString)
      v <- Visibility
        .of(
          compartments,
          RoomLabels.Public,
          Vector(
            Group(TestLabels.group("staff"), Set.empty, Set(T1)),
            Group(TestLabels.group("named"), Set(across, named))
          ),
          Vector(
            Grant(TestLabels.group("staff"), Internal),
            Grant(TestLabels.group("named"), Confidential)
          )
        )
        .left
        .map(_.toString)
    } yield v).fold(e => throw new java.lang.AssertionError(e), identity)

  def full(address: String): Standing = Standing.Full(Some(email(address)))

  /** Each row `sql` reads with `params`, as its columns' text. */
  def rows(sql: String, params: String*)(using in: DbConfig): Vector[Vector[String]] =
    LiveDb.transaction(in) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(conn.prepareStatement(sql)) { ps =>
        params.zipWithIndex.foreach((p, i) => ps.setString(i + 1, p))
        Using.resource(ps.executeQuery()) { rs =>
          val width = rs.getMetaData.getColumnCount
          val out = Vector.newBuilder[Vector[String]]
          while (rs.next()) out += (1 to width).map(rs.getString).toVector
          out.result()
        }
      }
    }

  /** `account`'s attestation: its email (empty for none) and membership. */
  def attestation(account: Account)(using DbConfig): Vector[Vector[String]] =
    rows(
      "SELECT coalesce(email, ''), member::text FROM grit.attestations WHERE account = ?",
      Account.written(account)
    )

  /** `account` seen, as a first message through it would make it. */
  def enrolled(account: Account)(using in: DbConfig): Unit =
    LiveDb
      .transaction(in)(SqlIdentities.enroll(Set(account)))
      .fold(e => throw new java.lang.AssertionError(s"enrolling: $e"), identity)

  /** Arranges `account`'s attestation as answered `ago` (an interval) before now. */
  def aged(account: Account, ago: String)(using DbConfig): Unit = {
    val _ = rows(
      """UPDATE grit.attestations SET seen_at = now() - ?::interval WHERE account = ?
        |RETURNING account""".stripMargin,
      ago,
      Account.written(account)
    )
  }

  /** How many people `in` holds. */
  def people(in: DbConfig): Long =
    rows("SELECT count(*) FROM grit.principals WHERE kind = 'person'")(using in).flatten.headOption
      .fold(0L)(_.toLong)

}
