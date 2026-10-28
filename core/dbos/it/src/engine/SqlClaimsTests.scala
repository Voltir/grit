package grit.dbos.engine

import grit.core.claim.{Claims, ClaimsContract}
import grit.core.id.PluginName
import grit.core.identity.{Account, Principal}
import grit.core.store.Tx
import grit.core.visibility.Clearance
import grit.dbos.sql.{LiveDb, SqlClaims, SqlIdentities, TestPostgres}

/** The claims contract, kept by the SQL store against a real Postgres. */
object SqlClaimsTests extends ClaimsContract {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_claims")
    LiveEngine.open(c, "test").close()
    c
  }

  protected def claims(plugin: PluginName): Claims = new SqlClaims(plugin)

  protected def person(account: Account): Principal.Person =
    LiveDb
      .transaction(config)(
        SqlIdentities.enroll(Set(account)).flatMap(_ => SqlIdentities.person(account))
      )
      .fold(e => throw new java.lang.AssertionError(e.toString), identity) match {
      case Some(p: Principal.Person) => p
      case other => throw new java.lang.AssertionError(s"$account is no person: $other")
    }

  protected def opened[A](clearance: Clearance)(body: (Tx^) ?=> A): A =
    LiveDb.transaction(config, clearance)(body)
}
