package grit.dbos.engine

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.claim.ClaimRole
import grit.core.document.{
  BriefClaim,
  DocLabel,
  DocRef,
  DocText,
  DocWeight,
  DocumentKeeper,
  DocumentTerms
}
import grit.core.edge.{EdgeContracts, EdgeSessions, EdgeSessionsContract}
import grit.core.either.Got.got
import grit.core.id.{ClaimHolder, DocKey, PluginName, SessionId}
import grit.core.identity.Principal
import grit.core.store.StoreError
import grit.core.visibility.{Clearance, Integrity, Label, Visibility}
import grit.dbos.sql.{
  LiveDb,
  Opener,
  SqlClaims,
  SqlDocuments,
  SqlEdgeSessions,
  SqlIdentities,
  SqlJot,
  SqlTombstones,
  TestPostgres
}

/** The sessions contract, kept by the SQL sessions against a real Postgres, on a database whose
  * two readers are enrolled once, each as a person of their own.
  */
object SqlEdgeSessionsContractTests extends EdgeSessionsContract {

  import EdgeContracts.*

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_edge_sessions_contract")
    LiveEngine.open(c, "test").close()
    LiveDb
      .transaction(c)(SqlIdentities.enroll(Set(Reader, Other)))
      .fold(e => throw new java.lang.AssertionError(e.toString), identity)
    c
  }

  private val documents = new SqlDocuments(new SqlTombstones)

  /** The one plugin keeping briefs here, declared enabled on first use. */
  private lazy val briefs: DocumentKeeper = {
    val terms = got(DocumentTerms.of(got(DocLabel.of("briefs")), DocWeight.Unscaled, 30.days, 100))
    LiveDb
      .transaction(config)(documents.declare(Vector(BriefPlugin -> terms)))
      .fold(e => throw new java.lang.AssertionError(e.toString), identity)
    documents.keeper(BriefPlugin, terms)
  }

  private val BriefPlugin = got(PluginName.of("contract-briefs"))

  protected def claimedBrief(
      holder: ClaimHolder,
      text: String,
      written: Instant,
      subject: DocKey,
      session: SessionId
  ): Unit = {
    val _ = LiveDb
      .transaction(config, Clearance.of(Label.Public))(
        for {
          _ <- briefs.write(
            got(DocKey.of(s"contract/${ClaimHolder.value(holder)}")),
            Label.Public,
            Integrity.Member,
            EdgeSessionsContract.Worktree,
            got(DocText.of(text)),
            BriefClaim.kept(ujson.Obj(), BriefClaim(subject, Some(EdgeSessionsContract.Moving))),
            None,
            written,
            Set(DocRef.brief(holder))
          )
          by <- SqlIdentities.person(Reader)
          person <- by
            .collect { case p: Principal.Person => p }
            .toRight(StoreError.Invalid("the reader is no person"))
          made <- new SqlClaims(BriefPlugin).claim(
            subject,
            holder,
            got(ClaimRole.of("builder")),
            Some(session),
            person,
            Label.Public,
            written
          )
        } yield made
      )
      .fold(e => throw new java.lang.AssertionError(e.toString), identity)
  }

  protected def recording[A](body: EdgeSessions^ => A): A = {
    val opener = new Opener(Visibility.Shipped)
    val ds = LiveDb.dataSource(config)
    body(new SqlEdgeSessions(new SqlJot(ds, opener), ds, opener))
  }

  protected def brief(holder: ClaimHolder, text: String, written: Instant): Unit = {
    val _ = LiveDb
      .transaction(config, Clearance.of(Label.Public))(
        briefs.write(
          got(DocKey.of(s"contract/${ClaimHolder.value(holder)}")),
          Label.Public,
          Integrity.Member,
          EdgeSessionsContract.Worktree,
          got(DocText.of(text)),
          ujson.Obj(),
          None,
          written,
          Set(DocRef.brief(holder))
        )
      )
      .fold(e => throw new java.lang.AssertionError(e.toString), identity)
  }
}
