package grit.core.document

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.claim.{ClaimRole, Claimed, Claims, InMemoryClaims, Moved}
import grit.core.either.Got.got
import grit.core.id.{ClaimHolder, DocKey, PluginName, SessionId}
import grit.core.identity.{Principal, TestAccounts}
import grit.core.place.{Namespace, Place}
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{Clearance, Integrity, Label, TestLabels}
import grit.dbos.sql.TestTx

import utest.*

/** A keeper fenced by [[DocumentKeeper.workedBy]], over the in-memory documents and claims. */
object WorkedByTests extends TestSuite {

  private val plugin: PluginName = got(PluginName.of("fenced"))

  private val at: Instant = Instant.parse("2026-03-01T00:00:00Z")

  private val terms: DocumentTerms = got(
    DocumentTerms.of(
      got(DocLabel.of("notes")),
      DocWeight.Unscaled,
      30.days,
      100,
      0,
      DocumentTerms.Full.Withdraw
    )
  )

  private val builder: ClaimRole = got(ClaimRole.of("builder"))

  private lazy val ann: Principal.Person =
    TestAccounts.principal(TestAccounts.account("slack:T1/U-ann")) match {
      case p: Principal.Person => p
      case Principal.Grit => throw new java.lang.AssertionError("grit is no person")
    }

  private val Low: Clearance = Clearance.of(Label.Public)

  private val High: Clearance = Clearance.of(TestLabels.Trial)

  private def key(s: String): DocKey = got(DocKey.of(s))

  private def holder(s: String): ClaimHolder = got(ClaimHolder.of(s))

  private def session(s: String): SessionId = got(SessionId.read(s"claude-code:$s"))

  private def ok[A](e: Either[StoreError, A]): A =
    e.fold(err => throw new java.lang.AssertionError(s"$err"), identity)

  private def opened[A](body: (Tx^) ?=> A): A = openedAt(Low)(body)

  private def openedAt[A](clearance: Clearance)(body: (Tx^) ?=> A): A =
    body(using TestTx.fake(clearance))

  /** A fresh store: its documents, the plugin's unfenced keeper, and its claims. */
  private def fresh(): (InMemoryDocuments, DocumentKeeper, Claims) = {
    val documents = new InMemoryDocuments
    (documents, documents.keeper(plugin, terms), new InMemoryClaims().of(plugin, _ => None))
  }

  private def claimed(
      claims: Claims,
      subject: String,
      by: String,
      working: Option[SessionId],
      label: Label = Label.Public,
      when: Instant = at,
      clearance: Clearance = Low
  ): Unit =
    openedAt(clearance)(
      ok(claims.claim(key(subject), holder(by), builder, working, ann, label, when))
    ) match {
      case Claimed.Made(_) => ()
      case other => throw new java.lang.AssertionError(s"$subject: $other")
    }

  private def write(
      k: DocumentKeeper,
      subject: String,
      words: String,
      when: Instant = at,
      clearance: Clearance = Low
  ) =
    openedAt(clearance)(
      k.write(
        key(subject),
        Label.Public,
        Integrity.Member,
        Place.under(Namespace.Task, Vector("fenced")),
        got(DocText.of(words)),
        ujson.Obj(),
        None,
        when
      ).map(_ => ())
    )

  private def withdraw(k: DocumentKeeper, subject: String) =
    opened(k.withdraw(key(subject), Label.Public, at.plusSeconds(60)).map(_.isDefined))

  private def text(documents: InMemoryDocuments, subject: String): Option[String] =
    opened(ok(documents.shelf(plugin).current(key(subject), Label.Public)))
      .map(d => DocText.value(d.text))

  val tests: Tests = Tests {
    test("another session's claim refuses a write and a withdraw of its key, writing nothing") {
      val (documents, keeper, claims) = fresh()
      ok(write(keeper, "p/a", "before"))
      claimed(claims, "p/a", "h1", Some(session("a")))
      val fenced = DocumentKeeper.workedBy(keeper, claims, session("b"))
      assert(
        write(fenced, "p/a", "after") == Left(StoreError.Worked(key("p/a"), session("a"))),
        withdraw(fenced, "p/a") == Left(StoreError.Worked(key("p/a"), session("a"))),
        text(documents, "p/a") == Some("before")
      )
    }

    test("the claim's own session writes and withdraws its key") {
      val (documents, keeper, claims) = fresh()
      claimed(claims, "p/a", "h1", Some(session("a")))
      val fenced = DocumentKeeper.workedBy(keeper, claims, session("a"))
      val wrote = write(fenced, "p/a", "mine")
      val kept = text(documents, "p/a")
      assert(wrote == Right(()), kept == Some("mine"), withdraw(fenced, "p/a") == Right(true))
    }

    test("a key with no live claim, or one no session works, is written by any session") {
      val (documents, keeper, claims) = fresh()
      claimed(
        claims,
        "p/lapsed",
        "h1",
        Some(session("a")),
        when = at.minusMillis(Claims.Lease.toMillis)
      )
      claimed(claims, "p/free", "h2", None)
      val fenced = DocumentKeeper.workedBy(keeper, claims, session("b"))
      assert(
        write(fenced, "p/none", "n") == Right(()),
        write(fenced, "p/lapsed", "l") == Right(()),
        write(fenced, "p/free", "f") == Right(()),
        Vector("p/none", "p/lapsed", "p/free").map(text(documents, _)) ==
          Vector(Some("n"), Some("l"), Some("f"))
      )
    }

    test("another key than the claimed one, such as its holder's brief, passes") {
      val (documents, keeper, claims) = fresh()
      claimed(claims, "p/a", "h1", Some(session("a")))
      val fenced = DocumentKeeper.workedBy(keeper, claims, session("b"))
      assert(write(fenced, "brief/h1", "the brief") == Right(()))
      text(documents, "brief/h1") ==> Some("the brief")
    }

    test("a session whose claim was moved from it is refused, and the one it was moved to writes") {
      val (documents, keeper, claims) = fresh()
      claimed(claims, "p/a", "h1", Some(session("a")))
      val moved = opened {
        val live = ok(claims.live(at.plusSeconds(1)))
        live.map(c => ok(claims.moved(c, holder("h2"), session("b"), builder, at.plusSeconds(1))))
      }
      assert(moved.collect { case Moved.Made(_) => () } == Vector(()))
      val old = DocumentKeeper.workedBy(keeper, claims, session("a"))
      val taker = DocumentKeeper.workedBy(keeper, claims, session("b"))
      assert(
        write(old, "p/a", "stale", at.plusSeconds(2)) ==
          Left(StoreError.Worked(key("p/a"), session("b"))),
        write(taker, "p/a", "fresh", at.plusSeconds(2)) == Right(()),
        text(documents, "p/a") == Some("fresh")
      )
    }

    test("claims at two labels refuse naming the older claim's session") {
      val (_, keeper, claims) = fresh()
      // The older claim is at the higher label and named by the later session, so that neither
      // a label's order nor a session's is the order asked for.
      claimed(claims, "p/a", "h1", Some(session("z")), TestLabels.Trial, clearance = High)
      claimed(claims, "p/a", "h2", Some(session("y")), when = at.plusSeconds(60))
      val fenced = DocumentKeeper.workedBy(keeper, claims, session("b"))
      write(fenced, "p/a", "x", at.plusSeconds(120), High) ==>
        Left(StoreError.Worked(key("p/a"), session("z")))
    }
  }
}
