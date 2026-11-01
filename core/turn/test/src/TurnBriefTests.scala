package grit.turn

import java.time.Instant

import scala.concurrent.duration.*

import grit.assembly.estimate.CharEstimate
import grit.core.context.{AssemblyError, AssemblyRequest, Shown, Window}
import grit.core.document.{
  DocLabel,
  DocRef,
  DocText,
  DocWeight,
  DocumentTerms,
  InMemoryDocuments,
  Written
}
import grit.core.durable.InMemoryDurable
import grit.core.id.{
  ClaimHolder,
  ConversationId,
  DocKey,
  DocumentVersionId,
  Harness,
  PluginName,
  PrincipalId,
  SessionId
}
import grit.core.identity.Principal
import grit.core.message.{Message, Tokens}
import grit.core.session.{Session, SessionDigest}
import grit.core.store.{Db, InMemoryEntryStore, InMemoryUsageLedger, StoreError}
import grit.core.visibility.{Integrity, Label}
import grit.dbos.sql.TestTx

import utest.*

/** A boundary of a turn whose conversation works as a claim's holder: the brief and the
  * session's digest it shows first, and what it reserves for them from what the assembler
  * draws.
  */
object TurnBriefTests extends TestSuite {
  import TurnFixtures.*

  private def got[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)
  private def ok[A](e: Either[StoreError, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why.toString), identity)

  private val holder = got(ClaimHolder.of("coder"))
  private val projects = got(PluginName.of("projects"))
  private val briefed = Instant.parse("2026-10-31T09:00:00Z")
  private val said = Instant.parse("2026-10-31T10:00:00Z")
  private val answering = Message.User(
    TurnShown.answering(said, Some(TurnOffer.Zoned(None, java.time.ZoneOffset.UTC)), direct = false)
  )

  /** `holder`'s brief, "Keep a brief.", written by the projects plugin at [[briefed]]. */
  private def brief(documents: InMemoryDocuments): DocumentVersionId = {
    given grit.core.store.Tx = TestTx.fake
    val terms = got(DocumentTerms.of(got(DocLabel.of("Projects")), DocWeight.Unscaled, 30.days, 10))
    documents.declare(Vector(projects -> terms))
    ok(
      documents
        .keeper(projects, terms)
        .write(
          got(DocKey.of("grit/c2b#brief")),
          Label.Public,
          Integrity.Authority,
          origin.place,
          got(DocText.of("Keep a brief.")),
          ujson.Obj(),
          None,
          briefed,
          Set(DocRef.brief(holder))
        )
    ) match {
      case Written.Versioned(v, _, _) => v
      case Written.Unchanged(v, _) => v
    }
  }

  /** What the conversation's session did lately: one commit. */
  private val lately: Session.Lately = {
    val id = got(SessionId.of(Harness.Grit, ConversationId.value(conversation)))
    Session.Lately(
      Session(
        id,
        None,
        Principal.Person(PrincipalId.Local, Set.empty),
        Label.Public,
        origin.place,
        briefed,
        None,
        briefed,
        None,
        Session.Counters.zero,
        false,
        None
      ),
      Vector(Session.Lately.Commit(briefed, "1a2b3c4d5e", Some("core: a brief"))),
      0,
      Vector.empty,
      Vector.empty
    )
  }

  /** An assembler whose every window is empty, keeping what each request reserved. */
  private final class Reserving extends Drawing {
    // Appended by the turn, which runs on the test's one thread; read by its assertions after.
    @caps.unsafe.untrackedCaptures
    var reserved: Vector[Tokens] = Vector.empty
    def window(request: AssemblyRequest)(using Db^): Either[AssemblyError, Window] = {
      reserved = reserved :+ request.reserved
      Right(Window(Vector.empty))
    }
  }

  /** The turn rooted on "one", said at [[said]], run as `holding` names its holder over
    * `documents`, its session's reads answering `latest`; what its model was shown, what the
    * assembler was asked to reserve, and its window's record.
    */
  private def run(
      documents: InMemoryDocuments,
      holding: Option[ClaimHolder],
      latest: Option[Session.Lately]
  ) = {
    val entries = new InMemoryEntryStore
    val provider = new RecordingProvider
    val assembler = new Reserving
    val turn = say(entries, "one", at = said)
    new InMemoryDurable().run(turn.workflowId)(
      turnBodyWith(
        entries,
        provider,
        assembler,
        new InMemoryUsageLedger,
        documents = documents,
        sessions = TurnSessions(new RecordedSessions(latest = latest), _ => holding)
      )
    )
    (
      provider.requests.headOption.map(_.messages),
      assembler.reserved,
      windows(entries).map(_._2).map(w => (w.brief, w.digest))
    )
  }

  val tests: Tests = Tests {
    test(
      "a boundary for a holder shows its brief first and its session's digest before its own turns, their cost reserved"
    ) {
      val documents = new InMemoryDocuments
      val version = brief(documents)
      val shownBrief = Message.User("<brief briefed=\"2026-10-31\">\nKeep a brief.\n</brief>")
      val digest =
        SessionDigest.of(lately, SessionDigest.Window.LacksLastSaid, TurnAssembly.DigestChars)
      val (messages, reserved, recorded) = run(documents, Some(holder), Some(lately))
      (messages, reserved, recorded) ==> (
        Some(Vector(shownBrief, Shown.sessionLog(digest), answering, Message.User("one"))),
        Vector(CharEstimate.message(shownBrief) + CharEstimate.message(Shown.sessionLog(digest))),
        Vector((Some(version), Some(digest)))
      )
      assert(digest.contains("1a2b3c4 core: a brief"))
    }

    test(
      "a conversation working as no holder, its session unread, shows neither and reserves nothing"
    ) {
      val documents = new InMemoryDocuments
      brief(documents)
      run(documents, None, None) ==> (
        Some(Vector(answering, Message.User("one"))),
        Vector(Tokens.Zero),
        Vector((None, None))
      )
    }

    test("a holder with no brief shows only its session's digest") {
      val documents = new InMemoryDocuments
      brief(documents)
      val other = got(ClaimHolder.of("reviewer"))
      val digest =
        SessionDigest.of(lately, SessionDigest.Window.LacksLastSaid, TurnAssembly.DigestChars)
      val (messages, _, recorded) = run(documents, Some(other), Some(lately))
      (messages, recorded) ==> (
        Some(Vector(Shown.sessionLog(digest), answering, Message.User("one"))),
        Vector((None, Some(digest)))
      )
    }
  }
}
