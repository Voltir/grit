package grit.turn

import java.time.Instant

import scala.concurrent.duration.*

import grit.assembly.estimate.CharEstimate
import grit.core.context.{
  AssemblyError,
  AssemblyNote,
  AssemblyRequest,
  CarriedWindow,
  Packing,
  Projection,
  Shown,
  Window
}
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
  SessionId,
  TurnSeq
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

  /** [[brief]] as a boundary shows it. */
  private val shownBrief = Message.User("<brief briefed=\"2026-10-31\">\nKeep a brief.\n</brief>")

  /** [[lately]]'s digest as a boundary shows it. */
  private def shownLog =
    Shown.sessionLog(
      SessionDigest.of(lately, SessionDigest.Window.LacksLastSaid, TurnAssembly.DigestChars)
    )

  /** What a boundary showing both reserves for them. */
  private def bothCost = CharEstimate.message(shownBrief) + CharEstimate.message(shownLog)

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

  /** An assembler whose every window is a boundary holding no entry, keeping what each request
    * reserved; a window drawn with nothing reserved notes a recall of turn 99, which no
    * conversation here reaches, so a record tells which draw it kept.
    */
  private class Reserving extends Drawing {
    // Appended by the turn, which runs on the test's one thread; read by its assertions after.
    @caps.unsafe.untrackedCaptures
    var reserved: Vector[Tokens] = Vector.empty
    def window(request: AssemblyRequest)(using Db^): Either[AssemblyError, Window] = {
      reserved = reserved :+ request.reserved
      val unreserved = Option.when(request.reserved == Tokens.Zero)(TurnSeq(99)).toVector
      Right(Window(Vector.empty, Vector(AssemblyNote.Recalled(unreserved))))
    }
  }

  /** As [[Reserving]], but appending to any window carried into its turn. */
  private final class Appending extends Reserving {
    override def assemble(request: AssemblyRequest, packing: Packing)(using
        Db^
    ): Either[AssemblyError, Projection] =
      window(request).map(w =>
        if (request.carried == CarriedWindow.Empty) boundary(w)
        else Projection.Appended(w, Tokens.Zero)
      )
  }

  /** Two turns, rooted on "one" then "two", of a conversation whose holder has a brief and whose
    * session committed lately, drawn by `assembler`: the second turn's request's messages, the
    * ids its session was read for, and each window's notes, brief and digest.
    */
  private def twice(assembler: Reserving) = {
    val documents = new InMemoryDocuments
    brief(documents)
    val entries = new InMemoryEntryStore
    val provider = new RecordingProvider
    val sessions = new RecordedSessions(latest = Right(Some(lately)))
    val durable = new InMemoryDurable()
    Vector(("one", said), ("two", said.plusSeconds(60))).foreach { (text, at) =>
      val turn = say(entries, text, at = at)
      durable.run(turn.workflowId)(
        turnBodyWith(
          entries,
          provider,
          assembler,
          new InMemoryUsageLedger,
          documents = documents,
          sessions = TurnSessions(sessions, _ => Some(holder))
        )
      )
    }
    (
      provider.requests.lastOption.map(_.messages),
      sessions.asked.size,
      windows(entries).map(_._2).map(w => (w.recalled, w.brief.nonEmpty, w.sessionLog.nonEmpty))
    )
  }

  /** The turn rooted on "one", said at [[said]], run as `holding` names its holder over
    * `documents`, its session's reads answering `latest`; what its model was shown, what the
    * assembler was asked to reserve, its window's record, and what that record says the window
    * cost (the assembler's window alone costs nothing).
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
        sessions = TurnSessions(new RecordedSessions(latest = Right(latest)), _ => holding)
      )
    )
    (
      provider.requests.headOption.map(_.messages),
      assembler.reserved,
      windows(entries).map(_._2).map(w => (w.brief, w.sessionLog)),
      windows(entries).map(_._2.carry.map(_.cost))
    )
  }

  val tests: Tests = Tests {
    test(
      "a boundary for a holder shows its brief first and its session's digest before its own turns, their cost reserved"
    ) {
      val documents = new InMemoryDocuments
      val version = brief(documents)
      val digest =
        SessionDigest.of(lately, SessionDigest.Window.LacksLastSaid, TurnAssembly.DigestChars)
      run(documents, Some(holder), Some(lately)) ==> (
        Some(Vector(shownBrief, shownLog, answering, Message.User("one"))),
        Vector(bothCost),
        Vector((Some(version), Some(digest))),
        Vector(Some(bothCost))
      )
      assert(digest.contains("1a2b3c4 core: a brief"))
    }

    test("a rebuilt boundary for a holder names its brief, and its digest as recalled") {
      given grit.core.store.Tx = TestTx.fake
      val documents = new InMemoryDocuments
      val _ = brief(documents)
      val entries = new InMemoryEntryStore
      val turn = say(entries, "one", at = said)
      val durable = new InMemoryDurable
      durable.run(turn.workflowId)(
        turnBodyWith(
          entries,
          new RecordingProvider,
          new Reserving,
          new InMemoryUsageLedger,
          documents = documents,
          sessions =
            TurnSessions(new RecordedSessions(latest = Right(Some(lately))), _ => Some(holder))
        )
      )
      val steps = durable
        .history(turn.workflowId)
        .map(s =>
          grit.core.durable.StepRecord(
            s.name,
            s.outcome match {
              case InMemoryDurable.Outcome.Output(value) => Some(value)
              case _ => None
            },
            None
          )
        )
      val reads = TurnRecord.Reads(
        hosting().conversations,
        entries,
        new grit.core.store.InMemoryPrincipals,
        documents,
        new grit.core.files.InMemoryPieces,
        Prompts,
        ToolSets,
        profilesKept()
      )
      TurnRecord.requests(turn, steps, reads).map(_.window) ==> Right(
        Vector(TurnRecord.WindowMessage.Brief, TurnRecord.WindowMessage.Recalled)
      )
    }

    test(
      "a conversation working as no holder, its session unread, shows neither and reserves nothing"
    ) {
      val documents = new InMemoryDocuments
      brief(documents)
      run(documents, None, None) ==> (
        Some(Vector(answering, Message.User("one"))),
        Vector(Tokens.Zero),
        Vector((None, None)),
        Vector(Some(Tokens.Zero))
      )
    }

    test(
      "an append does not read the session again, records neither, and its request shows each once"
    ) {
      val assembler = new Appending
      val (messages, asked, recorded) = twice(assembler)
      (
        asked,
        assembler.reserved.size,
        recorded.map((_, b, d) => (b, d)),
        messages.map(_.count(_ == shownBrief)),
        messages.map(_.count(_ == shownLog))
      ) ==> (1, 2, Vector((true, true), (false, false)), Some(1), Some(1))
    }

    test(
      "a boundary at the assembler's limit is drawn again with the brief and digest reserved, and packs that draw"
    ) {
      val assembler = new Reserving
      val (_, asked, recorded) = twice(assembler)
      (asked, assembler.reserved, recorded.lift(1)) ==> (
        2,
        Vector(bothCost, Tokens.Zero, bothCost),
        Some((Vector.empty, true, true))
      )
    }

    test("a holder with no brief shows only its session's digest") {
      val documents = new InMemoryDocuments
      brief(documents)
      val other = got(ClaimHolder.of("reviewer"))
      val digest =
        SessionDigest.of(lately, SessionDigest.Window.LacksLastSaid, TurnAssembly.DigestChars)
      val (messages, _, recorded, cost) = run(documents, Some(other), Some(lately))
      (messages, recorded, cost) ==> (
        Some(Vector(shownLog, answering, Message.User("one"))),
        Vector((None, Some(digest))),
        Vector(Some(CharEstimate.message(shownLog)))
      )
    }

    test(
      "a boundary reserves the digest that shows what its session last said, and its record costs what it showed"
    ) {
      val documents = new InMemoryDocuments
      brief(documents)
      val summary = got(Session.Summary.of("Kept the brief beside the session's log."))
      val closed = lately.copy(session = lately.session.copy(summary = Some(summary)))
      def log(window: SessionDigest.Window) =
        Shown.sessionLog(SessionDigest.of(closed, window, TurnAssembly.DigestChars))
      val lacking = log(SessionDigest.Window.LacksLastSaid)
      val cost = CharEstimate.message(shownBrief) + CharEstimate.message(lacking)
      val (messages, reserved, _, recorded) = run(documents, Some(holder), Some(closed))
      (
        lacking != log(SessionDigest.Window.HoldsLastSaid),
        messages.flatMap(_.lift(1)),
        reserved,
        recorded
      ) ==> (true, Some(lacking), Vector(cost), Vector(Some(cost)))
    }
  }
}
