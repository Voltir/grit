package grit.core.plugin

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.claim.{ClaimRole, Claimed, InMemoryClaims}
import grit.core.document.{
  DocLabel,
  DocText,
  DocWeight,
  DocumentKeeper,
  DocumentTerms,
  InMemoryDocuments
}
import grit.core.either.Got.got
import grit.core.id.{ClaimHolder, ConversationId, DocKey, PluginName, SessionId, TurnRef, TurnSeq}
import grit.core.identity.{Principal, TestAccounts}
import grit.core.place.Place
import grit.core.store.{StoreError, Tx}
import grit.core.tool.{Args, Field, ToolName, ToolSpec}
import grit.core.visibility.{Clearance, Integrity, Label}
import grit.dbos.sql.TestTx

import utest.*

/** [[KeepingCommand.run]]: the keeper a command is handed, fenced for the session calling. */
object KeepingCommandTests extends TestSuite {

  private val plugin = got(PluginName.of("notes"))
  private val terms =
    got(DocumentTerms.of(got(DocLabel.of("notes")), DocWeight.Unscaled, 30.days, 100))
  private val key = got(DocKey.of("note"))
  private val at = Instant.parse("2026-10-30T12:00:00Z")

  private val person: Principal.Person =
    TestAccounts.principal(TestAccounts.account("slack:T1/U1")) match {
      case p: Principal.Person => p
      case Principal.Grit => throw new java.lang.AssertionError("not a person")
    }

  private def session(s: String): SessionId = got(SessionId.read(s"claude-code:$s"))

  private val said = Args.of((text = Field.text("What."))).map(a => Word(a.text))
  final case class Word(text: String) extends caps.Pure

  /** Keeps `text` under [[key]]. */
  private object Note extends KeepingCommand[Word] {
    val spec: ToolSpec[Word] = ToolSpec(ToolName("note"), "Keeps a note.", said)
    def run(
        args: Word,
        by: CommandBy,
        own: DocumentKeeper,
        claims: grit.core.claim.Claims,
        at: Instant
    )(using
        Tx^
    ): Either[StoreError, CommandOutcome] =
      own
        .write(
          key,
          Label.Public,
          Integrity.Member,
          Place.Everywhere,
          got(DocText.of(args.text)),
          ujson.Obj(),
          None,
          at
        )
        .map(_ => CommandOutcome.Done(s"kept ${args.text}"))
  }

  /** `by`'s note `text`, through a keeper whose key the session `a` claims. */
  private def noted(by: CommandBy, text: String): Either[StoreError, CommandOutcome] = {
    given Tx = TestTx.fake(Clearance.of(Label.Public))
    val documents = new InMemoryDocuments
    val claims = new InMemoryClaims().of(plugin, _ => None)
    val made = claims.claim(
      key,
      got(ClaimHolder.of("h1")),
      got(ClaimRole.of("builder")),
      Some(session("a")),
      person,
      Label.Public,
      at
    )
    assert(made.map { case Claimed.Made(_) => true; case _ => false } == Right(true))
    KeepingCommand.run(
      Note,
      Word(text),
      by,
      documents.keeper(plugin, terms),
      claims,
      Place.Everywhere,
      Integrity.Member,
      at
    )
  }

  val tests: Tests = Tests {
    test("a direct caller's session other than the one working a claimed key is refused") {
      noted(CommandBy.Direct(person, Integrity.Member, Some(session("b"))), "x") ==>
        Left(StoreError.Worked(key, session("a")))
    }

    test("a direct caller with no session, or the working one, and a turn write a claimed key") {
      val turn = CommandBy.Turn(person, TurnRef(ConversationId("c1"), TurnSeq(1)))
      assert(
        noted(CommandBy.Direct(person, Integrity.Member, None), "n") == Right(
          CommandOutcome.Done("kept n")
        ),
        noted(CommandBy.Direct(person, Integrity.Member, Some(session("a"))), "a") ==
          Right(CommandOutcome.Done("kept a")),
        noted(turn, "t") == Right(CommandOutcome.Done("kept t"))
      )
    }
  }
}
