package grit.dbos.engine

import java.time.Instant

import grit.core.id.{EntryId, TurnRef, TurnSeq}
import grit.core.message.{Tokens, Usage}
import grit.core.period.Probability
import grit.core.place.Place
import grit.core.speech.{Decision, Heard, Reach}
import grit.core.store.{Entry, Origin, Payload}
import grit.core.triage.{Kind, Tags}
import grit.dbos.sql.{LiveDb, SqlEntryStore, SqlSpeechStore, SqlUsageLedger, TestPostgres}

import utest.*

/** `grit.speech` is ledger: its rows go with their period's usage. */
object SpeechRetentionLiveTests extends TestSuite {

  private lazy val config = {
    val c = TestPostgres.freshDatabase("speech_retention")
    LiveEngine.open(c, "test").close()
    c
  }

  private val entries = new SqlEntryStore()
  private val ledger = new SqlUsageLedger
  private val speech = new SqlSpeechStore

  private val At = Instant.parse("2026-09-30T10:00:00Z")

  val tests = Tests {
    test("forgetting a period's usage forgets its turns' speech decisions, and no others") {
      val c = LiveDb.conversation(config, Origin.Task("speech", "retention")).id
      val p = Probability.clamped(0.8)
      val tags = Tags.Weighed(
        Kind.Question,
        p,
        p,
        p,
        p,
        "jev",
        Usage(Tokens(1), Tokens.Zero, Tokens.Zero, None)
      )
      val turns = (0L to 1L).toVector.map { n =>
        val turn = TurnRef(c, TurnSeq(n))
        LiveDb.transaction(config) {
          entries.insert(Entry(EntryId(s"h$n"), c, turn.turnSeq, None, n, Payload.Heard("hi"), At))
          speech.decided(
            Heard(turn, n, Place.Everywhere, At, Reach(Some("x"), Set.empty), tags),
            Decision.Drafting(turn),
            At
          )
        }
        turn
      }
      LiveDb.transaction(config)(ledger.forget(c, TurnSeq(0), TurnSeq(0))) ==> Right(())
      LiveDb
        .transaction(config)(speech.spoken(At))
        .map(_.filter(_.turn.conversationId == c).map(_.turn)) ==>
        Right(Vector(turns(1)))
    }
  }
}
