package grit.core.triage

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, TurnSeq}
import grit.core.message.{Message, Tokens, Usage}
import grit.core.period.Probability
import grit.core.store.{Entry, Payload}

import utest.*

object EarningTests extends TestSuite {

  private def entry(n: Long, payload: Payload): Entry =
    Entry(EntryId(s"e$n"), ConversationId("c"), TurnSeq(n), None, n, payload, Instant.EPOCH)

  private def heard(n: Long): Entry = entry(n, Payload.Heard(s"heard $n"))

  private def weighed(kind: Kind, durable: Double): Tags =
    Tags.Weighed(
      kind,
      Probability.clamped(0.9),
      Probability.clamped(0.1),
      Probability.clamped(durable),
      Probability.clamped(0.1),
      "jev",
      Usage(Tokens(1), Tokens.Zero, Tokens.Zero, None)
    )

  private val chatter = weighed(Kind.Chatter, 0.1)

  val tests = Tests {
    test("a period only heard, every message weighed not worth keeping, does not earn") {
      val es = Vector(heard(0), heard(1))
      Earning.earns(es, Map(es(0).id -> chatter, es(1).id -> chatter)) ==> false
    }

    test("one message said to grit earns, whatever was heard beside it") {
      val es = Vector(heard(0), entry(1, Payload.Message(Message.User("@grit when is standup?"))))
      Earning.earns(es, Map(es(0).id -> chatter)) ==> true
    }

    test("a heard message worth keeping at DurableAt earns; just below it does not") {
      val es = Vector(heard(0), heard(1))
      val at = Probability.value(Earning.DurableAt)
      (
        Earning.earns(es, Map(es(0).id -> chatter, es(1).id -> weighed(Kind.Decision, at))),
        Earning.earns(es, Map(es(0).id -> chatter, es(1).id -> weighed(Kind.Decision, at - 0.01)))
      ) ==> (true, false)
      // The constant a deployment's closings turn on (ADR 0020).
      at ==> 0.5
    }

    test("a heard message untagged or unanswered earns: it fails open") {
      val es = Vector(heard(0), heard(1))
      (
        Earning.earns(es, Map(es(0).id -> chatter)),
        Earning.earns(es, Map(es(0).id -> chatter, es(1).id -> Tags.Unanswered("timeout")))
      ) ==> (true, true)
    }

    test("a period with no entries earns: nothing says it was only heard") {
      Earning.earns(Vector.empty, Map.empty) ==> true
    }
  }
}
