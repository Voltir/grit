package grit.core.speech

import java.time.{Instant, ZoneOffset}

import scala.concurrent.duration.*

import grit.core.id.{ConversationId, PrincipalId, TurnRef, TurnSeq}
import grit.core.message.{Cost, Tokens, Usage}
import grit.core.period.Probability
import grit.core.place.{Namespace, Place}
import grit.core.spend.{Budget, DailyCap, Spend}
import grit.core.triage.{Kind, Tags}

import utest.*

object SpeechTests extends TestSuite {

  private val now = Instant.parse("2026-09-30T12:00:00Z")
  private val usage = Usage(Tokens(1), Tokens.Zero, Tokens.Zero, None)
  private def p(x: Double) = Probability.clamped(x)

  private val cap: DailyCap = DailyCap.of("0.25").getOrElse(sys.error("a cap"))
  private val limits = Limits.suggested(cap)
  private val within = Speaking.Within(limits)
  private val budget = Budget(ZoneOffset.UTC, None)

  private val room = Place.under(Namespace.Slack, Vector("T", "C"))
  private val otherRoom = Place.under(Namespace.Slack, Vector("T", "D"))
  private val turn = TurnRef(ConversationId("c"), TurnSeq(5))

  private def tags(kind: Kind = Kind.Question, helps: Double = 0.9): Tags =
    Tags.Weighed(kind, p(0.9), p(0.5), p(0.5), p(helps), "jev", usage)

  private val heard = Heard(
    turn,
    10,
    room,
    now.minusSeconds(60),
    Reach(Some("C/1"), Set.empty),
    tags()
  )

  private val empty = Ledger(Vector.empty, Spend.Zero, Spend.Zero)

  private def posted(conversation: String, seq: Long, at: Instant, in: Place = room) =
    Spoken(TurnRef(ConversationId(conversation), TurnSeq(seq)), in, at, Stage.Posted(seq + 1))

  private def decide(
      h: Heard = heard,
      l: Ledger = empty,
      s: Speaking = within,
      b: Budget = budget
  ) =
    Speech.decide(s, h, l, b, now)

  private def held(why: Silence) = Decision.Held(why)

  private val judged = Judged(p(0.7), p(0.6), p(0.8), "jev", usage)

  val tests = Tests {
    test("a heard message that passes every check is drafted in its own turn") {
      assert(decide() == Decision.Drafting(turn))
    }

    test("the checks, in order: each holds with its own silence") {
      test("off") {
        assert(decide(s = Speaking.Off) == held(Silence.Off))
      }
      test("no reply address, as for a past message") {
        assert(decide(heard.copy(reach = Reach(None, Set.empty))) == held(Silence.NoAddress))
      }
      test("said longer ago than fresh") {
        val old = heard.copy(said = now.minusSeconds(11 * 60))
        assert(decide(old) == held(Silence.Stale(11.minutes)))
      }
      test("untagged fails closed") {
        val untagged = heard.copy(tags = Tags.Unanswered("down"))
        assert(decide(untagged) == held(Silence.Unweighed("down")))
      }
      test("chatter") {
        assert(decide(heard.copy(tags = tags(Kind.Chatter))) == held(Silence.Chatter))
      }
      test("helps under helpsAt") {
        assert(
          decide(heard.copy(tags = tags(helps = 0.59))) == held(Silence.Below(p(0.59), p(0.6)))
        )
      }
      test("helps at exactly helpsAt is not below") {
        assert(decide(heard.copy(tags = tags(helps = 0.6))) == Decision.Drafting(turn))
      }
      test("asked of someone else: the first by id") {
        val asked =
          heard.copy(reach =
            Reach(Some("C/1"), Set(PrincipalId("slack:T/U2"), PrincipalId("slack:T/U1")))
          )
        assert(decide(asked) == held(Silence.AskedOf(PrincipalId("slack:T/U1"))))
      }
      test("an earlier unprompted turn here still drafting") {
        val previous = TurnRef(ConversationId("c"), TurnSeq(3))
        val l =
          empty.copy(turns = Vector(Spoken(previous, room, now.minusSeconds(30), Stage.Drafting)))
        assert(decide(l = l) == held(Silence.Unanswered(previous)))
      }
      test("an earlier unprompted turn here posted after this message") {
        val previous = TurnRef(ConversationId("c"), TurnSeq(3))
        val l = empty.copy(turns =
          Vector(Spoken(previous, room, now.minusSeconds(30 * 60 * 60), Stage.Posted(11)))
        )
        assert(decide(l = l) == held(Silence.Unanswered(previous)))
      }
      test("posts in this thread within the thread rate") {
        // Posted before this message (at 4 < 10), within 6 hours: answered, but counted.
        val l = empty.copy(turns = Vector(posted("c", 3, now.minusSeconds(5 * 60 * 60))))
        assert(decide(l = l) == held(Silence.Thread(1)))
      }
      test("posts in this room within the room rate") {
        val l = empty.copy(turns =
          Vector(posted("a", 1, now.minusSeconds(600)), posted("b", 1, now.minusSeconds(1200)))
        )
        assert(decide(l = l) == held(Silence.Room(2)))
      }
      test("posts in all within the deployment rate") {
        val l = empty.copy(turns =
          (1 to 10).toVector.map(i => posted(s"x$i", 1, now.minusSeconds(3 * 60 * 60), otherRoom))
        )
        assert(decide(l = l) == held(Silence.Deployment(10)))
      }
      test("today's speech spend reached its cap") {
        val spent = Spend(3, Cost.Exact(BigDecimal("0.25")))
        assert(decide(l = empty.copy(speech = spent)) == held(Silence.OverSpeechCap(spent, cap)))
      }
      test("the deployment's budget does not admit today's spend") {
        val capped = Budget(ZoneOffset.UTC, DailyCap.of("1").toOption)
        val l = empty.copy(all = Spend(9, Cost.Exact(BigDecimal("1.5"))))
        assert(decide(l = l, b = capped) == held(Silence.OverBudget))
      }
    }

    test("a post outside its rate's window does not count") {
      val l = empty.copy(turns = Vector(posted("c", 3, now.minusSeconds(7 * 60 * 60))))
      assert(decide(l = l) == Decision.Drafting(turn))
    }

    test("a draft settled without a post does not count") {
      val settled = TurnRef(ConversationId("c"), TurnSeq(3))
      val l = empty.copy(turns = Vector(Spoken(settled, room, now.minusSeconds(60), Stage.Settled)))
      assert(decide(l = l) == Decision.Drafting(turn))
    }

    test("a draft's score is the weakest of its three") {
      assert(judged.score == p(0.6))
    }

    test("what becomes of a draft") {
      test("at or above postAt, within: posted") {
        assert(
          Speech.post(within, Right(judged.copy(grounded = p(0.5)))) ==
            Outcome.Posted(judged.copy(grounded = p(0.5)))
        )
      }
      test("at or above postAt, shadow: shadowed") {
        assert(Speech.post(Speaking.Shadow(limits), Right(judged)) == Outcome.Shadowed(judged))
      }
      test("under postAt: below") {
        val weak = judged.copy(worth = p(0.49))
        assert(Speech.post(within, Right(weak)) == Outcome.Below(weak, p(0.5)))
      }
      test("not judged: unjudged, with why") {
        assert(Speech.post(within, Left("down")) == Outcome.Unjudged("down"))
      }
      test("speaking switched off since: withdrawn") {
        assert(Speech.post(Speaking.Off, Right(judged)) == Outcome.Withdrawn)
      }
    }

    test("a rate needs a count of at least 1 and a positive window") {
      assert(
        Rate.of(0, 1.hour).isEmpty,
        Rate.of(1, Duration.Zero).isEmpty,
        Rate.of(1, 1.hour).map(r => (r.count, r.per)) == Some((1, 1.hour))
      )
    }
  }
}
