package grit.core.speech

import java.time.{Instant, ZoneOffset}

import scala.concurrent.duration.*

import grit.core.id.QuestionName
import grit.core.id.{ConversationId, EntryId, EntrySeq, PrincipalId, TurnRef, TurnSeq}
import grit.core.message.{AssistantBlock, Cost, Message, StopReason, Tokens, Usage}
import grit.core.period.Probability
import grit.core.place.{Namespace, Place}
import grit.core.spend.{Budget, DailyCap, Spend}
import grit.core.store.{Entry, Payload}
import grit.core.triage.{Bound, Gate, Kind, Reading, Tags}

import utest.*

object SpeechTests extends TestSuite {

  private val now = Instant.parse("2026-09-30T12:00:00Z")
  private val usage = Usage(Tokens(1), Tokens.Zero, Tokens.Zero, None)
  private def p(x: Double) = Probability.clamped(x)

  private val cap: DailyCap = DailyCap.of("0.25").getOrElse(sys.error("a cap"))
  private val limits = Limits.suggested(cap, Tags.V1.gate)
  private val within = Speaking.Within(limits)
  private val budget = Budget(ZoneOffset.UTC, None)

  private val room = Place.under(Namespace.Slack, Vector("T", "C"))
  private val otherRoom = Place.under(Namespace.Slack, Vector("T", "D"))
  private val turn = TurnRef(ConversationId("c"), TurnSeq(5))

  private def tags(kind: Kind = Kind.Question, helps: Double = 0.9): Tags =
    Tags.Weighed(Tags.V1.answers(kind, p(0.9), p(0.5), p(0.5), p(helps)), "jev", usage)

  private val heard = Heard(
    turn,
    EntrySeq(10),
    room,
    now.minusSeconds(60),
    Reach(Some("C/1"), Set.empty),
    tags()
  )

  private val empty = Ledger(Vector.empty, Spend.Zero, Spend.Zero)

  private def posted(conversation: String, seq: Long, at: Instant, in: Place = room) =
    Spoken(
      TurnRef(ConversationId(conversation), TurnSeq(seq)),
      in,
      at,
      Stage.Posted(EntrySeq(seq + 1))
    )

  private def decide(
      h: Heard = heard,
      l: Ledger = empty,
      s: Speaking = within,
      b: Budget = budget
  ) =
    Speech.decide(s, h, l, b, now)

  private def held(why: Silence) = Decision.Held(why)

  private val half = p(0.5)
  private val notChatter = Bound.Below(Reading.Chosen(Tags.V1.kind, "chatter"), half)
  private val helpful = Bound.AtLeast(Reading.Yes(Tags.V1.helps), half)
  private def gated(first: Gate.Failed, rest: Gate.Failed*) =
    held(Silence.Gated(first, rest.toVector))

  private val judged = Judged(Judged.Scores.Unprompted(p(0.6), p(0.8)), "jev", usage)

  private def scored(grounded: Double, worth: Double) =
    judged.copy(scores = Judged.Scores.Unprompted(p(grounded), p(worth)))

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
      test("said exactly fresh ago is not stale") {
        assert(decide(heard.copy(said = now.minusSeconds(10 * 60))) == Decision.Drafting(turn))
      }
      test("untagged fails closed") {
        val untagged = heard.copy(tags = Tags.Unanswered("down"))
        assert(decide(untagged) == held(Silence.Unweighed("down")))
      }
      test("chatter: gated on kind chosen chatter, which reads 1") {
        assert(
          decide(heard.copy(tags = tags(Kind.Chatter))) == gated(Gate.Failed(notChatter, p(1)))
        )
      }
      test("helps under the gate's 0.5: gated on helps, reading it") {
        assert(
          decide(heard.copy(tags = tags(helps = 0.49))) == gated(Gate.Failed(helpful, p(0.49)))
        )
      }
      test("helps at exactly the gate's 0.5 is not below") {
        assert(decide(heard.copy(tags = tags(helps = 0.5))) == Decision.Drafting(turn))
      }
      test("chatter that would not help: gated on both, chatter first") {
        assert(
          decide(heard.copy(tags = tags(Kind.Chatter, helps = 0.2))) ==
            gated(Gate.Failed(notChatter, p(1)), Gate.Failed(helpful, p(0.2)))
        )
      }
      test("a gate reading a question triage did not ask holds the message unasked") {
        val gap = QuestionName.of("gap").getOrElse(sys.error("a name"))
        val unread = Gate.bounds(helpful, Bound.AtLeast(Reading.Key(gap, "asks"), half))
        assert(
          decide(s = Speaking.Within(limits.copy(drafts = unread))) ==
            held(Silence.Unasked(Reading.Key(gap, "asks")))
        )
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
          Vector(Spoken(previous, room, now.minusSeconds(30 * 60 * 60), Stage.Posted(EntrySeq(11))))
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
      test("today's speech spend reached its cap, though some of it unpriced") {
        val spent = Spend(3, Cost.AtLeast(BigDecimal("0.25")))
        assert(decide(l = empty.copy(speech = spent)) == held(Silence.OverSpeechCap(spent, cap)))
      }
      test("the deployment's budget does not admit today's spend") {
        val capped = Budget(ZoneOffset.UTC, DailyCap.of("1").toOption)
        val l = empty.copy(all = Spend(9, Cost.Exact(BigDecimal("1.5"))))
        assert(decide(l = l, b = capped) == held(Silence.OverBudget))
      }
    }

    test("two checks failing: the earlier one's silence holds") {
      val stale = heard.copy(said = now.minusSeconds(11 * 60))
      val capped = Budget(ZoneOffset.UTC, DailyCap.of("1").toOption)
      val asked = Reach(Some("C/1"), Set(PrincipalId("slack:T/U1")))
      val threaded = empty.copy(
        turns = Vector(posted("c", 3, now.minusSeconds(5 * 60 * 60))),
        all = Spend(9, Cost.Exact(BigDecimal("1.5")))
      )
      Vector(
        decide(stale.copy(tags = tags(Kind.Chatter))),
        decide(heard.copy(tags = tags(Kind.Chatter), reach = asked)),
        decide(l = threaded, b = capped)
      ) ==> Vector(
        held(Silence.Stale(11.minutes)),
        gated(Gate.Failed(notChatter, p(1))),
        held(Silence.Thread(1))
      )
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

    test(
      "an unprompted draft's score is the weaker of grounded and worth; a named one's, answers"
    ) {
      assert(
        judged.score == p(0.6),
        scored(0.6, 0.3).score == p(0.3),
        judged.copy(scores = Judged.Scores.Named(p(0.45))).score == p(0.45)
      )
    }

    test("what becomes of a draft") {
      test("at or above postAt, within: posted") {
        assert(
          Speech.post(within, Right(scored(0.5, 0.8))) ==
            Outcome.Posted(scored(0.5, 0.8))
        )
      }
      test("at or above postAt, shadow: shadowed") {
        assert(Speech.post(Speaking.Shadow(limits), Right(judged)) == Outcome.Shadowed(judged))
      }
      test("under postAt: below, within or shadow") {
        val weak = scored(0.6, 0.49)
        Vector(within, Speaking.Shadow(limits)).map(Speech.post(_, Right(weak))) ==>
          Vector(Outcome.Below(weak, p(0.5)), Outcome.Below(weak, p(0.5)))
      }
      test("not judged: unjudged, with why") {
        assert(Speech.post(within, Left("down")) == Outcome.Unjudged("down"))
      }
      test("speaking switched off since: withdrawn, judged or not") {
        Vector(Right(judged), Left("down")).map(Speech.post(Speaking.Off, _)) ==>
          Vector(Outcome.Withdrawn, Outcome.Withdrawn)
      }
    }

    test(
      "a person's reply does not hold a draft; the assistant's own reply after its root does, in its thread or its strand"
    ) {
      def entry(n: Long, payload: Payload, c: String = "c", at: Instant = now) =
        Entry(EntryId(s"$c$n"), ConversationId(c), TurnSeq(n), None, EntrySeq(n), payload, at)
      val root = entry(10, Payload.Heard("where did we land?"))
      val reply = Payload.Message(
        Message.Assistant(
          Vector(AssistantBlock.Text("12 months.")),
          StopReason.EndTurn,
          Usage.Zero,
          "m"
        )
      )
      // The assistant's reply before the root answers something else.
      val earlier = entry(9, reply)
      val people = Vector(
        earlier,
        entry(11, Payload.Summary("grit's own")),
        entry(12, Payload.Heard("it's Thursday")),
        entry(13, Payload.Message(Message.User("@bort?")))
      )
      Speech.spoken(root, people, Vector.empty) ==> None
      Speech.spoken(root, people :+ entry(14, reply), Vector.empty) ==>
        Some(Outcome.Spoken(EntryId("c14")))
      // In the strand, by time: a reply before the root is not after it.
      val before = entry(3, reply, "s", now.minusSeconds(5))
      val after = entry(4, reply, "s", now.plusSeconds(5))
      Speech.spoken(root, people, Vector(before)) ==> None
      Speech.spoken(root, people, Vector(before, after)) ==> Some(Outcome.Spoken(EntryId("s4")))
      // The first by time, wherever it was said.
      val ownLater = entry(14, reply, at = now.plusSeconds(10))
      Speech.spoken(root, people :+ ownLater, Vector(after)) ==> Some(Outcome.Spoken(EntryId("s4")))
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
