package grit.turn

import scala.collection.immutable.VectorMap

import grit.core.classify.{Answer, Answers, Classifier, ClassifierError, Question}
import grit.core.durable.InMemoryDurable
import grit.core.id.{EntryId, TurnRef}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.period.Probability
import grit.core.provider.ProviderError
import grit.core.speech.{Judged, Outcome, Speaking}
import grit.core.store.Payload
import grit.core.triage.{InMemoryTriageStore, Tags}
import grit.dbos.sql.TestTx

import utest.*

/** A turn rooted on a heard message: its draft judged, then posted, shadowed or held
  * (ADR 0022).
  */
object TurnSpeechTests extends TestSuite {
  import TurnFixtures.*

  private def said(text: String): Message.Assistant =
    Message.Assistant(
      Vector(AssistantBlock.Text(text)),
      StopReason.EndTurn,
      Usage(Tokens(10), Tokens(2), Tokens.Zero, Some(BigDecimal("0.001"))),
      "m"
    )

  private def run(
      w: SpeechWorld,
      judge: Classifier^,
      speaking: Speaking = Speaking.Within(speechLimits),
      answer: Either[ProviderError, Message.Assistant] = Right(said("It moved to Thursday.")),
      kept: Option[InMemoryTriageStore] = None
  ): String =
    new InMemoryDurable().run(w.turn.workflowId)(
      turnBodyWith(
        w.entries,
        new Scripted((_, _) => answer),
        new Before(w.entries),
        w.ledger,
        judge,
        TurnSpeech(speaking, w.store, w.deliveries),
        weighing = TurnWeighing(
          kept.getOrElse(new InMemoryTriageStore(w.entries, NoPeriods)),
          new Weighs(Left(grit.core.triage.Weighing.Unweighed.Unavailable))
        )
      )
    )

  private def outcome(w: SpeechWorld): Option[Outcome] = w.store.outcomes.get(w.turn).map(_._1)

  private def awaited(w: SpeechWorld): Vector[(TurnRef, String)] =
    w.deliveries.pending()(using TestTx.fake).getOrElse(Vector.empty).map(p => (p.turn, p.to))

  private def reply(w: SpeechWorld): Option[Payload] =
    w.entries.get(w.turn.replyId)(using TestTx.fake).toOption.flatten.map(_.payload)

  private def judged(grounded: Double, worth: Double) =
    Judged(
      Judged.Scores.Unprompted(Probability.clamped(grounded), Probability.clamped(worth)),
      "jev",
      Usage(Tokens(40), Tokens.Zero, Tokens.Zero, Some(BigDecimal("0.0000017")))
    )

  /** A judge of named drafts: one question, answered `yes`, its words kept. */
  private final class NamedJudge(yes: Double) extends Classifier {
    @caps.unsafe.untrackedCaptures
    var asked = Vector.empty[Question]

    protected def answer(
        state: ujson.Value,
        questions: Vector[Question]
    ): Either[ClassifierError, Answers] = {
      asked = asked ++ questions
      Right(
        Answers(
          questions.map(_ => Answer.YesNo(yes)),
          Usage(Tokens(40), Tokens.Zero, Tokens.Zero, Some(BigDecimal("0.0000017"))),
          "jev"
        )
      )
    }
  }

  /** What triage kept for `w`'s heard message: a question to grit by name ([[Tags.V3.directed]]). */
  private def named(w: SpeechWorld): Option[InMemoryTriageStore] = {
    val store = new InMemoryTriageStore(w.entries, NoPeriods)
    store.record(
      EntryId("heard:is the freeze still on?"),
      Tags.Weighed(
        VectorMap(
          Tags.V2.gap -> Answer.Choice("asks", Vector(Answer.Weight("asks", 1.0)), 1.0),
          Tags.V2.open -> Answer.YesNo(0.9),
          Tags.V2.to -> Answer.YesNo(0.9),
          Tags.V3.toGrit -> Answer.YesNo(0.9)
        ),
        "jev",
        Usage.Zero
      ),
      java.time.Instant.EPOCH
    )(using TestTx.fake) ==> Right(true)
    Some(store)
  }

  val tests = Tests {
    test(
      "a draft answering a message directed at grit is judged on whether it answers, alone, even with nothing recalled, and posted at postAt"
    ) {
      for (recalled <- Vector(true, false)) {
        val w = speechWorld(recalled)
        val judge = new NamedJudge(0.8)
        run(w, judge, kept = named(w))
        (outcome(w), judge.asked, awaited(w)) ==> (
          Some(
            Outcome.Posted(
              Judged(
                Judged.Scores.Named(Probability.clamped(0.8)),
                "jev",
                Usage(Tokens(40), Tokens.Zero, Tokens.Zero, Some(BigDecimal("0.0000017")))
              )
            )
          ),
          Vector(
            Question.YesNo(
              "Read thread and draft. Someone in thread asked the assistant something. Does " +
                "draft answer what they asked, without making up facts?",
              None,
              None
            )
          ),
          Vector((w.turn, "C/1"))
        )
      }
    }

    test("a named draft whose answer is under postAt is kept below, nothing posted") {
      val w = speechWorld()
      run(w, new NamedJudge(0.4), kept = named(w))
      (outcome(w), awaited(w)) ==> (
        Some(
          Outcome.Below(
            Judged(
              Judged.Scores.Named(Probability.clamped(0.4)),
              "jev",
              Usage(Tokens(40), Tokens.Zero, Tokens.Zero, Some(BigDecimal("0.0000017")))
            ),
            speechLimits.postAt
          )
        ),
        Vector.empty
      )
    }

    test(
      "its weaker answer at or above postAt: the draft is the reply, awaited at its address, and kept posted with each answer"
    ) {
      val w = speechWorld()
      val judge = new Judge(Some((0.9, 0.5)))
      val done = run(w, judge)
      (outcome(w), awaited(w), reply(w), judge.calls) ==> (
        Some(Outcome.Posted(judged(0.9, 0.5))),
        Vector((w.turn, "C/1")),
        Some(Payload.Message(said("It moved to Thursday."))),
        1
      )
      assert(done.startsWith("posted: reply:"))
      // The judge's call is in the ledger, beside the draft's.
      w.ledger.rows.map(_._1).filter(id => EntryId.value(id).startsWith("judge:")) ==>
        Vector(TurnJudge.id(w.turn))
    }

    test("under Shadow, scored the same: nothing posted, kept shadowed") {
      val w = speechWorld()
      run(w, new Judge(Some((0.9, 0.5))), Speaking.Shadow(speechLimits))
      (outcome(w), awaited(w), reply(w)) ==> (
        Some(Outcome.Shadowed(judged(0.9, 0.5))),
        Vector.empty,
        None
      )
    }

    test("its weaker answer under postAt, though the other is above: nothing posted, kept below") {
      val w = speechWorld()
      run(w, new Judge(Some((0.49, 0.9))))
      (outcome(w), awaited(w), reply(w)) ==>
        (Some(Outcome.Below(judged(0.49, 0.9), speechLimits.postAt)), Vector.empty, None)
    }

    test(
      "a person's message after the root is not a reply: the draft is still judged and posted"
    ) {
      val w = speechWorld()
      hear(w.entries, "yes, Thursday")
      run(w, new Judge(Some((0.9, 0.9))))
      (outcome(w), awaited(w)) ==>
        (Some(Outcome.Posted(judged(0.9, 0.9))), Vector((w.turn, "C/1")))
    }

    test("the assistant replied in the thread after the root: spoken, nothing posted") {
      val w = speechWorld()
      val replied = {
        given grit.core.store.Tx = TestTx.fake
        val next = w.entries.lockNext(conversation).getOrElse(sys.error("store"))
        val e = grit.core.store.Entry(
          EntryId("replied"),
          conversation,
          next.turnSeq,
          None,
          next.seq,
          Payload.Message(said("Thursday, as before.")),
          java.time.Instant.EPOCH
        )
        w.entries.insert(e)
        e
      }
      run(w, new Judge(Some((0.9, 0.9))))
      (outcome(w), awaited(w)) ==> (Some(Outcome.Spoken(replied.id)), Vector.empty)
    }

    test("a draft that passes is kept passed, and the judge is not asked") {
      val w = speechWorld()
      val judge = new Judge(Some((0.9, 0.9)))
      run(w, judge, answer = Right(said("  Pass ")))
      (outcome(w), judge.calls, awaited(w)) ==> (Some(Outcome.Passed), 0, Vector.empty)
    }

    test("a window that recalled no record: nothing recalled, and the judge is not asked") {
      val w = speechWorld(recalled = false)
      val judge = new Judge(Some((0.9, 0.9)))
      run(w, judge)
      (outcome(w), judge.calls, awaited(w)) ==> (Some(Outcome.NothingRecalled), 0, Vector.empty)
    }

    test("heard with no reply address, a draft that would post: kept failed, nothing posted") {
      val w = speechWorld(replyTo = None)
      run(w, new Judge(Some((0.9, 0.9))))
      (outcome(w), awaited(w), reply(w)) ==>
        (Some(Outcome.Failed("no address to reply to")), Vector.empty, None)
    }

    test("the judge unavailable: unjudged, nothing posted") {
      val w = speechWorld()
      run(w, new Judge(None))
      (outcome(w), awaited(w)) ==> (Some(Outcome.Unjudged("unavailable: down")), Vector.empty)
    }

    test("the turn failing before its draft: kept failed with the turn's failure, nothing posted") {
      val w = speechWorld()
      run(w, new Judge(Some((0.9, 0.9))), answer = Left(ProviderError.Unavailable("down")))
      (outcome(w), awaited(w), reply(w)) ==>
        (
          Some(Outcome.Failed(TurnFailure.Model("down (after 3 tries)").toString)),
          Vector.empty,
          None
        )
    }
  }
}
