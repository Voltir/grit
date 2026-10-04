package grit.turn

import grit.core.classify.Classifier
import grit.core.durable.InMemoryDurable
import grit.core.id.{EntryId, TurnRef}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.period.Probability
import grit.core.provider.ProviderError
import grit.core.speech.{Cleared, Judged, Outcome, Speaking, SpeechJson}
import grit.core.store.Payload
import grit.core.triage.InMemoryTriageStore
import grit.dbos.sql.TestTx

import utest.*

/** A turn rooted on a heard message: its draft judged when unprompted, then posted, shadowed or held
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
      kept: Option[InMemoryTriageStore] = None,
      durable: InMemoryDurable = new InMemoryDurable
  ): String =
    durable.run(w.turn.workflowId)(
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

  /** The current epoch's recorded history `name`. */
  private def recorded(name: String): grit.core.durable.History =
    grit.core.durable.History
      .read(ujson.read(os.read(os.Path(sys.env("GRIT_HISTORIES")) / Turn.Epoch / s"$name.json")))
      .fold(why => throw new java.lang.AssertionError(s"$name: $why"), identity)

  private def outcome(w: SpeechWorld): Option[Outcome] = w.store.outcomes.get(w.turn).map(_._1)

  private def awaited(w: SpeechWorld): Vector[(TurnRef, String)] =
    w.deliveries.pending()(using TestTx.fake).getOrElse(Vector.empty).map(p => (p.turn, p.to))

  private def reply(w: SpeechWorld): Option[Payload] =
    w.entries.get(w.turn.replyId)(using TestTx.fake).toOption.flatten.map(_.payload)

  private def judged(grounded: Double, worth: Double) =
    Judged(
      Probability.clamped(grounded),
      Probability.clamped(worth),
      "jev",
      Usage(Tokens(40), Tokens.Zero, Tokens.Zero, Some(BigDecimal("0.0000017")))
    )

  val tests = Tests {
    test(
      "a named draft is posted unjudged, recalled or not: no judge step, nothing asked, no judge row in the ledger"
    ) {
      for (recalled <- Vector(true, false)) {
        val w = speechWorld(recalled)
        // Every question answered no: a judge asked would hold the draft.
        val judge = new AnswersYes(0.1)
        val durable = new InMemoryDurable
        val done = run(w, judge, kept = Some(directed(w)), durable = durable)
        (
          outcome(w).map(o => (SpeechJson.outcomeName(o), SpeechJson.judgedOf(o))),
          judge.asked,
          durable.history(w.turn.workflowId).map(_.name).contains(Turn.Step.Judge),
          w.ledger.rows.map(_._1).filter(id => EntryId.value(id).startsWith("judge:")),
          awaited(w),
          reply(w)
        ) ==> (
          Some(("posted", None)),
          Vector.empty,
          false,
          Vector.empty,
          Vector((w.turn, "C/1")),
          Some(Payload.Message(said("It moved to Thursday.")))
        )
        assert(done.startsWith("posted: reply:"))
      }
    }

    test(
      "a named turn resumed after the judge step it took before named drafts went unjudged posts unjudged, the judge's call in the ledger"
    ) {
      val history = recorded("named-judged-before-speech")
      keepAll(history.kept).fold(e => throw new java.lang.AssertionError(e), identity)
      val w = speechWorld()
      // What its replayed append wrote: the draft.
      {
        given grit.core.store.Tx = TestTx.fake
        val next = w.entries.lockNext(conversation).getOrElse(sys.error("store"))
        w.entries.insert(
          grit.core.store.Entry(
            w.turn.draftId,
            conversation,
            w.turn.turnSeq,
            None,
            next.seq,
            Payload.Draft(said("It moved to Thursday.")),
            java.time.Instant.EPOCH
          )
        )
      }.isRight ==> true
      val judge = new AnswersYes(0.1)
      new InMemoryDurable().replay(history.id, history.steps)(
        turnBodyWith(
          w.entries,
          new Scripted((_, _) => Right(said("unused"))),
          new Before(w.entries),
          w.ledger,
          judge,
          TurnSpeech(Speaking.Within(speechLimits), w.store, w.deliveries),
          weighing = TurnWeighing(
            directed(w),
            new Weighs(Left(grit.core.triage.Weighing.Unweighed.Unavailable))
          )
        )
      ) match {
        case Left(why) => throw new java.lang.AssertionError(why)
        case Right(_) => ()
      }
      (
        history.id == w.turn.workflowId,
        outcome(w),
        judge.asked,
        w.ledger.rows.collect {
          case (id, _, model, _, _, _) if id == TurnJudge.id(w.turn) => model
        },
        awaited(w)
      ) ==> (
        true,
        Some(Outcome.Posted(Cleared.Named)),
        Vector.empty,
        Vector("jev"),
        Vector((w.turn, "C/1"))
      )
    }

    test("a named draft under Shadow is shadowed unjudged, nothing posted") {
      val w = speechWorld()
      val judge = new AnswersYes(0.1)
      run(w, judge, Speaking.Shadow(speechLimits), kept = Some(directed(w)))
      (
        outcome(w).map(o => (SpeechJson.outcomeName(o), SpeechJson.judgedOf(o))),
        judge.asked,
        awaited(w),
        reply(w)
      ) ==> (Some(("shadowed", None)), Vector.empty, Vector.empty, None)
    }

    test("a named draft that passes is kept passed, nothing posted") {
      val w = speechWorld()
      run(w, new AnswersYes(0.9), answer = Right(said(" pass")), kept = Some(directed(w)))
      (outcome(w), awaited(w), reply(w)) ==> (Some(Outcome.Passed), Vector.empty, None)
    }

    test(
      "its weaker answer at or above postAt: the draft is the reply, awaited at its address, and kept posted with each answer"
    ) {
      val w = speechWorld()
      val judge = new Judge(Some((0.9, 0.5)))
      val done = run(w, judge)
      (outcome(w), awaited(w), reply(w), judge.calls) ==> (
        Some(Outcome.Posted(Cleared.Scored(judged(0.9, 0.5)))),
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
        Some(Outcome.Shadowed(Cleared.Scored(judged(0.9, 0.5)))),
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
        (Some(Outcome.Posted(Cleared.Scored(judged(0.9, 0.9)))), Vector((w.turn, "C/1")))
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
