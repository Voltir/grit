package grit.turn

import grit.core.durable.Flow
import grit.core.id.{EntryId, TurnRef}
import grit.core.speech.{Outcome, SpeechJson}

import TurnSteps.*

/** The durable turn, as one flow: what [[Turn.body]] runs. Its steps, as [[Turn.Step]] names
  * them, in order:
  *   1. `pin-models` — the catalog in force, pinned as the turn's profile, which a rerun runs
  *      under; recorded by its id.
  *   1. `weigh` — the answers the turn's root is weighed with ([[TurnWeighing]]). Never fails
  *      the turn.
  *   1. `route` — the pin each role's calls are made under, and the effort laid over the turn's
  *      ([[Route]]); `Uncleared` when no turn pin is cleared for the turn.
  *   1. `offer` — what the turn offers its model ([[TurnOffer]]), and the cost of its weighing
  *      and of its task questions; recorded by reference, its tool set and prompt by id, which
  *      each step that needs them reads back ([[TurnOffer.load]]).
  *   1. `record-failure` — only when `pin-models`, `route` or `offer` failed, ending the turn
  *      ([[TurnSteps.recordFailure]]).
  *   1. `stitched` — an opening's placement waited for ([[grit.core.stitch.Placements.awaited]]).
  *      Never fails the turn.
  *   1. `classify`, `record-topic` — the message placed among its conversation's topics, and that
  *      placement recorded ([[TurnTopics]]). Never fail the turn.
  *   1. `assemble` — a fresh window over what came before the turn, at its offer's width.
  *   1. `record-window` — the window and its queries recorded before the model is called, so an
  *      edge sees what the model will see, and the loop's limits, once its tools are checked to
  *      make one toolbox.
  *   1. The tool loop ([[TurnRound]]), from `ask:0`, until a reply calls no tool or the
  *      budget's last call.
  *   1. `record-verdict` — when the classifier was unsure, the model's verdict ([[TurnVerdict]]),
  *      once the loop has a reply. Never fails the turn.
  *   1. `append` — the reply recorded as the turn's entry, or as a draft when its root is a
  *      heard message it drafts a reply to ([[TurnOffer.Root]]).
  *   1. `judge` — an unprompted turn's draft scored against its thread ([[TurnJudge]]).
  *   1. `record-speech` — what becomes of a heard-rooted turn's draft
  *      ([[grit.core.speech.Speech]]), or `Failed` when the turn failed before it. Only a posted
  *      draft goes on.
  *   1. `summarise`, `append-summary` — the turn's summary ([[TurnSummary]]), recorded after the
  *      reply. A failed summary leaves the reply standing.
  * It passes what the turn did, for logs: its reply and summary are in the store, never in
  * this string.
  */
private[turn] object TurnFlow {

  /** The turn from its ref. */
  def flow: Flow[Env, TurnRef, Nothing, String] =
    Flow
      .attempt(early)
      .andThen(Flow.either(failing, Flow.attempt(ran).map(_.merge).andThen(ending)))

  /** From `pin-models` to `offer`: what the turn offers, or why it ended before it did. */
  private def early: Flow[Env, TurnRef, Failed, Offered] = {
    val profiled = Flow.decide(pinModels)((turn, pinned) =>
      pinned.map(Weighed(turn, _, None)).left.map(Failed(turn, _))
    )
    val weighed = profiled
      .andThen(
        Flow
          .step[Env, Weighed, Option[TurnWeighing.Weighed]](weigh)
          .keeping
          .map((w, kept) => w.copy(weighed = kept))
      )
    val routed = weighed.andThen(
      Flow.decide(route)((w, r) =>
        r.map(Offering(w.turn, w.weighed, _)).left.map(Failed(w.turn, _))
      )
    )
    val offered = Flow.decide(offer)((r, recorded) =>
      recorded
        .map(Offered(r.turn, Turn.Picked.of(r.route), _, ""))
        .left
        .map(Failed(r.turn, _))
    )
    routed.andThen(offered)
  }

  /** A turn ended before its offer: `record-failure`. */
  private def failing: Flow[Env, Failed, Nothing, String] =
    Flow
      .step(recordFailure)
      .keeping
      .map((f, kept) =>
        kept.fold(e => s"failed: ${f.failure}; not recorded: $e", _ => s"failed: ${f.failure}")
      )

  /** From `stitched` to `append`: what the turn's steps came to. Ends with a turn that failed
    * before its loop, or in it, its verdict recorded when the loop had a reply.
    */
  private def ran: Flow[Env, Offered, Ended, Ended] = {
    val placed = Flow
      .step[Env, Offered, Either[TurnFailure, Option[String]]](stitched)
      .keeping
      .map((o, s) => o.copy(notes = o.notes + s.fold(f => s"; not placed: $f", _ => "")))
      .andThen(
        Flow.step[Env, Offered, TurnTopics.Classification](classify).keeping.map(Placed(_, _))
      )
      .andThen(
        Flow
          .step(recordTopic)
          .keeping
          .map((p, kept) =>
            p.copy(at =
              p.at.copy(notes = p.at.notes + kept.fold(f => s"; topics not recorded: $f", _ => ""))
            )
          )
      )
    def unran(at: Offered, failure: TurnFailure): Ended = Ended(at, Left(failure), None, None)
    val assembled = Flow
      .decide(assemble)((p, w) => w.map(Assembled(p.at, p.placing, _)).left.map(unran(p.at, _)))
      .andThen(
        Flow.decide(recordWindow)((a, limits) =>
          limits.map(l => (a, looping(a, l))).left.map(unran(a.at, _))
        )
      )
    val looped = Flow
      .pass[(Assembled, TurnRound.Looping)]
      .map(_._2)
      .andThen(Flow.attempt(TurnRound.flow.over[Env](r => r.rounds)))
      .keeping
      .map((started, loop) => (started._1, loop))
    placed
      .andThen(assembled)
      .andThen(looped)
      .andThen(
        Flow.mayEnd(
          Flow.branch((started: (Assembled, Either[TurnRound.Stalled, TurnRound.Looped])) =>
            started._2.left.map((started._1, _)).map(Answered(started._1, _))
          )(stalled.map(Left(_)), answered.map(Right(_)))
        )
      )
  }

  /** What `a`'s loop runs under, within `limits`. */
  private def looping(a: Assembled, limits: TurnRound.Limits): TurnRound.Looping =
    TurnRound.Looping(
      a.turn,
      a.at.offer,
      a.at.pins,
      a.seen,
      TurnShown.asked(a.placing),
      limits.budget,
      a.at.pins.turn.settings.strict == grit.core.model.StrictSchemas.Enforced,
      limits.answerWithin
    )

  /** A loop that stalled: its verdict recorded when it had a reply. */
  private def stalled: Flow[Env, (Assembled, TurnRound.Stalled), Nothing, Ended] =
    Flow
      .pass[(Assembled, TurnRound.Stalled)]
      .map((a, s) => s.at.replies.headOption.flatMap(first => verdictOf(a, first, Vector(first))))
      .andThen(verdict)
      .keeping
      .map { case ((a, s), unrecorded) =>
        Ended(
          a.at,
          Left(s.failure),
          unrecorded,
          s.at.replies.headOption.map(_ => s.at.level)
        )
      }

  /** A loop that answered: its verdict recorded, then its answer appended. */
  private def answered: Flow[Env, Answered, Nothing, Ended] =
    Flow
      .pass[Answered]
      .map(a => a.looped.at.replies.headOption.flatMap(verdictOf(a.at, _, a.looped.at.replies)))
      .andThen(verdict)
      .keeping
      .andThen(
        Flow
          .pass[(Answered, Option[TurnFailure])]
          .map(_._1)
          .andThen(Flow.step(append))
          .keeping
          .map { case ((a, unrecorded), kept) =>
            Ended(a.at.at, kept, unrecorded, Some(a.looped.at.level))
          }
      )

  /** What `record-verdict` is asked of `a`'s replies, when the turn's model was asked. */
  private def verdictOf(
      a: Assembled,
      first: grit.core.message.Message.Assistant,
      replies: Vector[grit.core.message.Message.Assistant]
  ): Option[Verdict] =
    TurnShown.asked(a.placing).map(Verdict(a.turn, _, first, replies))

  /** `record-verdict` when there is one to record: why it was not, when it failed. */
  private def verdict: Flow[Env, Option[Verdict], Nothing, Option[TurnFailure]] =
    Flow.branch((v: Option[Verdict]) => v.toRight(()))(
      Flow.pass[Unit].map[Option[TurnFailure]](_ => None),
      Flow.step(recordVerdict).map(_.left.toOption)
    )

  /** What follows the reply, by what the turn's root is: a heard-rooted turn's draft settled,
    * and its summary made when it is posted; an addressed turn's summary.
    */
  private def ending: Flow[Env, Ended, Nothing, String] =
    Flow.branch((e: Ended) =>
      (e.result, e.at.offer.root) match {
        case (Left(failure), TurnOffer.Root.Addressed) => Left(s"failed: $failure${e.notes}")
        case (Right(draft), TurnOffer.Root.Heard | TurnOffer.Root.Named) => Right(Left((e, draft)))
        case (Left(failure), TurnOffer.Root.Heard | TurnOffer.Root.Named | TurnOffer.Root.ByName) =>
          Right(Right(Left((e, Failed(e.turn, failure)))))
        case (Right(reply), TurnOffer.Root.Addressed | TurnOffer.Root.ByName) =>
          Right(Right(Right((e, reply))))
      }
    )(
      Flow.pass[String],
      // A draft's branch first: a reader names the steps in the order a run reaches them,
      // and a drafting turn judges before it records its speech.
      Flow.either(drafted, Flow.either(speechFailing, summarised("replied")))
    )

  /** A heard-rooted turn that failed: `Failed` kept as what became of its draft. */
  private def speechFailing: Flow[Env, (Ended, Failed), Nothing, String] =
    Flow
      .pass[(Ended, Failed)]
      .map(_._2)
      .andThen(Flow.step(speechFailed))
      .keeping
      .map { case ((e, f), settled) =>
        s"failed: ${f.failure}${e.notes}${settled.fold(n => s"; not recorded: $n", _ => "")}"
      }

  /** A heard-rooted turn's draft settled: judged when unprompted, then `record-speech`; a
    * posted one summarised.
    */
  private def drafted: Flow[Env, (Ended, EntryId), Nothing, String] =
    Flow
      .branch((e: (Ended, EntryId)) =>
        if (e._1.at.offer.root == TurnOffer.Root.Named) Left(e._1) else Right(e._1)
      )(
        Flow.pass[Ended].map(Settled(_, Settling.Named)),
        Flow
          .step[Env, Ended, TurnJudge.Judgement](judge)
          .keeping
          .map((e, j) => Settled(e, Settling.Judged(j)))
      )
      .andThen(Flow.step(speechSettled).keeping)
      .keeping
      .map { case ((e, draft), (_, settled)) =>
        settled match {
          case Left(failure) =>
            Left(s"drafted: ${EntryId.value(draft)}; not settled: $failure${e.notes}")
          case Right(Outcome.Posted(_)) => Right((e, e.turn.replyId))
          case Right(other) =>
            Left(s"drafted: ${EntryId.value(draft)}; ${SpeechJson.outcomeName(other)}${e.notes}")
        }
      }
      .andThen(Flow.either(Flow.pass[String], summarised("posted")))

  /** `answered`, the turn's reply, summarised: what the turn did, as `did` and then its
    * summary say.
    */
  private def summarised(did: String): Flow[Env, (Ended, EntryId), Nothing, String] =
    Flow
      .pass[(Ended, EntryId)]
      .map((e, answered) => Summing(e.turn, e.at.pins, answered, e.level))
      .andThen(Flow.attempt(summary))
      .keeping
      .map { case ((e, answered), made) =>
        val said = made.fold(f => s"no summary: $f", id => s"summarised: ${EntryId.value(id)}")
        s"$did: ${EntryId.value(answered)}; $said${e.notes}"
      }

  /** `summarise` then `append-summary`. */
  private def summary: Flow[Env, Summing, TurnFailure, EntryId] =
    Flow
      .decide(summarise)((s, m) =>
        // Right whenever `m` is: the summarise step made its call under this pin.
        m.flatMap(message => s.pins.summary.map(pin => Summary(s, message, pin.grade)))
      )
      .andThen(Flow.mayEnd(Flow.step(appendSummary)))
}
