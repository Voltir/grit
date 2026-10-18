package grit.turn

import grit.core.durable.Flow
import grit.core.id.{EntryId, TurnRef}
import grit.core.speech.{Outcome, SpeechJson}

import TurnSteps.*

/** The durable turn, as one flow: what [[Turn.body]] runs. Its steps, their order and which
  * run when, are its description: each part below says what its steps do ([[Turn.Step.all]]
  * lists them). It passes what the turn did, for logs: its reply and summary are in the store,
  * never in this string.
  */
private[turn] object TurnFlow {

  /** The turn from its ref. */
  def flow: Flow[Env, TurnRef, Nothing, String] =
    Flow
      .attempt(early)
      .andThen(Flow.either(failing, ran.andThen(ending)))

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
        r.map(route => Offering(w.turn, w.weighed, Turn.Routed(w.profile, route)))
          .left
          .map(Failed(w.turn, _))
      )
    )
    val offered = Flow.decide(offer)((r, recorded) =>
      recorded
        .map(Offered(r.turn, r.route, _, ""))
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

  /** From `stitched` to `append`: what the turn's steps came to, whether it failed before its
    * loop, in it, or answered; its verdict recorded when the loop had a reply.
    */
  private def ran: Flow[Env, Offered, Nothing, Ended] = {
    val placed =
      (stitched: Flow[Env, Offered, Nothing, Either[TurnFailure, Option[String]]]).keeping
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
                p.at
                  .copy(notes = p.at.notes + kept.fold(f => s"; topics not recorded: $f", _ => ""))
              )
            )
        )
    def unran(at: Offered, result: Result): Ended = Ended(at, result, None, None)
    def windowed(a: Assembled, limits: Either[TurnFailure, TurnRound.Limits]) =
      limits.map(l => (a, looping(a, l))).left.map(f => unran(a.at, Result.Failed(f)))
    val assembled = Flow
      .mayEnd(
        TurnAssembly.flow.keeping
          .map((p, drawn) =>
            drawn match {
              case Left(f) => Left(unran(p.at, Result.Failed(f)))
              case Right(Left(root)) => Left(unran(p.at, Result.Unneeded(root)))
              case Right(Right(w)) => Right(Assembled(p.at, p.placing, w))
            }
          )
      )
      .andThen(Flow.decide(recordWindow)(windowed))
    val looped = Flow
      .pass[(Assembled, TurnRound.Looping)]
      .map(_._2)
      .andThen(Flow.attempt(TurnRound.flow.over[Env](r => r.rounds)))
      .keeping
      .map((started, loop) => (started._1, loop))
    placed
      .andThen(Flow.attempt(assembled))
      .andThen(
        Flow.either(
          Flow.pass[Ended],
          looped.andThen(
            Flow.branch((started: (Assembled, Either[TurnRound.Stalled, TurnRound.Looped])) =>
              started._2.left.map((started._1, _)).map(Answered(started._1, _))
            )(stalled, answered)
          )
        )
      )
  }

  /** What `a`'s loop runs under, within `limits`. */
  private def looping(a: Assembled, limits: TurnRound.Limits): TurnRound.Looping =
    TurnRound.Looping(
      a.turn,
      a.at.offer,
      a.at.routed,
      a.seen,
      TurnShown.asked(a.placing),
      limits.budget,
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
          Result.Failed(s.failure),
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
            Ended(
              a.at.at,
              kept.fold(Result.Failed(_), k => Result.Kept(k.reply)),
              unrecorded,
              Some(a.looped.at.level)
            )
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
    Flow.branch((e: Ended) => Ending.of(e).split)(
      Flow.pass[String],
      // A draft's branch first: a reader names the steps in the order a run reaches them,
      // and a drafting turn judges before it records its speech.
      Flow.either(drafted, Flow.either(undrafting, summarised("replied")))
    )

  /** What a turn's steps came to, by what its root is: what [[ending]] goes on with. */
  private enum Ending {

    /** An addressed turn that failed: what it did, said; nothing follows. */
    case Stopped(said: String)

    /** A heard or named turn's `draft`, to settle. */
    case Drafted(at: Ended, draft: EntryId)

    /** A heard, named or by-name turn that failed, or a heard or named one the needs gate
      * rejected: `undrafted`, kept as what became of its draft; `said`, what it did.
      */
    case Unsettled(at: Ended, undrafted: Undrafted, said: String)

    /** An addressed or by-name turn's `reply`, to summarise. */
    case Replied(at: Ended, reply: EntryId)

    /** The arm of [[ending]] that goes on with it, in the order the arms are written. */
    def split: Either[
      String,
      Either[(Ended, EntryId), Either[(Ended, Undrafted, String), (Ended, EntryId)]]
    ] =
      this match {
        case Stopped(said) => Left(said)
        case Drafted(at, draft) => Right(Left((at, draft)))
        case Unsettled(at, undrafted, said) => Right(Right(Left((at, undrafted, said))))
        case Replied(at, reply) => Right(Right(Right((at, reply))))
      }
  }

  private object Ending {

    /** What `e` came to, by its result and its offer's root: an unneeded turn's root is a
      * heard one by type ([[TurnNeeds.Unasked]]).
      */
    def of(e: Ended): Ending = (e.result, e.at.offer.root) match {
      case (Result.Unneeded(_), _) =>
        Unsettled(e, Undrafted(e.turn, Outcome.NothingRecalled), "unneeded")
      case (Result.Failed(failure), TurnOffer.Root.Addressed) =>
        Stopped(s"failed: $failure${e.notes}")
      case (Result.Kept(draft), TurnOffer.Root.Heard | TurnOffer.Root.Named) => Drafted(e, draft)
      case (
            Result.Failed(failure),
            TurnOffer.Root.Heard | TurnOffer.Root.Named | TurnOffer.Root.ByName
          ) =>
        Unsettled(e, Undrafted(e.turn, Outcome.Failed(failure.toString)), s"failed: $failure")
      case (Result.Kept(reply), TurnOffer.Root.Addressed | TurnOffer.Root.ByName) =>
        Replied(e, reply)
    }
  }

  /** A heard-rooted turn that drafted nothing: its outcome kept as what became of its draft. */
  private def undrafting: Flow[Env, (Ended, Undrafted, String), Nothing, String] =
    Flow
      .pass[(Ended, Undrafted, String)]
      .map(_._2)
      .andThen(Flow.step(speechUndrafted))
      .keeping
      .map { case ((e, _, said), settled) =>
        s"$said${e.notes}${settled.fold(n => s"; not recorded: $n", _ => "")}"
      }

  /** A heard-rooted turn's draft settled: judged when unprompted, then `record-speech`; a
    * posted one summarised.
    */
  private def drafted: Flow[Env, (Ended, EntryId), Nothing, String] =
    Flow
      .branch((e: (Ended, EntryId)) =>
        if (e._1.at.offer.root == TurnOffer.Root.Named) Left(e._1) else Right(e._1)
      )(
        Flow.pass[Ended].map(DraftSettled(_, Settling.Named)),
        Flow
          .step[Env, Ended, TurnJudge.Judgement](judge)
          .keeping
          .map((e, j) => DraftSettled(e, Settling.Judged(j)))
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
      .map((e, answered) => Summing(e.turn, e.at.routed, answered, e.level))
      .andThen(Flow.attempt(summary))
      .keeping
      .map { case ((e, answered), made) =>
        val said = made.fold(f => s"no summary: $f", id => s"summarised: ${EntryId.value(id)}")
        s"$did: ${EntryId.value(answered)}; $said${e.notes}"
      }

  /** `summarise` then `append-summary`. */
  private def summary: Flow[Env, Summing, TurnFailure, EntryId] =
    Flow
      .decide(summarise)((s, m) => m.map(Summary(s, _)))
      .andThen(Flow.mayEnd(Flow.step(appendSummary)))
}
