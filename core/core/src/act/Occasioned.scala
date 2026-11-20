package grit.core.act

import java.time.Instant

import grit.core.document.DocumentKeeper
import grit.core.id.{DocumentVersionId, TurnRef}
import grit.core.occasion.Occasion
import grit.core.place.Place
import grit.core.speech.{
  Decision,
  Ledger,
  Occasions,
  Outcome,
  Silence,
  Speaking,
  Speech,
  SpeechStore
}
import grit.core.spend.Budget
import grit.core.spend.Spending
import grit.core.store.{Origin, StoreError, Tx}
import grit.core.visibility.Integrity

/** What [[Keeping.occasion]] made of an occasion. */
enum Occasioned extends caps.Pure {

  /** Its turn, `turn`, was decided to be drafted, and started. */
  case Started(turn: TurnRef)

  /** Held for `why`: no turn runs. `turn` is `None` when nothing was decided for its key, held by
    * the move's first check (off, no address, a quiet room, below Outside, lapsed), which leaves
    * the key free for a later run; else the turn its entry began, its key spent.
    */
  case Held(turn: Option[TurnRef], why: Silence)

  /** Decided already for its key in its room's top-level conversation, by another run: nothing
    * is started.
    */
  case Already

  /** `room`'s label is not the run's: nothing is recorded. */
  case Mislabelled(room: Place)

  /** `about` is no current document of the acting plugin: nothing is recorded. */
  case Unread(about: DocumentVersionId)

  /** Its turn, `turn`, was decided to be drafted and could not be started, for `why`: its
    * decision is settled failed, and its key is spent.
    */
  case Unstarted(turn: TurnRef, why: String)
}

private[grit] object Occasioned {

  /** What the move's first check found of `occasion`, believed at `believed`, at `now`, before
    * anything is recorded: why it ends there (`Left`: `Mislabelled` when its room's label is not
    * this transaction's floor, `Unread` when `keeper` does not hold its document, `Held(None, _)`
    * as [[Speech.unrecorded]] holds it under `speaking`, believed at no more than its document,
    * its address `None` when no reach with a room address was kept in its room within
    * [[Occasions.Heard]] or the room is quiet); else the top level of its room, its reply address
    * there, and what it is believed at: `believed` met with its document's belief.
    */
  def found(
      occasion: Occasion,
      keeper: DocumentKeeper,
      speech: SpeechStore,
      speaking: Speaking,
      believed: Integrity,
      now: Instant
  )(using tx: Tx^): Either[StoreError, Either[Occasioned, (Origin, String, Integrity)]] =
    if (Tx.roomLabel(occasion.room) != Tx.floor(tx))
      Right(Left(Mislabelled(occasion.room)))
    else
      keeper.held(occasion.about).flatMap {
        case None => Right(Left(Unread(occasion.about)))
        case Some(document) =>
          val most = believed.meet(document)
          speech
            .addressIn(occasion.room, now.minusNanos(Occasions.Heard.toNanos))
            .map { address =>
              val reached = address.filterNot(_ => Tx.quiet(occasion.room))
              Speech
                .unrecorded(speaking, occasion, reached.map(_._2), most, now)
                .map(why => Held(None, why))
                .toLeft(reached)
                .flatMap(_.toRight(Held(None, Silence.NoAddress)))
                .map((origin, to) => (origin, to, most))
            }
      }

  /** `occasion`, whose entry began `turn` in a conversation with reply address `to`, believed
    * at `believed`, decided at `now` as [[Speech.occasion]] decides under `speaking` and
    * `budget` over the ledger `speech` and `spending` give, and kept on `turn` in its room: what
    * the move ends as (`Left`: `Already` when a decision was kept for `turn` before, `Held` with
    * `turn` when it is held), or `Right` when its turn is to start.
    */
  def decided(
      occasion: Occasion,
      turn: TurnRef,
      to: String,
      believed: Integrity,
      speaking: Speaking,
      budget: Budget,
      speech: SpeechStore,
      spending: Spending,
      now: Instant
  )(using Tx^): Either[StoreError, Either[Occasioned, Unit]] =
    for {
      ledger <- Ledger.read(
        speech,
        spending,
        Speech.limitsOf(speaking),
        budget,
        now
      )
      decision = Speech.occasion(
        speaking,
        turn,
        occasion,
        Some(to),
        believed,
        ledger,
        budget,
        now
      )
      kept <- speech.occasioned(turn, occasion.room, decision, now)
      ended <- decision match {
        case _ if !kept => Right(Left(Already))
        case Decision.Held(why) => Right(Left(Held(Some(turn), why)))
        case Decision.Drafting(_, _) => Right(Right(()))
        case Decision.Answering(_, _) => Left(StoreError.Invalid("an occasion is never answered"))
      }
    } yield ended

  /** `turn`, an occasion's drafting turn that could not be started for `why`, its decision
    * settled at `at` as a heard message's is ([[grit.core.speech.Outcome.Failed]]).
    */
  def unstarted(turn: TurnRef, why: String, speech: SpeechStore, at: Instant)(using
      Tx^
  ): Either[StoreError, Occasioned] =
    speech
      .drafted(turn, Outcome.Failed(s"not started: $why"), None, at)
      .map(_ => Unstarted(turn, why))
}
