package grit.lifecycle.triage

import grit.core.id.{EntryId, TriageRef, TurnSeq}
import grit.core.recipe.{Candidates, Pool}
import grit.core.stitch.{Along, StitchReads, Stitching, Strand, Tuning}
import grit.core.store.{Db, Entry, Focus, Payload, Position, Speakers, StoreError}
import grit.lifecycle.transcript.PeriodTranscript

/** How triage's question is built from the store. */
object TriageInput {

  /** A heard message's question as [[read]] builds it: the message's entry, the
    * [[TriageQuestion.State]] it is asked about, its thread by part (what it shows of the
    * strand and of its conversation's messages, and the start of those it leaves out), whose
    * text is the state's thread, and the focus it was said at, whose pool the state's sections
    * are.
    */
  final case class Read private[triage] (
      entry: EntryId,
      state: TriageQuestion.State,
      thread: Stitching.Fitted,
      focus: Focus
  )

  /** [[build]], with the state's thread by part and the message's focus. */
  def read(
      reads: StitchReads,
      db: Db^,
      triage: TriageRef,
      tuning: Tuning,
      recipe: TriageRecipe
  ): Either[String, Read] =
    // The `ask` step journals these Left strings: their words must not change (ADR 0004).
    for {
      all <- db
        .read(reads.entries.list(triage.period.conversationId))
        .left
        .map(e => s"thread unread: ${describe(e)}")
      heard <- all
        .collectFirst {
          case e @ Entry(_, _, turn, _, _, Payload.Heard(_), _) if turn == triage.turn => e
        }
        .toRight(s"no heard message at turn ${TurnSeq.value(triage.turn)}")
      // The strand before the heard message, read from the horizon before the thread began.
      read <- db
        .read { (tx: grit.core.store.Tx^) ?=>
          for {
            conversation <- reads.conversations.get(triage.period.conversationId)
            settings <- reads.lifecycle.current()
            read <- conversation.fold[Either[StoreError, Strand.Read]](Right(Strand.Read.empty)) {
              c =>
                val began = all.minByOption(_.seq).fold(heard.createdAt)(_.createdAt)
                Along.read(
                  reads.stitches,
                  c,
                  settings.locality.scope,
                  began.minusNanos(tuning.horizon.toNanos),
                  heard.createdAt
                )
            }
          } yield (read, conversation.map(_.origin))
        }
        .left
        .map(e => s"strand unread: ${describe(e)}")
      (strand, origin) = read
    } yield {
      val before = all.filter(_.seq < heard.seq)
      // Unread names are no names: each line is then its role's.
      val names = PeriodTranscript
        .speakers(db, reads.principals, (before :+ heard) ++ strand.shown)
        .getOrElse(Speakers.none)
      val text = heard.payload match {
        case Payload.Heard(t) => t
        case _ => ""
      }
      val thread = Stitching.fit(
        Stitching.excerpt(strand.opening, strand.said, names, tuning.strandChars),
        PeriodTranscript.of(before, names),
        TriageQuestion.ThreadChars
      )
      val position =
        if (all.minByOption(_.seq).exists(_.id == heard.id)) Position.Opening else Position.Reply
      val focus = origin.fold(Focus.Focused)(_.focus(position))
      // The room is not read yet, so a pool has nothing to choose among.
      val sections = Pool.show(recipe.at(focus), Candidates.none, heard.createdAt)
      Read(
        heard.id,
        TriageQuestion.State(text, names.of(heard.id).getOrElse("Someone"), thread.text, sections),
        thread,
        focus
      )
    }

  /** The heard message that is `triage`'s turn, and the [[TriageQuestion.State]] it is asked
    * about as the store stands now: its text, its author's name ("Someone" when not known),
    * its thread, the strand it joins first (read from `tuning.horizon` before its
    * conversation began, in the scope in force, at most `tuning.strandChars`), then its
    * conversation's messages before it, within [[TriageQuestion.ThreadChars]], and the
    * sections `recipe`'s pool shows at the focus it was said at ([[grit.core.store.Origin.focus]]: its
    * conversation's first message is its `Opening`; `Focused` when the conversation is not
    * found). Why not, when the thread or strand cannot be read or the turn holds no heard
    * message; these words are journaled.
    */
  def build(
      reads: StitchReads,
      db: Db^,
      triage: TriageRef,
      tuning: Tuning,
      recipe: TriageRecipe
  ): Either[String, (EntryId, TriageQuestion.State)] =
    read(reads, db, triage, tuning, recipe).map(r => (r.entry, r.state))

  /** `error` in the words triage's journal keeps. */
  private[triage] def describe(error: StoreError): String = error match {
    case StoreError.DuplicateId(id) => s"entry ${EntryId.value(id)} already exists"
    case StoreError.DatabaseError(cause) => cause
    case StoreError.Invalid(cause) => cause
  }
}
