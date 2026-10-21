package grit.lifecycle.triage

import scala.collection.immutable.VectorMap

import grit.core.document.DocumentSearch
import grit.core.id.{EntryId, TurnRef, TurnSeq}
import grit.core.message.Message
import grit.core.place.Place
import grit.core.recipe.{Pool, RoomReads, Section}
import grit.core.stitch.{Along, StitchReads, Stitching, Strand, Tuning}
import grit.core.store.{Conversation, Entry, Focus, Payload, Position, Reads, Speakers, StoreError}
import grit.lifecycle.transcript.PeriodTranscript

/** How triage's question is built from the store. */
object TriageInput {

  /** A person's message's question as [[read]] builds it: the message's entry, the
    * [[TriageQuestion.State]] it is asked about, the focus it was said at, whose pool the
    * state's sections are, its conversation's place ([[grit.core.store.Origin.place]]; `None`
    * when the conversation is not found), and whether it was `heard`
    * ([[grit.core.store.Payload.Heard]]), not said to grit.
    */
  final case class Read private[triage] (
      entry: EntryId,
      state: TriageQuestion.State,
      focus: Focus,
      place: Option[Place],
      heard: Boolean
  )

  /** [[build]], with the message's focus, place and how it was said ([[Read]]). */
  def read(
      reads: StitchReads,
      rooms: RoomReads,
      documents: DocumentSearch,
      db: Reads^,
      turn: TurnRef,
      tuning: Tuning,
      recipe: TriageRecipe
  ): Either[String, Read] =
    // The `ask` step journals these Left strings: their words must not change (ADR 0004).
    for {
      all <- db
        .read(reads.entries.list(turn.conversationId))
        .left
        .map(e => s"thread unread: ${describe(e)}")
      heard <- all
        .collectFirst {
          case e @ Entry(_, _, seq, _, _, Payload.Heard(_) | Payload.Message(Message.User(_)), _, _)
              if seq == turn.turnSeq =>
            e
        }
        .toRight(s"no heard message at turn ${TurnSeq.value(turn.turnSeq)}")
      // The strand before the heard message, read from the horizon before the thread began.
      read <- db
        .read { (tx: grit.core.store.Tx^) ?=>
          Along.read(
            reads.stitches,
            reads.conversations,
            turn.conversationId,
            all,
            tuning.horizon,
            heard.createdAt
          )
        }
        .left
        .map(e => s"strand unread: ${describe(e)}")
      Along.Read(conversation, strand) = read
      position =
        if (all.minByOption(_.seq).exists(_.id == heard.id)) Position.Opening else Position.Reply
      focus = conversation.fold(Focus.Focused)(_.origin.focus(position))
      before = all.filter(_.seq < heard.seq)
      sections <- pool(
        rooms,
        reads,
        documents,
        db,
        recipe.at(focus),
        conversation,
        strand,
        heard,
        before
      )
    } yield {
      // Unread names are no names: each line is then its role's.
      val names = PeriodTranscript
        .speakers(db, reads.principals, (before :+ heard) ++ strand.shown)
        .getOrElse(Speakers.none)
      val text = heard.payload.said.getOrElse("")
      val thread = Stitching.fit(
        Stitching.excerpt(strand.opening, strand.said, names, tuning.strandChars),
        PeriodTranscript.lines(before, names),
        "\n\n",
        TriageQuestion.ThreadChars
      )
      Read(
        heard.id,
        TriageQuestion.State(
          text,
          names.of(heard.id).getOrElse("Someone"),
          heard.createdAt,
          thread,
          sections
        ),
        focus,
        conversation.map(_.origin.place),
        heard.payload match {
          case Payload.Heard(_) => true
          case _ => false
        }
      )
    }

  /** The person's message that is `turn`'s first, heard or said to grit, and the
    * [[TriageQuestion.State]] it is asked about, the same for either: its text, its author's name ("Someone" when not known), its thread as the store
    * stands now, the strand it joins first (read from `tuning.horizon` before its
    * conversation began, at most `tuning.strandChars`; its entries
    * said before the message, its links as they stand now), then its conversation's messages
    * before it, within [[TriageQuestion.ThreadChars]]; and the sections `recipe`'s pool shows
    * at the focus it was said at ([[grit.core.store.Origin.focus]]: its conversation's first
    * message is its `Opening`; `Focused` when the conversation is not found), from what its
    * room held when it was said, read through `rooms`, and from the
    * records `documents` finds near it, both through `db` ([[Pool.read]]; none when the
    * conversation is not found). Why not, when the thread, strand or room cannot
    * be read or the turn holds no person's message; these words are journaled.
    */
  def build(
      reads: StitchReads,
      rooms: RoomReads,
      documents: DocumentSearch,
      db: Reads^,
      turn: TurnRef,
      tuning: Tuning,
      recipe: TriageRecipe
  ): Either[String, (EntryId, TriageQuestion.State)] =
    read(reads, rooms, documents, db, turn, tuning, recipe).map(r => (r.entry, r.state))

  /** What `pool` shows for `heard` in `conversation` after `before`
    * ([[Pool.read]]); nothing, opening no transaction, for a pool without sources or a
    * conversation not found.
    */
  private def pool(
      rooms: RoomReads,
      reads: StitchReads,
      documents: DocumentSearch,
      db: Reads^,
      pool: Pool,
      conversation: Option[Conversation],
      strand: Strand.Read,
      heard: Entry,
      before: Vector[Entry]
  ): Either[String, VectorMap[Section, String]] =
    conversation match {
      case Some(c) if pool.sources.nonEmpty =>
        db.read(
          Pool.read(
            pool,
            rooms,
            reads.stitches,
            reads.principals,
            documents,
            c,
            strand,
            heard,
            before
          )
        ).left
          .map(e => s"pool unread: ${describe(e)}")
      case _ => Right(VectorMap.empty)
    }

  /** [[read]] of the heard message that is `turn`'s; why not as [[read]] says, or, for a
    * message said to grit, as for no message. These words are journaled.
    */
  private[lifecycle] def heard(
      reads: StitchReads,
      rooms: RoomReads,
      documents: DocumentSearch,
      db: Reads^,
      turn: TurnRef,
      tuning: Tuning,
      recipe: TriageRecipe
  ): Either[String, Read] =
    read(reads, rooms, documents, db, turn, tuning, recipe).filterOrElse(
      _.heard,
      s"no heard message at turn ${TurnSeq.value(turn.turnSeq)}"
    )

  /** `error` in the words triage's journal keeps. */
  private[triage] def describe(error: StoreError): String = error match {
    case StoreError.DuplicateId(id) => s"entry ${EntryId.value(id)} already exists"
    case StoreError.DatabaseError(cause) => cause
    case StoreError.Invalid(cause) => cause
  }
}
