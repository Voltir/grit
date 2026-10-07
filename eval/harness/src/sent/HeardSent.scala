package grit.eval.harness.sent

import grit.core.classify.Request
import grit.core.id.{EntryId, QuestionName, TurnRef}
import grit.core.persona.Persona
import grit.core.stitch.{Placed, StitchReads, Stitching, Tuning}
import grit.core.store.{StoreError, Tx}
import grit.core.triage.{Corpora, Tags}
import grit.dbos.engine.Reader
import grit.eval.harness.corpus.Capture
import grit.lifecycle.triage.{TriageInput, TriageQuestions, TriageRecipe}

/** What Jev was asked of a heard message, and what it answered: the `entry`, its `turn`; the
  * stitch placement kept for its conversation's first message (`stitch`: the state the
  * classifier was sent, as recorded, and the tuning in force) with the stitch request this
  * build words for it (`stitchAsked`, `None` when the message is not its conversation's
  * first or nothing is offered now) and whether that request's state is the recorded one
  * (`stitchSame`); triage's request rebuilt through the shipped builder
  * ([[TriageInput.read]] under the shipped recipe, then [[TriageQuestions.shipped]] for
  * `persona` with `sources`), since triage keeps none (`triage`, or why it was not built),
  * the names its questions have (`asked`), and the tags triage kept (`tags`).
  */
final case class HeardSent(
    entry: EntryId,
    turn: TurnRef,
    stitch: Option[Placed],
    stitchAsked: Option[Request],
    stitchSame: Option[Boolean],
    triage: Either[String, Request],
    asked: Vector[QuestionName],
    tags: Option[Tags],
    persona: Persona,
    sources: Corpora
)

object HeardSent {

  /** The heard message kept as `entry`, read through `reader`, its triage request worded for
    * `persona` with `sources` (the deployment's, which grit's database does not keep). `Left`
    * when no such entry is kept or a store cannot be read; a triage request the builder
    * refuses is kept as its reason.
    */
  def read(
      reader: Reader^,
      entry: EntryId,
      persona: Persona,
      sources: Corpora
  ): Either[String, HeardSent] = {
    val id = EntryId.value(entry)
    def read[A](what: String)(body: (Tx^) ?=> Either[StoreError, A]): Either[String, A] =
      reader.db.read(body).left.map(e => s"$what of $id unread: ${Capture.kind(e)}")
    val reads = StitchReads(
      reader.entries,
      reader.conversations,
      reader.lifecycle,
      reader.stitches,
      reader.search,
      reader.principals
    )
    for {
      heard <- read("entry")(reader.entries.get(entry)).flatMap(_.toRight(s"no entry $id"))
      turn = TurnRef(heard.conversationId, heard.turnSeq)
      opening <- read("conversation")(
        reader.entries
          .list(heard.conversationId)
          .map(_.minByOption(e => grit.core.id.EntrySeq.value(e.seq)))
      )
      placed <-
        if (opening.exists(_.id == entry)) read("stitch")(reader.stitches.placed(entry))
        else Right(None)
      tuning = placed.fold(Tuning.Default)(_.seen.tuning)
      offer <-
        if (placed.isEmpty) Right(None)
        else
          Stitching
            .offered(reads, reader.db, turn, tuning)
            .left
            .map(e => s"stitch of $id unread: ${Capture.kind(e)}")
      tags <- read("tags")(reader.triage.of(Vector(entry))).map(_.get(entry))
    } yield {
      val set = TriageQuestions.shipped(persona)
      val triage = TriageInput
        .read(reads, reader.rooms, reader.db, turn, tuning, TriageRecipe.Shipped)
        .map(r => set.request(r.state, sources))
      val stitchAsked = offer.flatMap(Stitching.request)
      HeardSent(
        entry,
        turn,
        placed,
        stitchAsked,
        for { p <- placed; o <- offer } yield Stitching.shown(o) == p.seen.state,
        triage,
        set.questions(sources).keys.toVector,
        tags,
        persona,
        sources
      )
    }
  }

}
