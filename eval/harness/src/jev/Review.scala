package grit.eval.harness.jev

import grit.core.classify.Request
import grit.core.id.{TriageRef, TurnRef}
import grit.core.stitch.{StitchReads, Stitching, Tuning}
import grit.core.store.StoreError
import grit.dbos.engine.Reader
import grit.eval.harness.corpus.{Case, CaseId}
import grit.lifecycle.triage.{TriageInput, TriageQuestion, TriageRecipe}

/** A case's inputs as the shipped builders make them, text and all, for a person to read
  * beside the case: the harness never prints them.
  */
object Review {

  /** What triage's question left out of its thread: the `text`, and where in the state's
    * thread it was cut from (`at`, a character index).
    */
  final case class Cut(text: String, at: Int)

  /** Triage's question: the request the shipped builder makes, and what its thread left
    * out, when asked for.
    */
  final case class Triage(request: Request, cut: Option[Cut])

  /** Stitching's question: the request the shipped builder makes, and the state shown live,
    * when a placement keeps one.
    */
  final case class Stitch(request: Request, live: Option[ujson.Value])

  /** A case's questions as rebuilt; `None` where the builder makes none. */
  final case class Shown(id: CaseId, triage: Option[Triage], stitch: Option[Stitch])

  /** `shown` as one line of the review file: `case`, then `triage` and `stitch`, each `null`
    * or an object holding the `request` ([[Request.json]]: its state as the classifier
    * receives it and its questions' words) and its `digest`, triage's with its `cut` (`null`
    * unless asked for) and stitching's with the state shown `live` (`null` when none is
    * kept).
    */
  def line(shown: Shown): ujson.Value = ujson.Obj(
    "case" -> shown.id.written,
    "triage" -> shown.triage.fold[ujson.Value](ujson.Null)(t =>
      ujson.Obj(
        "request" -> Request.json(t.request),
        "digest" -> t.request.digest,
        "cut" -> t.cut.fold[ujson.Value](ujson.Null)(c => ujson.Obj("text" -> c.text, "at" -> c.at))
      )
    ),
    "stitch" -> shown.stitch.fold[ujson.Value](ujson.Null)(s =>
      ujson.Obj(
        "request" -> Request.json(s.request),
        "digest" -> s.request.digest,
        "live" -> s.live.getOrElse(ujson.Null)
      )
    )
  )

  /** `c`'s questions read through `reader`, in the shipped wording, under `c`'s own tuning
    * else `tuning`: triage's ([[TriageInput.read]], with its cut when `more`), and, when `c`
    * was placed live, stitching's ([[Stitching.offered]]) with the state its kept placement
    * shows. `Left` naming the case and what could not be read, by kind.
    */
  def of(reader: Reader^, c: Case, tuning: Tuning, more: Boolean): Either[String, Shown] = {
    val reads = StitchReads(
      reader.entries,
      reader.conversations,
      reader.lifecycle,
      reader.stitches,
      reader.search,
      reader.principals
    )
    val under = c.tuning.getOrElse(tuning)
    def unread(what: String)(e: StoreError): String = s"${c.id.written}: $what unread: ${kind(e)}"
    for {
      ref <- TriageRef
        .fromWorkflowId(c.triage.workflow)
        .toRight(s"${c.id.written}: not a triage workflow id")
      offer <-
        if (c.stitch.isEmpty) Right(None)
        else
          Stitching
            .offered(reads, reader.db, TurnRef(c.conversation, ref.turn), under)
            .left
            .map(unread("stitch"))
      live <-
        if (c.stitch.isEmpty) Right(None)
        else reader.db.read(reader.stitches.placed(c.entry)).left.map(unread("placement"))
    } yield {
      val triage =
        TriageInput
          .read(reads, reader.rooms, reader.db, ref, under, TriageRecipe.Shipped)
          .toOption
          .flatMap { r =>
            TriageQuestion
              .request(TriageQuestion.Wording.Shipped, r.state)
              .map(Triage(_, Option.when(more)(Cut(r.thread.cut, r.thread.at))))
          }
      val stitch =
        offer.flatMap(o => Stitching.request(o).map(Stitch(_, live.map(_.seen.state))))
      Shown(c.id, triage, stitch)
    }
  }

  private def kind(e: StoreError): String = e match {
    case StoreError.DuplicateId(_) => "duplicate id"
    case StoreError.DatabaseError(_) => "database error"
    case StoreError.Invalid(_) => "invalid"
  }
}
