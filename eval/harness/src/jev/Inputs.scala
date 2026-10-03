package grit.eval.harness.jev

import grit.core.classify.{Classifier, Request}
import grit.core.id.{TriageRef, TurnRef}
import grit.core.stitch.{Offer, StitchReads, Stitching, Tuning}
import grit.core.store.StoreError
import grit.dbos.engine.Reader
import grit.eval.harness.corpus.{Case, Digest}
import grit.lifecycle.triage.{TriageInput, TriageQuestion, TriageRecipe}

/** One of a case's questions, as the shipped call asks it. */
enum Asking {

  /** Triage's: `state`, in `wording`. */
  case Triage(state: TriageQuestion.State, wording: TriageQuestion.Wording)

  /** Stitching's: where `offer`'s message goes, under `tuning`. */
  case Stitch(offer: Offer, tuning: Tuning)
}

object Asking {

  /** `asking` put to `classifier` through the shipped call ([[TriageQuestion.judge]],
    * [[Stitching.place]]); what it makes of the answer is dropped.
    */
  def ask(asking: Asking, classifier: Classifier^): Unit = asking match {
    case Triage(state, wording) => val _ = TriageQuestion.judge(classifier, wording, state)
    case Stitch(offer, tuning) => val _ = Stitching.place(classifier, offer, tuning)
  }
}

/** A question as rebuilt: how it is asked, the request the shipped builder makes for it, and
  * the digest of the state it shows.
  */
final case class Posed(asking: Asking, request: Request, state: Digest)

/** A case's questions as the shipped builders make them now: triage's (`None` when the
  * builder refuses it) and, for a case whose placement was kept live, stitching's (`None` when
  * nothing is offered now).
  */
final case class Rebuilt(triage: Option[Posed], stitch: Option[Posed])

/** How a question's rebuilt state compares to the one its corpus captured. */
enum Drift {
  case Same

  /** Both were built, and their states differ. */
  case Changed

  /** It was built at capture and is not now. */
  case Unbuilt

  /** It is built now and was not at capture. */
  case Built

  /** Neither was built. */
  case Neither
}

object Drift {

  /** How `now`, a rebuilt state's digest, compares to `captured`. */
  def of(captured: Option[Digest], now: Option[Digest]): Drift = (captured, now) match {
    case (Some(a), Some(b)) => if (a == b) Same else Changed
    case (Some(_), None) => Unbuilt
    case (None, Some(_)) => Built
    case (None, None) => Neither
  }
}

/** A corpus's cases' questions, rebuilt through the shipped builders. */
object Inputs {

  /** `c`'s questions as `reader`'s database stands, under `variant`: triage's
    * ([[TriageInput.build]], then [[TriageQuestion.request]] in the variant's wording), and,
    * when `c` was placed live, stitching's ([[Stitching.offered]], then
    * [[Stitching.request]]), each under the variant's tuning, else `c`'s own, else `tuning`.
    * `Left` naming what could not be read: the case's triage workflow id, or the store, by
    * kind.
    */
  def rebuild(
      reader: Reader^,
      c: Case,
      variant: Variant,
      tuning: Tuning
  ): Either[String, Rebuilt] = {
    val reads = StitchReads(
      reader.entries,
      reader.conversations,
      reader.lifecycle,
      reader.stitches,
      reader.search,
      reader.principals
    )
    val under = Variant.tuning(variant).orElse(c.tuning).getOrElse(tuning)
    val wording = Variant.wording(variant)
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
            .map(e => s"${c.id.written}: stitch unread: ${kind(e)}")
    } yield {
      val triage =
        TriageInput
          .build(reads, reader.rooms, reader.db, ref, under, TriageRecipe.Shipped)
          .toOption
          .flatMap { (_, state) =>
            TriageQuestion
              .request(wording, state)
              .map(r => Posed(Asking.Triage(state, wording), r, Digest.json(r.state)))
          }
      val stitch = offer.flatMap(o =>
        Stitching
          .request(o)
          .map(r => Posed(Asking.Stitch(o, under), r, Digest.json(Stitching.shown(o))))
      )
      Rebuilt(triage, stitch)
    }
  }

  private def kind(e: StoreError): String = e match {
    case StoreError.DuplicateId(_) => "duplicate id"
    case StoreError.DatabaseError(_) => "database error"
    case StoreError.Invalid(_) => "invalid"
  }
}
