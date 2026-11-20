package grit.knowledge.topic

import java.time.{Instant, ZoneId}

import grit.core.act.{Dreaming, MoveError, MoveLimits, MoveName, Occasioned, Sorting}
import grit.core.document.DocumentKeeper
import grit.core.durable.Journaled
import grit.core.flow.{Tainted, Trusted}
import grit.core.id.JobName
import grit.core.job.{DreamingJob, JobRun}
import grit.core.speech.SpeechJson
import grit.core.store.{StoreError, Tx}
import grit.knowledge.ref.TopicKey

/** A run a topics run booked for one meeting names its occasion ([[MeetingsAhead.occasion]]);
  * none when the meeting has moved, is no longer [[MeetingsAhead.qualifying]] in its room or
  * names fewer than two people, or, its topic written since the booking, is judged settled.
  */
final class MeetingAhead(zone: ZoneId) extends DreamingJob[MeetingsAhead.Due] {

  val name: JobName = MeetingAhead.Name

  val version: Int = 1

  override val limits: MoveLimits = MoveLimits.of(0, 0, 1, 1).getOrElse(MoveLimits.Zero)

  def write(params: MeetingsAhead.Due): ujson.Value = MeetingsAhead.writeDue(params)

  def read(params: ujson.Value): Either[String, MeetingsAhead.Due] = MeetingsAhead.dueOf(params)

  def run(run: Trusted[JobRun[MeetingsAhead.Due]], moves: Dreaming^): String = {
    val booked = run.extract.params
    moves.scan(MeetingAhead.Scan, run.tainted.map(_.params))((d, keeper, _, _, at) =>
      MeetingAhead.again(d, keeper, zone, at)
    ) match {
      case Left(e) => s"Meeting ahead: not read (${MoveError.said(e)})."
      case Right(MeetingAhead.Again.Gone) => "Meeting ahead: no longer due."
      case Right(MeetingAhead.Again.Still(due)) =>
        val believed: Either[String, Tainted[Unit]] =
          if (due.version == booked.version) Right(run.tainted.map(_ => ()))
          else
            moves.sort(
              MeetingAhead.Sort,
              Dreaming.Item.Document(due.version),
              Sorting.Among(Vector.empty, Vector.empty, 0),
              run.tainted.map(_ => MeetingsAhead.Unsettled)
            ) match {
              case Left(e) => Left(s"Meeting ahead: failed (${MoveError.said(e)}).")
              case Right(None) => Left("Meeting ahead: failed (unsorted).")
              case Right(Some(sorted)) =>
                val answer = sorted.asked.reply
                val p = Trusted.asIs(answer).extract.probability(MeetingsAhead.Judged.Unsettled)
                if (p < MeetingsAhead.YesAt) Left("Meeting ahead: judged settled.")
                else Right(answer.map(_ => ()))
            }
        believed.fold(
          identity,
          from =>
            MeetingAhead.said(
              moves.occasion(MeetingAhead.Occasion, from.map(_ => MeetingsAhead.occasion(due)))
            )
        )
    }
  }
}

object MeetingAhead {

  val Name: JobName = JobName.literal("meeting-ahead")

  private val Scan: MoveName = MoveName.literal("ahead")
  private val Sort: MoveName = MoveName.literal("ahead-sort")
  private val Occasion: MoveName = MoveName.literal("occasion")

  /** `booked`'s meeting as its topic now stands, read at the transaction's floor; none when it
    * is no longer due in its room, names fewer than two people, or lapses at another instant.
    */
  private def again(
      booked: MeetingsAhead.Due,
      keeper: DocumentKeeper,
      zone: ZoneId,
      at: Instant
  )(using tx: Tx^): Either[StoreError, Again] =
    keeper
      .current(TopicKey.doc(booked.topic), Tx.floor(tx))
      .map(
        _.flatMap(MeetingsAhead.qualifying(_, Vector(booked.room), zone, at))
          .collect { case (due, true) if due.until == booked.until => Again.Still(due) }
          .getOrElse(Again.Gone)
      )

  private enum Again extends caps.Pure {
    case Still(due: MeetingsAhead.Due)
    case Gone
  }

  private given Journaled[Again] = Journaled.json(
    {
      case Again.Still(d) => MeetingsAhead.writeDue(d)
      case Again.Gone => ujson.Null
    },
    v => if (v.isNull) Right(Again.Gone) else MeetingsAhead.dueOf(v).map(Again.Still(_))
  )

  private def said(o: Either[MoveError, Occasioned]): String = o match {
    case Left(e) => s"Meeting ahead: failed (${MoveError.said(e)})."
    case Right(Occasioned.Started(_)) => "Meeting ahead: started."
    case Right(Occasioned.Held(_, why)) => s"Meeting ahead: held (${SpeechJson.silenceName(why)})."
    case Right(Occasioned.Already) => "Meeting ahead: already noted."
    case Right(Occasioned.Mislabelled(_)) => "Meeting ahead: held (mislabelled)."
    case Right(Occasioned.Unread(_)) => "Meeting ahead: held (unread)."
    case Right(Occasioned.Unstarted(_, why)) => s"Meeting ahead: failed ($why)."
  }
}
