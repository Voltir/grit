package grit.knowledge.sync

import java.time.Instant

import grit.core.act.Answer
import grit.core.id.CorpusName
import grit.core.place.{Place, Service}
import grit.core.tool.ToolName
import grit.core.visibility.Label

/** A card source as a sync run reads it, its listings read as `L`. */
trait Source[L <: caps.Pure] extends caps.Pure {

  /** What names it among a plugin's sources, and keys its cards. */
  def corpus: CorpusName

  /** The service its calls go to, the one its corpus supplies. */
  def service: Service

  /** Where its cards are drawn on: where a conversation's scope holds it (ADR 0028). */
  def at: Place

  /** At most how many cards it holds. */
  def maxUnits: Int

  /** Its sync schedules, none two of one key. */
  def syncs: Vector[SyncSchedule]

  /** The tool and arguments of `step`. */
  def call(step: Step.Fetching): (ToolName, ujson.Obj)

  /** What `answer`, a listing, holds, or why it is none. */
  def listing(answer: Answer): Either[String, L]

  /** What `answer`, a read, holds. */
  def fetched(answer: Answer): Fetched

  /** What of `listing` the integrity tree checks when it is believed below `Outside`: what
    * reaches a card or steers the run.
    */
  def checked(listing: L): String

  /** The line telling the summary model what the unit at `path` is. */
  def subject(path: String): String

  /** The most calls a run asking for `perRun` summaries makes. */
  def calls(perRun: Int): Int

  /** This run's walk, keeping at `floor`, holding `held`, its cards as [[Held.read]] reads
    * them, asking for at most `perRun` summaries, its slot due at `due`
    * ([[grit.core.job.JobRun.nominal]]).
    */
  def walk(perRun: Int, floor: Label, held: Vector[Held], due: Instant): Walk[L]
}
