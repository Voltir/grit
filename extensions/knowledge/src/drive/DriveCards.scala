package grit.knowledge.drive

import grit.core.act.Answer
import grit.core.document.Pointer
import grit.core.id.{CorpusName, ScheduleKey}
import grit.core.place.{Place, Service}
import grit.core.tool.ToolName
import grit.core.triage.Corpus
import grit.core.visibility.Label
import grit.knowledge.card.UnitVersion
import grit.knowledge.sync.{Fetched, Held, Source, Step, SyncSchedule}

/** The Docs, Sheets and text files of every shared drive its service reads, top level and one
  * folder deep, listed with `list` and read with `read` (`drive_list` and `drive_read`, as the
  * deployment offers them); each file carded at [[carding]] of the label its listing was read
  * at, by the run of that label; at most `maxUnits` cards. `corpus` names it and `service` is
  * the one its corpus supplies.
  *
  * `labels`, least first, are its runs' floors: one run of each of `syncs` per label. A file in
  * a folder no one has labelled, or above every label, is never carded and never named. Windows
  * draw on its cards where a conversation's scope holds `at` (ADR 0028). The head of each file
  * summarised (8 KB) goes to the catalog's summary model, a third party, whatever its label:
  * declaring the source consents to that.
  */
final case class DriveCards private (
    corpus: CorpusName,
    service: Service,
    list: ToolName,
    read: ToolName,
    labels: Vector[Label],
    maxUnits: Int,
    at: Place,
    syncs: Vector[SyncSchedule]
) extends Source[DriveListing] {

  /** The label a file whose listing was read at `at` is carded at: the least of [[labels]]
    * dominating it; `None` when none does.
    */
  def carding(at: Label): Option[Label] = labels.find(_.dominates(at))

  /** `list` with no arguments for the shared drives ([[DriveWalk.Drives]]), or with `folder` for
    * a drive's or a folder's id; `read` with `file`.
    */
  def call(step: Step.Fetching): (ToolName, ujson.Obj) = step match {
    case Step.List(path, _) if path == DriveWalk.Drives => (list, ujson.Obj())
    case Step.List(path, _) => (list, ujson.Obj("folder" -> path))
    case Step.Read(path, _) => (read, ujson.Obj("file" -> path))
  }

  /** [[DriveListing.read]] of `answer`'s facts: its text is never read. */
  def listing(answer: Answer): Either[String, DriveListing] = DriveListing.read(answer.stated)

  /** [[DriveOpened.read]] of `answer`'s facts, its text at when the file last changed;
    * [[Fetched.Unversioned]] when they are no read's, or state no change time. A read cut for
    * size is kept: its head, all a summary is asked of, is whole.
    */
  def fetched(answer: Answer): Fetched =
    DriveOpened.read(answer.stated) match {
      case Left(why) => Fetched.Unversioned(why)
      case Right(opened) =>
        opened.entry.changed match {
          case Some(at) => Fetched.Text(opened.text, UnitVersion.Changed(at))
          case None => Fetched.Unversioned("Drive did not say when it last changed")
        }
    }

  /** Its entries' names, one a line: a name reaches a card as its title, while an id is only
    * passed back to Drive's own tools, which refuse one that is no id.
    */
  def checked(listing: DriveListing): String = listing.entries.flatMap(_.name).mkString("\n")

  def subject(path: String): String = s"The Drive file with the id `$path`"

  /** `perRun`, plus [[maxUnits]] (the most folders a run lists), plus the shared drives' listing
    * and its most drives, at most `Int.MaxValue`.
    */
  def calls(perRun: Int): Int =
    (perRun.toLong + maxUnits + 1 + DriveCards.DrivesMost).min(Int.MaxValue.toLong).toInt

  def walk(perRun: Int, floor: Label, held: Vector[Held]): DriveWalk =
    DriveWalk.start(this, perRun, floor, held)

  /** The pointer of the file `id`: `read` with `file`; or why not: an id over
    * [[grit.core.document.Pointer.of]]'s 1000 characters of JSON.
    */
  def pointer(id: String): Either[String, Pointer] = Pointer.of(read, "file" -> ujson.Str(id))
}

object DriveCards {

  /** 100: the most shared drives a listing of them gives (Drive's tools list no more). */
  val DrivesMost: Int = 100

  /** These, `labels` least first, or why not: `corpus` supplies no service; `labels` empty,
    * holding one twice or one holding `unmapped`, or not a chain (two of them neither dominating
    * the other); `maxUnits` under 1; no `syncs`, or two of one key.
    */
  def of(
      corpus: Corpus,
      list: ToolName,
      read: ToolName,
      labels: Vector[Label],
      maxUnits: Int,
      at: Place,
      syncs: Vector[SyncSchedule]
  ): Either[String, DriveCards] =
    for {
      service <- corpus.supplies.toRight(
        s"the corpus ${CorpusName.value(corpus.name)} supplies no service"
      )
      _ <- Either.cond(labels.nonEmpty, (), "a Drive source has at least one label")
      _ <- Either.cond(
        labels.distinct.size == labels.size,
        (),
        "a Drive source's labels are each given once"
      )
      _ <- labels
        .find(_.dominates(Label.Unmapped))
        .fold(Right(()))(l =>
          Left(s"a Drive source's label is never one holding what no one labelled: ${Label.written(l)}")
        )
      _ <- labels
        .combinations(2)
        .collectFirst {
          case Vector(a, b) if !a.dominates(b) && !b.dominates(a) => (a, b)
        }
        .fold(Right(()))((a, b) =>
          Left(
            s"a Drive source's labels are a chain, each dominating the one below: " +
              s"${Label.written(a)} and ${Label.written(b)} are not"
          )
        )
      _ <- Either.cond(maxUnits >= 1, (), s"maxUnits is at least 1, not $maxUnits")
      _ <- Either.cond(syncs.nonEmpty, (), "a source has at least one sync schedule")
      _ <- syncs
        .map(_.key)
        .diff(syncs.map(_.key).distinct)
        .headOption
        .fold(Right(()))(key =>
          Left(s"a source's sync schedules have one key each: ${ScheduleKey.value(key)} is two")
        )
    } yield new DriveCards(
      corpus.name,
      service,
      list,
      read,
      labels.sortWith((a, b) => a != b && b.dominates(a)),
      maxUnits,
      at,
      syncs
    )
}
