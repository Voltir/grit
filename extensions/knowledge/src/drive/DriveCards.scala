package grit.knowledge.drive

import java.time.Instant

import grit.core.act.Answer
import grit.core.document.Pointer
import grit.core.files.{Formats, Kind as FileKind}
import grit.core.id.{CorpusName, FileHandle, ScheduleKey}
import grit.core.place.{Place, Service}
import grit.core.tool.ToolName
import grit.core.triage.Corpus
import grit.core.visibility.Label
import grit.knowledge.card.UnitVersion
import grit.knowledge.ref.CardKey
import grit.knowledge.sync.{Fetched, Held, Source, Step, SyncSchedule, Walk}

/** The files of every shared drive its service reads that it [[cards]], top level and one
  * folder deep, listed with `list` and read with `read` (`drive_list` and `drive_read`, as the
  * deployment offers them); each file carded at [[carding]] of the label its listing was read
  * at, by the run of that label; at most `maxUnits` cards. `corpus` names it and `service` is
  * the one its corpus supplies; `formatted`, the kinds of file a deployment's format reads that
  * it cards.
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
    syncs: Vector[SyncSchedule],
    formatted: Set[FileKind]
) extends Source[DriveListing] {

  /** Whether it cards a file of `kind`: a Doc, a Sheet or a text file always; a PDF, a Word
    * document or an Excel workbook when [[formatted]] holds its kind, and a Slides deck when it
    * holds PDFs; nothing else.
    */
  def cards(kind: DriveEntry.Kind): Boolean = kind.read.exists {
    case DriveEntry.Kind.Read.Exported => true
    case DriveEntry.Kind.Read.Formatted(k) => formatted(k)
  }

  /** What the summary model is told `entry`, a file, is ([[Step.Summarise]]): a Doc, a Sheet
    * (whose first tab its read gives as comma-separated rows, as Drive exports it as text, when
    * [[formatted]] holds no Excel workbook), a Slides deck, a text file, a PDF, a Word document
    * or an Excel workbook, named as its listing names it, in Google Drive.
    */
  def subject(entry: DriveEntry): String = {
    val kind = entry.kind match {
      case DriveEntry.Kind.Document => "A Doc"
      case DriveEntry.Kind.Spreadsheet => "A Sheet (a spreadsheet)"
      case DriveEntry.Kind.Slides => "A Slides deck"
      case DriveEntry.Kind.Text => "A text file"
      case DriveEntry.Kind.Pdf => "A PDF"
      case DriveEntry.Kind.Word => "A Word document"
      case DriveEntry.Kind.Excel => "An Excel workbook"
      case DriveEntry.Kind.Folder | DriveEntry.Kind.Drive | DriveEntry.Kind.Other(_) => "A file"
    }
    val named = entry.name.fold("")(n => s" named `$n`")
    val rows = entry.kind match {
      case DriveEntry.Kind.Spreadsheet if !formatted(FileKind.Excel) =>
        "; its first tab is given below as comma-separated rows"
      case _ => ""
    }
    s"$kind$named in Google Drive$rows"
  }

  /** The label a file whose listing was read at `at` is carded at: the least of [[labels]]
    * dominating it; `None` when none does.
    */
  def carding(at: Label): Option[Label] = labels.find(_.dominates(at))

  /** `list` with no arguments for the shared drives ([[DriveWalk.Drives]]), or with `folder` for
    * a drive's or a folder's id; `read` with `file`, for its card or its pieces.
    */
  def call(step: Step.Fetching): (ToolName, ujson.Obj) = step match {
    case Step.List(path, _) if path == DriveWalk.Drives => (list, ujson.Obj())
    case Step.List(path, _) => (list, ujson.Obj("folder" -> path))
    case Step.Read(path, _) => (read, ujson.Obj("file" -> path))
    case Step.Piece(path, _) => (read, ujson.Obj("file" -> path))
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

  /** The handle [[DriveOpened.read]] reads of `answer`'s facts: a file a format read. */
  override def keepsFiles: Boolean = true

  override def file(answer: Answer): Option[FileHandle] =
    DriveOpened.read(answer.stated).toOption.flatMap(_.file)

  /** Its entries' names, one a line: a name reaches a card as its title, while an id is only
    * passed back to Drive's own tools, which refuse one that is no id.
    */
  def checked(listing: DriveListing): String = listing.entries.flatMap(_.name).mkString("\n")

  /** Its most shared drives, each listed after the shared drives' listing, its first. */
  def besides: Int = DriveCards.DrivesMost

  /** [[DriveWalk.start]] at the [[turn]] of `due`. */
  def walk(
      perRun: Int,
      floor: Label,
      held: Vector[Held],
      due: Instant,
      repiece: Set[CardKey]
  ): Walk[DriveListing] =
    DriveWalk.start(this, perRun, floor, held, turn(due), repiece)

  /** How many slots of its [[syncs]] fall after [[DriveCards.TurnsFrom]] and at or before `due`:
    * of two runs of its schedules, the later's is one more for each slot between them.
    */
  def turn(due: Instant): Long =
    syncs.map(_.rule.between(DriveCards.TurnsFrom, due).size.toLong).sum

  /** The pointer of the file `id`: `read` with `file`; or why not: an id over
    * [[grit.core.document.Pointer.of]]'s 1000 characters of JSON.
    */
  def pointer(id: String): Either[String, Pointer] = Pointer.of(read, "file" -> ujson.Str(id))
}

object DriveCards {

  /** 100: the most shared drives a listing of them gives (Drive's tools list no more). */
  val DrivesMost: Int = 100

  /** 2026-01-01T00:00:00Z: where [[DriveCards.turn]] starts counting; a slot before it is not
    * counted.
    */
  val TurnsFrom: Instant = Instant.parse("2026-01-01T00:00:00Z")

  /** These, `labels` least first, carding the PDFs (Slides decks among them, as Drive exports
    * them), Word documents and Excel workbooks a format
    * of `formats` (the deployment's) reads, or why not: `corpus` supplies no service; `labels`
    * empty, holding one twice or one holding `unmapped`, or not a chain (two of them neither
    * dominating the other); `maxUnits` under 1; no `syncs`, or two of one key.
    */
  def of(
      corpus: Corpus,
      list: ToolName,
      read: ToolName,
      labels: Vector[Label],
      maxUnits: Int,
      at: Place,
      syncs: Vector[SyncSchedule],
      formats: Formats
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
          Left(
            s"a Drive source's label is never one holding what no one labelled: ${Label.written(l)}"
          )
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
      syncs,
      Set(FileKind.Pdf, FileKind.Word, FileKind.Excel).filter(formats.of(_).nonEmpty)
    )
}
