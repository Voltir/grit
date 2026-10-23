package grit.knowledge.sync

import java.time.Instant

import grit.core.act.Answer
import grit.core.document.Pointer
import grit.core.id.CorpusName
import grit.core.place.{Place, Service}
import grit.core.tool.ToolName
import grit.core.visibility.Label
import grit.knowledge.card.{Card, UnitVersion}
import grit.knowledge.ref.CardKey

/** A card source of two folders for [[Sync]]'s tests of a unit's own belief: `/` lists `a` and
  * `b`, `a` the file `a/x` and `b` the file `b/y`. Its walk lists `/` ([[Way.Run]]), then, for
  * each folder in `order`, lists it through `/`, reads its file through it, summarises the file
  * and writes its card ([[Way.Through]] each). A listing's answer is its children's paths, one a
  * line; a file's, its text, read at [[Folders.Version]]. Each call is `read` with the unit's
  * `path`; the first outcome not of its step's kind stops the walk, saying so.
  */
final class Folders(
    val corpus: CorpusName,
    val service: Service,
    val at: Place,
    order: Vector[String]
) extends Source[Folders.Names] {

  def maxUnits: Int = 10

  def syncs: Vector[SyncSchedule] = Vector.empty

  def call(step: Step.Fetching): (ToolName, ujson.Obj) =
    (Folders.Read, ujson.Obj("path" -> step.path))

  def listing(answer: Answer): Either[String, Folders.Names] =
    Right(Folders.Names(answer.text.linesIterator.toVector))

  def fetched(answer: Answer): Fetched = Fetched.Text(answer.text, Folders.Version)

  def checked(listing: Folders.Names): String = listing.paths.mkString("\n")

  def pointer(path: String): Either[String, Pointer] =
    Pointer.of(Folders.Read, "path" -> ujson.Str(path))

  def besides: Int = 0

  override def calls(perRun: Int): Int = 10

  def walk(
      perRun: Int,
      floor: Label,
      held: Vector[Held],
      due: Instant,
      repiece: Set[CardKey]
  ): Walk[Folders.Names] =
    Folders.Scripted(
      corpus,
      Step.List("/", Way.Run) :: order.toList.flatMap { folder =>
        val file = if (folder == "a") "a/x" else "b/y"
        List(Step.List(folder, Way.Through("/")), Step.Read(file, Way.Through(folder)))
      },
      0,
      None
    )
}

object Folders {

  /** A listing: its children's paths. */
  final case class Names(paths: Vector[String]) extends caps.Pure

  /** `read`: the one tool its calls name. */
  val Read: ToolName = ToolName.of("read").fold(why => throw new AssertionError(why), identity)

  /** The version every file is read at. */
  val Version: UnitVersion = UnitVersion.Changed(Instant.EPOCH)

  /** A walk with `todo` left, `written` cards kept so far, stopped saying `stop` if it was. */
  private final case class Scripted(
      corpus: CorpusName,
      todo: List[Step],
      written: Int,
      stop: Option[String]
  ) extends Walk[Names] {

    def next: Either[Synced, Step] = (stop, todo) match {
      case (None, step :: _) => Right(step)
      case _ => Left(Synced(corpus, written, 0, 0, 0, 0, 0, Vector.empty, stop))
    }

    def after(outcome: Outcome[Names]): Scripted = (todo, outcome) match {
      case (Step.List(_, _) :: rest, Outcome.Listed(_, _)) => copy(todo = rest)
      case (Step.Read(path, _) :: rest, Outcome.Fetched(Fetched.Text(text, version), _)) =>
        copy(todo = Step.Summarise(path, s"The file `$path`", text, version) :: rest)
      case (Step.Summarise(path, _, _, version) :: rest, Outcome.Summarised(summary)) =>
        val pointer = Pointer.of(Read, "path" -> ujson.Str(path))
        pointer.fold(
          why => copy(stop = Some(why)),
          p => copy(todo = Step.Write(Card.file(corpus, path, version, summary, p), None) :: rest)
        )
      case (Step.Write(_, _) :: rest, Outcome.Kept) => copy(todo = rest, written = written + 1)
      case (_, Outcome.NotTaken(why)) => copy(stop = Some(why))
      case (_, Outcome.Stopped(why)) => copy(stop = Some(why))
      case (_, other) => copy(stop = Some(s"unexpected $other"))
    }
  }
}
