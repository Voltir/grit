package grit.knowledge.drive

import grit.core.files.Kind
import grit.core.visibility.Label
import grit.knowledge.card.{Card, UnitVersion}
import grit.knowledge.ref.CardKey
import grit.knowledge.sync.{CardCopy, Held, Holding, Walk, Walking, Way}

/** One run's walk of a [[DriveCards]] source ([[Walking]]). It lists the shared drives first,
  * [[Way.Run]]; then each drive [[Way.Through]] that listing, its files read before the next
  * drive; then the folders directly in the drives, each through its drive's listing, in the order
  * listed. A folder within a folder is not listed. A run lists at most `maxUnits` folders: when
  * the drives hold more, the `maxUnits` from index `turn × maxUnits` of them, counted around the
  * end, so runs at consecutive turns list them all within ⌈folders / maxUnits⌉ runs.
  *
  * A listing is the run's to card when [[DriveCards.carding]] of the label it was read at is the
  * floor; of another's it only follows the folders. Of a listing it cards, a file of a kind
  * [[DriveCards.cards]] is read through that listing unless its card held at the floor records its change
  * time and that listing, and its card records the listing and is titled with the file's name. A
  * current card [[Holding.copying]] names a label of is relabelled from it; one whose pieces are
  * due ([[Holding.repiece]]) is read again for them ([[Walking.current]]). Past `perRun` reads a
  * changed or due file is left, and the listings go on.
  *
  * It drops the cards held at the floor, damaged ones too, of files another run's listing
  * showed; of the damaged ones no listing showed, only when every listing it asked for came
  * complete, it listed every folder it found and it left nothing for the next run; of the
  * others no listing showed, only when the listing each records was made and complete; those held at a
  * label not among the source's whose [[DriveCards.carding]] is the floor; and, past `maxUnits`
  * (when it was lowered), the files last by id.
  */
object DriveWalk {

  /** `/`: the path of the shared drives' listing, which no Drive id is. */
  val Drives: String = "/"

  /** The walk of `cards` for a run reading at most `perRun` files and keeping at `floor`,
    * holding `held`, its source's cards as [[Held.read]] reads them, reading again the files of
    * `repiece` for their pieces ([[Holding.repiece]]), at `turn`
    * ([[DriveCards.turn]] of its slot), which picks the folders it lists; the copies in
    * `damaged` held as no card ([[Holding.damaged]]).
    */
  def start(
      cards: DriveCards,
      perRun: Int,
      floor: Label,
      held: Vector[Held],
      turn: Long,
      repiece: Set[CardKey] = Set.empty,
      damaged: Vector[Held.Read.Damaged] = Vector.empty
  ): Walk[DriveListing] =
    Walking.start(
      Walking
        .Routine[DriveListing, Task, Own](
          cards,
          take(cards, turn, _, _),
          (t, _) => left(t),
          drops(cards, _)
        ),
      perRun,
      Holding(floor, held, repiece, damaged),
      List(Task.Shared),
      Own(Vector.empty, Set.empty, Set.empty, Set.empty, Set.empty)
    )

  /** What is still to do, in walk order: the first three list, and [[Task.Folders]], once every
    * drive is listed, picks the folders to list.
    */
  private enum Task extends caps.Pure {
    case Shared
    case Drive(id: String)
    case Folder(id: String, drive: String)
    case Folders
    case Read(entry: DriveEntry, kind: Kind, within: String)
    case Relabel(key: CardKey, from: Label)
    case Repiece(key: CardKey, kind: Kind, within: String)
  }

  /** Its record: the folders found in the drives, the files listings showed, those another run's
    * showed, the listings made complete, and every listing asked for.
    */
  private final case class Own(
      found: Vector[Task.Folder],
      seen: Set[String],
      foreign: Set[String],
      complete: Set[String],
      asked: Set[String]
  ) extends caps.Pure

  private type W = Walking[DriveListing, Task, Own]

  private def take(cards: DriveCards, turn: Long, task: Task, w: W): W = task match {
    case Task.Shared =>
      asking(w, Drives).list(Drives, Way.Run) { (w, listing, _) =>
        val drives = listing.entries.filter(_.kind == DriveEntry.Kind.Drive)
        val complete = if (listing.complete) w.own.complete + Drives else w.own.complete
        w.recording(w.own.copy(complete = complete))
          .push(drives.map(e => Task.Drive(e.id)) :+ Task.Folders)
      }
    case Task.Drive(id) =>
      asking(w, id).list(id, Way.Through(Drives)) { (w, listing, at) =>
        val found = listing.entries
          .filter(_.kind == DriveEntry.Kind.Folder)
          .map(f => new Task.Folder(f.id, id))
        files(cards, w.recording(w.own.copy(found = w.own.found ++ found)), id, listing, at)
      }
    case Task.Folder(id, drive) =>
      asking(w, id).list(id, Way.Through(drive))(files(cards, _, id, _, _))
    case Task.Folders => w.push(picked(w.own.found, cards.maxUnits, turn))
    case Task.Relabel(key, from) => w.relabel(key, from)
    case Task.Repiece(key, kind, within) => w.current(key, Way.Through(within), kind)
    case Task.Read(entry, kind, within) =>
      w.read(entry.id, Way.Through(within), cards.subject(entry), kind)(
        Card.file(cards.corpus, entry.id, _, _, _, entry.name, Some(within))
      )
  }

  /** `w` with the listing `id` asked for. */
  private def asking(w: W, id: String): W = w.recording(w.own.copy(asked = w.own.asked + id))

  /** Whether `w` saw every file a listing of its could show: each listing it asked for came
    * complete, it listed every folder it found, and it left nothing for the next run.
    */
  private def whole(cards: DriveCards, w: W): Boolean =
    w.own.asked.subsetOf(w.own.complete) && w.own.found.size <= cards.maxUnits &&
      w.tally.left == 0 && w.tally.unwritten.isEmpty

  /** Of `found`, in the order found, the folders a run at `turn` lists: every one when they are
    * at most `most`; else the `most` from index `turn × most`, counted around the end.
    */
  private def picked(found: Vector[Task.Folder], most: Int, turn: Long): Vector[Task.Folder] =
    if (found.size <= most) found
    else {
      val n = found.size.toLong
      val from = (turn % n) * most % n
      val at = (0 until most).map(i => (from + i) % n).toSet
      found.zipWithIndex.collect { case (f, i) if at(i.toLong) => f }
    }

  /** `w` once the listing of `within` was read at `at`: its files seen, and, when it is this
    * run's to card, those changed pushed to read, those to copy to relabel, the rest unchanged.
    */
  private def files(
      cards: DriveCards,
      w: W,
      within: String,
      listing: DriveListing,
      at: Label
  ): W = {
    // Each file it cards, with the kind its pieces record (DriveEntry.Kind.Read.first).
    val readable = listing.entries
      .filter(e => cards.cards(e.kind))
      .flatMap(e => e.kind.read.map(r => (e, r.first)))
    val own = w.own.copy(
      seen = w.own.seen ++ readable.map(_._1.id),
      complete = if (listing.complete) w.own.complete + within else w.own.complete
    )
    val floor = w.holding.floor
    if (cards.carding(at).contains(floor)) {
      val (current, changed) = readable.partition((e, _) =>
        e.changed.exists(t =>
          w.holding.best(CardKey.of(cards.corpus, e.id)).exists { h =>
            h.label == floor && h.version == UnitVersion.Changed(t) && h.within.contains(within)
          }
        )
      )
      val (relabels, unchanged) = current
        .map((e, kind) => (CardKey.of(cards.corpus, e.id), kind))
        .partitionMap((k, kind) => w.holding.copying(k).map(Task.Relabel(k, _)).toLeft((k, kind)))
      // A read for pieces is a step, so each due file is a task of its own: steps made in a fold
      // here would each replace the last, and the next task taken would replace the last one.
      val (due, kept) = unchanged.partition((k, _) => w.holding.repiece(k))
      val pushed = w
        .recording(own)
        .push(
          relabels ++ changed.map(Task.Read(_, _, within)) ++
            due.map(Task.Repiece(_, _, within))
        )
      kept.foldLeft(pushed)((w, k) => w.unchanged(k._1))
    } else w.recording(own.copy(foreign = own.foreign ++ readable.map(_._1.id)))
  }

  /** Whether `task`, never taken, is a file to read, read again for its pieces, or relabel. */
  private def left(task: Task): Boolean = task match {
    case Task.Read(_, _, _) | Task.Relabel(_, _) | Task.Repiece(_, _, _) => true
    case Task.Shared | Task.Drive(_) | Task.Folder(_, _) | Task.Folders => false
  }

  /** What it drops, as [[DriveWalk]] says. */
  private def drops(cards: DriveCards, w: W): Vector[CardCopy] = {
    val (holding, own) = (w.holding, w.own)
    val gone = holding.held.filter { h =>
      if (h.label == holding.floor)
        own.foreign(h.path) || (h.within.exists(own.complete) && !own.seen(h.path))
      else
        !cards.labels.contains(h.label) &&
        cards.carding(h.label).contains(holding.floor) &&
        !w.replaced(h)
    }
    // A damaged copy records no listing it was shown in: it goes when another run's listing
    // showed its file, or when this run saw every listing whole and none showed it.
    val damaged = holding.damaged.filter(d =>
      d.label == holding.floor && (own.foreign(d.path) || whole(cards, w) && !own.seen(d.path))
    )
    // Past maxUnits (it was lowered): the files last by id go, so the source holds no more.
    w.past(gone, Vector(_), _ => Vector.empty) ++ damaged
  }
}
