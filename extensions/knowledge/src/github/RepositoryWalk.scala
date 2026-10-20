package grit.knowledge.github

import grit.core.visibility.Label
import grit.knowledge.card.{Card, CardSummary, UnitKind, UnitVersion}
import grit.knowledge.ref.CardKey
import grit.knowledge.sync.{Held, Holding, Walk, Walking, Way}

/** One run's walk of a repository ([[Walking]]): depth first by path from the repository's root,
  * `/`. Of the repository it walks the paths `include` names but those under `exclude`, and, of
  * files, those whose extension is one of `extensions`; the directories on the way to an
  * included path are listed and carry no card.
  *
  * A directory whose held card records its tree SHA, made under the source's
  * [[GitHubCards.scope]], is not listed: each card held beneath it is unchanged. One made under
  * another scope is listed, so a changed `include` or `extensions` reaches beneath it. A file is
  * read only when its listed SHA is not its held card's. A directory's card is written from its
  * listing, with no ask, naming the files in scope in it and the directories holding more, of
  * what [[GitHubCards.keeps]] says the source keeps, after its children, and only when each was
  * brought up to date; a directory with no file in scope beneath it, as its children then show,
  * gets no card and is not named in its parent's. An unchanged unit held only at another label,
  * or kept elsewhere than its source's place, is relabelled from the label it is held at.
  *
  * It lists at most `maxUnits` directories in scope (those on the way to an included path
  * besides): at the next read past `perRun` or listing past `maxUnits`, it ends, counting what it
  * left. A directory listed only in part is unwritten (`listed only in part`), and nothing beneath
  * a withheld directory is listed, read or dropped. A unit unwritten or not taken leaves the cards
  * of the directories above it unwritten, so the next run lists those again.
  *
  * It drops the cards of units gone from directories it listed in full (or outside the source
  * now, a directory it lists only as on the way to an included path among them), and of
  * directories it found with no file in scope beneath them, with every card beneath each; the
  * copies at other labels of units it kept current at the floor; and, past `maxUnits` (when it
  * was lowered), the units last in walk order and the directories above them. A `Partial`
  * listing, or a directory an ended walk never reached, drops nothing beneath it.
  */
object RepositoryWalk {

  /** The walk of `cards` for a run reading at most `perRun` files and keeping at `floor`,
    * holding `held`, its source's cards as [[Held.read]] reads them. Its first step lists the
    * repository's root, `/`; when `cards` includes nothing, it has none and holds nothing gone.
    */
  def start(cards: GitHubCards, perRun: Int, floor: Label, held: Vector[Held]): Walk[Listing] =
    Walking.start(
      Walking.Routine[Listing, Task, Own](cards, take(cards, _, _), left(cards, _, _), drops),
      perRun,
      Holding(floor, held),
      if (cards.include.isEmpty) Nil else List(Task.Pass(Root)),
      Own(0, Map.empty, Set.empty, Set.empty, Set.empty)
    )

  private val Root = "/"

  /** What is still to do, in walk order. */
  private enum Task extends caps.Pure {

    /** A unit in scope, as its directory's listing gave it. */
    case Visit(entry: Entry)

    /** A directory on the way to an included path: listed, never carded. */
    case Pass(path: String)

    /** A held unit beneath a directory whose tree is unchanged. */
    case Known(key: CardKey)

    /** The card of the directory `path`, at `version`, holding `children`. */
    case Finish(path: String, version: UnitVersion, children: Vector[Entry])
  }

  /** Its record: directories listed in scope, each full listing's children, those passed, those
    * found empty, and the paths neither up to date nor unwritten.
    */
  private final case class Own(
      lists: Int,
      listed: Map[String, Set[String]],
      passed: Set[String],
      empty: Set[String],
      undone: Set[String]
  ) extends caps.Pure

  private type W = Walking[Listing, Task, Own]

  private def take(cards: GitHubCards, task: Task, w: W): W = task match {
    case Task.Pass(path) => w.list(path, Way.Run)(listed(cards, path, None))
    case Task.Known(key) => known(w, key)
    case Task.Visit(entry) =>
      val key = CardKey.of(cards.corpus, entry.path)
      if (current(cards, w.holding, entry)) {
        entry.kind match {
          case UnitKind.File => known(w, key)
          case UnitKind.Directory =>
            val beneath = w.holding.held
              .filter(h => h.path.startsWith(entry.path + "/"))
              .sortBy(_.path)
              .map(_.key)
              .distinct
            w.push((key +: beneath).map(Task.Known(_)))
        }
      } else if (w.full(key)) refuse(w, entry.path)
      else
        entry.kind match {
          case UnitKind.Directory =>
            if (w.own.lists >= cards.maxUnits) w.leave.end
            else
              w.recording(w.own.copy(lists = w.own.lists + 1))
                .list(entry.path, Way.Run)(listed(cards, entry.path, Some(entry.version)))
          case UnitKind.File =>
            if (w.spent) w.leave.end
            else
              w.read(entry.path, Way.Run, cards.subject(entry.path))(
                Card.file(cards.corpus, entry.path, _, _, _)
              )
        }
    case Task.Finish(path, version, all) =>
      // Each child is brought up to date by now, so whether one keeps anything is known: a
      // file in scope does, a directory does unless found empty here, and a directory held
      // current at this form does (none is written empty).
      val children = all.filterNot(c => w.own.empty.contains(c.path))
      val undone = w.own.undone ++ w.tally.unwritten.map(_.path)
      if (undone.exists(u => u == path || u.startsWith(path + "/")))
        w.recording(w.own.copy(undone = w.own.undone + path))
      else if (children.isEmpty) w.recording(w.own.copy(empty = w.own.empty + path))
      else if (w.full(CardKey.of(cards.corpus, path))) refuse(w, path)
      else
        cards
          .pointer(path)
          .flatMap(pointer =>
            CardSummary
              .of(described(path, cards.keeps, children), Vector.empty)
              .map(Card.directory(cards.corpus, path, version, cards.scope, _, pointer))
          )
          .fold(w.unwritten(path, _), w.write)
  }

  /** A walk once the directory `path` was listed, whatever label it was read at: its children in
    * scope, and those on the way to one, pushed in path order, then its own card when it has one
    * (`version`).
    */
  private def listed(
      cards: GitHubCards,
      path: String,
      version: Option[UnitVersion]
  ): (W, Listing, Label) -> W = (w, listing, _) => {
    val entries = listing match {
      case Listing.Complete(entries) => entries
      case Listing.Partial(shown) => shown
    }
    // Only the directory's own children: a listing naming anything else cannot loop the walk.
    val children = entries
      .filter(e => parent(e.path) == path)
      .filter(e => inScope(cards, e) || passage(cards, e))
      .sortBy(_.path)
    val tasks =
      children.map(e => if (inScope(cards, e)) Task.Visit(e) else Task.Pass(e.path)) ++
        version.map(Task.Finish(path, _, children.filter(inScope(cards, _))))
    val own = w.own.copy(passed = if (version.isEmpty) w.own.passed + path else w.own.passed)
    val pushed = w.push(tasks)
    listing match {
      case Listing.Complete(_) =>
        pushed.recording(own.copy(listed = own.listed.updated(path, children.map(_.path).toSet)))
      case Listing.Partial(_) =>
        if (version.isEmpty) pushed.recording(own.copy(undone = own.undone + path))
        else pushed.recording(own).unwritten(path, "listed only in part")
    }
  }

  /** `w` relabelling the current held unit `key` when it is to be copied, else unchanged. */
  private def known(w: W, key: CardKey): W =
    w.holding.copying(key).fold(w.unchanged(key))(w.relabel(key, _))

  /** `w` with the new unit at `path` not taken, so the directories above it are not written. */
  private def refuse(w: W, path: String): W =
    w.refuse.recording(w.own.copy(undone = w.own.undone + path))

  /** Whether `entry`'s held card is up to date: at its version and, a directory's, made under
    * the source's scope.
    */
  private def current(cards: GitHubCards, holding: Holding, entry: Entry): Boolean = {
    val best = holding.best(CardKey.of(cards.corpus, entry.path))
    best.exists(_.version == entry.version) &&
    (entry.kind == UnitKind.File || best.exists(_.scope.contains(cards.scope)))
  }

  private def inScope(cards: GitHubCards, entry: Entry): Boolean =
    cards.include.exists(i => i.isEmpty || under(entry.path, i)) &&
      !excluded(cards, entry) &&
      (entry.kind == UnitKind.Directory || cards.extensions.isEmpty || {
        val name = entry.path.substring(entry.path.lastIndexOf('/') + 1)
        val dot = name.lastIndexOf('.')
        dot > 0 && cards.extensions.contains(name.substring(dot + 1))
      })

  /** Whether `entry` is a directory on the way to an included path. */
  private def passage(cards: GitHubCards, entry: Entry): Boolean =
    entry.kind == UnitKind.Directory && !excluded(cards, entry) &&
      cards.include.exists(_.startsWith(entry.path + "/"))

  private def excluded(cards: GitHubCards, entry: Entry): Boolean =
    cards.exclude.exists(under(entry.path, _))

  /** Whether `path` is `prefix` or beneath it. */
  private def under(path: String, prefix: String): Boolean =
    path == prefix || path.startsWith(prefix + "/")

  /** Whether `task`, never taken, is a unit seen changed or new, or one to relabel. */
  private def left(cards: GitHubCards, task: Task, w: W): Boolean = task match {
    case Task.Visit(entry) => !current(cards, w.holding, entry)
    case Task.Known(key) => w.holding.copying(key).nonEmpty
    case Task.Finish(_, _, _) => true
    case Task.Pass(_) => false
  }

  /** What it drops, as [[RepositoryWalk]] says. */
  private def drops(w: W): Vector[Held] = {
    val (holding, own) = (w.holding, w.own)
    val gone = holding.held.filter { h =>
      if (w.settled(h.key)) h.label != holding.floor && !w.replaced(h)
      else
        own.passed.contains(h.path) || own.listed.exists { (dir, children) =>
          beneath(h.path, dir) && !children.contains(toward(dir, h.path))
        } || own.empty.exists(e => h.path == e || beneath(h.path, e))
    }
    // Past maxUnits (it was lowered): the units last in walk order go, and the directories
    // above them, so the next run lists those again and takes the units back when there is room.
    w.past(gone, _.split('/').toVector, ancestors)
  }

  /** The paths of the directories above `path`. */
  private def ancestors(path: String): Vector[String] =
    path.split('/').toVector.inits.toVector.drop(1).filter(_.nonEmpty).map(_.mkString("/"))

  private def parent(path: String): String = path.lastIndexOf('/') match {
    case -1 => Root
    case i => path.substring(0, i)
  }

  private def beneath(path: String, dir: String): Boolean =
    if (dir == Root) path != Root else path.startsWith(dir + "/")

  /** The path of `dir`'s child on the way to `path`, beneath it. */
  private def toward(dir: String, path: String): String = {
    val rest = if (dir == Root) path else path.substring(dir.length + 1)
    val first = rest.takeWhile(_ != '/')
    if (dir == Root) first else s"$dir/$first"
  }

  /** What a directory holds, as its card's summary says it: of what its source keeps, its files
    * by name, and the directories holding more; `children` is never empty.
    */
  private def described(path: String, keeps: String, children: Vector[Entry]): String = {
    def name(e: Entry) = e.path.substring(e.path.lastIndexOf('/') + 1)
    val (files, dirs) = children.partition(_.kind == UnitKind.File)
    val inDirs = inWords(dirs.map(d => s"${name(d)}/"))
    val holds = s"The directory $path holds $keeps"
    if (files.isEmpty) s"$holds in $inDirs."
    else if (dirs.isEmpty) s"$holds: ${inWords(files.map(name))}."
    else s"$holds: ${inWords(files.map(name))}, and more in $inDirs."
  }

  /** `names` as a list in words: `a`, `a and b`, `a, b and c`. */
  private def inWords(names: Vector[String]): String =
    if (names.size < 2) names.mkString
    else s"${names.init.mkString(", ")} and ${names.last}"
}
