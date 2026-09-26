package grit.core.period

import grit.core.id.{LineId, PeriodSeq}

/** A section of a conversation's balance, by the key its lines are stored and hashed under. */
enum Section(val key: String) {

  /** What someone is still waiting on: a question unanswered, a task unfinished, something
    * not yet known.
    */
  case Open extends Section("open")

  /** Decisions settled and facts established that later work relies on. */
  case Standing extends Section("standing")

  /** The topics spoken in, each by its name. */
  case Topics extends Section("topics")
}

object Section {

  /** The section keyed `key`; `None` for no section's key. */
  def of(key: String): Option[Section] = Section.values.find(_.key == key)
}

/** A line of a balance: its `text` as first written, added at the close of period `since`,
  * and last added, confirmed or relied on at the close of `touched`, never before `since`.
  */
final case class Line private (
    section: Section,
    text: String,
    since: PeriodSeq,
    touched: PeriodSeq
) {
  def id: LineId = LineId.of(section.key, text)
}

object Line {

  /** The line, or why not: `text` blank, or not already trimmed to one line with single
    * spaces, or `touched` before `since`.
    */
  private[period] def of(
      section: Section,
      text: String,
      since: PeriodSeq,
      touched: PeriodSeq
  ): Either[String, Line] =
    if (text.isEmpty) Left("a line's text is blank")
    else if (normal(text) != text) Left(s"a line's text is not normalised: $text")
    else if (PeriodSeq.value(touched) < PeriodSeq.value(since))
      Left(
        s"a line touched at ${PeriodSeq.value(touched)} before its since ${PeriodSeq.value(since)}"
      )
    else Right(new Line(section, text, since, touched))

  /** `l`'s text in UTF-8 bytes. */
  private[period] def bytes(l: Line): Int = l.text.getBytes("UTF-8").length

  /** `text` trimmed, each run of whitespace one space. */
  private[period] def normal(text: String): String = text.trim.split("\\s+").mkString(" ")

  private[period] def added(section: Section, text: String, period: PeriodSeq): Line =
    new Line(section, text, period, period)

  extension (l: Line) {
    private[period] def touchedAt(period: PeriodSeq): Line = l.copy(touched = period)
  }
}

/** What a close proposes to do to the balance ([[Balance.edit]]). */
enum Edit {

  /** A new line `text` in `section`. */
  case Add(section: Section, text: String)

  /** `line` taken out as answered or done: `how` it was. */
  case Resolve(line: LineId, how: String)

  /** `line` taken out as no longer holding, and `why`. */
  case Drop(line: LineId, why: String)

  /** `line` confirmed or relied on. */
  case Touch(line: LineId)

  /** What the writer wrote as an edit that reads as none, and why. */
  case Unread(written: String, why: String)
}

object Edit {

  /** `e` as the flows record it when it is ignored: a line it names by its text in `named`,
    * or else by its id.
    */
  private[period] def shown(e: Edit, named: Map[LineId, String]): String = {
    def line(id: LineId) = named.get(id).fold(LineId.value(id))(t => s"\"$t\"")
    e match {
      case Add(section, text) => s"add ${section.key}: $text"
      case Resolve(id, how) => s"resolve ${line(id)}: $how"
      case Drop(id, why) => s"drop ${line(id)}: $why"
      case Touch(id) => s"touch ${line(id)}"
      case Unread(written, _) => written
    }
  }
}

/** What a close did to the balance, as its flows list it. */
enum Change {
  case Added(line: Line)
  case Resolved(line: Line, how: String)
  case Dropped(line: Line, why: String)

  /** Taken out to hold the cap. Its text stays in the flows of the closing of `line.since`. */
  case Evicted(line: Line)

  /** Added, then taken out again at once: the cap had no room for it. */
  case Refused(line: Line)

  /** An edit that changed nothing, as written, and why. */
  case Ignored(edit: String, why: String)
}

/** A conversation's state after a close: its open items, then its standing decisions and
  * facts, then its topics, each section's lines in the order they were last added or
  * touched, oldest first. No two lines share an id.
  */
final case class Balance private (lines: Vector[Line]) {

  def in(section: Section): Vector[Line] = lines.filter(_.section == section)

  /** Its lines' text in UTF-8 bytes, summed: what [[fit]] holds to a cap. */
  def bytes: Int = lines.map(Line.bytes).sum

  /** `edits`, made at the close of `period`, applied in order. `Add` puts its text, trimmed
    * and with each run of whitespace made one space, last in its section with `since` and
    * `touched` at `period`, or touches the line already there. `Resolve` and `Drop` take a
    * line out. `Touch` sets its `touched` to `period` and moves it last in its section. An
    * edit naming no line of the balance, a blank `Add`, or `Unread` changes nothing and is
    * listed as `Ignored`. Every other edit but a touch is listed as the change it made.
    */
  def edit(edits: Vector[Edit], period: PeriodSeq): Balance.Changed = {
    // An edit is shown by the text of the line it names, as the balance held it before.
    val named: Map[LineId, String] = lines.map(l => l.id -> l.text).toMap
    edits.foldLeft(Balance.Changed(this, Vector.empty)) { (done, e) =>
      val b = done.balance
      def ignored(why: String) =
        Balance.Changed(b, done.changes :+ Change.Ignored(Edit.shown(e, named), why))
      def present(id: LineId)(f: Line => Balance.Changed): Balance.Changed =
        b.lines.find(_.id == id).fold(ignored("names no line"))(f)
      e match {
        case Edit.Add(section, text) =>
          val normal = Line.normal(text)
          if (normal.isEmpty) ignored("blank")
          else {
            val line = Line.added(section, normal, period)
            b.lines.find(_.id == line.id) match {
              case Some(there) => Balance.Changed(b.moved(there.touchedAt(period)), done.changes)
              case None =>
                Balance.Changed(
                  Balance.grouped(b.lines :+ line),
                  done.changes :+ Change.Added(line)
                )
            }
          }
        case Edit.Resolve(id, how) =>
          present(id)(l => Balance.Changed(b.without(l), done.changes :+ Change.Resolved(l, how)))
        case Edit.Drop(id, why) =>
          present(id)(l => Balance.Changed(b.without(l), done.changes :+ Change.Dropped(l, why)))
        case Edit.Touch(id) =>
          present(id)(l => Balance.Changed(b.moved(l.touchedAt(period)), done.changes))
        case Edit.Unread(_, why) => ignored(why)
      }
    }
  }

  /** Held to `cap` bytes ([[bytes]]) at the close of `period`: lines are taken out one at a
    * time until the rest fits, first those not touched at `period` (least recently touched,
    * then oldest `since`, then by id), listed `Evicted`; then those added at `period` (by
    * id), listed `Refused`; last, those touched at `period` but added before it, in the
    * first order, listed `Evicted`. So a period's own adds never push out a line it
    * touched. A cap below 1 keeps no line.
    */
  def fit(cap: Int, period: PeriodSeq): Balance.Changed = {
    val n = PeriodSeq.value(period)
    def key(l: Line) = (PeriodSeq.value(l.touched), PeriodSeq.value(l.since), LineId.value(l.id))
    val (now, before) = lines.partition(l => PeriodSeq.value(l.touched) >= n)
    val (added, touched) = now.partition(l => PeriodSeq.value(l.since) >= n)
    val order: Vector[(Line, Change)] =
      before.sortBy(key).map(l => l -> Change.Evicted(l)) ++
        added.sortBy(l => LineId.value(l.id)).map(l => l -> Change.Refused(l)) ++
        touched.sortBy(key).map(l => l -> Change.Evicted(l))
    val budget = math.max(cap, 0)
    order
      .foldLeft((Balance.Changed(this, Vector.empty), bytes)) { case ((done, left), (l, out)) =>
        if (left <= budget) (done, left)
        else
          (
            Balance.Changed(done.balance.without(l), done.changes :+ out),
            left - Line.bytes(l)
          )
      }
      ._1
  }

  private def without(l: Line): Balance = new Balance(lines.filterNot(_.id == l.id))

  /** `l` in place of the line with its id, moved last in its section. */
  private def moved(l: Line): Balance = Balance.grouped(lines.filterNot(_.id == l.id) :+ l)
}

object Balance {

  val empty: Balance = new Balance(Vector.empty)

  /** A balance after [[Balance.edit]] or [[Balance.fit]], and what they changed, in order. */
  final case class Changed(balance: Balance, changes: Vector[Change])

  /** The balance of `lines`, in order, or why not: two lines share an id. */
  private[period] def of(lines: Vector[Line]): Either[String, Balance] =
    lines.map(_.id).diff(lines.map(_.id).distinct).headOption match {
      case Some(dup) => Left(s"two lines share the id ${LineId.value(dup)}")
      case None => Right(grouped(lines))
    }

  /** `lines` grouped by section, in [[Section]]'s order, each section's in theirs. */
  private def grouped(lines: Vector[Line]): Balance = new Balance(lines.sortBy(_.section.ordinal))
}
