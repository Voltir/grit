package grit.core.visibility

import grit.core.place.Place

/** How far a thing may be believed to steer an effect (Biba, ADR 0036): a level and the
  * compartments it is endorsed in. One dominates another when its level is at least the
  * other's and it holds every compartment of the other. What is made from several is believed
  * at their meet. Opaque, as `Label` is.
  */
opaque type Integrity = Levelled[Integrity.Level]

object Integrity {

  /** grit's integrity levels (Biba), lowest first; not the confidentiality `Level` beside it.
    * Each unlocks what those below it do and its own.
    */
  enum Level extends caps.Pure {

    /** Content no source vouches for (a relaying service's results, an undeclared one's).
      * Unlocks nothing.
      */
    case Untrusted

    /** An unattested account's words, a guest's. May be kept and replied to. */
    case Outside

    /** An attested member of a realm. May drive a write out of grit within the realm. */
    case Member

    /** An administrator, or a steward in their compartments. May set standing policy,
      * relabel rooms and declare schedules.
      */
    case Authority
  }

  /** [[Level.Untrusted]], no compartment: the least. */
  val Untrusted: Integrity = at(Level.Untrusted)

  /** [[Level.Outside]], no compartment. */
  val Outside: Integrity = at(Level.Outside)

  /** [[Level.Member]], no compartment. */
  val Member: Integrity = at(Level.Member)

  /** The least integrity that may steer a write out of grit: [[Member]], the team's. Below it,
    * a thing came from outside the team.
    */
  val WritesOut: Integrity = Member

  /** [[Level.Authority]], no compartment. */
  val Authority: Integrity = at(Level.Authority)

  /** grit's own text: [[Level.Authority]] in every one of `compartments` and the unmapped mark;
    * the meet's identity.
    */
  def top(compartments: Compartments): Integrity =
    Levelled.at(Level.Authority, compartments.declared.toVector :+ Mark.Unmapped)

  /** `level` in `compartments`, read as the compartments it is *endorsed in*: the reverse of
    * `Label.at`'s reading, so the usual value names none.
    */
  def at(level: Level, compartments: Compartment*): Integrity = Levelled.at(level, compartments)

  extension (i: Integrity) {
    def dominates(other: Integrity): Boolean = i.dominates(other)

    /** Whether it is believed below [[Outside]]: at [[Level.Untrusted]], in any compartments. */
    def belowOutside: Boolean = !i.dominates(Outside)

    /** Whether it may steer a write out of grit: it dominates [[WritesOut]]. */
    def mayWriteOut: Boolean = i.dominates(WritesOut)

    /** The greatest integrity both dominate: what is made from both. */
    def meet(other: Integrity): Integrity = i.meet(other)

    /** The least integrity dominating both. */
    def join(other: Integrity): Integrity = i.join(other)
  }

  given CanEqual[Integrity, Integrity] = CanEqual.derived

  /* Its levels' names, as the written form holds them: changing one changes every written
   * integrity. */
  private[visibility] given Levels[Level] {
    val all: Vector[Level] = Level.values.toVector
    def name(level: Level): String = level match {
      case Level.Untrusted => "untrusted"
      case Level.Outside => "outside"
      case Level.Member => "member"
      case Level.Authority => "authority"
    }
    def shownAs: String = "integrity"
    def refusedAs: String = "integrity level"
  }

  /** Its written form, stable across releases; parsed by [[read]]. */
  def written(i: Integrity): String = i.written

  /** Its form for people and the model, e.g. `[integrity: member]`, or with the unmapped mark
    * `[integrity: authority, in: {acme}, and what no one has labelled yet]`. Never parsed.
    */
  def shown(i: Integrity): String = i.shown

  /** The integrity `text` writes, or why not. */
  def read(text: String): Either[String, Integrity] = Levelled.read[Level](text)

  /** `i`'s compartments, in order and without the unmapped mark, for this package's own checks. */
  private[visibility] def compartments(i: Integrity): Vector[Compartment] =
    i.compartments.toVector.flatMap(Compartment.marked)

  /** `level` in `marks`, the unmapped mark among them or not. */
  private[visibility] def marked(level: Level, marks: Seq[Mark]): Integrity =
    Levelled.at(level, marks)

  /** `i`'s compartments and mark, in order: what it is stored as. */
  private[visibility] def marks(i: Integrity): Vector[Mark] = i.compartments.toVector

  /** `i`'s level, for this package's own checks. */
  private[visibility] def level(i: Integrity): Level = i.level
}

/** The deployment declares how far what is said at `place`, or at any place within it that no
  * deeper cap covers, is believed (ADR 0036): a room's speech at its author's level met with
  * `integrity`, whoever said it; a service's results whose items its extension does not read at
  * `integrity` itself, the cap standing in for the author the service cannot name (a service
  * that relays others' text, mail or the web, declares [[Integrity.Untrusted]]); an item it does
  * read (a repository's, a folder's or a mailbox's) at its author's level met with the cap at the
  * item's place ([[Visibility.believed]]). A cap may stand above or below the one it lies within. A
  * cap at a directory under `fs`, a direct message's room or everywhere is refused
  * ([[VisibilityRefusal.CapOutside]]).
  */
final case class Cap(place: Place, integrity: Integrity) derives CanEqual

object Cap {

  /** What a service's result is believed at when no cap covers its place, and a tool that
    * declares nothing: the least, [[Integrity.Untrusted]].
    */
  val Undeclared: Integrity = Integrity.Untrusted
}

/** An integrity as the level and compartments it is stored as (`grit.integrity_labels`), for the
  * code that stores and checks integrity (`grit.core.visibility`, `grit.dbos`) and for nothing
  * else (enola-intent.yaml).
  */
object IntegrityParts {

  /** `i`'s level as stored: its rank on core's scale, from 0 for [[Integrity.Level.Untrusted]]. */
  def rank(i: Integrity): Int = Integrity.level(i).ordinal

  /** `i`'s compartments' names, distinct and sorted bytewise: the form
    * `grit.canonical_compartments` keeps.
    */
  def compartments(i: Integrity): Vector[String] = Integrity.marks(i).map(Mark.name)

  /** The integrity stored as `rank` and `compartments`, the unmapped mark among them as its
    * name. Total, and failing closed: a rank naming no level reads as
    * [[Integrity.Level.Untrusted]], and a name that is no compartment's ([[Compartment.of]]) is
    * dropped, so it is endorsed in none it cannot name.
    */
  def of(rank: Int, compartments: Vector[String]): Integrity =
    Integrity.marked(
      Integrity.Level.values.lift(rank).getOrElse(Integrity.Level.Untrusted),
      compartments.flatMap(Mark.read(_).toOption)
    )
}
