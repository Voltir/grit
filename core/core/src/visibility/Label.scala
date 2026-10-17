package grit.core.visibility

/** What a label holds beside its level: a declared [[Compartment]], or grit's mark for what no
  * label placed, which only a label's own operations and its stored forms make
  * ([[Label.Unmapped]]).
  */
opaque type Mark = String

private[visibility] object Mark {

  /** The mark of what no label placed: read only by a clearance holding it, which grit's own
    * top does and no grant can.
    */
  val Unmapped: Mark = "unmapped"

  /** `name` as a stored or written form holds it: the mark, or a compartment
    * ([[Compartment.of]]).
    */
  def read(name: String): Either[String, Mark] =
    if (name == Unmapped) Right(Unmapped) else Compartment.of(name)

  def name(m: Mark): String = m

  given Ordering[Mark] = Ordering.String
}

/** A named area a deployment declares (a team, a client, a project): a thing in it is read
  * only by a clearance holding it, at whatever level.
  */
opaque type Compartment <: Mark = String

object Compartment {

  private val Shape = "[a-z0-9-]{1,32}".r

  /** `name` as a compartment: lowercase letters, digits and `-`, 1 to 32 of them, no level's
    * name, not `at`, the word a label command names a place by, and not `unmapped`, grit's own
    * mark; or why not.
    */
  def of(name: String): Either[String, Compartment] =
    if (!Shape.matches(name)) {
      Left(s"$name is not a compartment's name: lowercase letters, digits and -, 1 to 32 of them")
    } else if (Level.named(name).isDefined) {
      Left(s"$name is a level's name, never a compartment's")
    } else if (name == "at") {
      Left(s"$name names a place in a label command, never a compartment")
    } else if (name == Mark.Unmapped) {
      Left(s"$name is grit's mark for what no label placed, never a compartment's name")
    } else Right(name)

  /** Its name, as [[of]] took it: what a person reads in a refusal naming it, and what it is
    * stored as. It says nothing of any label's structure: no public operation takes a
    * compartment out of a label.
    */
  def name(c: Compartment): String = c

  /** `m` as a compartment; `None` for the unmapped mark. */
  private[visibility] def marked(m: Mark): Option[Compartment] = {
    val name = Mark.name(m)
    Option.when(name != Mark.name(Mark.Unmapped))(name)
  }

  private[visibility] given Ordering[Compartment] = Ordering.String
}

/** What a thing is classified at: a level and some compartments, and perhaps the mark of what
  * no label placed ([[Label.Unmapped]]). One label dominates another when its level is at
  * least the other's and it holds every compartment and mark of the other.
  * Opaque: a label is compared, joined, met and passed to core as a key, never taken apart.
  */
opaque type Label = Levelled[Level]

/** A label of declared compartments alone, never the unmapped mark: what a grant gives and a
  * room is declared at. [[Label.at]] makes one; a join or a meet, a read or stored form, and
  * [[Compartments.top]] are [[Label]]s.
  */
opaque type DeclaredLabel <: Label = Levelled[Level]

object Label {

  /** The bottom: [[Level.Public]], no compartment. Every label dominates it. */
  val Public: DeclaredLabel = at(Level.Public)

  /** [[Level.Public]] and the unmapped mark: what a label is joined with when what it labels
    * was not placed, so that only a clearance holding the mark reads it.
    */
  val Unmapped: Label = marked(Level.Public, Vector(Mark.Unmapped))

  def at(level: Level, compartments: Compartment*): DeclaredLabel =
    Levelled.at(level, compartments)

  extension (l: Label) {
    def dominates(other: Label): Boolean = l.dominates(other)

    /** The least label dominating both: what is made from both is kept at least here. */
    def join(other: Label): Label = l.join(other)

    /** The greatest label both dominate. */
    def meet(other: Label): Label = l.meet(other)
  }

  given CanEqual[Label, Label] = CanEqual.derived

  /** Its written form, stable across releases: what logs, recorded step outputs and files
    * outside the database hold. Parseable by [[read]], so a label is not secret from whoever
    * holds its written form; opacity buys representation independence, not secrecy.
    */
  def written(l: Label): String = l.written

  /** A thing's label (a room's, a place's, what a read took in) for people and the model to
    * read, e.g. `[level: confidential, in: {acme, trial}]`, or `[level: internal]` with no
    * compartment; one holding the unmapped mark, whatever its level, is `not labelled yet (no
    * one can read it)`. A clearance, bound or floor is shown by [[clearance]]. Never parsed: [[written]] is the form a
    * file or a recorded output holds.
    */
  def shown(l: Label): String = if (unmapped(l)) NotLabelled else l.shown

  /** What a thing labelled `l` is, to follow "is" or "was": `labelled ` and its [[shown]] form,
    * or for one holding the unmapped mark, `not labelled yet (no one can read it)`.
    */
  def described(l: Label): String = if (unmapped(l)) NotLabelled else s"labelled ${l.shown}"

  /** `l` as what something reads or is held to (a clearance, a bound a call was routed for, a
    * floor), for people and the model: as [[shown]], but one holding the unmapped mark is shown
    * by its level and compartments and `, and what no one has labelled yet`, e.g.
    * `[level: restricted, in: {acme}, and what no one has labelled yet]`. A thing's own label
    * is [[shown]] or [[described]].
    */
  def clearance(l: Label): String = l.plain

  private val NotLabelled = "not labelled yet (no one can read it)"

  /** The label `text` writes, or why not. */
  def read(text: String): Either[String, Label] = Levelled.read[Level](text)

  /** `l`'s level, for this package's own checks. */
  private[visibility] def level(l: Label): Level = l.level

  /** `l`'s compartments, in order and without the mark, for this package's own checks. */
  private[visibility] def compartments(l: Label): Vector[Compartment] =
    l.compartments.toVector.flatMap(Compartment.marked)

  /** Whether `l` holds the unmapped mark. */
  private[visibility] def unmapped(l: Label): Boolean = l.compartments.contains(Mark.Unmapped)

  /** `level` in `marks`, the unmapped mark among them or not. */
  private[visibility] def marked(level: Level, marks: Seq[Mark]): Label = Levelled.at(level, marks)

  /** `l`'s compartments and mark, in order: what it is stored as. */
  private[visibility] def marks(l: Label): Vector[Mark] = l.compartments.toVector
}

/** A label as the level and compartments it is stored as (`grit.classifications`), for the code that
  * stores and checks labels (`grit.core.visibility`, `grit.dbos`) and for nothing else
  * (enola-intent.yaml).
  */
object LabelParts {

  /** `l`'s level as stored: its rank on core's scale, from 0 for [[Level.Public]]. */
  def rank(l: Label): Int = Label.level(l).ordinal

  /** `l`'s compartments' names, distinct and sorted bytewise: the form
    * `grit.canonical_compartments` keeps.
    */
  def compartments(l: Label): Vector[String] = Label.marks(l).map(Mark.name)

  /** The label stored as `rank` and `compartments`, the unmapped mark among them as its name.
    * Total, and failing closed: a rank naming no level reads as [[Level.Restricted]], and a
    * name that is no compartment's ([[Compartment.of]]) is dropped; either way the label also
    * holds the mark ([[Label.Unmapped]]).
    */
  def of(rank: Int, compartments: Vector[String]): Label = {
    val level = Level.values.lift(rank)
    val named = compartments.flatMap(Mark.read(_).toOption)
    val placed = Label.marked(level.getOrElse(Level.Restricted), named)
    if (level.nonEmpty && named.size == compartments.size) placed
    else placed.join(Label.Unmapped)
  }
}
