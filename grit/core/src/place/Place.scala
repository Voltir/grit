package grit.core.place

/** A namespace under the root: one per source of conversations, by the name a place is
  * written with.
  */
enum Namespace(val key: String) {

  /** Directories on the machine grit runs on. */
  case Fs extends Namespace("fs")

  /** Slack teams, their channels and threads. */
  case Slack extends Namespace("slack")

  /** Triggered tasks and their runs. */
  case Task extends Namespace("task")

  /** Outside services, each hosting tools an edge serves there: `service:github`. */
  case Service extends Namespace("service")
}

object Namespace {

  /** The namespace named `key`; `None` for no namespace's name. */
  def of(key: String): Option[Namespace] = Namespace.values.find(_.key == key)
}

/** Where a conversation happens: a path in one containment tree, from a [[Namespace]] down;
  * the empty path is the root, [[Place.Everywhere]]. No segment is empty or holds `/`.
  */
final case class Place private (segments: Vector[String]) {

  /** The directory this place is, when it is one under `fs`; `None` for any other place. */
  def directory: Option[Directory] =
    segments.headOption
      .filter(_ == Namespace.Fs.key)
      .flatMap(_ => Directory.of("/" + segments.drop(1).mkString("/")).toOption)

  /** The service this place is, when it is `service:{name}` with `name` a service's
    * ([[Service.of]]); `None` for any other place.
    */
  def service: Option[Service] = segments match {
    case Vector(ns, name) if ns == Namespace.Service.key => Service.of(name).toOption
    case _ => None
  }

  /** Whether this place is `other` or lies under it. Everywhere holds every place. */
  def within(other: Place): Boolean = segments.startsWith(other.segments)

  /** `fs:/home/nick/Projects/grit`, `slack:acme/#grit-dev/1712.3`, `everywhere`:
    * [[Place.read]] reads it back to this place.
    */
  def written: String =
    segments.headOption match {
      case None => Place.EverywhereWritten
      case Some(ns) =>
        val slash = if (ns == Namespace.Fs.key) "/" else ""
        s"$ns:$slash${segments.drop(1).mkString("/")}"
    }
}

object Place {

  private val EverywhereWritten = "everywhere"

  val Everywhere: Place = new Place(Vector.empty)

  /** `dir` under `fs`. */
  def of(dir: Directory): Place = under(Namespace.Fs, Vector(Directory.value(dir)))

  /** A place under `namespace`, each of `parts` split at `/`, empty pieces dropped. */
  def under(namespace: Namespace, parts: Vector[String]): Place =
    new Place(namespace.key +: parts.flatMap(_.split('/').toVector).filter(_.nonEmpty))

  /** `everywhere`, or `ns:rest` with `ns` a namespace's name and `rest` split at `/` (a
    * leading `/` optional); why not, naming an unknown namespace or a missing `:`.
    */
  def read(text: String): Either[String, Place] = {
    val t = text.trim
    if (t == EverywhereWritten) Right(Everywhere)
    else
      t.indexOf(':') match {
        case -1 =>
          Left(s"no namespace in $t: write it as fs:/a/path, slack:team/channel or task:name")
        case i =>
          val ns = t.take(i)
          Namespace
            .of(ns)
            .toRight(s"no namespace $ns: ${Namespace.values.map(_.key).mkString(", ")}")
            .map(n => under(n, Vector(t.drop(i + 1))))
      }
  }
}

/** One of a [[Scope]]'s prefixes: a place, or the conversation's own room
  * ([[grit.core.store.Origin.room]]), which differs from one conversation to the next.
  */
enum Prefix {
  case At(place: Place)
  case Room

  /** A place's written form, or `room`. */
  def written: String = this match {
    case At(place) => place.written
    case Room => Prefix.RoomWritten
  }
}

object Prefix {
  private[place] val RoomWritten = "room"

  /** `room`, or a written place ([[Place.read]]); why not, as [[Place.read]] says. */
  def read(word: String): Either[String, Prefix] =
    if (word.trim == RoomWritten) Right(Room) else Place.read(word).map(At(_))
}

/** The places whose open periods and closings a window may draw on besides its own
  * conversation's: those within any of `prefixes`, the room standing for the conversation's
  * own room. No prefixes turns cross-place recall off.
  */
final case class Scope(prefixes: Vector[Prefix]) {

  /** Whether a conversation at `place` is in scope for one whose room is `room`. */
  def holds(room: Place, place: Place): Boolean = prefixes.exists {
    case Prefix.At(p) => place.within(p)
    case Prefix.Room => place.within(room)
  }

  /** `none`, or the prefixes' written forms separated by spaces: [[Scope.read]] reads it
    * back.
    */
  def written: String =
    if (prefixes.isEmpty) Scope.NoneWritten else prefixes.map(_.written).mkString(" ")
}

object Scope {

  private val NoneWritten = "none"

  val Everywhere: Scope = Scope(Vector(Prefix.At(Place.Everywhere)))

  val Off: Scope = Scope(Vector.empty)

  /** The conversation's own room alone. */
  val Room: Scope = Scope(Vector(Prefix.Room))

  /** `none`, or words separated by spaces, each `room` or a written place (so a path with a
    * space cannot be written here); why not, naming the first word that is neither.
    */
  def read(text: String): Either[String, Scope] = {
    val t = text.trim
    if (t == NoneWritten) Right(Off)
    else if (t.isEmpty) Left("a scope is none, everywhere, room, or places such as fs:/home/you")
    else
      t.split("\\s+")
        .toVector
        .foldLeft[Either[String, Vector[Prefix]]](Right(Vector.empty)) { (acc, w) =>
          acc.flatMap(done => Prefix.read(w).map(done :+ _))
        }
        .map(Scope(_))
  }
}

/** How far a conversation's own search hits outweigh those from other places: their scores
  * are multiplied by it before one pool is ranked. At least 1.
  */
opaque type Weight = Double

object Weight {

  /** `w` as a weight, or why not: below 1, or not a number. */
  def of(w: Double): Either[String, Weight] =
    if (w.isNaN || w < 1.0) Left(s"a weight is a number of at least 1, not $w") else Right(w)

  def value(w: Weight): Double = w

  /** 2: a hit elsewhere must match twice as well to outrank one here. */
  val Default: Weight = 2.0
}

/** Which places' open periods a turn's window draws on (`scope`), and how much the
  * conversation's own candidates are weighted up against theirs (`weight`).
  */
final case class Locality(scope: Scope, weight: Weight)

object Locality {

  /** Everywhere, [[Weight.Default]]. */
  val Default: Locality = Locality(Scope.Everywhere, Weight.Default)
}
