package grit.core.files

import scala.concurrent.duration.*

/** A format reader's name, as a tree kept from it records it. */
opaque type FormatName = String

object FormatName {
  def apply(name: String): FormatName = name

  extension (n: FormatName) {
    def value: String = n
  }
}

/** A file format's reader (an extension point, `docs/extending.md`), holding no capability:
  * it reaches the file only through the one it is handed.
  */
trait Format extends caps.Pure {

  /** Its name, recorded with every tree kept from it. */
  def name: FormatName

  /** Its version, recorded beside [[name]]: a tree kept from an older version is read again. */
  def version: Int

  /** `file`'s document, or why not: [[Unread.NotMine]] when its bytes are not this format's, so
    * the next is tried. Stops at each of `limits` with what it has read, saying where in
    * [[Understood.covers]]; never throws for a file it cannot read.
    */
  def read(file: OpenFile^, limits: ReadLimits): Either[Unread, Understood]
}

/** A deployment's readers: for each kind, the formats tried in order; and the limits every read
  * stops at.
  */
final case class Formats(of: Kind -> Vector[Format], limits: ReadLimits = ReadLimits.Default)

object Formats {

  /** No reader for any kind: every file is [[Unread.Unsupported]]. */
  val Empty: Formats = Formats(_ => Vector.empty)
}

/** Which of a read's limits stopped it. */
enum Limit derives CanEqual {

  /** [[ReadLimits.chars]] of text were read. */
  case Chars

  /** [[ReadLimits.glyphs]] were drawn: a page's text drawn many times over. */
  case Glyphs

  /** [[ReadLimits.inflated]] bytes were unpacked from a zip. */
  case Inflated

  /** [[ReadLimits.time]] passed. */
  case Time

  /** The rest of the file could not be read: it is damaged past that point. */
  case Damaged

  /** Why the read stopped, as the model is told: "it is too long to read whole here". */
  def words: String = this match {
    case Chars | Glyphs | Inflated => "it is too long to read whole here"
    case Time => "it took too long to read whole here"
    case Damaged => "the rest of it is damaged"
  }
}

/** The guards every reader stops at: [[ReadLimits.Default]]'s values, which a deployment may only
  * lower ([[ReadLimits.lowered]]). Past `bytes`, nothing is read ([[Unread.TooLarge]]); past
  * `entries` in a zip, it is [[Unread.Damaged]]; past any other, the read keeps what it read
  * before it ([[Covers.Part]] or [[Covers.Start]], `by` that [[Limit]]).
  */
final case class ReadLimits private (
    bytes: Long,
    chars: Int,
    glyphs: Int,
    inflated: Long,
    entries: Int,
    time: FiniteDuration
)

object ReadLimits {

  /** 20 MiB of file, 1 Mi characters of text out, 2 Mi glyphs drawn, 64 MiB unpacked from a
    * zip, 1000 zip entries, and 10 seconds.
    */
  val Default: ReadLimits = ReadLimits(
    bytes = 20L * 1024 * 1024,
    chars = 1024 * 1024,
    glyphs = 2 * 1024 * 1024,
    inflated = 64L * 1024 * 1024,
    entries = 1000,
    time = 10.seconds
  )

  /** [[Default]] with each value given in its place, each kept at most [[Default]]'s and at
    * least 1 (a millisecond for `time`).
    */
  def lowered(
      bytes: Long = Default.bytes,
      chars: Int = Default.chars,
      glyphs: Int = Default.glyphs,
      inflated: Long = Default.inflated,
      entries: Int = Default.entries,
      time: FiniteDuration = Default.time
  ): ReadLimits =
    ReadLimits(
      bytes.max(1L).min(Default.bytes),
      chars.max(1).min(Default.chars),
      glyphs.max(1).min(Default.glyphs),
      inflated.max(1L).min(Default.inflated),
      entries.max(1).min(Default.entries),
      if (time < 1.millisecond) 1.millisecond else if (time > Default.time) Default.time else time
    )
}
