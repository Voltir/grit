package grit.formats.limits

import java.io.{IOException, InputStream}
import java.util.concurrent.atomic.AtomicLong
import java.util.zip.{CheckedInputStream, Checksum}
import javax.xml.stream.{XMLInputFactory, XMLStreamException, XMLStreamReader}

import scala.util.control.ControlThrowable

import grit.core.files.{Kind, Limit, OpenFile, ReadLimits, Unread}

/** A read stopped by `by`: thrown by a guard deep inside a reader, a library's callback
  * included, and caught by the format whose read it stops ([[Guard.until]]).
  */
final class Stopped(val by: Limit) extends ControlThrowable

/** The guards one read of `file` keeps under `limits`: the characters it has read, the bytes
  * it has unpacked, and its deadline, checked every [[Guard.Every]] steps.
  */
object Guard {

  /** How many steps pass between two checks of the deadline: 1024. */
  val Every: Int = 1024

  /** `body` run with a fresh [[Count]] of `file`'s read; the limit that stopped it, if one did:
    * [[Limit.Damaged]] when its XML or its zip could not be read past some point, or a library
    * reading it failed.
    */
  def until(file: OpenFile^, limits: ReadLimits)(body: Count^ => Unit): Option[Limit] = {
    val chars = new AtomicLong
    val inflated = new AtomicLong
    val steps = new AtomicLong
    val count = new Count {
      def keep(s: String, into: String => Unit): Unit = {
        val room = limits.chars - chars.get
        if (s.length > room) {
          into(s.take(room.toInt))
          chars.set(limits.chars.toLong)
          throw new Stopped(Limit.Chars)
        }
        chars.addAndGet(s.length.toLong)
        into(s)
      }
      def unpacked(n: Int): Unit =
        if (inflated.addAndGet(n.toLong) > limits.inflated) throw new Stopped(Limit.Inflated)
      def step(): Unit =
        if (steps.incrementAndGet() % Every == 0 && file.late(limits)) throw new Stopped(Limit.Time)
    }
    try { body(count); None }
    catch {
      case s: Stopped => Some(s.by)
      case _: XMLStreamException | _: IOException => Some(Limit.Damaged)
      // A library's own failure on a malformed file: the file, not grit, is what is wrong.
      case _: RuntimeException => Some(Limit.Damaged)
    }
  }

  /** What a read of a file of `kind`, stopped by `stopped` after keeping `kept` of its units (a
    * workbook's tables, a document's body blocks, a deck's slides), is: when it kept none, [[Unread.TooSlow]] by its time and
    * [[Unread.Damaged]] by damage; `None` when it kept any, for any other stop, or none, so it is
    * answered as what it covers.
    */
  def unread(stopped: Option[Limit], kept: Int, kind: Kind): Option[Unread] =
    stopped.filter(_ => kept == 0) match {
      case Some(Limit.Time) => Some(Unread.TooSlow)
      case Some(Limit.Damaged) => Some(Unread.Damaged(kind))
      case Some(Limit.Chars | Limit.Glyphs | Limit.Inflated) | None => None
    }

  /** A read's counts, each throwing [[Stopped]] past its limit. */
  trait Count extends caps.SharedCapability {

    /** `s` handed to `into`, counted against the characters read; when it would pass them, its
      * start that fits is handed on, then [[Stopped]] with [[Limit.Chars]].
      */
    def keep(s: String, into: String => Unit): Unit

    /** `n` more bytes unpacked; [[Stopped]] with [[Limit.Inflated]] past them. */
    def unpacked(n: Int): Unit

    /** One more step; [[Stopped]] with [[Limit.Time]] once the deadline has passed. */
    def step(): Unit
  }

  /** `in`, each byte read from it counted by `count` as unpacked. */
  def counted(in: InputStream^, count: Count^): InputStream^{in, count} =
    // The JDK's checked stream hands its checksum every byte it reads; this one counts them.
    new CheckedInputStream(
      in,
      new Checksum {
        def update(b: Int): Unit = count.unpacked(1)
        def update(b: Array[Byte], off: Int, len: Int): Unit = count.unpacked(len)
        def getValue(): Long = 0L
        def reset(): Unit = ()
      }
    )

  /** XML read with no DTD and no external entity: an entity a DTD would declare is an error. */
  def xml(in: InputStream^): XMLStreamReader = {
    val factory = XMLInputFactory.newInstance()
    factory.setProperty(XMLInputFactory.SUPPORT_DTD, false)
    factory.setProperty(XMLInputFactory.IS_SUPPORTING_EXTERNAL_ENTITIES, false)
    factory.setProperty(XMLInputFactory.IS_COALESCING, false)
    factory.createXMLStreamReader(in)
  }
}
