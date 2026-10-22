package grit.formats.ooxml

import java.io.IOException
import java.util.zip.{ZipException, ZipFile}
import javax.xml.stream.XMLStreamConstants.{CDATA, CHARACTERS, SPACE}
import javax.xml.stream.XMLStreamReader

import scala.jdk.CollectionConverters.*

import grit.core.files.{Kind, OpenFile, ReadLimits, Unread}
import grit.formats.limits.Guard

/** An Office Open XML package, a zip of XML parts read by their local names, so a strict
  * document reads as a transitional one does.
  */
private[ooxml] object Package {

  /** A package's parts, each read once through the guards of one read. */
  trait Parts {

    /** Every part's name. */
    def names: Vector[String]

    /** `body` over part `name`'s XML, `None` when there is none. */
    def xml[A](name: String)(body: XMLStreamReader => A): Option[A]
  }

  /** `body` over `file`'s parts when it is a zip holding `main`: [[Unread.NotMine]] when it is
    * no zip or holds no `main`, [[Unread.Damaged]] of `kind` past `limits.entries` or when its
    * directory cannot be read.
    */
  def open[A](file: OpenFile^, limits: ReadLimits, kind: Kind, main: String, count: Guard.Count^)(
      body: Parts^{count} => A
  ): Either[Unread, A] = {
    val opened =
      try Right(new ZipFile(file.path.toFile))
      catch {
        case _: ZipException => Left(Unread.NotMine)
        case _: IOException => Left(Unread.Damaged(kind))
      }
    opened.flatMap { zip =>
      try {
        if (zip.size() > limits.entries) Left(Unread.Damaged(kind))
        else {
          val listed = zip.entries().asScala.map(_.getName).toVector
          if (!listed.contains(main)) Left(Unread.NotMine)
          else
            Right(body(new Parts {
              def names: Vector[String] = listed
              def xml[B](name: String)(read: XMLStreamReader => B): Option[B] =
                Option(zip.getEntry(name)).map { entry =>
                  val in = Guard.counted(zip.getInputStream(entry), count)
                  try read(Guard.xml(in))
                  finally in.close()
                }
            }))
        }
      } finally zip.close()
    }
  }

  /** The value of `r`'s attribute of local name `local`, if it has one. */
  def attr(r: XMLStreamReader, local: String): Option[String] =
    (0 until r.getAttributeCount).collectFirst {
      case i if r.getAttributeLocalName(i) == local => r.getAttributeValue(i)
    }

  /** Whether the event `r` stands at is text. */
  def isText(event: Int): Boolean = event == CHARACTERS || event == SPACE || event == CDATA
}
