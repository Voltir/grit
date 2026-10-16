package grit.mcp.drive

import grit.core.host.Clipped
import grit.core.identity.Email
import grit.core.visibility.{Integrity, Itemised, Part, Provenance, Readers}

/** How Drive's exchanges are believed (ADR 0036): each hit and each read a part of its own, at
  * the place it lies within, so a label on that place decides who reads it.
  */
object DriveItems {

  /** The longest name shown: 80 characters. */
  val NameMax: Int = 80

  /** Whether `name` is shown as it is: one line of at most [[NameMax]] characters, holding no
    * control character, no markup (`<`, `>`, `[`, `]`, a backtick) and no web address.
    */
  def shown(name: String): Boolean =
    name.nonEmpty && name.length <= NameMax && !name.exists(c =>
      c.isControl || "<>[]`".contains(c)
    ) &&
      !name.toLowerCase.contains("://") && !name.toLowerCase.contains("www.")

  /** What `reading` makes of `called`:
    *   - a search: its head, then each hit as a names part, one line (its name, or a note that
    *     it is not shown when [[shown]] refuses it, its type, its id, where it lies, and its
    *     shared drive by the name the search lists, by its id when none is listed or [[shown]]
    *     refuses it), at
    *     member, at the place it lies within ([[DriveReading.place]]), a folder's at the
    *     service's own place; a hit lying nowhere is left out;
    *   - a read: a head that names nothing of the file, then one part, its name and its text
    *     (cut to whole lines that fit beside the head, saying how many were kept), at the place
    *     it lies within, at the meet over its writers' integrities: the deployment's own
    *     address skipped; a declared writer at its integrity; a person, or everyone, in the
    *     domain at member; a person outside it, a group, or everyone in another domain at
    *     outside; anyone at untrusted; untrusted when the grants were not read whole or no
    *     writer is left.
    *
    * Readers: a file with no owner (a shared drive's) whoever reads its place; one owned by
    * one address in the domain, only that person; any other is left out. `Left`, in words the
    * model reads, for a read of a file lying nowhere.
    */
  def items(reading: DriveReading, called: DriveCalled): Either[String, Itemised.Parts] =
    called match {
      case DriveCalled.Searched(hits, drives) =>
        val head =
          "Drive files whose text holds those words, one a line, each with its id; read one " +
            "with drive_read." +
            (if (hits.incomplete)
               " Drive has more than these; search with more words to see others."
             else "")
        val parts = hits.files.flatMap { f =>
          val place = if (f.folder) Some(reading.service.place) else f.within.map(reading.place)
          for {
            p <- place
            r <- readers(reading, f)
          } yield Part(Provenance(p, Integrity.Member), r, line(f, drives))
        }
        Right(Itemised.Parts(head, parts))
      case DriveCalled.Read(file, grants, text) =>
        file.within.toRight("This file's folder could not be read, so it is not shown.").map { at =>
          val head = "The Drive file asked for:"
          val part = readers(reading, file).map { r =>
            Part(
              Provenance(reading.place(at), believed(reading, grants)),
              r,
              fitted(head, s"${title(file)}:", text)
            )
          }
          Itemised.Parts(head, part.toVector)
        }
    }

  /** Who may read `file`'s part, or `None` when no one may. */
  private def readers(reading: DriveReading, file: DriveFile): Option[Readers] =
    file.owners match {
      case Vector() => Some(Readers.AtPlace)
      case Vector(owner) if Email.domain(owner) == reading.domain => Some(Readers.Originator(owner))
      case _ => None
    }

  /** The meet over `grants`' writers, as [[items]] says. */
  private def believed(reading: DriveReading, grants: Option[Vector[Grant]]): Integrity =
    grants.toVector.flatten
      .filter(_.role.writes)
      .flatMap {
        case Grant.Person(email, _) if Email.value(email) == Email.value(reading.self) => None
        case Grant.Person(email, _) =>
          Some(
            reading.writers.getOrElse(
              email,
              if (Email.domain(email) == reading.domain) Integrity.Member else Integrity.Outside
            )
          )
        case Grant.Group(_, _) => Some(Integrity.Outside)
        case Grant.Everyone(domain, _) =>
          Some(if (domain == reading.domain) Integrity.Member else Integrity.Outside)
        case Grant.Anyone(_) => Some(Integrity.Untrusted)
      }
      .reduceOption(_.meet(_))
      .getOrElse(Integrity.Untrusted)

  /** `file`'s name as shown, and its type. */
  private def title(file: DriveFile): String =
    if (shown(file.name)) s"${file.name} (${file.kind})"
    else s"A file whose name is not shown, since it is not a plain title (${file.kind})"

  /** A hit's line, its shared drive named as `drives` names it when that is shown as it is. */
  private def line(file: DriveFile, drives: Map[FileId, String]): String = {
    def drive(d: FileId): String =
      s"the shared drive ${drives.get(d).filter(shown).getOrElse(FileId.value(d))}"
    val where = (file.within, file.drive) match {
      case (Some(w), Some(d)) if w == d => s", in ${drive(d)} (${FileId.value(d)})"
      case (Some(w), Some(d)) => s", in ${FileId.value(w)}, in ${drive(d)}"
      case (Some(w), None) => s", in ${FileId.value(w)}"
      case (None, _) => ""
    }
    s"- ${title(file)}, id ${FileId.value(file.id)}$where"
  }

  /** `title` and then `text`'s leading whole lines, as many as fit after `head` within
    * [[Clipped]]'s limits, with a line saying how many were kept when some were not.
    */
  private def fitted(head: String, title: String, text: String): String = {
    val lines = Clipped.lines(text)
    def kept(k: Int): String = {
      val shown = (title +: lines.take(k)).mkString("\n")
      if (k == lines.size) shown
      else s"$shown\n[Only the first $k of its ${lines.size} lines are shown.]"
    }
    if (Clipped.fits(s"$head\n${kept(lines.size)}")) kept(lines.size)
    else kept(Clipped.most(lines.size - 1)(k => s"$head\n${kept(k)}"))
  }
}
