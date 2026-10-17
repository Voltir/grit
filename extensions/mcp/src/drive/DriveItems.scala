package grit.mcp.drive

import grit.core.host.{Clipped, Kept}
import grit.core.identity.Email
import grit.core.visibility.{Integrity, Itemised, Provenance, Readers, ResultPart}

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

  /** The label command that opens a folder no one has labelled yet, as a person types it. */
  private[drive] val Opening: String = "`label internal at <the folder's link or id>`"

  /** What `reading` makes of `called`:
    *   - a search: its head (saying that files in folders no one has labelled yet aren't
    *     searched, and how an administrator opens one: [[Opening]]), then each hit as a names part, one line (its name, or a note that
    *     it is not shown when [[shown]] refuses it, its type, its id, where it lies, and its
    *     shared drive by the name the search lists, by its id when none is listed or [[shown]]
    *     refuses it, and when it last changed, when Drive says), at
    *     member, at the place it lies within ([[DriveReading.place]]), a folder's at the
    *     service's own place; a hit lying nowhere is left out;
    *   - the shared drives: a head (saying, when Drive had more, that only the first
    *     [[DriveRest.DrivesMost]] are listed, and ending as a search's, "listed" for
    *     "searched"), then each
    *     drive as a names part, one line (its name, or a note that it is not shown, and its
    *     id), at member, at the service's own place, by name;
    *   - a listing of one folder: a head (saying, when Drive had more, that only the first
    *     [[DriveRest.ListMost]] are listed, and ending as the drives' does), then each file as
    *     a search's hit is, its line its name or that note, its type, its id and when it last
    *     changed, when Drive says;
    *   - a read: a head that names nothing of the file, then one part, its name, when it last
    *     changed when Drive says, and its text
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
          "Drive files whose text holds those words, one a line, each with its id and when it " +
            "last changed; read one with drive_read." +
            (if (hits.incomplete)
               " Drive has more than these; search with more words to see others."
             else "") +
            // Said of every search, never only of one whose hits were cut for lying in a place
            // no one has labelled: that would tell the asker their words matched a file no one
            // may read.
            unlabelled("searched")
        Right(Itemised.Parts(head, names(reading, hits.files, line(_, drives))))
      case DriveCalled.Drives(drives) =>
        val head = "The shared drives here, one a line, each with its id; list one's files and " +
          "folders by its id." +
          (if (drives.incomplete)
             s" Drive has more than these; only the first ${DriveRest.DrivesMost} are listed."
           else "") +
          unlabelled("listed")
        val parts = drives.names.toVector
          .sortBy((id, name) => (Some(name).filter(shown).getOrElse(""), FileId.value(id)))
          .map { (id, name) =>
            val title =
              if (shown(name)) s"$name (a shared drive)"
              else "A shared drive whose name is not shown, since it is not a plain title"
            ResultPart(
              Provenance(reading.service.place, Integrity.Member),
              Readers.AtPlace,
              s"- $title, id ${FileId.value(id)}"
            )
          }
        Right(Itemised.Parts(head, parts))
      case DriveCalled.Listed(files) =>
        val head = "The files and folders directly in that folder, one a line, each with its id " +
          "and when it last changed; read a file, or list a folder, by its id." +
          (if (files.incomplete)
             s" Drive has more than these; only the first ${DriveRest.ListMost} are listed."
           else "") +
          // Said of every listing, as of every search.
          unlabelled("listed")
        Right(
          Itemised.Parts(
            head,
            names(reading, files.files, f => s"- ${title(f)}, id ${FileId.value(f.id)}${since(f)}")
          )
        )
      case DriveCalled.Read(file, grants, text) =>
        file.within.toRight("This file's folder could not be read, so it is not shown.").map { at =>
          val head = "The Drive file asked for:"
          val part = readers(reading, file).map { r =>
            ResultPart(
              Provenance(reading.place(at), believed(reading, grants)),
              r,
              fitted(head, s"${title(file)}${since(file)}:", text)
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

  /** Each of `files` a names part, one line `line` writes, as [[items]] says of a search's. */
  private def names(
      reading: DriveReading,
      files: Vector[DriveFile],
      line: DriveFile => String
  ): Vector[ResultPart] =
    files.flatMap { f =>
      val place = if (f.folder) Some(reading.service.place) else f.within.map(reading.place)
      for {
        p <- place
        r <- readers(reading, f)
      } yield ResultPart(Provenance(p, Integrity.Member), r, line(f))
    }

  /** The sentence every search's and listing's head ends with: files in folders no one has
    * labelled yet aren't `done` (searched, listed), and how an administrator opens one.
    */
  private def unlabelled(done: String): String =
    s" Files in folders that no one has labelled yet aren't $done. An administrator can open " +
      s"such a folder by typing $Opening."

  /** When `file` last changed, after a comma, as Drive gives it; nothing when Drive does not. */
  private def since(file: DriveFile): String = file.changed.fold("")(t => s", last changed $t")

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
    s"- ${title(file)}, id ${FileId.value(file.id)}$where${since(file)}"
  }

  /** `title` and then `text`'s leading whole lines, as many as fit after `head` within
    * [[Clipped]]'s limits, with [[Clipped.cutHint]]'s hint when some were not.
    */
  private def fitted(head: String, title: String, text: String): String = {
    val lines = Clipped.lines(text)
    // `title` and `k` lines fit whole by the search below, so `head` keeps them, and its hint
    // is said of the text's lines, never of the cut it did not make.
    def kept(k: Int): String =
      Clipped
        .head(
          (title +: lines.take(k)).mkString("\n"),
          _ => Clipped.cutHint(Option.when(k < lines.size)(Kept(k, lines.size, partial = false)))
        )
        .show
    if (Clipped.fits(s"$head\n${kept(lines.size)}")) kept(lines.size)
    else kept(Clipped.most(lines.size - 1)(k => s"$head\n${kept(k)}"))
  }
}
