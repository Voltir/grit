package grit.mcp.drive

import grit.core.files.FileRead
import grit.core.host.{Clipped, Kept}
import grit.core.identity.Email
import grit.core.tool.{Outcome, Trimmed}
import grit.core.visibility.{Facts, Integrity, Itemised, Provenance, Readers, ResultPart}

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
    * Facts ([[DriveFacts]]), each part's text rendered from its own: a listed file's or a
    * folder's an entry, a drive's its drive, a read's what it opened, and a listing's head its
    * listing's, as Drive gave it; a search and its hits have none.
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
        Right(Itemised.Parts(head, names(reading, hits.files, f => (line(f, drives), None))))
      case DriveCalled.Drives(drives) =>
        val head = "The shared drives here, one a line, each with its id; list one's files and " +
          "folders by its id." +
          (if (drives.incomplete)
             s" Drive has more than these; only the first ${DriveRest.DrivesMost} are listed."
           else "") +
          unlabelled("listed")
        val parts = drives.names.toVector
          .map((id, name) => new DriveFacts.Drive(id, Some(name).filter(shown)))
          .sortBy(d => (d.name.getOrElse(""), FileId.value(d.id)))
          .map { d =>
            ResultPart(
              Provenance(reading.service.place, Integrity.Member),
              Readers.AtPlace,
              driveLine(d),
              Some(stated(d))
            )
          }
        Right(Itemised.Parts(head, parts, listing(called).map(stated)))
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
            names(
              reading,
              files.files,
              f => {
                val e = DriveFacts.entry(f)
                (entryLine(e), Some(stated(e)))
              }
            ),
            listing(called).map(stated)
          )
        )
      case DriveCalled.Read(file, grants, text) => opened(reading, file, grants, text, None)
      case DriveCalled.Parsed(file, grants, read) =>
        opened(reading, file, grants, read.shown, Some(read))
    }

  /** A read's parts: `text`, of `file`, kept as its part fits; `read`, what a format made of it. */
  private def opened(
      reading: DriveReading,
      file: DriveFile,
      grants: Option[Vector[Grant]],
      text: String,
      read: Option[FileRead]
  ): Either[String, Itemised.Parts] =
    file.within.toRight("This file's folder could not be read, so it is not shown.").map { at =>
      val head = "The Drive file asked for:"
      val part = readers(reading, file).map { r =>
        val (shown, opened) = fitted(head, DriveFacts.entry(file), text, read)
        ResultPart(
          Provenance(reading.place(at), believed(reading, grants)),
          r,
          shown,
          Some(stated(opened))
        )
      }
      Itemised.Parts(head, part.toVector)
    }

  /** `called` as the model reads it: [[items]]' head and parts, trimmed as [[Trimmed.of]] trims
    * them, with their facts ([[DriveFacts]]); a listing's own facts say it is incomplete when
    * Drive had more or a part was trimmed. `Left` as [[items]] says.
    */
  def answer(reading: DriveReading, called: DriveCalled): Either[String, Outcome.Done] =
    items(reading, called).map { items =>
      val trimmed = Trimmed.of(items.head, items.parts)
      val whole = trimmed.parts.exists(_.size == items.parts.size)
      Outcome.Done(
        trimmed.head,
        parts = trimmed.parts,
        // Written after trimming, so a listing cut here never says it is whole.
        facts = listing(called).map(l => stated(DriveFacts.Listing(l.complete && whole)))
      )
    }

  /** A listing's own facts, before any trimming: complete when Drive had no more; `None` for a
    * search or a read.
    */
  private def listing(called: DriveCalled): Option[DriveFacts.Listing] = called match {
    case DriveCalled.Drives(drives) => Some(DriveFacts.Listing(!drives.incomplete))
    case DriveCalled.Listed(files) => Some(DriveFacts.Listing(!files.incomplete))
    case DriveCalled.Searched(_, _) | DriveCalled.Read(_, _, _) | DriveCalled.Parsed(_, _, _) =>
      None
  }

  private def stated(facts: DriveFacts): Facts = Facts.of(DriveFacts.written(facts))

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

  /** Each of `files` a names part, its line and facts as `part` writes them, as [[items]] says
    * of a search's.
    */
  private def names(
      reading: DriveReading,
      files: Vector[DriveFile],
      part: DriveFile => (String, Option[Facts])
  ): Vector[ResultPart] =
    files.flatMap { f =>
      val place = if (f.folder) Some(reading.service.place) else f.within.map(reading.place)
      for {
        p <- place
        r <- readers(reading, f)
      } yield {
        val (line, facts) = part(f)
        ResultPart(Provenance(p, Integrity.Member), r, line, facts)
      }
    }

  /** The sentence every search's and listing's head ends with: files in folders no one has
    * labelled yet aren't `done` (searched, listed), and how an administrator opens one.
    */
  private def unlabelled(done: String): String =
    s" Files in folders that no one has labelled yet aren't $done. An administrator can open " +
      s"such a folder by typing $Opening."

  /** When `e` last changed, after a comma, as Drive gives it; nothing when Drive does not. */
  private def since(e: DriveFacts.Entry): String = e.changed.fold("")(t => s", last changed $t")

  /** `e`'s name as shown, and its type. */
  private def title(e: DriveFacts.Entry): String = e.name match {
    case Some(name) => s"$name (${e.kind.words})"
    case None => s"A file whose name is not shown, since it is not a plain title (${e.kind.words})"
  }

  /** A listed file's line. */
  private def entryLine(e: DriveFacts.Entry): String =
    s"- ${title(e)}, id ${FileId.value(e.id)}${since(e)}"

  /** A shared drive's line. */
  private def driveLine(d: DriveFacts.Drive): String = {
    val title =
      d.name.fold("A shared drive whose name is not shown, since it is not a plain title")(name =>
        s"$name (a shared drive)"
      )
    s"- $title, id ${FileId.value(d.id)}"
  }

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
    val e = DriveFacts.entry(file)
    s"- ${title(e)}, id ${FileId.value(e.id)}$where${since(e)}"
  }

  /** A read's part: `entry`'s title, when it last changed, and then `text`'s leading whole
    * lines, as many as fit after `head` within [[Clipped]]'s limits, with [[Clipped.cutHint]]'s
    * hint when some were not; beside it, the facts of what it kept.
    */
  private def fitted(
      head: String,
      entry: DriveFacts.Entry,
      text: String,
      read: Option[FileRead]
  ): (String, DriveFacts.Opened) = {
    val named = s"${title(entry)}${since(entry)}:"
    val lines = Clipped.lines(text)
    // `named` and `k` lines fit whole by the search below, so `head` keeps them, and its hint
    // is said of the text's lines, never of the cut it did not make.
    def kept(k: Int): String =
      Clipped
        .head(
          (named +: lines.take(k)).mkString("\n"),
          _ => Clipped.cutHint(Option.when(k < lines.size)(Kept(k, lines.size, partial = false)))
        )
        .show
    // The whole text is asked of unclipped: `kept` of every line cuts it with no hint when it
    // is too long alone, and that cut may still fit after `head`.
    val k =
      if (Clipped.fits((head +: named +: lines).mkString("\n"))) lines.size
      else Clipped.most(lines.size - 1)(k => s"$head\n${kept(k)}")
    (
      kept(k),
      DriveFacts.Opened(
        entry,
        lines.take(k).mkString("\n"),
        k == lines.size && read.forall(_.whole),
        read.map(_.covered)
      )
    )
  }
}
