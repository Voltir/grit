package grit.mcp.drive

import grit.core.identity.{Domain, Email, Identities}
import grit.core.place.{Place, Service}
import grit.core.tool.{Outcome, Trimmed}
import grit.core.visibility.{Integrity, Provenance, Readers, ResultPart}

import utest.*

/** [[DriveItems]] over Drive's recorded answers, read as a deployment claiming `example.com`
  * reads them.
  */
object DriveItemsTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private def email(e: String): Email = right(Email.of(e))

  private val Drive: Service = right(Service.of("drive"))
  private val Example: Domain = right(Domain.of("example.com"))
  private val Self = email("self@project.iam.example")
  private val Bot = email("writer-bot-1@project.iam.example")

  private val reading: DriveReading = right(
    DriveReading
      .of(
        Drive,
        Example,
        Identities
          .of(Vector.empty, Set(Example))
          .fold(e => throw new java.lang.AssertionError(e), identity),
        Self,
        Map(Bot -> Integrity.Member)
      )
      .left
      .map(_.message)
  )

  private def place(id: String): Place = reading.place(Recorded.id(id))

  private def hits: Hits = right(DriveJson.hits(Recorded.json("search-fulltext")))

  private def file(id: String): DriveFile =
    hits.files.find(f => FileId.value(f.id) == id).getOrElse(throw new java.lang.AssertionError(id))

  private def grants(recording: String): Option[Vector[Grant]] =
    right(DriveJson.grants(Recorded.json(recording)))

  private def parts(called: DriveCalled): Vector[ResultPart] = right(
    DriveItems.items(reading, called)
  ).parts

  /** What the read of `f` with `grants` is believed at. */
  private def believed(f: DriveFile, grants: Option[Vector[Grant]]): Vector[Integrity] =
    parts(DriveCalled.Read(f, grants, "text")).map(_.item.vouched)

  private val doc = file("file-1")

  val tests = Tests {
    test(
      "a search hit is a names part, at member, at the place it lies within, a folder's at the service's own place; each names its shared drive as listed, by its id when it is not"
    ) {
      val folder = right(DriveJson.file(Recorded.json("metadata-folder")))
      parts(
        DriveCalled.Searched(
          Hits(Vector(file("file-4"), doc, folder), incomplete = false),
          Map(Recorded.id("drive-a") -> "Engineering", Recorded.id("drive-c") -> "Finance")
        )
      ) ==> Vector(
        ResultPart(
          Provenance(place("drive-a"), Integrity.Member),
          Readers.AtPlace,
          "- name-13 (a PDF), id file-4, in the shared drive Engineering (drive-a)"
        ),
        ResultPart(
          Provenance(place("folder-3"), Integrity.Member),
          Readers.AtPlace,
          "- name-10 (a Doc), id file-1, in folder-3, in the shared drive Engineering"
        ),
        ResultPart(
          Provenance(Drive.place, Integrity.Member),
          Readers.AtPlace,
          "- name-3 (a folder), id folder-2, in folder-1, in the shared drive drive-b"
        )
      )
    }

    test(
      "a name that is not a plain title is not shown, the hit saying so, and a drive's is named by its id; the head names no file"
    ) {
      val named = (name: String) => doc.copy(name = name)
      val items = right(
        DriveItems.items(
          reading,
          DriveCalled.Searched(
            Hits(
              Vector(
                named("see https://example.com"),
                named("two\nlines"),
                named("x" * 81),
                named("<b>plan</b>"),
                named("Q3 plan_v2")
              ),
              incomplete = true
            ),
            Map(Recorded.id("drive-a") -> "see https://example.com")
          )
        )
      )
      (
        items.head.contains("name-10"),
        items.head.contains("Drive has more than these"),
        items.parts.map(_.text)
      ) ==> (
        false,
        true,
        Vector.fill(4)(
          "- A file whose name is not shown, since it is not a plain title (a Doc), id file-1, in folder-3, in the shared drive drive-a"
        ) :+
          "- Q3 plan_v2 (a Doc), id file-1, in folder-3, in the shared drive drive-a"
      )
    }

    test(
      "a search's head says files in folders no one has labelled yet aren't searched, and how an administrator opens one, whether it found any hit or none"
    ) {
      val head = (files: Vector[DriveFile]) =>
        right(
          DriveItems.items(
            reading,
            DriveCalled.Searched(Hits(files, incomplete = false), Map.empty)
          )
        ).head
      val opens =
        "Files in folders that no one has labelled yet aren't searched. An administrator can " +
          "open such a folder by typing `label internal at <the folder's link or id>`."
      (head(Vector.empty), head(Vector(doc))) ==> (
        "Drive files whose text holds those words, one a line, each with its id and when it " +
          s"last changed; read one with drive_read. $opens",
        "Drive files whose text holds those words, one a line, each with its id and when it " +
          s"last changed; read one with drive_read. $opens"
      )
    }

    test("a search hit names when its file last changed, as a listing's does, when Drive says") {
      val changed = doc.copy(changed = Some(java.time.Instant.parse("2026-09-05T10:00:00Z")))
      parts(DriveCalled.Searched(Hits(Vector(changed, doc), incomplete = false), Map.empty))
        .map(_.text) ==> Vector(
        "- name-10 (a Doc), id file-1, in folder-3, in the shared drive drive-a, last changed " +
          "2026-09-05T10:00:00Z",
        "- name-10 (a Doc), id file-1, in folder-3, in the shared drive drive-a"
      )
    }

    test(
      "a listing's file is a names part at member at the place it lies within, a folder's at the service's own place, each line naming when it last changed; a file lying nowhere is left out"
    ) {
      val listed = right(DriveJson.hits(Recorded.json("made-children-drive-a")))
      val nowhere = doc.copy(parent = None, drive = None)
      val got = parts(DriveCalled.Listed(listed.copy(files = listed.files :+ nowhere)))
      (got.size, got.take(2).map(_.copy(facts = None))) ==> (
        7,
        Vector(
          ResultPart(
            Provenance(Drive.place, Integrity.Member),
            Readers.AtPlace,
            "- name-5 (a folder), id folder-3, last changed 2026-09-01T10:00:00Z"
          ),
          ResultPart(
            Provenance(place("drive-a"), Integrity.Member),
            Readers.AtPlace,
            "- name-13 (a PDF), id file-4, last changed 2026-09-02T10:00:00Z"
          )
        )
      )
    }

    test(
      "a listing's head says files in folders no one has labelled yet aren't listed, and how an administrator opens one; one Drive had more of says so"
    ) {
      val head = (incomplete: Boolean) =>
        right(DriveItems.items(reading, DriveCalled.Listed(Hits(Vector(doc), incomplete)))).head
      val opens =
        "Files in folders that no one has labelled yet aren't listed. An administrator can " +
          "open such a folder by typing `label internal at <the folder's link or id>`."
      val lead = "The files and folders directly in that folder, one a line, each with its id " +
        "and when it last changed; read a file, or list a folder, by its id."
      (head(false), head(true)) ==> (
        s"$lead $opens",
        s"$lead Drive has more than these; only the first 100 are listed. $opens"
      )
    }

    test("the shared drives' head says, when Drive had more, that only the first are listed") {
      val head = (incomplete: Boolean) =>
        right(
          DriveItems.items(
            reading,
            DriveCalled.Drives(SharedDrives(Map(Recorded.id("drive-a") -> "a"), incomplete))
          )
        ).head
      val opens =
        "Files in folders that no one has labelled yet aren't listed. An administrator can " +
          "open such a folder by typing `label internal at <the folder's link or id>`."
      val lead = "The shared drives here, one a line, each with its id; list one's files and " +
        "folders by its id."
      (head(false), head(true)) ==> (
        s"$lead $opens",
        s"$lead Drive has more than these; only the first 100 are listed. $opens"
      )
    }

    test(
      "the shared drives are names parts at the service's own place, by name, one whose name is not a plain title saying so"
    ) {
      val listed = right(
        DriveItems.items(
          reading,
          DriveCalled.Drives(
            SharedDrives(
              Map(
                Recorded.id("drive-b") -> "Finance",
                Recorded.id("drive-a") -> "Engineering",
                Recorded.id("drive-c") -> "<b>x</b>"
              ),
              incomplete = false
            )
          )
        )
      )
      // Their facts are pinned apart, below.
      (listed.head, listed.parts.map(_.copy(facts = None))) ==> (
        "The shared drives here, one a line, each with its id; list one's files and folders " +
          "by its id. Files in folders that no one has labelled yet aren't listed. An " +
          "administrator can open such a folder by typing " +
          "`label internal at <the folder's link or id>`.",
        Vector(
          ResultPart(
            Provenance(Drive.place, Integrity.Member),
            Readers.AtPlace,
            "- A shared drive whose name is not shown, since it is not a plain title, id drive-c"
          ),
          ResultPart(
            Provenance(Drive.place, Integrity.Member),
            Readers.AtPlace,
            "- Engineering (a shared drive), id drive-a"
          ),
          ResultPart(
            Provenance(Drive.place, Integrity.Member),
            Readers.AtPlace,
            "- Finance (a shared drive), id drive-b"
          )
        )
      )
    }

    test("a read's part names when its file last changed, when Drive says") {
      val changed = doc.copy(changed = Some(java.time.Instant.parse("2026-09-05T10:00:00Z")))
      parts(DriveCalled.Read(changed, grants("permissions-drive-a"), "line one")).map(_.text) ==>
        Vector("name-10 (a Doc), last changed 2026-09-05T10:00:00Z:\nline one")
    }

    test(
      "a read is believed at the meet over its writers:the deployment's own address skipped, a declared writer at its integrity, the domain's people member"
    ) {
      (
        believed(doc, grants("permissions-drive-a")),
        believed(doc, grants("permissions-drive-b"))
      ) ==> (Vector(Integrity.Member), Vector(Integrity.Member))
    }

    test(
      "a writer outside the domain, a group or another domain is outside; anyone, unread grants or no writer but grit untrusted"
    ) {
      val people = grants("permissions-drive-a").toVector.flatten
      val also = (g: Grant) => Some(people :+ g)
      (
        believed(doc, also(Grant.Person(email("person@gmail.example"), Role.Writer))),
        believed(doc, also(Grant.Group(email("team@example.com"), Role.FileOrganizer))),
        believed(doc, also(Grant.Everyone(right(Domain.of("sub.example.com")), Role.Writer))),
        believed(doc, also(Grant.Person(email("person@gmail.example"), Role.Commenter))),
        believed(doc, also(Grant.Anyone(Role.Writer))),
        believed(doc, None),
        believed(doc, Some(Vector(Grant.Person(Self, Role.Writer))))
      ) ==> (
        Vector(Integrity.Outside),
        Vector(Integrity.Outside),
        Vector(Integrity.Outside),
        Vector(Integrity.Member),
        Vector(Integrity.Untrusted),
        Vector(Integrity.Untrusted),
        Vector(Integrity.Untrusted)
      )
    }

    test(
      "a file owned in the domain is read only by its owner; one owned outside it, or in a subdomain, by no one"
    ) {
      val owned = (owner: String) =>
        parts(DriveCalled.Read(doc.copy(owners = Vector(email(owner))), None, "text"))
          .map(_.readers)
      (
        owned("person-1@example.com"),
        owned("person@gmail.example"),
        owned("person@sub.example.com")
      ) ==>
        (Vector(Readers.Originator(email("person-1@example.com"))), Vector.empty, Vector.empty)
    }

    test(
      "a read's part holds its name and text at the place it lies within; a file lying nowhere is not placed"
    ) {
      (
        parts(DriveCalled.Read(doc, grants("permissions-drive-a"), "line one\nline two")).map(p =>
          (p.item.place, p.text)
        ),
        DriveItems.items(
          reading,
          DriveCalled.Read(doc.copy(parent = None, drive = None), None, "text")
        )
      ) ==> (
        Vector((place("folder-3"), "name-10 (a Doc):\nline one\nline two")),
        Left("This file's folder could not be read, so it is not shown.")
      )
    }

    test(
      "a long read is cut to its leading whole lines, saying how many, so its part is kept when fitted"
    ) {
      val line = "text text text text text text text text"
      val items = right(
        DriveItems.items(
          reading,
          DriveCalled.Read(doc, None, Vector.fill(5000)(line).mkString("\n"))
        )
      )
      val fitted = Trimmed.of(items.head, items.parts)
      val shown = items.parts.headOption.toVector.flatMap(_.text.linesIterator.toVector)
      val note = "\\[The answer was cut to its first (\\d+) of 5000 lines.\\]".r
      // Its name's line, then the lines the note counts, a blank line, then the note.
      val counted = shown.lastOption.collect { case note(k) => k.toInt }
      (
        fitted.parts.map(_.size),
        counted.map(_ + 3),
        shown.slice(1, shown.size - 2).forall(_ == line),
        shown.lift(shown.size - 2)
      ) ==> (Some(1), Some(shown.size), true, Some(""))
    }

    test(
      "each listed part's facts name its id, kind and change time, and its name only when its line shows it"
    ) {
      val listed = right(DriveJson.hits(Recorded.json("made-children-folder-3")))
      val odd = doc.copy(name = "<b>x</b>", changed = None)
      val files =
        parts(DriveCalled.Listed(listed.copy(files = listed.files.take(2) :+ odd))).map(p =>
          (p.text, p.facts.map(_.json))
        )
      val drives = parts(
        DriveCalled.Drives(
          SharedDrives(
            Map(Recorded.id("drive-a") -> "Engineering", Recorded.id("drive-c") -> "<b>x</b>"),
            incomplete = false
          )
        )
      ).map(_.facts.map(_.json))
      (files, drives) ==> (
        Vector(
          (
            "- name-10 (a Doc), id file-1, last changed 2026-09-08T10:00:00Z",
            Some(
              ujson.Obj(
                "id" -> "file-1",
                "kind" -> "document",
                "name" -> "name-10",
                "changed" -> "2026-09-08T10:00:00Z",
                "within" -> "folder-3"
              )
            )
          ),
          (
            "- name-12 (a file of type application/vnd.openxmlformats-officedocument.wordprocessingml.document), id file-3, last changed 2026-09-09T10:00:00Z",
            Some(
              ujson.Obj(
                "id" -> "file-3",
                "kind" -> "other",
                "type" -> "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
                "name" -> "name-12",
                "changed" -> "2026-09-09T10:00:00Z",
                "within" -> "folder-3"
              )
            )
          ),
          (
            "- A file whose name is not shown, since it is not a plain title (a Doc), id file-1",
            Some(ujson.Obj("id" -> "file-1", "kind" -> "document", "within" -> "folder-3"))
          )
        ),
        Vector(
          Some(ujson.Obj("id" -> "drive-c", "kind" -> "drive")),
          Some(ujson.Obj("id" -> "drive-a", "kind" -> "drive", "name" -> "Engineering"))
        )
      )
    }

    test("a listing's own facts say whether Drive had more; a search's and a read's have none") {
      val facts =
        (called: DriveCalled) => right(DriveItems.items(reading, called)).facts.map(_.json)
      (
        facts(DriveCalled.Listed(Hits(Vector(doc), incomplete = false))),
        facts(DriveCalled.Listed(Hits(Vector(doc), incomplete = true))),
        facts(DriveCalled.Drives(SharedDrives(Map.empty, incomplete = true))),
        facts(DriveCalled.Searched(Hits(Vector(doc), incomplete = false), Map.empty)),
        facts(DriveCalled.Read(doc, None, "text"))
      ) ==> (
        Some(ujson.Obj("complete" -> true)),
        Some(ujson.Obj("complete" -> false)),
        Some(ujson.Obj("complete" -> false)),
        None,
        None
      )
    }

    test("a read's facts hold its text as its part shows it, and whether that is all of it") {
      val line = "text text text text text text text text"
      val read = (text: String) =>
        parts(DriveCalled.Read(doc, None, text)).flatMap(p => p.facts.map(f => (p.text, f.json)))
      val short = read("line one\nline two")
      val long = read(Vector.fill(5000)(line).mkString("\n"))
      val kept = long.headOption.toVector.flatMap((text, _) => text.linesIterator.toVector)
      (
        short.map(_._2),
        long.map((_, f) => (f("text").str, f("complete").bool)),
        long.map((_, f) => f.obj.keySet.toSet -- Set("text", "complete"))
      ) ==> (
        Vector(
          ujson.Obj(
            "id" -> "file-1",
            "kind" -> "document",
            "name" -> "name-10",
            "within" -> "folder-3",
            "text" -> "line one\nline two",
            "complete" -> true
          )
        ),
        // Its name's line, then the kept lines, a blank line and the cut's note.
        Vector((kept.slice(1, kept.size - 2).mkString("\n"), false)),
        Vector(Set("id", "kind", "name", "within"))
      )
    }

    test("a listing cut for size says in its facts that it is incomplete") {
      val many = Vector.tabulate(3000)(i => doc.copy(id = Recorded.id(s"file-$i")))
      val answered = (files: Vector[DriveFile]) =>
        right(
          DriveItems.answer(reading, DriveCalled.Listed(Hits(files, incomplete = false)))
        ) match {
          case Outcome.Done(_, _, _, parts, facts) => (parts.map(_.size), facts.map(_.json))
        }
      val (kept, cut) = answered(many)
      (kept.exists(k => k > 0 && k < many.size), cut, answered(Vector(doc))) ==> (
        true,
        Some(ujson.Obj("complete" -> false)),
        (Some(1), Some(ujson.Obj("complete" -> true)))
      )
    }
  }
}
