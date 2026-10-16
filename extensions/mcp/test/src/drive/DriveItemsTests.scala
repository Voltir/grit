package grit.mcp.drive

import grit.core.identity.{Domain, Email, Identities}
import grit.core.place.{Place, Service}
import grit.core.tool.Fitted
import grit.core.visibility.{Integrity, Itemised, Part, Provenance, Readers}

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
    DriveReading.of(
      Drive,
      Example,
      Identities
        .of(Vector.empty, Set(Example))
        .fold(e => throw new java.lang.AssertionError(e), identity),
      Self,
      Map(Bot -> Integrity.Member)
    )
  )

  private def place(id: String): Place = reading.place(Recorded.id(id))

  private def hits: Hits = right(DriveJson.hits(Recorded.json("search-fulltext")))

  private def file(id: String): DriveFile =
    hits.files.find(f => FileId.value(f.id) == id).getOrElse(throw new java.lang.AssertionError(id))

  private def grants(recording: String): Option[Vector[Grant]] =
    right(DriveJson.grants(Recorded.json(recording)))

  private def parts(called: DriveCalled): Vector[Part] = right(
    DriveItems.items(reading, called)
  ).parts

  /** What the read of `f` with `grants` is believed at. */
  private def believed(f: DriveFile, grants: Option[Vector[Grant]]): Vector[Integrity] =
    parts(DriveCalled.Read(f, grants, "text")).map(_.item.vouched)

  private val doc = file("file-1")

  val tests = Tests {
    test(
      "a search hit is a names part, at member, at the place it lies within; a folder's at the service's own place"
    ) {
      val folder = right(DriveJson.file(Recorded.json("metadata-folder")))
      parts(
        DriveCalled.Searched(Hits(Vector(file("file-4"), doc, folder), incomplete = false))
      ) ==> Vector(
        Part(
          Provenance(place("drive-a"), Integrity.Member),
          Readers.AtPlace,
          "- name-13 (a PDF), id file-4, in drive-a"
        ),
        Part(
          Provenance(place("folder-3"), Integrity.Member),
          Readers.AtPlace,
          "- name-10 (a Doc), id file-1, in folder-3"
        ),
        Part(
          Provenance(Drive.place, Integrity.Member),
          Readers.AtPlace,
          "- name-3 (a folder), id folder-2, in folder-1"
        )
      )
    }

    test(
      "a name that is not a plain title is not shown, the hit saying so; the head names no file"
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
            )
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
          "- A file whose name is not shown, since it is not a plain title (a Doc), id file-1, in folder-3"
        ) :+
          "- Q3 plan_v2 (a Doc), id file-1, in folder-3"
      )
    }

    test(
      "a read is believed at the meet over its writers: the deployment's own address skipped, a declared writer at its integrity, the domain's people member"
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
      val fitted = Fitted.of(items.head, items.parts)
      (
        fitted.parts.map(_.size),
        items.parts
          .map(_.text.linesIterator.toVector.last)
          .headOption
          .map(_.matches("\\[Only the first \\d+ of its 5000 lines are shown.\\]"))
      ) ==> (Some(1), Some(true))
    }
  }
}
