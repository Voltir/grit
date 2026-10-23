package grit.knowledge.drive

import java.time.Instant

import utest.*

/** Drive's facts as knowledge decodes them, over the goldens `grit.app`'s `DriveFactsPinTests`
  * holds equal to what grit.mcp answers.
  */
object DriveFactsTests extends TestSuite {

  private def at(day: Int): Option[Instant] =
    Some(Instant.parse(f"2026-09-$day%02dT10:00:00Z"))

  /** The golden `name` with `key` removed from its part `n` (from 1). */
  private def without(name: String, n: Int, key: String): ujson.Value = {
    val v = FactsGoldens.json(name)
    v("parts").arr(n - 1).obj.remove(key)
    v
  }

  val tests = Tests {
    test("the shared drives' golden decodes as each drive, by id and name, the listing complete") {
      val listing = DriveListing.read(FactsGoldens.stated("drives"))
      assert(
        listing == Right(
          DriveListing(
            Vector(
              DriveEntry("drive-a", DriveEntry.Kind.Drive, Some("name-1"), None, None),
              DriveEntry("drive-b", DriveEntry.Kind.Drive, Some("name-2"), None, None)
            ),
            complete = true
          )
        )
      )
    }

    test("a drive's listing decodes each entry's kind, name, change time and drive, in order") {
      val listing = DriveListing.read(FactsGoldens.stated("listing-drive-a"))
      val kinds = listing.map(_.entries.map(e => (e.id, e.kind)))
      assert(
        kinds == Right(
          Vector(
            ("folder-3", DriveEntry.Kind.Folder),
            ("file-4", DriveEntry.Kind.Pdf),
            ("file-5", DriveEntry.Kind.Pdf),
            ("file-6", DriveEntry.Kind.Pdf),
            ("file-18", DriveEntry.Kind.Document),
            ("file-21", DriveEntry.Kind.Spreadsheet),
            ("file-22", DriveEntry.Kind.Other("other"))
          )
        )
      )
      val first = listing.map(_.entries.headOption)
      assert(
        first == Right(
          Some(
            DriveEntry("folder-3", DriveEntry.Kind.Folder, Some("name-5"), at(1), Some("drive-a"))
          )
        )
      )
      assert(listing.map(_.complete) == Right(true))
    }

    test("a folder's listing decodes its text file as readable, within the folder") {
      val listing = DriveListing.read(FactsGoldens.stated("listing-folder-3"))
      val source = DriveCardsTests.cards(Vector(DriveCardsTests.internal))
      val text = listing.map(_.entries.filter(e => source.cards(e.kind)).map(_.id))
      assert(text == Right(Vector("file-1", "file-text")))
      assert(listing.map(_.entries.map(_.within).distinct) == Right(Vector(Some("folder-3"))))
    }

    test("a read's golden decodes as its entry, its text and whether that is all of it") {
      val opened = DriveOpened.read(FactsGoldens.stated("read-file-text"))
      assert(
        opened == Right(
          DriveOpened.Read(
            DriveEntry(
              "file-text",
              DriveEntry.Kind.Text,
              Some("name-30"),
              at(13),
              Some("folder-3")
            ),
            "text text.\ntext text, text.",
            complete = true,
            file = None
          )
        )
      )
    }

    test(
      "a PDF's read by pages decodes as its entry, its text, not all of it, and the handle the area keeps it under"
    ) {
      val opened = DriveOpened.read(FactsGoldens.stated("read-file-pdf"))
      assert(
        opened.map {
          case DriveOpened.Read(e, _, complete, file) =>
            Some((e.id, e.kind, complete, file.map(_.written)))
          case DriveOpened.Unread(_, _) => None
        } == Right(Some(("file-4", DriveEntry.Kind.Pdf, false, Some("f41"))))
      )
    }

    test("an unreadable file's golden decodes as its entry and why it was not read") {
      val opened = DriveOpened.read(FactsGoldens.stated("read-file-unread"))
      assert(
        opened.map {
          case DriveOpened.Unread(e, why) => Some((e.id, e.kind, why))
          case DriveOpened.Read(_, _, _, _) => None
        } == Right(
          Some(
            (
              "file-4",
              DriveEntry.Kind.Pdf,
              "That file looks damaged, or isn't really a PDF, so it couldn't be read. " +
                "Opening it and saving it again may fix it."
            )
          )
        )
      )
    }

    test("a part missing a key is refused, naming the part and the key") {
      val listing = DriveListing.read(FactsGoldens.stated(without("listing-drive-a", 2, "id")))
      assert(listing == Left("part 2 states no `id`"))
      val opened = DriveOpened.read(FactsGoldens.stated(without("read-file-text", 1, "text")))
      assert(opened == Left("part 1 states no `text`"))
    }

    test("a listing stating no facts of its own is refused as not saying whether it is complete") {
      val v = FactsGoldens.json("drives")
      v.obj.remove("answer")
      assert(DriveListing.read(FactsGoldens.stated(v)) == Left("the listing states no `complete`"))
    }

    test("a change time not in ISO-8601 is refused, naming the key") {
      val v = FactsGoldens.json("listing-folder-3")
      v("parts").arr(0)("changed") = "yesterday"
      assert(
        DriveListing.read(FactsGoldens.stated(v)) == Left(
          "part 1's `changed` is no ISO-8601 instant"
        )
      )
    }

    test("a read stating two parts is refused, counting them") {
      val v = FactsGoldens.json("read-file-text")
      v("parts").arr.append(v("parts").arr(0))
      assert(DriveOpened.read(FactsGoldens.stated(v)) == Left("a read states one part, not 2"))
    }
  }
}
