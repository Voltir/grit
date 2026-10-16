package grit.mcp.drive

import grit.core.identity.{Domain, Email, Identities}
import grit.core.place.{Place, Service}
import grit.core.visibility.Integrity

import utest.*

/** [[DriveReading]]: the declarations a Drive is read with, and where its files are kept. */
object DriveReadingTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val Drive: Service = right(Service.of("drive"))
  private val Example: Domain = right(Domain.of("example.com"))
  private val Claimed: Identities =
    Identities
      .of(Vector.empty, Set(Example))
      .fold(e => throw new java.lang.AssertionError(e), identity)
  private val Self: Email = right(Email.of("self@project.iam.example"))

  private def of(identities: Identities, writers: String*): Either[String, DriveReading] =
    DriveReading
      .of(
        Drive,
        Example,
        identities,
        Self,
        writers.map(w => right(Email.of(w)) -> Integrity.Member).toMap
      )
      .left
      .map(_.message)

  val tests = Tests {
    test(
      "an unclaimed domain is refused; a declared writer in the claimed domain, or the deployment's own address declared a writer, is refused"
    ) {
      (
        of(Identities.Shipped).map(_ => ()),
        of(Claimed, "person-1@example.com").map(_ => ()),
        of(Claimed, "self@project.iam.example").map(_ => ())
      ) ==> (
        Left("Drive's domain example.com is not one the deployment claims"),
        Left("a Drive writer in example.com is one of its people: person-1@example.com"),
        Left("the deployment's own address self@project.iam.example is never a Drive writer")
      )
    }

    test("a folder's or a drive's files are kept at its id under the service's place") {
      of(Claimed, "writer-bot-1@project.iam.example").map(_.place(Recorded.id("folder-3"))) ==>
        Place.read("service:drive/folder-3")
    }

    test(
      "a folder's or a shared drive's link, in each form Drive gives it, or an id of 19 characters or more alone, names the place its files are kept; a file's link, a shorter word or another site's link names none"
    ) {
      val names = right(of(Claimed)).names
      val folder = Place.read("service:drive/1AbC-dEf_GhIjKlMnOpQrStUvWxYz01234")
      val drive = Place.read("service:drive/0AbCdEfGhIjKlUk9PVA")
      (
        Vector(
          "https://drive.google.com/drive/folders/1AbC-dEf_GhIjKlMnOpQrStUvWxYz01234",
          "https://drive.google.com/drive/u/0/folders/1AbC-dEf_GhIjKlMnOpQrStUvWxYz01234?usp=sharing",
          "drive.google.com/drive/folders/1AbC-dEf_GhIjKlMnOpQrStUvWxYz01234/",
          "https://drive.google.com/open?id=1AbC-dEf_GhIjKlMnOpQrStUvWxYz01234",
          "1AbC-dEf_GhIjKlMnOpQrStUvWxYz01234"
        ).map(names.place),
        Vector("https://drive.google.com/drive/folders/0AbCdEfGhIjKlUk9PVA", "0AbCdEfGhIjKlUk9PVA")
          .map(names.place),
        Vector(
          "https://docs.google.com/document/d/1AbC-dEf_GhIjKlMnOpQrStUvWxYz01234/edit",
          "0AbCdEfGhIjKlUk9PV",
          "engineering",
          "https://example.com/drive/folders/1AbC-dEf_GhIjKlMnOpQrStUvWxYz01234",
          "https://drive.google.com/drive/folders/has/a/slash"
        ).map(names.place)
      ) ==> (
        Vector.fill(5)(folder.toOption),
        Vector.fill(2)(drive.toOption),
        Vector.fill(5)(None)
      )
    }
  }
}
