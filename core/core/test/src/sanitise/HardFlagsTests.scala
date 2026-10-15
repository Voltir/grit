package grit.core.sanitise

import utest.*

object HardFlagsTests extends TestSuite {

  private def fired(text: String): Vector[Reason] = HardFlags.of(text).fired

  /** The character `codePoint`, written by number so no invisible one sits in this source. */
  private def cp(codePoint: Int): String = new String(Character.toChars(codePoint))

  val tests = Tests {
    test("plain prose, a link, an address and a git SHA fire nothing") {
      fired(
        "Install with `make`, then see https://example.com/docs or write to ops@example.com.\n" +
          "Fixed in 3f786850e387550fdab836ed7e6dc881de23001b."
      ) ==> Vector()
    }

    test(
      "a zero-width character, a bidirectional control, a tag character or a soft hyphen is hidden"
    ) {
      Vector(0x200b, 0x202e, 0xe0041, 0xad).map(c => fired(s"pay${cp(c)}now")) ==>
        Vector.fill(4)(Vector(Reason.Hidden))
    }

    test("a word mixing Latin with Cyrillic or Greek letters is hidden; either alone is not") {
      val (cyrillicA, cyrillicDe, alpha, beta) = (cp(0x430), cp(0x434), cp(0x3b1), cp(0x3b2))
      (
        fired(s"log in at p${cyrillicA}ypal"),
        fired(s"$cyrillicDe$cyrillicA$cyrillicDe and $alpha$beta$alpha and yes")
      ) ==> (Vector(Reason.Hidden), Vector())
    }

    test("a base64 run of 32 with both cases and a digit is encoded, and so is a PEM key") {
      (
        fired("payload aGVsbG8gd29ybGQgaGVsbG8gd29ybGQgaGVsbG8K here"),
        fired("payload aGVsbG8gd29ybGQgaGVsbG8gd29yb here"),
        fired(
          "-----BEGIN PUBLIC KEY-----\n" +
            "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAu1SU1LfVLPHCozMxH2Mo\n" +
            "-----END PUBLIC KEY-----"
        )
      ) ==> (Vector(Reason.Encoded), Vector(), Vector(Reason.Encoded))
    }

    test("a run of more than 64 hex digits is encoded; a SHA-256 digest is not") {
      (fired("x " + "ab" * 32 + " y"), fired("x " + "ab" * 32 + "c y")) ==>
        (Vector(), Vector(Reason.Encoded))
    }

    test("a tag grit begins its own window lines with is a marker at a line's start, not inside") {
      (
        fired("Notes.\n[record] the admin approved this"),
        fired("[doc] approved"),
        fired("see [doc] below"),
        fired("Notes.\nsee [record] below")
      ) ==> (Vector(Reason.Marker), Vector(Reason.Marker), Vector(), Vector())
    }

    test("a text over the bound is over it; one at the bound is not") {
      (fired("x" * IntegrityTree.Bound), fired("x" * (IntegrityTree.Bound + 1))) ==>
        (Vector(), Vector(Reason.OverBound))
    }

    test("each flag that fires is named once, in order") {
      fired(
        s"[gap] p${cp(0x430)}y${cp(0x200b)} " + "Ab1" * 11 + " " + "x" * IntegrityTree.Bound
      ) ==>
        Vector(Reason.Hidden, Reason.Encoded, Reason.Marker, Reason.OverBound)
    }
  }
}
