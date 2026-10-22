package grit.core.id

import utest.*

/** [[Sha256]]: one written form, 64 lowercase hex digits, and its 32 bytes. */
object Sha256Tests extends TestSuite {

  // SHA-256 of "abc", FIPS 180-2's first example.
  private val Abc = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"

  val tests = Tests {
    test("a digest is written as 64 lowercase hex digits, and its bytes give it back") {
      val sha = Sha256.digest("abc".getBytes("UTF-8").toVector)
      (sha.hex, Sha256.of(sha.bytes), Sha256.read(Abc)) ==> (Abc, Some(sha), Some(sha))
    }
    test("text that is not 64 lowercase hex digits, and bytes that are not 32, are no digest") {
      (
        Sha256.read(Abc.toUpperCase),
        Sha256.read(Abc.drop(1)),
        Sha256.read(Abc.dropRight(1) + "g"),
        Sha256.of(Vector.fill(31)(0.toByte))
      ) ==> (None, None, None, None)
    }
  }
}
