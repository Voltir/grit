package grit.core.store

import grit.core.prompt.Voice

import utest.*

/** The contract every [[VoiceStore]] keeps, run against the in-memory fake in core and the SQL
  * store in grit.dbos. Each test is given a store of its own, since the voice is one per
  * database.
  */
abstract class VoiceContract extends TestSuite {

  /** A store with no voice set. */
  protected def fresh(): VoiceStore

  /** Runs `body` in one transaction, committed when it returns. */
  protected def transaction[A](body: (Tx^) ?=> A): A

  private def own(text: String): Voice =
    Voice.of(text).fold(e => throw new java.lang.AssertionError(e), identity)

  val tests = Tests {
    test("the voice is the default, sassy, until one is set") {
      val voices = fresh()
      transaction(voices.current()) ==> Right(Voice.Named.Sassy)
    }

    test("setting a voice replaces the one set, named or the person's own words") {
      val voices = fresh()
      transaction(voices.set(Voice.Named.Plain)) ==> Right(())
      transaction(voices.current()) ==> Right(Voice.Named.Plain)
      transaction(voices.set(own("be brief"))) ==> Right(())
      transaction(voices.current()) ==> Right(own("be brief"))
      transaction(voices.set(Voice.Named.Colleague)) ==> Right(())
      transaction(voices.current()) ==> Right(Voice.Named.Colleague)
    }
  }
}
