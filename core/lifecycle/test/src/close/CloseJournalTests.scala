package grit.lifecycle.close

import grit.core.id.TurnSeq
import grit.core.message.{Tokens, Usage}
import grit.core.period.{CloseReason, Ground, Section, TestClosings}

import utest.*
import Close.{Checked, Cost, Summarised}

/** [[CloseJournal]]: the close's recorded step outputs, read back across versions. */
object CloseJournalTests extends TestSuite {

  private val j = CloseJournal.checked

  val tests = Tests {
    // Pins of recorded forms: a close in flight reads back what an earlier build wrote.
    test("a check record without a version reads its balance as version 2; one with 3 strictly") {
      val v2 =
        """{"ok":{"due":"lapsed","first":0,"known":{"standing":[{"text":"s","since":1,"touched":1}]},"cap":4096}}"""
      j.decode(v2)
        .map(_.map {
          case Checked.Due(_, _, known, _) => known.lines.map(_.ground)
          case other => Vector(other)
        }) ==> Right(Right(Vector(Some(Ground.Claimed))))
      val v3 =
        """{"ok":{"due":"lapsed","first":0,"known":{"standing":[{"text":"s","since":1,"touched":1}]},"cap":4096,"v":3}}"""
      j.decode(v3) ==> Left("a standing line has no ground: s")
    }

    test("a check record is written with its balance's version, and reads back") {
      val known = TestClosings.balance(
        TestClosings.line(Section.Standing, "s", 1, 1, ground = Ground.Tool)
      )
      val due: Either[String, Checked] =
        Right(Checked.Due(TurnSeq(0), CloseReason.Lapsed, known, 4096))
      j.encode(due) ==>
        """{"ok":{"due":"lapsed","first":0,"known":{"open":[],"standing":[{"text":"s","since":1,"touched":1,"ground":"tool"}],"topics":[]},"cap":4096,"v":3}}"""
      j.decode(j.encode(due)) ==> Right(due)
    }

    test("a summary's cost keeps who served it, and one recorded before reads with none") {
      val s = CloseJournal.summarised
      val usage = Usage(Tokens(3), Tokens(2), Tokens.Zero, None)
      val served = Summarised(None, Some(Cost("m", usage, Tokens(4), Some("Fireworks"))), None)
      s.decode(s.encode(served)) ==> Right(served)
      val before =
        """{"cost":{"model":"m","usage":{"input":3,"output":2,"cachedInput":0},"estimate":4}}"""
      s.decode(before).map(_.cost.flatMap(_.upstream)) ==> Right(None)
    }
  }
}
