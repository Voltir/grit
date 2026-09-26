package grit.core.period

import grit.core.id.{LineId, PeriodSeq}

import utest.*

object BalanceTests extends TestSuite {

  private def p(n: Long): PeriodSeq =
    PeriodSeq.of(n).getOrElse(throw new java.lang.AssertionError(s"period $n"))

  private def line(section: Section, text: String, since: Long, touched: Long): Line =
    Line
      .of(section, text, p(since), p(touched))
      .fold(e => throw new java.lang.AssertionError(e), identity)

  private def balance(lines: Line*): Balance =
    Balance.of(lines.toVector).fold(e => throw new java.lang.AssertionError(e), identity)

  private def texts(b: Balance): Vector[String] = b.lines.map(_.text)

  val tests = Tests {
    test(
      "a line's id is the first 16 hex digits of SHA-256 of its section key, a newline, its text"
    ) {
      // Pinned against `printf 'open\nx' | sha256sum | cut -c1-16`, computed outside grit:
      // stored closings are keyed by it, so it may never change.
      LineId.value(line(Section.Open, "x", 1, 1).id) ==> "794f80eb6a946e33"
      LineId.value(line(Section.Standing, "Photos are renamed with exiftool", 1, 1).id) ==>
        "cfc0e92e80c6a05b"
    }

    test("an add enters its section last, normalised, with since and touched at its period") {
      val before = balance(line(Section.Open, "a", 1, 1), line(Section.Standing, "s", 1, 2))
      val after = before.edit(Vector(Edit.Add(Section.Open, "  new \n  item ")), p(3))
      val added = line(Section.Open, "new item", 3, 3)
      after.balance ==> balance(
        line(Section.Open, "a", 1, 1),
        line(Section.Standing, "s", 1, 2),
        added
      )
      after.changes ==> Vector(Change.Added(added))
    }

    test("an add of a line already there touches it instead, and adds no second one") {
      val before = balance(line(Section.Open, "a", 1, 1), line(Section.Open, "b", 1, 1))
      val after = before.edit(Vector(Edit.Add(Section.Open, "a")), p(2))
      after.balance ==> balance(line(Section.Open, "b", 1, 1), line(Section.Open, "a", 1, 2))
      after.changes ==> Vector()
    }

    test("resolve and drop take their line out, listed with how and why") {
      val a = line(Section.Open, "a", 1, 1)
      val s = line(Section.Standing, "s", 1, 1)
      val after =
        balance(a, s).edit(Vector(Edit.Resolve(a.id, "done"), Edit.Drop(s.id, "wrong")), p(2))
      after.balance ==> Balance.empty
      after.changes ==> Vector(Change.Resolved(a, "done"), Change.Dropped(s, "wrong"))
    }

    test("a touch sets touched to its period and moves the line last in its section") {
      val a = line(Section.Open, "a", 1, 1)
      val after =
        balance(a, line(Section.Open, "b", 1, 1)).edit(Vector(Edit.Touch(a.id)), p(4))
      after.balance ==> balance(line(Section.Open, "b", 1, 1), line(Section.Open, "a", 1, 4))
      after.changes ==> Vector()
    }

    test("an edit naming no line, a blank add, or an unread edit is ignored and changes nothing") {
      val a = line(Section.Open, "a", 1, 1)
      val gone = line(Section.Open, "gone", 1, 1).id
      val before = balance(a)
      val after = before.edit(
        Vector(
          Edit.Resolve(gone, "x"),
          Edit.Drop(gone, "y"),
          Edit.Touch(gone),
          Edit.Add(Section.Standing, "  "),
          Edit.Unread("o9: done", "names no line")
        ),
        p(2)
      )
      after.balance ==> before
      after.changes.map {
        case Change.Ignored(_, why) => why
        case other => s"not ignored: $other"
      } ==> Vector("names no line", "names no line", "names no line", "blank", "names no line")
    }

    test("under its cap a balance is kept as it is") {
      val b = balance(line(Section.Open, "abcd", 1, 1), line(Section.Standing, "ef", 1, 1))
      b.bytes ==> 6
      b.fit(6, p(2)) ==> Balance.Changed(b, Vector())
    }

    test("over its cap, the least recently touched line is evicted first") {
      // Strict on touched alone: since and id would pick the other.
      val older = line(Section.Open, "zz", 2, 2)
      val newer = line(Section.Open, "aa", 1, 3)
      balance(newer, older).fit(2, p(4)) ==>
        Balance.Changed(balance(newer), Vector(Change.Evicted(older)))
    }

    test("equally recently touched, the oldest since is evicted first") {
      val old = line(Section.Standing, "zz", 1, 3)
      val young = line(Section.Standing, "aa", 2, 3)
      balance(young, old).fit(2, p(4)) ==>
        Balance.Changed(balance(young), Vector(Change.Evicted(old)))
    }

    test("equal in touched and since, the lower id is evicted first") {
      val x = line(Section.Open, "x", 1, 1)
      val y = line(Section.Open, "y", 1, 1)
      val (first, second) =
        if (LineId.value(x.id) < LineId.value(y.id)) (x, y) else (y, x)
      balance(x, y).fit(1, p(2)) ==>
        Balance.Changed(balance(second), Vector(Change.Evicted(first)))
    }

    test("a period's own adds are refused rather than evict a line it touched") {
      // By (touched, since, id) alone the touched line (since 1) would go before the add.
      val touched = line(Section.Standing, "kept", 1, 5)
      val carried = line(Section.Open, "old", 1, 2)
      val add = line(Section.Open, "new!", 5, 5)
      balance(carried, touched, add).fit(4, p(5)) ==>
        Balance.Changed(
          balance(touched),
          Vector(Change.Evicted(carried), Change.Refused(add))
        )
    }

    test("the cap holds even below what the period touched, and a cap below 1 keeps nothing") {
      val b = balance(
        line(Section.Open, "one", 1, 3),
        line(Section.Open, "two", 2, 3),
        line(Section.Standing, "three", 3, 3),
        line(Section.Topics, "four", 1, 2)
      )
      for (cap <- -1 to 16) {
        val fitted = b.fit(cap, p(3))
        assert(fitted.balance.bytes <= math.max(cap, 0))
      }
      b.fit(0, p(3)).balance ==> Balance.empty
      texts(b.fit(3, p(3)).balance) ==> Vector("two")
    }
  }
}
