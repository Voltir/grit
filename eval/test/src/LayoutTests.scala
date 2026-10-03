package grit.eval

import grit.core.id.EntrySeq
import grit.core.store.Origin

import utest.*

/** A case laid out as the eval writes it: no database. */
object LayoutTests extends TestSuite {

  private def laid(text: String, variant: Variant = Variant.Plain): Layout =
    Case
      .parse("near", text)
      .flatMap(Layout.of(_, variant))
      .fold(e => throw new java.lang.AssertionError(e), identity)

  private val crossPlace =
    """query: invoice
      |turn
      |you: which fix? [must]
      |grit: that one
      |place fs:/home/api
      |turn
      |you: invoice fix is TZ [must]
      |grit: and UTC [must]
      |place task:nightly closed
      |turn
      |you: nothing here
      |carried: kept
      |reopen
      |turn
      |grit: reopened [must]
      |ask
      |you: which invoice fix?
      |""".stripMargin

  val tests = Tests {
    test("a layout's [must] entries by their conversation's origin, its own first, in seq order") {
      val layout = laid(crossPlace)
      layout.must.map((o, seqs) => (o.place.written, seqs.map(EntrySeq.value))) ==> Vector(
        "task:eval/near/plain" -> List(0L),
        "fs:/eval/near/plain/home/api" -> List(0L, 1L),
        // The reopened period's seqs follow the first's and its closing.
        "task:eval/near/plain/nightly" -> List(2L)
      )
    }

    test("a layout's [never] entries by their conversation's origin, as its [must] are") {
      val layout = laid(
        crossPlace
          .replace("and UTC [must]", "and UTC [never]")
          .replace("reopened [must]", "reopened [never]")
      )
      layout.never.map((o, seqs) => (o.place.written, seqs.map(EntrySeq.value))) ==> Vector(
        "fs:/eval/near/plain/home/api" -> List(1L),
        "task:eval/near/plain/nightly" -> List(2L)
      )
    }

    test("a case that labels no [must] entry is laid out expecting none") {
      laid(crossPlace.replace(" [must]", "")).must ==> Vector.empty
    }

    test("a place closed before any turn there is refused, naming it") {
      Case
        .parse("bare", "query: q\nplace fs:/a closed\nask\nyou: now?\n")
        .flatMap(Layout.of(_, Variant.Plain)) ==>
        Left("bare: fs:/a is closed before any turn there")
    }

    test("the ask is the turn after the case's own, filler and all when buried") {
      laid(crossPlace).ask.toString ==> "1"
      laid(crossPlace, Variant.Buried).ask.toString ==> (1 + Variant.FillerPerGap).toString
      laid(crossPlace).own ==> Origin.Task("eval", "near/plain")
    }
  }
}
