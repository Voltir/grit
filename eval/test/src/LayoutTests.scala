package grit.eval

import grit.core.document.DocText
import grit.core.id.{DocKey, EntrySeq}
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

    test("a layout's documents rooted under the case, keyed by case, variant and order") {
      val layout = laid(
        """query: billing
          |turn
          |you: hi
          |document fs:/home/billing [must]
          |doc: Closed here:
          |doc: rounding is banker's
          |document task:nightly
          |doc: nothing
          |ask
          |you: which rounding?
          |""".stripMargin,
        Variant.Buried
      )
      layout.documents.map(d =>
        (DocKey.value(d.key), d.place.written, DocText.value(d.text), d.must)
      ) ==> Vector(
        (
          "near/buried/d0",
          "fs:/eval/near/buried/home/billing",
          "Closed here:\nrounding is banker's",
          true
        ),
        ("near/buried/d1", "task:eval/near/buried/nightly", "nothing", false)
      )
    }

    test("a document holding no text is refused, naming its place") {
      Case
        .parse("bare", "query: q\ndocument fs:/a [must]\nask\nyou: now?\n")
        .flatMap(Layout.of(_, Variant.Plain)) ==>
        Left("bare: the document at fs:/a: a document's text is not blank")
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
