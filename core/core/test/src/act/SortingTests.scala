package grit.core.act

import grit.core.classify.{Ask, Criterion, Decision, Request}

import utest.*

/** [[Sorting]]'s requests, in the shape the placement spike measured (one choice over every
  * candidate plus `new` and `aside` for a discussion; a yes/no per nearest candidate for a
  * document), their texts neutral; and the sortings its constructors refuse.
  */
object SortingTests extends TestSuite {

  /** A candidate or a fixed criterion, by name. */
  final case class To(name: String) extends caps.Pure

  private val choosing: Sorting[To, Decision[To]] = Sorting
    .choice(
      "Which topic is this discussion part of?",
      1500,
      700,
      Criterion(To("new"), "new", Some("A matter none of these covers.")),
      Criterion(To("aside"), "aside", Some("Too slight to keep."))
    )
    .fold(e => throw new java.lang.AssertionError(e.toString), identity)

  private val each: Sorting[To, Vector[Decision.Weight[To]]] = Sorting
    .each[To]("Is this file about this topic's matter?", "It is.", "It is not.", 1500, 700, 8)
    .fold(e => throw new java.lang.AssertionError(e), identity)

  /** `sorting`'s request about `item`, a discussion, among `candidates`, as JSON. */
  private def request[R](
      sorting: Sorting[To, R],
      kind: String,
      item: String,
      candidates: Vector[(To, String)]
  ): String = {
    val asked = sorting.nearest(item, candidates.map(_._2)).flatMap(candidates.lift)
    sorting
      .questions(asked)
      .fold("nothing asked")(qs =>
        ujson.write(Request.json(Request.of(Sorting.Shown(kind, item), qs)))
      )
  }

  val tests = Tests {
    test(
      "a choice's request is its item as the state, then one choice keyed by each candidate's number from 1, then its fixed keys"
    ) {
      request(
        choosing,
        "discussion",
        "A: one\nB: two",
        Vector(To("a") -> "Topic A", To("b") -> "Topic B")
      ) ==>
        """{"state":{"item":{"kind":"discussion","text":"A: one\nB: two"}},"questions":[""" +
        """{"kind":"choice","instructions":"Which topic is this discussion part of?","keys":[""" +
        """{"name":"1","description":"Topic A"},{"name":"2","description":"Topic B"},""" +
        """{"name":"new","description":"A matter none of these covers."},""" +
        """{"name":"aside","description":"Too slight to keep."}]}]}"""
    }

    test(
      "a yes/no sort's request asks one question per nearest candidate, best match first, each its instructions then the candidate's text"
    ) {
      request(
        each,
        "document",
        "Release notes\nThe release slipped.",
        Vector(To("a") -> "Lunch", To("b") -> "The release")
      ) ==>
        """{"state":{"item":{"kind":"document","text":"Release notes\nThe release slipped."}},"questions":[""" +
        """{"kind":"yes_no","instructions":"Is this file about this topic's matter?\n\nThe release","yes":"It is.","no":"It is not."},""" +
        """{"kind":"yes_no","instructions":"Is this file about this topic's matter?\n\nLunch","yes":"It is.","no":"It is not."}]}"""
    }

    test(
      "a yes/no sort asks nothing of no candidate, and a choice still asks among its fixed keys"
    ) {
      (
        request(each, "document", "x", Vector()),
        request(choosing, "discussion", "x", Vector())
      ) ==> (
        "nothing asked",
        """{"state":{"item":{"kind":"discussion","text":"x"}},"questions":[""" +
          """{"kind":"choice","instructions":"Which topic is this discussion part of?","keys":[""" +
          """{"name":"new","description":"A matter none of these covers."},""" +
          """{"name":"aside","description":"Too slight to keep."}]}]}"""
      )
    }

    test(
      "a choice whose fixed key is a number or repeats another is refused, as is a yes/no of under 1 or over Most candidates"
    ) {
      def choice(a: String, b: String) =
        Sorting
          .choice("q", 1, 1, Criterion(To(a), a, None), Criterion(To(b), b, None))
          .left
          .toOption
      def nearest(n: Int) = Sorting.each[To]("q", "y", "n", 1, 1, n).isLeft
      (
        choice("new", "2"),
        choice("new", "new"),
        nearest(0),
        nearest(Sorting.Most + 1),
        nearest(Sorting.Most)
      ) ==>
        (Some(Ask.DuplicateKey("2")), Some(Ask.DuplicateKey("new")), true, true, false)
    }
  }
}
