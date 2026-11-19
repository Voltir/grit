package grit.core.act

import grit.core.classify.{Criterion, Decision, Question, Request}

import utest.*

/** [[Sorting]]'s requests, in the shape the placement spike measured (one choice over every
  * candidate plus `new` and `aside` for a discussion; a yes/no per nearest candidate for a
  * document), their texts neutral; their size held within the classifier's limit; and the
  * sortings its constructors refuse.
  */
object SortingTests extends TestSuite {

  /** A candidate or a fixed criterion, by name. */
  final case class To(name: String) extends caps.Pure

  private val choosing: Sorting[To, Decision[To]] = Sorting
    .choice(
      "Which topic is this discussion part of?",
      1500,
      700,
      Sorting.Chosen,
      Criterion(To("new"), "new", Some("A matter none of these covers.")),
      Criterion(To("aside"), "aside", Some("Too slight to keep."))
    )
    .fold(e => throw new java.lang.AssertionError(e), identity)

  private val each: Sorting[To, Vector[Decision.Weight[To]]] = Sorting
    .each[To]("Is this file about this topic's matter?", "It is.", "It is not.", 1500, 700, 8)
    .fold(e => throw new java.lang.AssertionError(e), identity)

  /** `sorting`'s request about `item`, of `kind`, among `candidates`, as it is sent. */
  private def request[R](
      sorting: Sorting[To, R],
      kind: String,
      item: String,
      candidates: Vector[(To, String)]
  ): Either[Sorting.Unasked, Request] = {
    val shown = Sorting.Shown(kind, item)
    Sorting.fitted(sorting, shown, candidates).map(Request.of(shown, _))
  }

  private def json[R](
      sorting: Sorting[To, R],
      kind: String,
      item: String,
      candidates: Vector[(To, String)]
  ): String =
    request(sorting, kind, item, candidates).fold(
      _ => "nothing asked",
      r => ujson.write(Request.json(r))
    )

  /** What each candidate is shown as, in the order asked. */
  private def described(r: Request): Vector[String] = r.questions.flatMap {
    case c: Question.Choice => c.keys.filter(_.name.forall(_.isDigit)).flatMap(_.description)
    case y: Question.YesNo => Vector(y.instructions.split("\n\n", 2).last)
    case _ => Vector()
  }

  val tests = Tests {
    test(
      "a choice's request is its item as the state, then one choice keyed by each candidate's number from 1, then its fixed keys"
    ) {
      json(
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
      "a yes/no sort's request asks one question per candidate in the order asked, each its instructions then the candidate's text"
    ) {
      json(
        each,
        "document",
        "Release notes\nThe release slipped.",
        Vector(To("b") -> "The release", To("a") -> "Lunch")
      ) ==>
        """{"state":{"item":{"kind":"document","text":"Release notes\nThe release slipped."}},"questions":[""" +
        """{"kind":"yes_no","instructions":"Is this file about this topic's matter?\n\nThe release","yes":"It is.","no":"It is not."},""" +
        """{"kind":"yes_no","instructions":"Is this file about this topic's matter?\n\nLunch","yes":"It is.","no":"It is not."}]}"""
    }

    test(
      "a yes/no sort asks nothing of no candidate, and a choice still asks among its fixed keys"
    ) {
      (
        json(each, "document", "x", Vector()),
        json(choosing, "discussion", "x", Vector())
      ) ==> (
        "nothing asked",
        """{"state":{"item":{"kind":"discussion","text":"x"}},"questions":[""" +
          """{"kind":"choice","instructions":"Which topic is this discussion part of?","keys":[""" +
          """{"name":"new","description":"A matter none of these covers."},""" +
          """{"name":"aside","description":"Too slight to keep."}]}]}"""
      )
    }

    test(
      "a choice over its most candidates, each of long text, stays within the classifier's limit, each cut to an equal share"
    ) {
      val long = Vector.tabulate(Sorting.Chosen)(i => To(s"t$i") -> ("word \"quoted\" " * 600))
      val r = request(choosing, "discussion", "A: " + "said " * 300, long)
      val shares = r.map(described(_).map(ujson.Str(_).render().length - 2))
      (
        r.map(Request.size(_) <= Request.MostChars),
        shares.map(_.distinct.size),
        shares.map(_.forall(s => s > 1300 && s < 1500))
      ) ==> (Right(true), Right(1), Right(true))
    }

    test("a choice over its most candidates within the classifier's limit shows each text whole") {
      val view = "word " * 139 + "last"
      val r = request(
        choosing,
        "discussion",
        "A: hello",
        Vector.tabulate(Sorting.Chosen)(i => To(s"t$i") -> view)
      )
      r.map(described(_).distinct) ==> Right(Vector(view))
    }

    test("a yes/no sort of long candidates stays within the classifier's limit") {
      val long = Vector.tabulate(8)(i => To(s"t$i") -> ("word " * 20000))
      request(each, "document", "x", long).map(Request.size(_) <= Request.MostChars) ==>
        Right(true)
    }

    test(
      "a choice whose fixed key is a number or repeats another is refused, as is one of under 1 or over Chosen candidates, or a yes/no of under 1 or over Most"
    ) {
      def choice(a: String, b: String, most: Int = 1) =
        Sorting.choice("q", 1, 1, most, Criterion(To(a), a, None), Criterion(To(b), b, None))
      def nearest(n: Int) = Sorting.each[To]("q", "y", "n", 1, 1, n).isLeft
      (
        choice("new", "2").left.toOption,
        choice("new", "new").left.toOption,
        choice("new", "aside", 0).isLeft,
        choice("new", "aside", Sorting.Chosen + 1).isLeft,
        choice("new", "aside", Sorting.Chosen).isLeft,
        nearest(0),
        nearest(Sorting.Most + 1),
        nearest(Sorting.Most)
      ) ==> (
        Some("a choice's fixed key 2 is a number or repeats another"),
        Some("a choice's fixed key new is a number or repeats another"),
        true,
        true,
        false,
        true,
        true,
        false
      )
    }
  }
}
