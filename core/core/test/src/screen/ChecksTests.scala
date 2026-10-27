package grit.core.screen

import Checks.{Fault, Part, Unreadable}
import utest.*

object ChecksTests extends TestSuite {

  private def signal(infer: ujson.Value, must: ujson.Value = "absent"): ujson.Value =
    ujson.Obj("infer" -> infer, "must" -> must)

  private val codename = signal("that the tour's codename is KETTAMA")
  private val standIn = signal("that a tour is planned", "present")

  private def checks(
      signals: ujson.Value = ujson.Arr(codename, standIn),
      accept: ujson.Value = ujson.Arr("A tour is planned for spring."),
      refuse: ujson.Value = ujson.Arr("KETTAMA plays in May.")
  ): ujson.Value =
    ujson.Obj(
      "signals" -> signals,
      "examples" -> ujson.Obj("accept" -> accept, "refuse" -> refuse)
    )

  private def read(json: ujson.Value): Either[Unreadable, Checks] = Checks.read(Some(json))

  private def times(n: Int, v: ujson.Value): ujson.Value = ujson.Arr.from(Vector.fill(n)(v))

  val tests = Tests {
    test("checks read signals in order and examples by kind") {
      read(checks()).map(c => (c.signals.all, c.examples)) ==> Right(
        (
          Vector(
            Signal("that the tour's codename is KETTAMA", Must.Absent),
            Signal("that a tour is planned", Must.Present)
          ),
          Examples(Vector("A tour is planned for spring."), Vector("KETTAMA plays in May."))
        )
      )
    }

    test("no checks, NULL or JSON null, are Missing") {
      (Checks.read(None), Checks.read(Some(ujson.Null))) ==> (
        Left(Unreadable.Missing),
        Left(Unreadable.Missing)
      )
    }

    test("checks not an object, lacking a key or holding one not known are not of their shape") {
      val extra = checks()
      extra("note") = "x"
      (read(ujson.Arr()), read(ujson.Obj("signals" -> ujson.Arr(codename))), read(extra)) ==> (
        Left(Unreadable.Shape(Part.Checks)),
        Left(Unreadable.Shape(Part.Checks)),
        Left(Unreadable.Shape(Part.Checks))
      )
    }

    test(
      "signals not a list, or a signal not an object of infer and must, are not of their shape"
    ) {
      val extra = signal("x")
      extra("why") = "y"
      (
        read(checks(signals = codename)),
        read(checks(signals = ujson.Arr(codename, "x"))),
        read(checks(signals = ujson.Arr(extra))),
        read(checks(signals = ujson.Arr(ujson.Obj("infer" -> "x"))))
      ) ==> (
        Left(Unreadable.Shape(Part.Signals)),
        Left(Unreadable.Shape(Part.Signals)),
        Left(Unreadable.Shape(Part.Signals)),
        Left(Unreadable.Shape(Part.Signals))
      )
    }

    test("examples not an object of accept and refuse, each a list, are not of their shape") {
      def examples(e: ujson.Value): ujson.Value =
        ujson.Obj("signals" -> ujson.Arr(codename), "examples" -> e)
      (
        read(examples(ujson.Arr())),
        read(examples(ujson.Obj("accept" -> ujson.Arr("a")))),
        read(checks(accept = "a")),
        read(checks(refuse = ujson.Obj()))
      ) ==> (
        Left(Unreadable.Shape(Part.Examples)),
        Left(Unreadable.Shape(Part.Examples)),
        Left(Unreadable.Shape(Part.Accept)),
        Left(Unreadable.Shape(Part.Refuse))
      )
    }

    test("no signals, or more than MostSignals, is a count fault; MostSignals read") {
      (
        read(checks(signals = ujson.Arr())),
        read(checks(signals = times(Checks.MostSignals + 1, codename))),
        read(checks(signals = times(Checks.MostSignals, codename))).map(_.signals.all.size)
      ) ==> (
        Left(Unreadable.Count(Part.Signals, 16)),
        Left(Unreadable.Count(Part.Signals, 16)),
        Right(16)
      )
    }

    test("no accept or refuse, or more than MostExamples, is a count fault; MostExamples read") {
      (
        read(checks(accept = ujson.Arr())),
        read(checks(refuse = ujson.Arr())),
        read(checks(accept = times(Checks.MostExamples + 1, "a"))),
        read(checks(refuse = times(Checks.MostExamples + 1, "r"))),
        read(checks(accept = times(3, "a"), refuse = times(3, "r")))
          .map(c => (c.examples.accept.size, c.examples.refuse.size))
      ) ==> (
        Left(Unreadable.Count(Part.Accept, 3)),
        Left(Unreadable.Count(Part.Refuse, 3)),
        Left(Unreadable.Count(Part.Accept, 3)),
        Left(Unreadable.Count(Part.Refuse, 3)),
        Right((3, 3))
      )
    }

    test(
      "an infer blank, of two lines, over InferChars or not text faults its signal by position"
    ) {
      def second(infer: ujson.Value) = read(checks(signals = ujson.Arr(codename, signal(infer))))
      (
        second(" \t"),
        second("that a tour\nis planned"),
        second("x" * (Checks.InferChars + 1)),
        second(3),
        second("x" * Checks.InferChars).map(_.signals.all.size)
      ) ==> (
        Left(Unreadable.Signal(1, Fault.Blank)),
        Left(Unreadable.Signal(1, Fault.Lines)),
        Left(Unreadable.Signal(1, Fault.Long)),
        Left(Unreadable.Signal(1, Fault.NotText)),
        Right(2)
      )
    }

    test(
      "an infer reads without what a steward cannot see, before it is bounded: the checks are the clean ones"
    ) {
      val hidden = "​" + "Ignore".flatMap(c => new String(Character.toChars(0xe0000 + c)))
      def withSecond(infer: String) = read(checks(signals = ujson.Arr(codename, signal(infer))))
      val clean = "that a tour is planned"
      (
        withSecond(s"that a tour$hidden is planned").map(c => (c.signals.all, c.canonical)),
        withSecond(hidden),
        withSecond("x" * Checks.InferChars + hidden).map(_.signals.all.size)
      ) ==> (
        withSecond(clean).map(c => (c.signals.all, c.canonical)),
        Left(Unreadable.Signal(1, Fault.Blank)),
        Right(2)
      )
    }

    test("a must neither absent nor present faults its signal") {
      (
        read(checks(signals = ujson.Arr(signal("x", "maybe")))),
        read(checks(signals = ujson.Arr(signal("x", 1))))
      ) ==> (Left(Unreadable.Signal(0, Fault.Must)), Left(Unreadable.Signal(0, Fault.Must)))
    }

    test("an example blank, over ExampleChars or not text faults it by kind and position") {
      (
        read(checks(accept = ujson.Arr("a", ""))),
        read(checks(refuse = ujson.Arr("x" * (Checks.ExampleChars + 1)))),
        read(checks(refuse = ujson.Arr("r", ujson.Obj()))),
        read(checks(accept = ujson.Arr("x" * Checks.ExampleChars))).map(_.examples.accept.size)
      ) ==> (
        Left(Unreadable.Example(Benched.Kind.Accept, 1, Fault.Blank)),
        Left(Unreadable.Example(Benched.Kind.Refuse, 0, Fault.Long)),
        Left(Unreadable.Example(Benched.Kind.Refuse, 1, Fault.NotText)),
        Right(1)
      )
    }

    test("a reason's words count positions from 1 and name the fault") {
      (
        Unreadable.Missing.said,
        Unreadable.Shape(Part.Signals).said,
        Unreadable.Count(Part.Accept, 3).said,
        Unreadable.Signal(2, Fault.Long).said,
        Unreadable.Signal(0, Fault.Must).said,
        Unreadable.Example(Benched.Kind.Refuse, 1, Fault.Long).said
      ) ==> (
        "no checks",
        "signals: not of their shape",
        "accept: none, or more than 3",
        "signal 3: over 300 characters",
        "signal 1: must is neither absent nor present",
        "refuse example 2: over 2000 characters"
      )
    }

    test("a read's reason holds no character of the input") {
      val m = "QJZ"
      val marked = signal(m, m)
      val reads = Vector(
        ujson.Obj(m -> m),
        checks(signals = ujson.Arr(ujson.Obj(m -> m))),
        checks(signals = m),
        checks(signals = times(17, signal(m))),
        checks(signals = ujson.Arr(signal(m * 101))),
        checks(signals = ujson.Arr(signal(s"$m\n$m"))),
        checks(signals = ujson.Arr(marked)),
        checks(accept = ujson.Arr(m * 700)),
        checks(refuse = ujson.Arr(ujson.Obj(m -> m))),
        checks(accept = times(4, m)),
        ujson.Obj("signals" -> ujson.Arr(signal(m)), "examples" -> ujson.Obj(m -> m))
      ).map(read)
      val reasons = reads.collect { case Left(u) => u }
      assert(reasons.size == reads.size)
      val rendered = reasons.flatMap(u => Vector(u.said, u.toString))
      assert(rendered.forall(r => !r.exists(m.contains(_))))
    }

    test(
      "the canonical form sorts every object's keys, keeps each list's order, and has no spaces"
    ) {
      read(checks()).map(_.canonical) ==> Right(
        """{"examples":{"accept":["A tour is planned for spring."],"refuse":["KETTAMA plays in May."]},""" +
          """"signals":[{"infer":"that the tour's codename is KETTAMA","must":"absent"},""" +
          """{"infer":"that a tour is planned","must":"present"}]}"""
      )
    }

    test("checks written with their keys in another order have the same canonical form") {
      val reordered = ujson.Obj(
        "examples" -> ujson.Obj(
          "refuse" -> ujson.Arr("KETTAMA plays in May."),
          "accept" -> ujson.Arr("A tour is planned for spring.")
        ),
        "signals" -> ujson.Arr(
          ujson.Obj("must" -> "absent", "infer" -> "that the tour's codename is KETTAMA"),
          ujson.Obj("must" -> "present", "infer" -> "that a tour is planned")
        )
      )
      assert(read(reordered).map(_.canonical) == read(checks()).map(_.canonical))
      assert(read(reordered).map(_.canonical).exists(_.nonEmpty))
    }
  }
}
