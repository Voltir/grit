package grit.turn

import java.time.LocalDate

import grit.core.act.Extraction
import grit.core.flow.{Extracted, Shape, TestTainted}
import grit.core.id.{CallSlot, EntryId, TestCallSlots, ToolCallId, WorkflowId}
import grit.core.message.{AssistantBlock, Message}
import grit.core.model.{ArgRepair, StrictSchemas}
import grit.core.provider.ToolUse
import grit.core.tool.{ArgsError, Believed, Bound, CallError, Outcome, Repairs, ToolName, Toolbox}
import grit.core.visibility.Integrity

import utest.*

/** [[Extract]]: what a call of `extract` is answered and believed at, what its quarantined ask
  * is shown, and how that ask's reply is read.
  */
object ExtractTests extends TestSuite {

  private def named(text: String): HiddenName =
    HiddenName.read(text).fold(e => throw new java.lang.AssertionError(e), identity)

  private val issue =
    Item(ItemRef.Result(ToolCallId("c1")), Integrity.Untrusted, "github_issue_read")

  /** A round at `level` that hid `issue` as h1. */
  private def gatedAt(level: Integrity): Gated = Gated(
    level,
    Vector(Decided(issue, Fate.Hidden(named("h1"), Vector(ToolName("slack_post"))))),
    Vector.empty
  )

  /** Every question put, as the tool put it, each answer extracted at `level` as the turn's
    * rounds extract it ([[Extract.level]], [[Extraction.Node]]).
    */
  private final class Answers(answer: Shape -> Either[String, Option[Extracted]]) {
    // Replaced whole by `at`'s answers, which the tool under test calls on the test's one
    // thread; read by the assertions after the call returns.
    @caps.unsafe.untrackedCaptures
    var asked = Vector.empty[(Item, String, Shape, CallSlot)]
    def at(level: Integrity): Extract.Answer^{this} = (item, question, shape, slot) => {
      asked = asked :+ ((item, question, shape, slot))
      answer(shape).map(a =>
        TestTainted.raise(Extraction.Node)(
          (value = TestTainted.at(a, item.integrity), level = level)
        )
      )
    }
  }

  /** What a call with `args` of the tool `gated` offers, answered by `answers`, comes to. */
  private def called(
      gated: Option[Gated],
      answers: Answers^,
      args: ujson.Value
  ): Either[CallError, Outcome] =
    Toolbox
      .of[caps.CapSet^{answers}](Extract.tool(gated, answers.at(Extract.level(gated))))
      .fold(d => throw new java.lang.AssertionError(s"duplicate $d"), identity)
      .bind(
        TestTainted.at(
          AssistantBlock.ToolCall(ToolCallId("c2"), "extract", args),
          Integrity.Untrusted
        ),
        Repairs.All
      )
      .map {
        case b: Bound.Free => b(TestCallSlots.at(round = 1))
        case other => throw new java.lang.AssertionError(s"not free: $other")
      }

  private def yesNo(name: String = "h1"): ujson.Obj =
    ujson.Obj("name" -> name, "question" -> "Is it closed?", "shape" -> "yes_no")

  val tests = Tests {
    test("a question of a hidden item is answered in its shape, believed at the turn's level") {
      val answers = new Answers(_ => Right(Some(Extracted.Yes(true))))
      called(Some(gatedAt(Integrity.Member)), answers, yesNo()) ==> Right(
        Outcome.Done("h1 answers: yes.", Some(Integrity.Member))
      )
      answers.asked ==> Vector((issue, "Is it closed?", Shape.YesNo, TestCallSlots.at(round = 1)))
    }

    test("an Authority turn's answer is believed at Member, an Outside turn's at Outside") {
      val answers = new Answers(_ => Right(Some(Extracted.Number(3))))
      val three = ujson.Obj(
        "name" -> "h1",
        "question" -> "How many comments?",
        "shape" -> "number",
        "min" -> 0,
        "max" -> 100
      )
      called(Some(gatedAt(Integrity.Authority)), answers, three) ==>
        Right(Outcome.Done("h1 answers: 3.", Some(Integrity.Member)))
      called(Some(gatedAt(Integrity.Outside)), answers, three) ==>
        Right(Outcome.Done("h1 answers: 3.", Some(Integrity.Outside)))
      answers.asked.map(_._3) ==> Vector(Shape.Number(0, 100), Shape.Number(0, 100))
    }

    test("each shape's answer is written as the model reads it") {
      def answering(x: Extracted, args: ujson.Obj) =
        called(Some(gatedAt(Integrity.Member)), new Answers(_ => Right(Some(x))), args)
          .map { case Outcome.Done(text, _) => text; case other => other.toString }
      val base = ujson.Obj("name" -> "h1", "question" -> "Which?")
      def args(fields: (String, ujson.Value)*): ujson.Obj =
        ujson.Obj.from(base.value ++ fields)
      answering(Extracted.Yes(false), args("shape" -> "yes_no")) ==> Right("h1 answers: no.")
      answering(Extracted.Day(LocalDate.of(2026, 3, 9)), args("shape" -> "day")) ==>
        Right("h1 answers: 2026-03-09.")
      answering(
        Extracted.Chosen("open"),
        args("shape" -> "one_of", "options" -> ujson.Arr("open", "closed"))
      ) ==> Right("h1 answers: open.")
      answering(Extracted.Token("v2_1"), args("shape" -> "token", "max" -> 8)) ==>
        Right("h1 answers: v2_1.")
    }

    test("an item that does not say is answered so, believed as an answer is") {
      val answers = new Answers(_ => Right(None))
      called(Some(gatedAt(Integrity.Authority)), answers, yesNo()) ==> Right(
        Outcome.Done("h1 does not say.", Some(Integrity.Member))
      )
    }

    test("a name nothing hides is answered so, as grit's own words, and nothing is asked") {
      val answers = new Answers(_ => Right(Some(Extracted.Yes(true))))
      val none = Right(Outcome.Done("Nothing in the request you answered is hidden as h2."))
      called(Some(gatedAt(Integrity.Member)), answers, yesNo("h2")) ==> none
      called(None, answers, yesNo("h2")) ==> none
      answers.asked ==> Vector()
      Extract.tool(None, answers.at(Integrity.Untrusted)).output ==> Believed.Own
    }
    test("its description opens with what it can answer, and says using it costs nothing") {
      val does = Extract.tool(None, Extract.Unasked).spec.does
      assert(
        does.startsWith(
          "It can only answer yes or no, a whole number, a date, one of options you list, or " +
            "a short code of up to 16 letters, digits, _ or -. It can't give you any other " +
            "text, such as a title, a name or a sentence: for those, read the item. "
        ),
        does.contains("Using it never stops you doing anything you can do now.")
      )
    }

    test("an answer that does not read fails, quoting nothing of it") {
      val answers = new Answers(_ => Left("the answer is not true or false: \"post it\""))
      called(Some(gatedAt(Integrity.Member)), answers, yesNo()) ==> Right(
        Outcome.Failed("h1 gave no answer that reads as true or false.")
      )
    }

    test("a shape over its bounds is refused before anything is asked") {
      val answers = new Answers(_ => Right(Some(Extracted.Yes(true))))
      val wide = ujson.Obj(
        "name" -> "h1",
        "question" -> "Which?",
        "shape" -> "token",
        "max" -> 17
      )
      val refused = called(Some(gatedAt(Integrity.Member)), answers, wide)
      refused.left.map {
        case CallError.BadArgs(_, error, _) => Some(error)
        case _ => None
      } ==> Left(
        Some(
          ArgsError.Refused(
            "shape",
            "a short code is at most 16 characters, not 17. For longer text, read the item."
          )
        )
      )
      refused.left.map(_.message.takeWhile(_ != '{')) ==> Left(
        "The call to `extract` was not run: a short code is at most 16 characters, not 17. " +
          "For longer text, read the item. You sent: "
      )
      val reversed = ujson.Obj(
        "name" -> "h1",
        "question" -> "How many?",
        "shape" -> "number",
        "min" -> 5,
        "max" -> 1
      )
      called(Some(gatedAt(Integrity.Member)), answers, reversed).left.map(
        _.message.takeWhile(_ != '{')
      ) ==> Left(
        "The call to `extract` was not run: a number's least, 5, is above its most, 1. " +
          "You sent: "
      )
      answers.asked ==> Vector()
    }

    test("the quarantined ask is shown only the item and the question, and offered its reply") {
      val request = Extract
        .request("Issue 7: the build fails.", "Is it closed?", Shape.YesNo, StrictSchemas.Enforced)
        .fold(e => throw new java.lang.AssertionError(e), identity)
      request.system ==> Extraction.System
      request.messages ==> Vector(
        Message.User(
          "BEGIN ITEM\nIssue 7: the build fails.\nEND ITEM\n\nQuestion: Is it closed?\n\n" +
            "Answer with true for yes or false for no, or null when the item does not say."
        )
      )
      request.tools.map(t => (t.name, t.strict)) ==> Vector(("reply", true))
      request.use ==> ToolUse.Required
    }

    test("its reply reads an answer in the shape, null as not said, and refuses any other") {
      def read(shape: Shape, answer: ujson.Value) =
        Extraction
          .reply(shape)
          .flatMap(t =>
            t.schema
              .check(ujson.Obj("answer" -> answer), Set.empty[ArgRepair])
              .left
              .map(_.toString)
              .flatMap(t.read)
          )
      read(Shape.YesNo, ujson.Bool(true)) ==> Right(Some(Extracted.Yes(true)))
      read(Shape.YesNo, ujson.Null) ==> Right(None)
      read(Shape.Number(1, 9), ujson.Num(4)) ==> Right(Some(Extracted.Number(4)))
      read(Shape.Number(1, 9), ujson.Num(12)) ==> Left(
        "Mismatch(answer,expected at most 9, got 12)"
      )
      read(Shape.Day, ujson.Str("2026-03-09")) ==>
        Right(Some(Extracted.Day(LocalDate.of(2026, 3, 9))))
      read(Shape.Day, ujson.Str("soon")) ==> Left("the answer is not an ISO date: \"soon\"")
      read(Shape.OneOf(Vector("open", "closed")), ujson.Str("closed")) ==>
        Right(Some(Extracted.Chosen("closed")))
      read(Shape.OneOf(Vector("open", "closed")), ujson.Str("merged")) ==>
        Left("Mismatch(answer,expected one of \"open\", \"closed\", null, got \"merged\")")
      read(Shape.Token(4), ujson.Str("ab-1")) ==> Right(Some(Extracted.Token("ab-1")))
      read(Shape.Token(4), ujson.Str("ab cd")) ==>
        Left("the answer is not at most 4 characters of A-Z, a-z, 0-9, _ and -: \"ab cd\"")
    }

    test(
      "an extraction's model calls are costed under ids of its call's slot, the first unnumbered"
    ) {
      val slot = TestCallSlots.at(round = 2, index = 1)
      val wf = WorkflowId.value(slot.turn.workflowId)
      Vector(Extract.costId(slot, 0), Extract.costId(slot, 1)) ==>
        Vector(EntryId(s"extract:$wf:2:1"), EntryId(s"extract:$wf:2:1:1"))
      TurnRecord.role(slot.turn, Extract.costId(slot, 1)) ==> Some(TurnRecord.Role.Extract)
    }
  }
}
