package grit.core.classify

import grit.core.message.{Tokens, Usage}
import grit.core.place.Service
import grit.core.route.Routed
import grit.core.visibility.*
import grit.dbos.sql.TestTx

import utest.*

/** The deployment's classifier asked only about what its service is cleared for (ADR 0037). */
object ClearedClassifierTests extends TestSuite {

  private final case class Note(text: String)
  private given StateJson[Note] = StateJson.instance(n => ujson.Obj("note" -> n.text))

  private val urgent = Ask.yesNo[Note]("Is `note` urgent?", None, None)

  /** Answers yes to every call, counting them. */
  private final class Counting extends Classifier {
    @caps.unsafe.untrackedCaptures
    var calls = 0
    protected def answer(
        state: ujson.Value,
        questions: Vector[Question]
    ): Either[ClassifierError, Answers] = {
      calls = calls + 1
      Right(
        Answers(
          Vector(Answer.YesNo(1.0)),
          Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None),
          "jev"
        )
      )
    }
  }

  private val jev = Service.of("jev").fold(e => throw new java.lang.AssertionError(e), identity)
  private val internal = Label.at(Level.Internal)

  /** The jev service cleared for internal. */
  private val visibility: Visibility = {
    val rooms: Labeller[Room] = new Labeller[Room] {
      def label(item: Room): Labelled = Labelled.Unmapped(Label.Public)
      def requires: Vector[Compartment] = Vector.empty
    }
    Visibility
      .of(
        Compartments
          .of(Vector.empty)
          .fold(c => throw new java.lang.AssertionError(c.toString), identity),
        rooms,
        Vector.empty,
        Vector.empty,
        Vector(Cleared(Party.Service(jev), internal))
      )
      .fold(e => throw new java.lang.AssertionError(e.toString), identity)
  }

  private def routedAt(floor: Label): Routed =
    Routed.of(using TestTx.fake(Clearance.of(floor), visibility))

  val tests = Tests {
    test("a classifier whose service is cleared for what was read is asked") {
      val inner = new Counting
      val cleared = ClearedClassifier.of(jev, inner)
      val asked = Vector(Label.Public, internal).map(l =>
        cleared.at(routedAt(l)).ask(Note("x"), urgent).map(_.model)
      )
      (asked, inner.calls) ==> (Vector(Right("jev"), Right("jev")), 2)
    }

    test(
      "a classifier whose service is not cleared for what was read is Unavailable, naming the classification, and is not called"
    ) {
      val inner = new Counting
      val restricted = Label.at(Level.Restricted)
      ClearedClassifier.of(jev, inner).at(routedAt(restricted)).ask(Note("x"), urgent) ==>
        Left(
          ClassifierError.Unavailable(
            "its service, service:jev, is not cleared for what was read, " +
              Label.shown(restricted)
          )
        )
      inner.calls ==> 0
    }

    test("no classifier answers Unavailable with its reason, and records no upstream") {
      val none = ClearedClassifier.none("topics are off")
      (none.at(routedAt(Label.Public)).ask(Note("x"), urgent), none.upstream) ==>
        (Left(ClassifierError.Unavailable("topics are off")), None)
    }

    test("a cleared classifier's ledger rows record its service as their upstream") {
      ClearedClassifier.of(jev, new Counting).upstream ==> Some("service:jev")
    }
  }
}
