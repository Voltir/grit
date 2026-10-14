package grit.core.route

import grit.core.model.*
import grit.core.visibility.*

import utest.*

object RoutingTests extends TestSuite {

  private def model(id: String): ModelId =
    ModelId.of(id).getOrElse(throw new java.lang.AssertionError(id))
  private def upstream(slug: String): Upstream =
    Upstream.of(slug).getOrElse(throw new java.lang.AssertionError(slug))

  private val trial = Compartment.of("trial").getOrElse(throw new java.lang.AssertionError())
  private val internal = Label.at(Level.Internal)
  private val confidential = Label.at(Level.Restricted, trial)

  private val strong = ModelRef(model("anthropic/claude-haiku"), Some(upstream("anthropic")))
  private val open = ModelRef(model("openai/gpt-oss-120b"), None)
  private val flash = ModelRef(model("deepseek/deepseek-v4.1-flash"), Some(upstream("fireworks")))

  private def party(ref: ModelRef): Party =
    Party.Model(ref.model, ref.upstream.getOrElse(throw new java.lang.AssertionError(ref.toString)))

  // A ladder of three rungs: the open pair first, then flash, then the strong pair.
  private val pins: Pins = {
    val ladder = Ladder
      .of(
        Vector(
          Rung(Grade.Standard, Assignment(open, 100, None)),
          Rung(Grade.Light, Assignment(flash, 100, None)),
          Rung(Grade.Strong, Assignment(strong, 100, None))
        )
      )
      .getOrElse(throw new java.lang.AssertionError())
    Catalog.of(Policy(ladder, ladder, ladder, ladder), Vector.empty).pin.turn
  }

  private def pinsOf(rungs: (Grade, ModelRef)*): Pins = {
    val ladder = Ladder
      .of(rungs.toVector.map((g, r) => Rung(g, Assignment(r, 100, None))))
      .getOrElse(throw new java.lang.AssertionError())
    Catalog.of(Policy(ladder, ladder, ladder, ladder), Vector.empty).pin.turn
  }

  private val clearances = Clearances
    .of(Vector(Cleared(party(flash), internal), Cleared(party(strong), confidential)))
    .fold(p => throw new java.lang.AssertionError(p.toString), identity)

  private def atMostRefs(p: Pins, ceiling: Grade): Vector[ModelRef] =
    Routing.atMost(p, ceiling).all.map(_.assignment.ref)

  private def refs(p: Option[Pins]): Option[Vector[ModelRef]] = p.map(_.all.map(_.assignment.ref))

  val tests = Tests {
    test("a pair's party is its model at its upstream; one OpenRouter routes has none") {
      Party.of(flash) ==> Some(Party.Model(flash.model, upstream("fireworks")))
      Party.of(open) ==> None
    }

    test("cleared keeps, in the ladder's order, each pin cleared for the classification") {
      refs(Routing.cleared(pins, clearances, Classification(internal))) ==>
        Some(Vector(flash, strong))
      refs(Routing.cleared(pins, clearances, Classification(confidential))) ==> Some(Vector(strong))
    }

    test("a pair with no party is cleared for public alone, as is one no clearance names") {
      refs(Routing.cleared(pins, clearances, Classification(Label.Public))) ==>
        Some(Vector(open, flash, strong))
      refs(Routing.cleared(pins, Clearances.Empty, Classification(internal))) ==> None
    }

    test(
      "pick takes the first of the least grade at or above the one wanted, else the first of the greatest below"
    ) {
      Routing.pick(pins, Grade.Strong).assignment.ref ==> strong
      Routing.pick(pins, Grade.Standard).assignment.ref ==> open
      Routing.pick(pins, Grade.Light).assignment.ref ==> flash
      val twoStandard =
        pinsOf(Grade.Light -> flash, Grade.Standard -> open, Grade.Standard -> strong)
      Routing.pick(twoStandard, Grade.Strong).assignment.ref ==> open
      Routing
        .pick(pinsOf(Grade.Strong -> strong, Grade.Light -> flash), Grade.Standard)
        .assignment
        .ref ==> strong
    }

    test("atMost keeps the pins at or below the ceiling in order, and all of them when none is") {
      atMostRefs(pins, Grade.Standard) ==> Vector(open, flash)
      atMostRefs(pins, Grade.Strong) ==> Vector(open, flash, strong)
      atMostRefs(pinsOf(Grade.Strong -> strong, Grade.Strong -> open), Grade.Standard) ==>
        Vector(strong, open)
    }

    test("atMost at standard narrows a light and strong ladder to its light pin alone") {
      atMostRefs(pinsOf(Grade.Light -> flash, Grade.Strong -> strong), Grade.Standard) ==>
        Vector(flash)
    }

    test("the fixed reply names the classification it could not be routed for") {
      Uncleared.reply(Uncleared(confidential)) ==>
        "No model this deployment uses is cleared for this conversation's classification, " +
        "[level: restricted, in: {trial}]. An administrator can clear one; until then I can't " +
        "answer here."
    }
  }
}
