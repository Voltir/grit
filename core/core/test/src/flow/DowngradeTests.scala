package grit.core.flow

import grit.core.document.{Declassified, DocText, Pointer, PolicyApplication}
import grit.core.durable.{Flow, Flows, InMemoryDurable, Step, StepName}
import grit.core.id.{DocKey, DocumentVersionId, PluginName, WorkflowId}
import grit.core.period.Probability
import grit.core.place.{Namespace, Place}
import grit.core.tool.ToolName
import grit.core.visibility.Integrity.Level
import grit.core.visibility.Level as Classified
import grit.core.visibility.{Compartment, DeclassificationPolicy, Integrity, Label}

import utest.*

object DowngradeTests extends TestSuite {

  private val team: Compartment =
    Compartment.of("team").getOrElse(throw new java.lang.AssertionError("team is a compartment"))

  private val extraction = Downgrade.extraction[Boolean, 1]

  /** A sanitiser that raises when its text says `fine`, finding the text's length. */
  private val sanitiser =
    Downgrade.sanitiser[Unit, Int]("fine")((text, _, _) => (text.length, text == "fine"))

  private val a: Step[Any, String, String] = Step[Any, String, String](StepName("a"))(_ => in => in)

  /** What `node` makes of `in`. */
  private def raised[A, B](node: Downgrade[A, B], in: A): B = node.rule(in)

  val tests = Tests {

    test("downgrades lists each node made on a step's output or within a body, at its family") {
      val made = sanitiser.step[Any, String, String](StepName("hosting"))(_ => _ => in => in)
      val flow = extraction
        .on(a)(_ => None)
        .map(_._1)
        .andThen(Flow.step(made))
        .andThen(
          Flow.program[Any, String, String]("plan", Vector(extraction, sanitiser))(_ => s => s)
        )
      Flows.downgrades(flow).map(d => (d.at.name, d.sits, d.node)) ==> Vector(
        ("a", Flows.Sits.On, extraction),
        ("hosting", Flows.Sits.Within, sanitiser),
        ("plan", Flows.Sits.Within, extraction),
        ("plan", Flows.Sits.Within, sanitiser)
      )
    }

    test("a program's steps raise only the nodes its within declares") {
      val flow = Flow.program[Any, Unit, String]("plan", Vector(extraction)) { steps => _ =>
        given steps.type = steps
        s"${extraction.raise.isDefined} ${sanitiser.raise.isDefined}"
      }
      new InMemoryDurable().run(WorkflowId("plan:1"))(_ => Flows.run(flow, (), ()).merge) ==>
        "true false"
    }

    test("on records only its step, and passes its output beside what the node made of it") {
      val flow = extraction.on(a)(o => Some((answer = Some(o == "x"), level = Integrity.Member)))
      val durable = new InMemoryDurable()
      val out = durable.run(WorkflowId("on:1"))(_ =>
        Flows.run(flow, (), "x").map((o, t) => s"$o ${t.map(t => (t.extract, t.integrity))}").merge
      )
      (out, durable.history(WorkflowId("on:1")).map(_.name)) ==>
        (s"x Some((Some(true),${Integrity.Member}))", Vector("a"))
    }

    test("an extraction trusts its answer at the run's level met with Member") {
      def at(level: Integrity): Integrity =
        raised(extraction, (answer = Some(true), level = level)).integrity
      (at(Integrity.at(Level.Authority, team)), at(Integrity.Outside)) ==>
        (Integrity.Member, Integrity.Outside)
    }

    test("an extraction's capacity is its answer's bits and one for whether there is one") {
      Downgrade.extraction[Extracted, Bounded.K].described.capacity ==>
        Downgrade.Capacity.Bits(257)
    }

    test("the sanitiser raises below Outside to it, keeping compartments, only when judged so") {
      def sanitised(text: String, i: Integrity): (Integrity, Int) = {
        val (t, found) = raised(sanitiser, Mint.tainted((text, ()), i))
        (t.integrity, found)
      }
      (
        sanitised("fine", Integrity.Untrusted),
        sanitised("fine", Integrity.at(Level.Untrusted, team)),
        sanitised("nope", Integrity.Untrusted),
        sanitised("fine", Integrity.Member)
      ) ==> (
        (Integrity.Outside, 4),
        (Integrity.at(Level.Outside, team), 4),
        (Integrity.Untrusted, 4),
        (Integrity.Member, 4)
      )
    }

    test("a description says who decides and how much passes, a person at their own integrity") {
      (
        extraction.shown,
        sanitiser.shown,
        Downgrade
          .Described(
            "approval",
            Downgrade.Axis.Integrity,
            Downgrade.Authority.Person,
            Downgrade.Capacity.Text
          )
          .shown
      ) ==> (
        s"extraction: raises integrity, by code, to at most ${Integrity.shown(Integrity.Member)}, at most 2 bits",
        s"fine: raises integrity, by code, to at most ${Integrity.shown(Integrity.Outside)}, free text",
        "approval: raises integrity, by the answering person, to their own integrity, free text"
      )
    }

    test(
      "the declassification removes its compartment when its leak is below Declassified.Below and the policy lowers the label; nothing otherwise"
    ) {
      def made(label: Label, leak: Double): Option[Label] =
        raised(
          Declassified.declassification,
          (
            rewrite = rewrite,
            source = source(label, card),
            applied = applied(card),
            leak = Probability.clamped(leak)
          )
        ).map(_.label)
      (
        made(Label.at(Classified.Confidential, team, other), 0.09),
        made(Label.at(Classified.Confidential, team, other), 0.1),
        made(Label.at(Classified.Confidential, other), 0.0),
        made(Label.Unmapped.join(Label.at(Classified.Internal, team)), 0.0)
      ) ==> (Some(Label.at(Classified.Confidential, other)), None, None, None)
    }

    test(
      "the declassification refuses an application that names another document than its source"
    ) {
      def made(applying: Pointer): Boolean =
        raised(
          Declassified.declassification,
          (
            rewrite = rewrite,
            source = source(Label.at(Classified.Internal, team), card),
            applied = applied(applying),
            leak = Probability.Zero
          )
        ).isDefined
      (made(card), made(otherCard)) ==> (true, false)
    }
  }

  private val other: Compartment =
    Compartment.of("other").getOrElse(throw new java.lang.AssertionError("other is a compartment"))

  private def pointer(path: String): Pointer =
    Pointer
      .of(ToolName("drive_read_file"), "file" -> ujson.Str(path))
      .getOrElse(throw new java.lang.AssertionError("a pointer"))

  private val card = pointer("card")
  private val otherCard = pointer("other-card")

  private val rewrite: DocText =
    DocText.of("a rewrite").getOrElse(throw new java.lang.AssertionError("a text"))

  private def source(label: Label, read: Pointer): Declassified.Source =
    Declassified.Source(
      DocumentVersionId.of(7).getOrElse(throw new java.lang.AssertionError("a version")),
      PluginName.of("cards").getOrElse(throw new java.lang.AssertionError("a plugin")),
      DocKey.of("card").getOrElse(throw new java.lang.AssertionError("a key")),
      Place.under(Namespace.Task, Vector("cards")),
      label,
      read,
      "r1"
    )

  private def applied(document: Pointer): PolicyApplication =
    PolicyApplication(
      DeclassificationPolicy(DeclassificationPolicy.Over.Compartment(team), 1),
      document,
      1
    )
}
