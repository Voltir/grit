package grit.lifecycle.shadow

import scala.concurrent.duration.*

import grit.core.classify.{Answer, ClassifierError}
import grit.core.durable.InMemoryDurable
import grit.core.id.{QuestionName, ShadowRef, TurnRef, WorkflowId}
import grit.core.stitch.Tuning
import grit.core.triage.{KnowledgeSource, KnowledgeSources, ShadowAnswers, Shadowed}
import grit.lifecycle.triage.{
  TriageFixtures,
  TriageInput,
  TriageQuestion,
  TriageQuestions,
  TriageRecipe
}

import utest.*

object ShadowTests extends TestSuite {
  import ShadowFixtures.*
  import TriageFixtures.{Scripted, Spent}

  private def decided =
    new Scripted(Vector(0.125, 0.0, 0.75, 0.125, 0.0), Vector(0.125, 0.875, 0.25))

  /** The digest of the request triage's question makes of `t` in `wording`, as `w` stands. */
  private def digest(w: World, t: grit.core.id.TriageRef, wording: TriageQuestion.Wording) =
    TriageInput
      .build(
        w.triaged.reads,
        w.triaged.rooms,
        TriageFixtures.FakeDb,
        t,
        Tuning.Default,
        TriageRecipe.Shipped
      )
      .toOption
      .flatMap((_, state) => TriageQuestion.request(wording, state))
      .map(_.digest)

  /** The digest of the request V2 makes of `t` with `sources`, as `w` stands. */
  private def setDigest(w: World, t: grit.core.id.TriageRef, sources: Vector[KnowledgeSource]) =
    TriageInput
      .build(
        w.triaged.reads,
        w.triaged.rooms,
        TriageFixtures.FakeDb,
        t,
        Tuning.Default,
        TriageRecipe.Shipped
      )
      .toOption
      .zip(KnowledgeSources.of(sources).toOption)
      .map { case ((_, state), catalog) => TriageQuestions.V2.request(state, catalog).digest }

  /** What `name` kept of `t`, its answers in order: a `VectorMap`'s equality ignores it. */
  private def keptInOrder(w: World, t: grit.core.id.TriageRef, name: grit.core.id.ShadowName) =
    w.kept(t, name).map {
      case Shadowed.Answered(request, ShadowAnswers.Named(as), usage, requested, model, ms) =>
        Right(
          (
            request,
            as.toVector.map((n, a) => (QuestionName.value(n), a)),
            usage,
            requested,
            model,
            ms
          )
        )
      case other => Left(other)
    }

  val tests = Tests {
    test(
      "a question set's variant keeps each answer under its name, asking a source's question only where the source covers the conversation"
    ) {
      val w = new World
      val t = w.hear("who owns the deploy?", "Ana", 0)
      val shadow = ShadowRef(t, Asks)
      // gap weighs asks, owes, closes, nothing; then open, to, durable, anchor, source:github.
      val classifier =
        new Scripted(Vector(0.75, 0.125, 0.0, 0.125), Vector(0.875, 0.25, 0.125, 0.0, 0.5))
      val durable = new InMemoryDurable
      durable.run(shadow.workflowId)(w.body(classifier, 5)) ==>
        "kept: answered by jev-1.13.0 in 250 ms"
      (classifier.calls, durable.recordedSteps(shadow.workflowId)) ==> (1, Vector("ask", "record"))
      val gap = Answer.choice(
        Vector("asks" -> 0.75, "owes" -> 0.125, "closes" -> 0.0, "nothing" -> 0.125)
          .map(Answer.Weight(_, _))
      )
      keptInOrder(w, t, Asks) ==> Some(
        Right(
          (
            setDigest(w, t, Vector(Github)).getOrElse("no request"),
            gap.toVector.map("gap" -> _) ++ Vector(
              "open" -> Answer.YesNo(0.875),
              "to" -> Answer.YesNo(0.25),
              "durable" -> Answer.YesNo(0.125),
              "anchor" -> Answer.YesNo(0.0),
              "source:github" -> Answer.YesNo(0.5)
            ),
            Spent,
            "jev-variant",
            "jev-1.13.0",
            250.millis
          )
        )
      )
      w.triaged.tags(t) ==> None
    }

    test(
      "a question set's variant whose classifier is unavailable is kept as failed, with its request's digest"
    ) {
      val w = new World
      val t = w.hear("who owns the deploy?", "Ana", 0)
      new InMemoryDurable().run(ShadowRef(t, Asks).workflowId)(
        w.body(new Scripted(Vector.empty, Vector.empty), 5)
      ) ==> "kept: failed: unavailable"
      w.kept(t, Asks) ==> Some(
        Shadowed.Failed(
          setDigest(w, t, Vector(Github)).getOrElse("no request"),
          ClassifierError.Kind.Unavailable,
          250.millis
        )
      )
    }

    test(
      "a shadow asks its variant once, in its wording, and keeps every answer, the digest, both models and the latency"
    ) {
      val w = new World
      val t = w.hear("standup moves to 10:00 from Monday", "Ana", 0)
      val shadow = ShadowRef(t, Words)
      val classifier = decided
      val durable = new InMemoryDurable
      durable.run(shadow.workflowId)(w.body(classifier, 5)) ==>
        "kept: answered by jev-1.13.0 in 250 ms"
      (classifier.calls, durable.recordedSteps(shadow.workflowId)) ==> (1, Vector("ask", "record"))
      classifier.asked.collectFirst { case q: grit.core.classify.Question.Choice =>
        q.instructions
      } ==>
        Some(Reworded.kind)
      val weights = Vector(
        "question" -> 0.125,
        "answer" -> 0.0,
        "decision" -> 0.75,
        "announcement" -> 0.125,
        "chatter" -> 0.0
      )
      w.kept(shadow.triage, Words) ==> Some(
        Shadowed.Answered(
          digest(w, t, Reworded).getOrElse("no request"),
          ShadowAnswers.Worded(
            Answer.choice(weights.map(Answer.Weight(_, _))).toVector ++
              Vector(Answer.YesNo(0.125), Answer.YesNo(0.875), Answer.YesNo(0.25))
          ),
          Spent,
          "jev-variant",
          "jev-1.13.0",
          250.millis
        )
      )
      // Triage's own tags are not the shadow's to write.
      w.triaged.tags(t) ==> None
    }

    test("a resumed shadow asks nothing again: the recorded answer is kept") {
      val w = new World
      val t = w.hear("standup moves to 10:00 from Monday", "Ana", 0)
      val shadow = ShadowRef(t, Words)
      val first = new InMemoryDurable
      first.run(shadow.workflowId)(w.body(decided, 5))
      val history = first.history(shadow.workflowId).take(1)
      val again = new World
      val t2 = again.hear("standup moves to 10:00 from Monday", "Ana", 0)
      val other = new Scripted(Vector(0, 0, 0, 0, 1), Vector(0, 0, 0))
      new InMemoryDurable().replay(ShadowRef(t2, Words).workflowId, history)(
        again.body(other, 5)
      ) ==>
        Right("kept: answered by jev-1.13.0 in 250 ms")
      (other.calls, again.kept(t2, Words)) ==> (0, w.kept(t, Words))
    }

    test("an unavailable classifier is kept as failed, unavailable, with the request's digest") {
      val w = new World
      val t = w.hear("lunch?", "Ana", 0)
      val shadow = ShadowRef(t, Words)
      new InMemoryDurable().run(shadow.workflowId)(
        w.body(new Scripted(Vector.empty, Vector.empty), 5)
      ) ==> "kept: failed: unavailable"
      w.kept(t, Words) ==> Some(
        Shadowed.Failed(
          digest(w, t, Reworded).getOrElse("no request"),
          ClassifierError.Kind.Unavailable,
          250.millis
        )
      )
    }

    test("answers that do not read as triage's are kept as failed, unreadable") {
      val w = new World
      val t = w.hear("lunch?", "Ana", 0)
      val shadow = ShadowRef(t, Words)
      // Two yes/no answers to three questions.
      new InMemoryDurable().run(shadow.workflowId)(
        w.body(new Scripted(Vector(0, 0, 0, 0, 1), Vector(0.1, 0.1)), 5)
      ) ==> "kept: failed: unreadable"
      w.kept(t, Words).map {
        case Shadowed.Failed(_, kind, _) => Some(kind)
        case Shadowed.Answered(_, _, _, _, _, _) => None
      } ==> Some(Some(ClassifierError.Kind.Unreadable))
    }

    test("a message gone while it was asked keeps nothing") {
      val w = new World
      val t = w.hear("lunch?", "Ana", 0)
      val purging = new Scripted(
        Vector(0, 0, 0, 0, 1),
        Vector(0.1, 0.1, 0.1),
        () => w.triaged.purge(TurnRef(TriageFixtures.c, t.turn))
      )
      new InMemoryDurable().run(ShadowRef(t, Words).workflowId)(w.body(purging, 5)) ==>
        "ignored: the message is gone or shadowed already"
      w.kept(t, Words) ==> None
    }

    test(
      "an id that is not a shadow's, names no heard message, or no declared variant, asks nothing"
    ) {
      val w = new World
      val said = w.triaged.say("hello", 0)
      val heard = w.hear("lunch?", "Ana", 1)
      val classifier = decided
      new InMemoryDurable().run(WorkflowId("triage:c1:1:0"))(w.body(classifier, 0)) ==>
        "not a shadow: triage:c1:1:0"
      new InMemoryDurable().run(
        ShadowRef(grit.core.id.TriageRef(TriageFixtures.p1, said.turnSeq), Words).workflowId
      )(w.body(classifier, 0)) ==> "not asked: no heard message at turn 0"
      new InMemoryDurable().run(ShadowRef(heard, named("other")).workflowId)(
        w.body(classifier, 0)
      ) ==>
        "not asked: no variant other is declared"
      (classifier.calls, w.kept(heard, named("other"))) ==> (0, None)
    }
  }
}
