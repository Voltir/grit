package grit.lifecycle.triage

import scala.collection.immutable.VectorMap

import grit.core.classify.{Answer, Question, Request}
import grit.core.id.{KnowledgeSourceName, QuestionName}
import grit.core.period.Probability
import grit.core.place.Place
import grit.core.triage.{KnowledgeSource, KnowledgeSources}
import grit.lifecycle.triage.TriageQuestions.{Bound, Gate, Item, Reading, Refusal}

import utest.*

object TriageQuestionsTests extends TestSuite {

  private def fail(what: String) = throw new java.lang.AssertionError(what)

  private def n(text: String): QuestionName = QuestionName.of(text).getOrElse(fail(text))

  private def source(name: String, line: String): KnowledgeSource =
    (for {
      s <- KnowledgeSourceName.of(name)
      p <- Place.read("slack:")
    } yield KnowledgeSource(s, line, p)).getOrElse(fail(name))

  /** Declared out of alphabetical order, so an order the catalog did not declare shows. */
  private val catalog = KnowledgeSources
    .of(
      Vector(
        source("github", "the team's GitHub repository: code, issues and pull requests"),
        source(
          "conversations",
          "the team's past Slack conversations: what was discussed and decided"
        )
      )
    )
    .getOrElse(fail("catalog"))

  private def yesNo(words: String) = Question.YesNo(words, None, None)

  private val half = Probability.clamped(0.5)

  private val gap = Question
    .choice("?", Question.Key("asks", None), Question.Key("nothing", None))
    .getOrElse(fail("gap"))

  private val state =
    TriageQuestion.State("can someone send me the Q3 deck?", "ana", "bo: planning starts monday")

  private def w(key: String, p: Double) = Answer.Weight(key, p)

  val tests = Tests {
    test("questions: each One in order, a PerSource's yes/no per source in the catalog's order") {
      val set = TriageQuestions
        .of(
          Item.One(n("a"), yesNo("A?")),
          Vector(Item.PerSource(n("src"), "Could ", " help?"), Item.One(n("b"), yesNo("B?"))),
          Gate(Vector.empty)
        )
        .getOrElse(fail("set"))
      set.questions(catalog).toVector ==> Vector(
        n("a") -> yesNo("A?"),
        QuestionName.read("src:github").getOrElse(fail("github")) ->
          yesNo("Could the team's GitHub repository: code, issues and pull requests help?"),
        QuestionName.read("src:conversations").getOrElse(fail("conversations")) ->
          yesNo("Could the team's past Slack conversations: what was discussed and decided help?"),
        n("b") -> yesNo("B?")
      )
      set.questions(KnowledgeSources.Empty).toVector ==>
        Vector(n("a") -> yesNo("A?"), n("b") -> yesNo("B?"))
    }

    test("of refuses two items of one name, a One's name and a PerSource's prefix among them") {
      val none = Gate(Vector.empty)
      TriageQuestions.of(
        Item.One(n("a"), yesNo("A?")),
        Vector(Item.One(n("a"), yesNo("B?"))),
        none
      ) ==>
        Left(Refusal.NameRepeated(n("a")))
      TriageQuestions.of(
        Item.One(n("b"), yesNo("B?")),
        Vector(Item.PerSource(n("a"), "", ""), Item.One(n("a"), yesNo("A?"))),
        none
      ) ==> Left(Refusal.NameRepeated(n("a")))
    }

    test("of refuses a bound reading no One of its kind, or a key its choice lacks") {
      def gated(reading: Reading) =
        TriageQuestions
          .of(
            Item.One(n("gap"), gap),
            Vector(Item.One(n("open"), yesNo("Open?")), Item.PerSource(n("source"), "", "")),
            Gate(Vector(Bound.AtLeast(reading, half)))
          )
          .map(_ => ())
      val refused = Vector(
        Reading.Key(n("gap"), "maybe"),
        Reading.Yes(n("gap")),
        Reading.Key(n("open"), "asks"),
        Reading.Yes(n("source")),
        Reading.Yes(n("missing"))
      )
      refused.map(gated) ==> refused.map(r => Left(Refusal.Unbounded(r)))
      Vector(Reading.Key(n("gap"), "asks"), Reading.Yes(n("open"))).map(gated) ==>
        Vector(Right(()), Right(()))
    }

    test("V2 asks the synthetic run's words in order, one source question per source covering it") {
      // The words as the synthetic runs asked them; a pin, since a shadow name is bound to
      // one set, so a change of words is a new set under a new name.
      def yes(words: String) =
        ujson.Obj(
          "kind" -> "yes_no",
          "instructions" -> words,
          "yes" -> ujson.Null,
          "no" -> ujson.Null
        )
      def key(name: String, means: String) = ujson.Obj("name" -> name, "description" -> means)
      val request = TriageQuestions.V2.request(state, catalog)
      Request.json(request)("questions") ==> ujson.Arr(
        ujson.Obj(
          "kind" -> "choice",
          "instructions" ->
            "Read new_message, said by author, and its thread. What does new_message leave open?",
          "keys" -> ujson.Arr(
            key(
              "asks",
              "it asks for information, a document, or for something to be done, of anyone"
            ),
            key("owes", "its author commits to doing something"),
            key("closes", "it answers or settles something asked earlier"),
            key("nothing", "greetings, thanks, reactions, or news with nothing asked")
          )
        ),
        yes(
          "Read new_message and thread. Is what new_message asks for still unanswered in the thread?"
        ),
        yes(
          "Read new_message and thread. Is new_message directed at a particular person, named or the one it replies to, rather than at the room? Mentioning someone, or replying in a thread they wrote in, does not by itself direct it at them."
        ),
        yes(
          "Read new_message and thread. Does new_message state something worth keeping for later: a decision, a date, a name, a number, how something works, or an answer to a question? Greetings, thanks and small talk do not count. Count only what new_message itself states; the thread is only for reading it."
        ),
        yes(
          "Read new_message and thread. Is what new_message asks for something no stored record could supply, such as an opinion, someone's current availability, or a choice nobody has made yet?"
        ),
        yes(
          "Read new_message and thread. Could the team's GitHub repository: code, issues and pull requests supply what new_message asks for?"
        ),
        yes(
          "Read new_message and thread. Could the team's past Slack conversations: what was discussed and decided supply what new_message asks for?"
        )
      )
      TriageQuestions.V2.questions(catalog).keys.map(QuestionName.value).toVector ==>
        Vector("gap", "open", "to", "durable", "anchor", "source:github", "source:conversations")
      // A pin of the digest a shadow row records for this state and catalog.
      request.digest ==> "d04b85b3eadd8236c04209fe11c521cd34662f7023f5ffaa848809bf8efed88f"
    }

    test("V2 drafts at its bounds: 0.5 passes at least and fails below; a missing answer is None") {
      val passing = VectorMap[QuestionName, Answer](
        n("gap") -> Answer.Choice("asks", Vector(w("asks", 0.5), w("nothing", 0.5)), 0.0),
        n("open") -> Answer.YesNo(0.5),
        n("to") -> Answer.YesNo(0.49),
        n("durable") -> Answer.YesNo(0.0),
        n("anchor") -> Answer.YesNo(0.49)
      )
      val drafts = TriageQuestions.V2.speak.drafts
      Vector(
        passing,
        passing.updated(n("to"), Answer.YesNo(0.5)),
        passing.updated(n("anchor"), Answer.YesNo(0.5)),
        passing.updated(n("open"), Answer.YesNo(0.49)),
        passing.updated(n("gap"), Answer.Choice("asks", Vector(w("asks", 0.49)), 0.0)),
        // A key the answer does not weigh reads 0.
        passing.updated(n("gap"), Answer.Choice("nothing", Vector(w("nothing", 1.0)), 1.0)),
        passing.removed(n("anchor")),
        passing.updated(n("open"), Answer.Choice("asks", Vector(w("asks", 1.0)), 1.0))
      ).map(drafts) ==>
        Vector(
          Some(true),
          Some(false),
          Some(false),
          Some(false),
          Some(false),
          Some(false),
          None,
          None
        )
    }

    test("ask keeps each answer under its question's name, in the order asked") {
      val classifier = new TriageFixtures.Scripted(
        Vector(0.7, 0.1, 0.1, 0.1),
        Vector(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
      )
      val gapAnswer = Answer
        .choice(Vector(w("asks", 0.7), w("owes", 0.1), w("closes", 0.1), w("nothing", 0.1)))
        .getOrElse(fail("gap"))
      TriageQuestions.V2.ask(classifier, state, catalog).map(_.value.toVector) ==> Right(
        Vector(
          n("gap") -> gapAnswer,
          n("open") -> Answer.YesNo(0.1),
          n("to") -> Answer.YesNo(0.2),
          n("durable") -> Answer.YesNo(0.3),
          n("anchor") -> Answer.YesNo(0.4),
          QuestionName.read("source:github").getOrElse(fail("github")) -> Answer.YesNo(0.5),
          QuestionName.read("source:conversations").getOrElse(fail("conv")) -> Answer.YesNo(0.6)
        )
      )
    }
  }
}
