package grit.lifecycle.triage

import scala.collection.immutable.VectorMap

import grit.core.classify.{Answer, Question, Request}
import grit.core.id.{CorpusName, QuestionName}
import grit.core.period.Probability
import grit.core.persona.Persona
import grit.core.place.Place
import grit.core.triage.{Bound, Gate, Reading, Tags}
import grit.core.triage.{Corpora, Corpus}
import grit.lifecycle.triage.TriageQuestions.{Item, Refusal}

import utest.*

object TriageQuestionsTests extends TestSuite {

  private def fail(what: String) = throw new java.lang.AssertionError(what)

  private def n(text: String): QuestionName = QuestionName.of(text).getOrElse(fail(text))

  private def source(name: String, line: String): Corpus =
    (for {
      s <- CorpusName.of(name)
      p <- Place.read("slack:")
    } yield Corpus(s, line, p)).getOrElse(fail(name))

  /** Declared out of alphabetical order, so an order the catalog did not declare shows. */
  private val catalog = Corpora
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
          Gate.Open
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
      set.questions(Corpora.Empty).toVector ==>
        Vector(n("a") -> yesNo("A?"), n("b") -> yesNo("B?"))
    }

    test("of refuses two items of one name, a One's name and a PerSource's prefix among them") {
      val none = Gate.Open
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
            Gate.bounds(Bound.AtLeast(reading, half))
          )
          .map(_ => ())
      val refused = Vector(
        Reading.Key(n("gap"), "maybe"),
        Reading.Yes(n("gap")),
        Reading.Key(n("open"), "asks"),
        Reading.Yes(n("source")),
        Reading.Yes(n("missing")),
        Reading.Chosen(n("gap"), "maybe"),
        Reading.Chosen(n("open"), "asks")
      )
      refused.map(gated) ==> refused.map(r => Left(Refusal.Unbounded(r)))
      Vector(
        Reading.Key(n("gap"), "asks"),
        Reading.Yes(n("open")),
        Reading.Chosen(n("gap"), "asks")
      )
        .map(gated) ==> Vector(Right(()), Right(()), Right(()))
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

    test("v3 asks V2's words with to-grit after to, naming the persona, and drafts by v3's gate") {
      val pip = Persona.of("Pip").getOrElse(fail("a persona"))
      val set = TriageQuestions.v3(pip)
      set.questions(catalog).keys.map(QuestionName.value).toVector ==> Vector(
        "gap",
        "open",
        "to",
        "to-grit",
        "durable",
        "anchor",
        "source:github",
        "source:conversations"
      )
      val v2 = TriageQuestions.V2.questions(catalog)
      // Every question but to-grit in V2's words: a v3 answer under a v2 name means what it did.
      set.questions(catalog).removed(n("to-grit")) ==> v2
      set.questions(catalog).get(n("to-grit")) ==> Some(
        Question.YesNo(
          "Read new_message and thread. Is new_message directed at the assistant, whom people " +
            "here call Pip, rather than at someone else or at the room? Saying its name to " +
            "it, or replying to what Assistant said in thread, directs it at the assistant; " +
            "talking about it does not.",
          None,
          None
        )
      )
      (set.speak, TriageQuestions.v3(Persona.Grit).speak) ==> (Tags.V3.drafts, Tags.V3.drafts)
      set.unread(set.speak, Corpora.Empty) ==> None
    }

    test(
      "v4 asks v3's words with anchor-record after anchor, drafts by v4's gate, and is shipped"
    ) {
      val pip = Persona.of("Pip").getOrElse(fail("a persona"))
      val set = TriageQuestions.v4(pip)
      set.questions(catalog).keys.map(QuestionName.value).toVector ==> Vector(
        "gap",
        "open",
        "to",
        "to-grit",
        "durable",
        "anchor",
        "anchor-record",
        "source:github",
        "source:conversations"
      )
      // Every question but anchor-record in v3's words: a v4 answer under a v3 name means what
      // it did, so v3's gate reads v4's answers.
      set.questions(catalog).removed(n("anchor-record")) ==>
        TriageQuestions.v3(pip).questions(catalog)
      set.questions(catalog).get(n("anchor-record")) ==> Some(
        Question.YesNo(
          "Read new_message and thread. Is what new_message asks for something no stored " +
            "record could supply, such as an opinion, someone's current availability, their " +
            "own plans or work, or a choice nobody has made yet? A fact asked of a particular " +
            "person may still be in a record.",
          None,
          None
        )
      )
      (set.speak, TriageQuestions.v4(Persona.Grit).speak, TriageQuestions.ShippedSpeak) ==>
        (Tags.V4.drafts, Tags.V4.drafts, Tags.V4.drafts)
      (set.unread(set.speak, Corpora.Empty), set.unread(Tags.V3.drafts, catalog)) ==>
        (None, None)
      TriageQuestions.shipped(pip) ==> set
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

    test("V1 asks kind, waiting, durable and helps, in the request triage's question sends") {
      // The digest TriageTests pins for this state: what triage sent before its words were a
      // value.
      val fixed = TriageQuestion.State(
        "standup moves to 10:00 from Monday",
        "Ana",
        "Ben: when is standup?"
      )
      TriageQuestions.V1.request(fixed, catalog).digest ==>
        "8ec33547bce9c3e3a9c64edef8eecae7f216738a1407cc8088957d609cb78edd"
      TriageQuestions.V1.questions(catalog).keys.map(QuestionName.value).toVector ==>
        Vector("kind", "waiting", "durable", "helps")
      val reworded = TriageQuestion.Wording.Shipped.copy(
        kind = "What kind of message is new_message?",
        helps = "Would a reply help?"
      )
      // A rewording's request, as triage's question sent it in those words: pinned while the
      // two were compared, before the question was retired for the set.
      TriageQuestions.v1(reworded).request(fixed, catalog).digest ==>
        "7592ee1c208b16e6c863dbd94e158c8c026de7fa6b1bcf5cb6439cc1b90c2e12"
    }

    test(
      "V1 holds a message whose most weighted kind is chatter, and drafts at helps 0.5 otherwise"
    ) {
      def kind(chatter: Double, question: Double) = Answer.Choice(
        "chatter",
        Vector(w("question", question), w("chatter", chatter)),
        0.0
      )
      def answers(kindAnswer: Answer, helps: Double) = VectorMap[QuestionName, Answer](
        n("kind") -> kindAnswer,
        n("waiting") -> Answer.YesNo(0.5),
        n("durable") -> Answer.YesNo(0.5),
        n("helps") -> Answer.YesNo(helps)
      )
      Vector(
        answers(kind(0.4, 0.3), 0.9),
        answers(kind(0.3, 0.4), 0.5),
        answers(kind(0.3, 0.4), 0.49),
        answers(kind(0.6, 0.4), 0.9)
      ).map(TriageQuestions.V1.speak.drafts) ==> Vector(
        Some(false),
        Some(true),
        Some(false),
        Some(false)
      )
    }

    test(
      "unread names the first reading a set does not ask in its kind, or a key its choice lacks"
    ) {
      val kind = n("kind")
      Vector(
        TriageQuestions.V1.unread(TriageQuestions.V1.speak, Corpora.Empty),
        TriageQuestions.V2.unread(TriageQuestions.V2.speak, Corpora.Empty),
        TriageQuestions.V1.unread(TriageQuestions.V2.speak, Corpora.Empty),
        TriageQuestions.V2.unread(TriageQuestions.V1.speak, Corpora.Empty),
        TriageQuestions.V1.unread(
          Gate.bounds(Bound.AtLeast(Reading.Chosen(kind, "maybe"), half)),
          Corpora.Empty
        ),
        TriageQuestions.V1
          .unread(Gate.bounds(Bound.AtLeast(Reading.Yes(kind), half)), Corpora.Empty)
      ) ==> Vector(
        None,
        None,
        Some(Reading.Key(n("gap"), "asks")),
        Some(Reading.Chosen(kind, "chatter")),
        Some(Reading.Chosen(kind, "maybe")),
        Some(Reading.Yes(kind))
      )
    }

    test(
      "unread reads a source's yes/no as asked when the set asks per source and the catalog declares it, and as unread otherwise"
    ) {
      def per(name: String) = CorpusName.of(name).getOrElse(fail(name))
      val github = Reading.Yes(QuestionName.per(n("source"), per("github")))
      val drive = Reading.Yes(QuestionName.per(n("source"), per("drive")))
      def gate(r: Reading) = Gate.bounds(Bound.AtLeast(r, half))
      Vector(
        TriageQuestions.V2.unread(gate(github), catalog),
        TriageQuestions.V2.unread(gate(drive), catalog),
        TriageQuestions.V2.unread(gate(github), Corpora.Empty),
        TriageQuestions.V1.unread(gate(github), catalog)
      ) ==> Vector(None, Some(drive), Some(github), Some(github))
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
