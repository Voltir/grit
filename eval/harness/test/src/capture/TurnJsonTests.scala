package grit.eval.harness.capture

import java.time.Instant

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.context.Width
import grit.core.id.{
  ConversationId,
  CorpusName,
  DocumentVersionId,
  EntryId,
  EntrySeq,
  PluginName,
  QuestionName,
  TurnSeq,
  WorkflowId
}
import grit.core.message.{Tokens, Usage}
import grit.core.period.Probability
import grit.core.place.{Place, Service}
import grit.core.prompt.Layer
import grit.core.recipe.ServiceOffer
import grit.core.store.Focus
import grit.core.tool.{ToolName, ToolSetId}
import grit.core.triage.{Tags, Weighing}
import grit.dbos.engine.Build
import grit.turn.{TurnOffer, TurnRecord, TurnShape, TurnWeighing}

import utest.*

/** A turn of a capture as `turns.jsonl` keeps it. */
object TurnJsonTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val c = ConversationId("c1")
  private val at = Instant.parse("2026-10-02T17:05:00Z")
  private def p(x: Double) = Probability.clamped(x)
  private def version(n: Long) = DocumentVersionId.of(n).getOrElse(sys.error(s"version $n"))

  private val repo = right(CorpusName.of("repo"))

  /** `repo`'s answer at `x`, as live triage's set names it. */
  private def repoAt(x: Double): Tags.Weighed = Tags.Weighed(
    VectorMap(QuestionName.per(Tags.V2.sourcePrefix, repo) -> Answer.YesNo(x)),
    "jev",
    Usage(Tokens(700), Tokens(1), Tokens.Zero, Some(BigDecimal("0.00003")))
  )

  /** A shape with both ways a service's tools come, every verdict, and a width within. */
  private val shape = TurnShape(
    Width.Within(Tokens(16000), 12),
    right(ToolSetId.of("fedcba9876543210")),
    Vector(
      TurnShape.Took(
        ServiceOffer(
          right(Service.of("github")),
          Vector(repo),
          ServiceOffer.Verdict.Checked(
            Tags.V2.source(repo, p(0.2)).check(repoAt(0.1).answers, grit.core.triage.Cues.Empty)
          )
        ),
        TurnShape.Via.Workspace,
        Vector(ToolName("search"))
      ),
      TurnShape.Took(
        ServiceOffer(right(Service.of("slack")), Vector.empty, ServiceOffer.Verdict.Ungated),
        TurnShape.Via.Reached,
        Vector(ToolName("post"))
      ),
      TurnShape.Took(
        ServiceOffer(right(Service.of("docs")), Vector(repo), ServiceOffer.Verdict.Unweighed),
        TurnShape.Via.Reached,
        Vector.empty
      )
    )
  )

  /** A heard turn with a tool loop, every kind of part, settling and role. */
  private val heard = TurnCase(
    WorkflowId("c1:3"),
    c,
    TurnSeq(3),
    Said.Slack(right(CaseId.read("C1/1000.1"))),
    TurnOffer.Root.Heard,
    Focus.Open,
    at,
    Build.Known("a" * 40, dirty = true),
    Some(Live.Unanswered(Failure.Unreadable)),
    Some(
      Offered(
        Vector(ToolName("read"), ToolName("post")),
        right(ToolSetId.of("0123456789abcdef")),
        Tokens(120),
        VectorMap(Layer.Base -> Tokens(900), Layer.Edge -> Tokens(80)),
        Some(right(Place.read("slack:T1/C1"))),
        Vector(right(Place.read("slack:T1/C2"))),
        Some(shape)
      )
    ),
    TurnRecord.Weigh.Recorded(Some(TurnWeighing.Weighed.Kept(repoAt(0.1)))),
    Some(
      Parts(
        Part.Kind.values.toVector.zipWithIndex.map((k, i) =>
          Part(
            k,
            ConversationId(s"c$i"),
            Vector(EntrySeq(i.toLong)),
            Tokens(10L * i),
            Option.when(i % 2 == 0)(right(Support.read(i / 10.0).toRight("support")))
          )
        ),
        // A card, a document with no pointer that no reply measured, and one forgotten since.
        Vector(
          Held.Kept(
            version(7),
            right(PluginName.of("knowledge")),
            Some(ToolName("github__get_file_contents")),
            Tokens(55),
            Some(right(Support.read(0.4).toRight("support")))
          ),
          Held.Kept(version(8), right(PluginName.of("digest")), None, Tokens.Zero, None),
          Held.Gone(version(9))
        ),
        Tokens(7),
        Tokens(40)
      )
    ),
    Vector(
      Round(
        Vector(
          Call(Called.Topic, Settled.Ok(5), Reading.Other),
          Call(
            Called.Tool(ToolName("read")),
            Settled.Ok(12),
            Reading.Indexed(followed = true, Support.read(0.5))
          ),
          Call(Called.Unnamed, Settled.Failed(3), Reading.Other)
        )
      ),
      Round(
        Vector(
          Call(Called.Tool(ToolName("search")), Settled.Expired, Reading.Indexed(false, None)),
          Call(Called.Tool(ToolName("search")), Settled.Abandoned, Reading.Other),
          Call(Called.Tool(ToolName("read")), Settled.Unsettled, Reading.Other)
        )
      )
    ),
    Ended.Replied(4, passed = true),
    Vector(
      TurnRecord.Role.Needs,
      TurnRecord.Role.Query,
      TurnRecord.Role.Relevance(2),
      TurnRecord.Role.Agenda,
      TurnRecord.Role.Round(1),
      TurnRecord.Role.Reply,
      TurnRecord.Role.Judge,
      TurnRecord.Role.Summary,
      TurnRecord.Role.Weigh,
      TurnRecord.Role.Extract
    ).map(r => Some(r))
      .appended(None)
      .map(r =>
        Spent(
          r,
          "m/x",
          Usage(Tokens(5), Tokens(2), Tokens(1), Some(BigDecimal("0.0015"))),
          Tokens(6)
        )
      ),
    Some(Drafted(Drafted.Kind.Below, Some(p(0.4)), Some(p(0.7)), Some(p(0.5))))
  )

  /** A TUI turn that failed before its window and a task's that never finished. */
  private val failed = heard.copy(
    workflow = WorkflowId("c2:0"),
    said = Said.Tui(EntryId("e1")),
    root = TurnOffer.Root.Addressed,
    focus = Focus.Focused,
    build = Build.Unknown,
    triage = None,
    offered = None,
    weighed = TurnRecord.Weigh.Recorded(
      Some(TurnWeighing.Weighed.Failed(Weighing.Unweighed.PlacementLate))
    ),
    window = None,
    rounds = Vector.empty,
    ended = Ended.Failed("assemble", Ended.Kind.Assembly),
    spend = Vector(Spent(None, "m/x", Usage(Tokens(1), Tokens(1), Tokens(0), None), Tokens(1))),
    speech = None
  )

  private val unfinished = failed.copy(
    said = Said.Task(EntryId("e2")),
    weighed = TurnRecord.Weigh.Unrecorded,
    ended = Ended.Unfinished("PENDING")
  )

  /** Turns said to grit whose root was asked, and weighed nothing. */
  private val asked = failed.copy(
    workflow = WorkflowId("c3:0"),
    weighed = TurnRecord.Weigh.Recorded(
      Some(TurnWeighing.Weighed.Asked(Weighing.Weighed(repoAt(0.4), Tokens(321))))
    )
  )
  private val nothing = asked.copy(weighed = TurnRecord.Weigh.Recorded(None))

  val tests = Tests {
    test(
      "a turn of every shape reads back as it was written: its offer's shape and set, its weighing of every kind, and its window's documents"
    ) {
      val all = Vector(heard, failed, unfinished, asked, nothing)
      all.map(t => TurnJson.read(ujson.read(TurnJson.write(t).render()))) ==> all.map(Right(_))
    }

    test(
      "a named draft's line has no score, and one written with the named judge's answers reads without them"
    ) {
      val named = ujson.read(TurnJson.write(heard.copy(root = TurnOffer.Root.Named)).render())
      named("speech") = ujson.Obj(
        "outcome" -> "Posted",
        "grounded" -> ujson.Null,
        "worth" -> ujson.Null,
        "answers" -> 0.8,
        "post_at" -> ujson.Null
      )
      // Read and written back as a recapture would write it: scores only, and none for a
      // named draft.
      TurnJson.read(named).map(t => ujson.read(TurnJson.write(t).render())("speech")) ==> Right(
        ujson.Obj(
          "outcome" -> "Posted",
          "grounded" -> ujson.Null,
          "worth" -> ujson.Null,
          "post_at" -> ujson.Null
        )
      )
    }

    test("a line of an older format, or of none, is refused, naming the recapture") {
      // As an earlier build wrote it: no format, or the first, and no documents in its window.
      def older(format: Option[Int]) = {
        val line = ujson.read(TurnJson.write(heard).render())
        line.obj.remove("format")
        format.foreach(n => line("format") = n)
        line("window").obj.remove("documents")
        line
      }
      Vector(older(None), older(Some(1))).map(TurnJson.read) ==> Vector(
        Left("turn names no format: written by an older capture; recapture"),
        Left("turn: format 1, not 2: written by an older capture; recapture")
      )
    }

    test(
      "a window's documents are pinned: a kept one by its plugin and pointer, a gone one alone"
    ) {
      // The stored form: a change here is a change to every capture written before it.
      val line = TurnJson.write(heard).render()
      line.slice(line.indexOf("\"documents\""), line.indexOf(",\"gaps\"")) ==>
        """"documents":[{"kept":{"version":7,"plugin":"knowledge",""" +
        """"pointer":"github__get_file_contents","tokens":55,"support":0.4}},""" +
        """{"kept":{"version":8,"plugin":"digest","pointer":null,"tokens":0,"support":null}},""" +
        """{"gone":{"version":9}}]"""
    }

    test("a turn's line is pinned: its keys in order, its said and ended by kind") {
      // The stored form: a change here is a change to every capture written before it.
      TurnJson.write(unfinished.copy(spend = Vector.empty)).render() ==>
        """{"format":2,"workflow":"c2:0","conversation":"c1","turn":3,"said":{"task":"e2"},""" +
        """"root":"addressed","focus":"focused","started":"2026-10-02T17:05:00Z",""" +
        """"build":"unknown","triage":null,"offered":null,"weighed":"unrecorded",""" +
        """"window":null,"rounds":[],""" +
        """"ended":{"unfinished":"PENDING"},"spend":[],"speech":null}"""
    }

    test("a spend's role is pinned by its written name, the agenda's as topic") {
      // The stored form, as above: a role renamed in Scala keeps the name captures hold.
      ujson.read(TurnJson.write(heard).render())("spend").arr.map(_("role")).toVector ==>
        Vector("needs", "query", "relevance:2", "topic", "round:1", "reply", "judge", "summary")
          .map(ujson.Str(_)) ++ Vector(ujson.Str("weigh"), ujson.Str("extract"), ujson.Null)
    }

    test(
      "a call's form is pinned: an offered tool by name, a topic call and an unnamed one, each with its reading"
    ) {
      // The stored form, as above; a line written before topic calls were kept has no
      // "topic", and reads as before.
      val calls = Vector(
        Call(
          Called.Tool(ToolName("read")),
          Settled.Ok(12),
          Reading.Indexed(true, Support.read(0.5))
        ),
        Call(Called.Topic, Settled.Ok(5), Reading.Other),
        Call(Called.Unnamed, Settled.Expired, Reading.Indexed(false, None))
      )
      val line = TurnJson.write(unfinished.copy(rounds = Vector(Round(calls)))).render()
      line.slice(line.indexOf("\"rounds\""), line.indexOf(",\"ended\"")) ==>
        """"rounds":[[{"tool":"read","topic":false,"settled":"ok","length":12,""" +
        """"reading":{"indexed":{"followed":true,"support":0.5}}},""" +
        """{"tool":null,"topic":true,"settled":"ok","length":5,"reading":"other"},""" +
        """{"tool":null,"topic":false,"settled":"expired","length":null,""" +
        """"reading":{"indexed":{"followed":false,"support":null}}}]]"""
      val older = line.replace(",\"topic\":false", "").replace(",\"topic\":true", "")
      TurnJson.read(ujson.read(older)).map(_.rounds.flatMap(_.calls.map(_.tool))) ==>
        Right(Vector(Called.Tool(ToolName("read")), Called.Unnamed, Called.Unnamed))
    }

    test("a call that names an offered tool and is a topic call is refused") {
      val line = TurnJson
        .write(
          unfinished.copy(rounds =
            Vector(Round(Vector(Call(Called.Tool(ToolName("read")), Settled.Ok(1), Reading.Other))))
          )
        )
        .render()
        .replace("\"topic\":false", "\"topic\":true")
      TurnJson.read(ujson.read(line)) ==> Left("call: read is a topic call")
    }
  }
}
