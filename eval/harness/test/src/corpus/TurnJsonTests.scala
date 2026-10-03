package grit.eval.harness.corpus

import java.time.Instant

import scala.collection.immutable.VectorMap

import grit.core.id.{ConversationId, EntryId, EntrySeq, TurnSeq, WorkflowId}
import grit.core.message.{Tokens, Usage}
import grit.core.period.Probability
import grit.core.place.Place
import grit.core.prompt.Layer
import grit.core.store.Focus
import grit.core.tool.ToolName
import grit.dbos.engine.Build
import grit.turn.{TurnOffer, TurnRecord}

import utest.*

/** A turn of a corpus as `turns.jsonl` keeps it. */
object TurnJsonTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val c = ConversationId("c1")
  private val at = Instant.parse("2026-10-02T17:05:00Z")
  private def p(x: Double) = Probability.clamped(x)

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
        Vector(ToolName("read"), ToolName("search")),
        Tokens(120),
        VectorMap(Layer.Base -> Tokens(900), Layer.Edge -> Tokens(80)),
        Some(right(Place.read("slack:T1/C1"))),
        Vector(right(Place.read("slack:T1/C2")))
      )
    ),
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
        Tokens(7),
        Tokens(40)
      )
    ),
    Vector(
      Round(Vector(Call(Some(ToolName("read")), Settled.Ok(12)), Call(None, Settled.Failed(3)))),
      Round(
        Vector(
          Call(Some(ToolName("search")), Settled.Expired),
          Call(Some(ToolName("search")), Settled.Abandoned),
          Call(Some(ToolName("read")), Settled.Unsettled)
        )
      )
    ),
    Ended.Replied(4, passed = true),
    Vector(
      TurnRecord.Role.Query,
      TurnRecord.Role.Topic,
      TurnRecord.Role.Round(1),
      TurnRecord.Role.Reply,
      TurnRecord.Role.Judge,
      TurnRecord.Role.Summary
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
    window = None,
    rounds = Vector.empty,
    ended = Ended.Failed("assemble", Ended.Why.Assembly),
    spend = Vector(Spent(None, "m/x", Usage(Tokens(1), Tokens(1), Tokens(0), None), Tokens(1))),
    speech = None
  )

  private val unfinished =
    failed.copy(said = Said.Task(EntryId("e2")), ended = Ended.Unfinished("PENDING"))

  val tests = Tests {
    test("a turn of every shape reads back as it was written") {
      Vector(heard, failed, unfinished).map(t =>
        TurnJson.read(ujson.read(TurnJson.write(t).render()))
      ) ==>
        Vector(Right(heard), Right(failed), Right(unfinished))
    }

    test("a turn's line is pinned: its keys in order, its said and ended by kind") {
      // The stored form: a change here is a change to every corpus written before it.
      TurnJson.write(unfinished.copy(spend = Vector.empty)).render() ==>
        """{"workflow":"c2:0","conversation":"c1","turn":3,"said":{"task":"e2"},""" +
        """"root":"addressed","focus":"focused","started":"2026-10-02T17:05:00Z",""" +
        """"build":"unknown","triage":null,"offered":null,"window":null,"rounds":[],""" +
        """"ended":{"unfinished":"PENDING"},"spend":[],"speech":null}"""
    }
  }
}
