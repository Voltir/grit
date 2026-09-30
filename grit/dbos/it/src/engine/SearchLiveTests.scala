package grit.dbos.engine

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, PeriodSeq, ToolCallId, TurnRef, TurnSeq}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.period.{Change, CloseReason, Closing, Flows, Section, TestClosings}
import grit.core.place.Place
import grit.core.store.{Entry, EntrySearch, OpenPeriod, Origin, Payload}
import grit.dbos.sql.{LiveDb, SqlEntrySearch, SqlEntryStore, TestPostgres}

import utest.*

/** [[SqlEntrySearch]] against a real Postgres with pg_textsearch (ADR 0005). */
object SearchLiveTests extends TestSuite {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("search_live")
    LiveEngine.open(c, "test").close()
    c
  }

  private val entries = new SqlEntryStore()
  private val search = new SqlEntrySearch()

  private def reply(blocks: AssistantBlock*): Message =
    Message.Assistant(
      blocks.toVector,
      StopReason.EndTurn,
      Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None),
      "m"
    )

  /** Writes `payloads` as one entry each, turn `t` for the `t`-th, ids `e{t}`. */
  private def conversation(name: String, payloads: Payload*): ConversationId = {
    val c = LiveDb.conversation(config, Origin.Task("search", name)).id
    LiveDb.transaction(config) {
      payloads.zipWithIndex.foreach { (p, i) =>
        val _ = entries.insert(
          Entry(EntryId(s"$name:e$i"), c, TurnSeq(i.toLong), None, i.toLong, p, Instant.EPOCH)
        )
      }
    }
    c
  }

  private def said(text: String): Payload = Payload.Message(Message.User(text))

  private def ids(hits: Vector[EntrySearch.Hit]): Vector[String] =
    hits.map(h => EntryId.value(h.id))

  private def find(
      c: ConversationId,
      query: String,
      before: Long = 1000,
      limit: Int = 10,
      from: Long = 0
  ) =
    LiveDb.transaction(config)(search.search(c, TurnSeq(from), TurnSeq(before), query, limit))

  /** `c`'s period open from turn `first`, as openElsewhere would give it. */
  private def open(c: ConversationId, first: Long): OpenPeriod =
    OpenPeriod(c, Place.Everywhere, TurnSeq(first))

  private def near(query: String, periods: OpenPeriod*) =
    LiveDb.transaction(config)(search.nearby(periods.toVector, query, 10))

  val tests = Tests {
    test("nearby finds other conversations' entries from their open period's first turn on") {
      val x = conversation(
        "near-x",
        said("sourdough starter smells of acetone"),
        Payload.Closed(PeriodSeq.First, CloseReason.Lapsed, TestClosings.prose("sourdough")),
        said("the sourdough levain doubled overnight"),
        said("nothing about bread here")
      )
      val y = conversation("near-y", said("sourdough"))
      near("sourdough levain", open(x, 2), open(y, 0)).map(ids) ==>
        Right(Vector("near-x:e2", "near-y:e0"))
      near("sourdough", open(x, 2), open(y, 0)).map((h: Vector[EntrySearch.Hit]) =>
        h.map(_.turn)
      ) ==>
        Right(Vector(TurnRef(y, TurnSeq(0)), TurnRef(x, TurnSeq(2))))
    }

    test("one scale: the same text scores the same through search and through nearby") {
      val a = conversation("scale-a", said("the flaky invoice test fails in CI"))
      val b = conversation("scale-b", said("the flaky invoice test fails in CI"))
      val both = LiveDb.transaction(config) {
        for {
          own <- search.search(a, TurnSeq(0), TurnSeq(1000), "flaky invoice test", 10)
          there <- search.nearby(Vector(open(b, 0)), "flaky invoice test", 10)
        } yield (own.map(_.score), there.map(_.score))
      }
      both.map((own, there) => (own.size, own == there)) ==> Right((1, true))
    }

    test("nearby over no periods, with a blank query or no limit, is empty") {
      val z = conversation("near-z", said("kumquat"))
      near("kumquat") ==> Right(Vector.empty)
      near("  ", open(z, 0)) ==> Right(Vector.empty)
      LiveDb.transaction(config)(search.nearby(Vector(open(z, 0)), "kumquat", 0)) ==>
        Right(Vector.empty)
    }

    test("the best match comes first, with a positive score, and only matches come back") {
      val c = conversation(
        "rank",
        said("the chat screen scrolls with the wheel"),
        said("Testcontainers starts a throwaway Postgres for the live tests"),
        said("Postgres 18 runs in docker compose")
      )
      val hits = find(c, "testcontainers postgres")
      hits.map(ids) ==> Right(Vector("rank:e1", "rank:e2"))
      assert(hits.exists(_.forall(_.score > 0)))
      assert(hits.exists(h => h.size == 2 && h(0).score > h(1).score))
    }

    test("equal scores come back latest first, and a limit keeps the latest") {
      val c = conversation("ties", said("flaky build"), said("flaky build"), said("flaky build"))
      find(c, "flaky").map(ids) ==> Right(Vector("ties:e2", "ties:e1", "ties:e0"))
      find(c, "flaky", limit = 2).map(ids) ==> Right(Vector("ties:e2", "ties:e1"))
    }

    test("only the conversation's own entries, in turns from the first given and before the last") {
      val c = conversation("scope", said("mill shutdown"), said("mill shutdown"), said("mill"))
      val other = conversation("elsewhere", said("mill shutdown"))
      find(c, "mill", before = 2).map(ids) ==> Right(Vector("scope:e1", "scope:e0"))
      find(c, "mill", from = 1).map(ids).map((v: Vector[String]) => v.sorted) ==> Right(
        Vector("scope:e1", "scope:e2")
      )
      find(other, "mill").map(ids) ==> Right(Vector("elsewhere:e0"))
    }

    test("a heard message is searched by its text") {
      val c = conversation(
        "heard",
        Payload.Heard("standup moves to ten from Monday"),
        said("lunch at noon")
      )
      find(c, "standup").map(ids) ==> Right(Vector("heard:e0"))
    }

    test("a draft is never searched: an unposted draft is not something grit said") {
      val c = conversation(
        "draft",
        said("when is the freeze"),
        Payload.Draft(
          Message.Assistant(
            Vector(AssistantBlock.Text("the freeze moved to Thursday")),
            StopReason.EndTurn,
            Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None),
            "m"
          )
        )
      )
      find(c, "freeze thursday").map(ids) ==> Right(Vector("draft:e0"))
    }

    test("a limit smaller than the matches keeps the best") {
      val c = conversation(
        "limit",
        said("reset-db wipes the databases"),
        said("reset-db"),
        said("unrelated words entirely")
      )
      find(c, "reset-db wipes databases", limit = 1).map(ids) ==> Right(Vector("limit:e0"))
    }

    test("nothing matching, a blank query or no limit is empty, not an error") {
      val c = conversation("empty", said("a few words"), said("some more"))
      find(c, "zeppelin") ==> Right(Vector.empty)
      find(c, "   ") ==> Right(Vector.empty)
      find(c, "words", limit = 0) ==> Right(Vector.empty)
    }

    test(
      "summaries, reply text and tool results are searched; reasoning, tool arguments and queries are not"
    ) {
      val c = conversation(
        "kinds",
        Payload.Summary("the user asked about quokka habitats"),
        Payload.Message(reply(AssistantBlock.Text("wombats dig burrows"))),
        Payload.Message(
          Message.ToolResult(ToolCallId("c1"), "platypus found in the logs", isError = false)
        ),
        Payload.Message(
          reply(
            AssistantBlock.Reasoning("thinking about echidnas", None),
            AssistantBlock.ToolCall(ToolCallId("c2"), "grep", ujson.Obj("q" -> "numbat"))
          )
        )
      )
      find(c, "quokka").map(ids) ==> Right(Vector("kinds:e0"))
      find(c, "wombats").map(ids) ==> Right(Vector("kinds:e1"))
      find(c, "platypus").map(ids) ==> Right(Vector("kinds:e2"))
      find(c, "echidnas") ==> Right(Vector.empty)
      find(c, "numbat") ==> Right(Vector.empty)
      val queried = conversation("query", Payload.Query("bilby"), said("a bilby burrow"))
      find(queried, "bilby").map(ids) ==> Right(Vector("query:e1"))
    }

    test("closings ranks only the closing entries of the conversations given, on search's scale") {
      def closedOn(prose: String) =
        Payload.Closed(PeriodSeq.First, CloseReason.Lapsed, TestClosings.prose(prose))
      val a = conversation(
        "closings-a",
        said("the deploy freeze starts friday"),
        closedOn("we set the deploy freeze for friday afternoon")
      )
      val b = conversation("closings-b", closedOn("the deploy freeze starts friday"))
      val other = conversation("closings-other", closedOn("the deploy freeze starts friday"))
      val found = LiveDb.transaction(config) {
        for {
          hits <- search.closings(Vector(a, b), "deploy freeze friday", 10)
          own <- search.search(b, TurnSeq(0), TurnSeq(1000), "deploy freeze friday", 10)
        } yield (hits, own)
      }
      found.map((hits, _) => ids(hits)) ==> Right(Vector("closings-b:e0", "closings-a:e1"))
      found.map((hits, _) => hits.map(_.turn)) ==>
        Right(Vector(TurnRef(b, TurnSeq(0)), TurnRef(a, TurnSeq(1))))
      found.map((hits, own) => hits.headOption.map(_.score) == own.headOption.map(_.score)) ==>
        Right(true)
      LiveDb.transaction(config)(search.closings(Vector.empty, "deploy", 10)) ==> Right(
        Vector.empty
      )
      LiveDb.transaction(config)(search.closings(Vector(a), " ", 10)) ==> Right(Vector.empty)
      val _ = other
    }

    test("a closing entry is searched by its flows, never by the lines it only carries") {
      import TestClosings.{balance, line}
      val added = line(Section.Standing, "feed the takahe seeds", 2, 2)
      val closing = Closing(
        Flows
          .of(
            "we compared kakapo diets",
            Some("kea eat anything"),
            Vector(
              Change.Added(added),
              Change.Resolved(line(Section.Open, "where do weka roost", 1, 1), "the tuatara knows"),
              Change
                .Dropped(line(Section.Standing, "moa survive", 1, 1), "extinct, says the kokako")
            )
          )
          .getOrElse(sys.error("flows")),
        balance(line(Section.Standing, "the kiwi is nocturnal", 1, 1), added)
      )
      val c = conversation(
        "closed",
        Payload.Closed(PeriodSeq.of(2).getOrElse(sys.error("p")), CloseReason.Lapsed, closing),
        said("nothing to see")
      )
      Vector("kakapo", "kea", "takahe", "weka", "tuatara", "moa", "kokako").map(w =>
        find(c, w).map(ids)
      ) ==> Vector.fill(7)(Right(Vector("closed:e0")))
      find(c, "kiwi").map(ids) ==> Right(Vector.empty)
    }
  }
}
