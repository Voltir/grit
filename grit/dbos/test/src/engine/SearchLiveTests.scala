package grit.dbos.engine

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, ToolCallId, TurnSeq}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.store.{Entry, EntrySearch, Origin, Payload}
import grit.dbos.sql.{LiveDb, SqlEntrySearch, SqlEntryStore, TestPostgres}

import utest.*

/** [[SqlEntrySearch]] against a real Postgres with pg_textsearch (ADR 0005). */
object SearchLiveTests extends TestSuite {

  // Opening an engine applies schema.sql; opening a second applies it again.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("search_live")
    Engine.open(c, "test").close()
    Engine.open(c, "test").close()
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
    hits.map(h => EntryId.value(h.id).split(':').last)

  private def find(c: ConversationId, query: String, before: Long = 1000, limit: Int = 10) =
    LiveDb.transaction(config)(search.search(c, TurnSeq(before), query, limit))

  val tests = Tests {

    test("the best match comes first, with a positive score, and only matches come back") {
      val c = conversation(
        "rank",
        said("the chat screen scrolls with the wheel"),
        said("Testcontainers starts a throwaway Postgres for the live tests"),
        said("Postgres 18 runs in docker compose")
      )
      val hits = find(c, "testcontainers postgres")
      hits.map(ids) ==> Right(Vector("e1", "e2"))
      assert(hits.exists(_.forall(_.score > 0)))
      assert(hits.exists(h => h.size == 2 && h(0).score > h(1).score))
    }

    test("equal scores come back latest first, and a limit keeps the latest") {
      val c = conversation("ties", said("flaky build"), said("flaky build"), said("flaky build"))
      find(c, "flaky").map(ids) ==> Right(Vector("e2", "e1", "e0"))
      find(c, "flaky", limit = 2).map(ids) ==> Right(Vector("e2", "e1"))
    }

    test("only the conversation's own entries, in turns before the one given") {
      val c = conversation("scope", said("mill shutdown"), said("mill shutdown"), said("mill"))
      val other = conversation("elsewhere", said("mill shutdown"))
      find(c, "mill", before = 2).map(ids) ==> Right(Vector("e1", "e0"))
      find(other, "mill").map(ids) ==> Right(Vector("e0"))
    }

    test("a limit smaller than the matches keeps the best") {
      val c = conversation(
        "limit",
        said("reset-db wipes the databases"),
        said("reset-db"),
        said("unrelated words entirely")
      )
      find(c, "reset-db wipes databases", limit = 1).map(ids) ==> Right(Vector("e0"))
    }

    test("nothing matching, a blank query or no limit is empty, not an error") {
      val c = conversation("empty", said("a few words"), said("some more"))
      find(c, "zeppelin") ==> Right(Vector.empty)
      find(c, "   ") ==> Right(Vector.empty)
      find(c, "words", limit = 0) ==> Right(Vector.empty)
    }

    test(
      "summaries, reply text and tool results are searched; reasoning and tool arguments are not"
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
      find(c, "quokka").map(ids) ==> Right(Vector("e0"))
      find(c, "wombats").map(ids) ==> Right(Vector("e1"))
      find(c, "platypus").map(ids) ==> Right(Vector("e2"))
      find(c, "echidnas") ==> Right(Vector.empty)
      find(c, "numbat") ==> Right(Vector.empty)
    }
  }
}
