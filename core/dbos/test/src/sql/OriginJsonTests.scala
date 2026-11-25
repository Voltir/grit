package grit.dbos.sql

import grit.core.id.{ConversationId, TurnRef, TurnSeq}
import grit.core.identity.Account
import grit.core.place.{Directory, Place, Repository, WorkName}
import grit.core.store.Origin

import utest.*

object OriginJsonTests extends TestSuite {

  // The stored form is the conversation's unique key: these pin it, so a
  // change that would orphan existing conversations fails here first.
  private val home = Directory.of("/home/nick").fold(e => sys.error(e), identity)

  private val person: Account.Sourced =
    Account.of("slack", "T1/U1").fold(e => sys.error(e), identity)

  private val run = Origin.Work(
    Repository.of("grit").fold(e => sys.error(e), identity),
    WorkName.of("fix-typo").fold(e => sys.error(e), identity)
  )

  private val child = Origin.Spawned(
    TurnRef(ConversationId("0193a3c2-7c1e-7000-8000-000000000001"), TurnSeq(4)),
    WorkName.of("s1-0-0123456789abcdef").fold(e => sys.error(e), identity),
    Place.read("repository:grit").fold(e => sys.error(e), identity)
  )

  val tests = Tests {
    test("tui") {
      SqlConversationStore.originJson(Origin.Tui(home, "s1")) ==>
        ujson.Obj("kind" -> "tui", "directory" -> "/home/nick", "session" -> "s1")
    }

    test("slack") {
      SqlConversationStore.originJson(Origin.Slack("T1", "C1", "1700000000.000100")) ==>
        ujson.Obj(
          "kind" -> "slack",
          "team" -> "T1",
          "channel" -> "C1",
          "threadTs" -> "1700000000.000100"
        )
    }

    test("room") {
      SqlConversationStore.originJson(Origin.Room("T1", "C1")) ==>
        ujson.Obj("kind" -> "room", "team" -> "T1", "channel" -> "C1")
    }

    test("thread") {
      SqlConversationStore.originJson(Origin.Thread("T1", "C1", "1700000000.000100")) ==>
        ujson.Obj(
          "kind" -> "thread",
          "team" -> "T1",
          "channel" -> "C1",
          "threadTs" -> "1700000000.000100"
        )
    }

    test("task") {
      SqlConversationStore.originJson(Origin.Task("e2e", "2026-09-23")) ==>
        ujson.Obj("kind" -> "task", "name" -> "e2e", "run" -> "2026-09-23")
    }

    test("work") {
      SqlConversationStore.originJson(run) ==>
        ujson.Obj("kind" -> "work", "repository" -> "grit", "work" -> "fix-typo")
    }

    test("spawned") {
      SqlConversationStore.originJson(child) ==>
        ujson.Obj(
          "kind" -> "spawned",
          "conversation" -> "0193a3c2-7c1e-7000-8000-000000000001",
          "turn" -> 4,
          "name" -> "s1-0-0123456789abcdef",
          "within" -> "repository:grit"
        )
    }

    test("direct") {
      SqlConversationStore.originJson(Origin.Direct(person, "1.2")) ==>
        ujson.Obj("kind" -> "direct", "account" -> "slack:T1/U1", "thread" -> "1.2")
    }

    test("a direct message's stored account is one a source names") {
      SqlConversationStore.readOrigin(
        ujson.Obj("kind" -> "direct", "account" -> Account.written(Account.Grit), "thread" -> "1")
      ) ==> Left("a direct message is with an account a source names: grit")
    }

    test("every origin reads back from its stored form; any other form is refused") {
      val origins =
        Vector(
          Origin.Tui(home, "s1"),
          Origin.Slack("T1", "C1", "1.2"),
          Origin.Room("T1", "C1"),
          Origin.Thread("T1", "C1", "1.2"),
          Origin.Task("e2e", "r"),
          Origin.Direct(person, "1.2"),
          run,
          child
        )
      origins.map(o => SqlConversationStore.readOrigin(SqlConversationStore.originJson(o))) ==>
        origins.map(Right(_))
      SqlConversationStore.readOrigin(ujson.Obj("kind" -> "email", "to" -> "x")) ==>
        Left("unknown origin kind: email")
      SqlConversationStore.readOrigin(
        ujson.Obj("kind" -> "work", "repository" -> "grit", "work" -> "Fix")
      ) ==> Left("a run of work's name holds 'F', not a lowercase letter, a digit or '-': Fix")
      SqlConversationStore.readOrigin(
        SqlConversationStore.originJson(child).obj.clone().addOne("within" -> ujson.Str("nowhere"))
      ) ==> Left(
        "no namespace in nowhere: write it as fs:/a/path, slack:team/channel or task:name"
      )
      SqlConversationStore.readOrigin(ujson.Obj("kind" -> "tui", "directory" -> "/home/nick")) ==>
        Left("missing field: session")
      SqlConversationStore.readOrigin(
        ujson.Obj("kind" -> "tui", "directory" -> "home", "session" -> "s1")
      ) ==> Left("not an absolute path: home")
    }
    test("a stored terminal origin whose directory holds a control character reads as it is") {
      SqlConversationStore
        .readOrigin(ujson.Obj("kind" -> "tui", "directory" -> "/home/a\nb", "session" -> "s1"))
        .map { case o: Origin.Tui => Directory.value(o.directory); case o => o.toString } ==>
        Right("/home/a\nb")
    }
  }
}
