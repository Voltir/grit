package grit.turn

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.clock.Clock
import grit.core.context.{AssemblyError, AssemblyRequest, ContextAssembler, Window}
import grit.core.document.{
  DocLabel,
  DocText,
  DocWeight,
  DocumentTerms,
  InMemoryDocuments,
  Placement,
  Written
}
import grit.core.durable.{Durable, InMemoryDurable}
import grit.core.id.{
  ConversationId,
  DocKey,
  DocumentVersion,
  EntryId,
  EntrySeq,
  PluginName,
  TurnSeq,
  WorkflowId
}
import grit.core.message.Message
import grit.core.place.Place
import grit.core.store.{
  Db,
  Entry,
  InMemoryEntryStore,
  InMemoryUsageLedger,
  Nearby,
  Payload,
  StoreError
}
import grit.dbos.sql.TestTx

import utest.*

/** A turn whose window holds plugins' documents: what the model is shown of them, and what
  * `record-window` counts.
  */
object TurnDocumentsTests extends TestSuite {
  import TurnFixtures.*

  private def got[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)
  private def ok[A](e: Either[StoreError, A]): A =
    e.fold(err => throw new java.lang.AssertionError(s"$err"), identity)

  private val digest = got(PluginName.of("digest"))
  private val board = got(Place.read("slack:T1/C1"))
  private val written = Instant.parse("2026-09-30T23:30:00Z")

  private def terms(label: String): DocumentTerms =
    got(DocumentTerms.of(got(DocLabel.of(label)), DocWeight.Unscaled, 30.days, 10))

  /** `text` written under `key` by `plugin`'s keeper in `documents`; the version written. */
  private def write(
      documents: InMemoryDocuments,
      plugin: PluginName,
      label: String,
      key: String,
      text: String
  ): DocumentVersion =
    ok(
      documents
        .keeper(plugin, terms(label))
        .write(got(DocKey.of(key)), board, got(DocText.of(text)), ujson.Obj(), written)(using
          TestTx.fake
        )
    ) match {
      case Written.Versioned(v, _) => v
      case Written.Unchanged(v) => v
    }

  /** An assembler whose every window is `window`. */
  private def windowing(window: Window): ContextAssembler = new ContextAssembler {
    def assemble(request: AssemblyRequest)(using Db^): Either[AssemblyError, Window] =
      Right(window)
  }

  val tests = Tests {
    test(
      "each document is shown under its plugin's label after the nearby sections and before the own turns; one gone or no longer enabled by call time is left out, the turn not failed"
    ) {
      val entries = new InMemoryEntryStore
      val provider = new RecordingProvider
      val documents = new InMemoryDocuments
      documents.declare(Vector(digest -> terms("Weekly digest")))(using TestTx.fake)
      val week = write(documents, digest, "Weekly digest", "week", "Deploys frozen until Friday.")
      val gone = write(documents, digest, "Weekly digest", "old", "Deploys open.")
      // Its plugin wrote it, then was left out of the deployment: no terms in force.
      val dropped = write(documents, got(PluginName.of("notes")), "Notes", "n", "A note.")
      ok(documents.forget(gone)(using TestTx.fake))
      say(entries, "one")
      val web = ConversationId("web")
      entries.insert(
        Entry(
          EntryId("web:u"),
          web,
          TurnSeq(0),
          None,
          EntrySeq(0),
          Payload.Message(Message.User("the login page is blank")),
          Instant.EPOCH
        )
      )(using TestTx.fake)
      val nearby =
        Vector(Nearby.Open(web, got(Place.read("fs:/home/nick/web")), Vector(EntrySeq(0))))
      val held = Vector(week, gone, dropped)
      val turn = say(entries, "two")
      val output = new InMemoryDurable().run(turn.workflowId)(
        turnBodyWith(
          entries,
          provider,
          windowing(Window(Vector(EntrySeq(0)), Vector.empty, nearby, held)),
          new InMemoryUsageLedger,
          documents = documents
        )
      )
      provider.requests.headOption.map(_.messages) ==> Some(
        Vector(
          Message.User(
            "[afar] another conversation, shown by grit, still open, at fs:/home/nick/web:\n" +
              "User: the login page is blank"
          ),
          Message.User(
            "[doc] Weekly digest, kept by grit, at slack:T1/C1, written 2026-09-30:\n" +
              "Deploys frozen until Friday."
          ),
          Message.User("one"),
          Message.User("two")
        )
      )
      assert(output.startsWith("replied:"))
      windows(entries).lastOption.map(_._2) ==>
        Some(Payload.Window(Vector(EntrySeq(0)), Vector.empty, nearby, held))
    }

    test(
      "record-window counts each document the window holds as placed once, at the window's time; a replayed turn counts none again"
    ) {
      val entries = new InMemoryEntryStore
      val documents = new InMemoryDocuments
      documents.declare(Vector(digest -> terms("Weekly digest")))(using TestTx.fake)
      val week = write(documents, digest, "Weekly digest", "week", "Deploys frozen until Friday.")
      val month = write(documents, digest, "Weekly digest", "month", "Two releases shipped.")
      val turn = say(entries, "hello")
      val durable = new InMemoryDurable
      val provider = new RecordingProvider
      val at = Instant.parse("2026-10-01T09:00:00Z")
      val clock = new Clock {
        def now(): Instant = at
        def millis(): Long = at.toEpochMilli
        def sleep(duration: FiniteDuration): Unit = ()
      }
      def body(id: WorkflowId)(using Durable^): String =
        turnBodyWith(
          entries,
          provider,
          windowing(Window(Vector(EntrySeq(0)), Vector.empty, Vector.empty, Vector(week, month))),
          new InMemoryUsageLedger,
          documents = documents,
          clock = clock
        )(id)
      durable.run(turn.workflowId)(body(_))
      ok(entries.list(conversation)(using TestTx.fake))
        .collectFirst { case e if e.id == Turn.windowId(turn) => e.createdAt } ==> Some(at)
      def placements = ok(documents.read(Vector(week, month))(using TestTx.fake)).map(_.placement)
      placements ==> Vector.fill(2)(Placement(1, at))
      assert(durable.replay(turn.workflowId, durable.history(turn.workflowId))(body(_)).isRight)
      placements ==> Vector.fill(2)(Placement(1, at))
    }
  }
}
