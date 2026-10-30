package grit.kit.run

import java.time.Instant

import grit.core.claim.InMemoryClaims
import grit.core.clock.SetClock
import grit.core.document.InMemoryDocuments
import grit.core.flow.TestTainted
import grit.core.id.{
  ConversationId,
  JobName,
  PluginName,
  TestCallSlots,
  ToolCallId,
  TurnRef,
  TurnSeq
}
import grit.core.identity.Principal
import grit.core.job.{InMemorySchedules, ScheduleDesk}
import grit.core.message.AssistantBlock
import grit.core.model.{ModelSetting, ModelSettings}
import grit.core.need.PluginReads
import grit.core.period.CloseOrdinal
import grit.core.persona.Persona
import grit.core.place.Place
import grit.core.plugin.InMemoryPlugins
import grit.core.recipe.InMemoryRoomReads
import grit.core.store.{
  Askers,
  Db,
  EntrySearch,
  InMemoryEntryStore,
  InMemoryPrincipals,
  OpenPeriod,
  Origin,
  StoreError,
  Tx
}
import grit.core.tool.{Bound, Outcome, Repairs, ToolName, Toolbox}
import grit.core.visibility.Subject
import grit.dbos.sql.TestTx
import grit.kit.deployment.{Desks, PluginBinding, TestPlugins}
import grit.models.StubModels
import grit.tools.{Coding, MessageIds, Names}

import utest.*

/** How the kit binds a deployment's plugins' tools, and which names grit's own tools hold. */
object PluginToolsTests extends TestSuite {
  import TestPlugins.name

  /** Reads in the one fake transaction. */
  private object FakeDb extends Db {
    def read[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake)
  }

  /** Keeps nothing; only its tool's name is read. */
  private object Unkept extends ModelSettings {
    def keep(setting: ModelSetting): Either[String, Unit] = Right(())
  }

  /** A search finding nothing: what the own tools' names need. */
  private object Unsearched extends EntrySearch {
    def search(c: ConversationId, from: TurnSeq, before: TurnSeq, query: String, limit: Int)(using
        Tx^
    ): Either[StoreError, Vector[EntrySearch.Hit]] = Right(Vector.empty)
    def nearby(open: Vector[OpenPeriod], query: String, limit: Int)(using
        Tx^
    ): Either[StoreError, Vector[EntrySearch.Hit]] = Right(Vector.empty)
    def closings(conversations: Vector[ConversationId], query: String, limit: Int)(using
        Tx^
    ): Either[StoreError, Vector[EntrySearch.Hit]] = Right(Vector.empty)
    def room(room: Place, from: Instant, until: Instant, query: String, limit: Int)(using
        Tx^
    ): Either[StoreError, Vector[EntrySearch.Hit]] = Right(Vector.empty)
  }

  val tests = Tests {
    test("a plugin's tool reads the plugin it needs through its service, not its own documents") {
      val plugins = new InMemoryPlugins
      val documents = new InMemoryDocuments
      val keys = new TestPlugins.Keys(name("keys"))
      val reading = new TestPlugins.Reading(name("reading"), keys, declared = true)
      locally {
        given Tx = TestTx.fake
        val at = CloseOrdinal.of(1).getOrElse(sys.error("ordinal"))
        for (p <- Vector(keys.name, reading.name)) {
          val _ = plugins.cursors.start(p, 1, Instant.EPOCH)
        }
        val _ = plugins.cache(keys.name, at).put("theirs-1", ujson.Str("x"))
        val _ = plugins.cache(reading.name, at).put("mine-1", ujson.Str("x"))
      }
      val bound = PluginBinding
        .bound(Vector(keys, reading), n => PluginReads(plugins.docs(n), documents.shelf(n)))
        .fold(u => sys.error(u.toString), identity)
      val desk =
        new InMemorySchedules().desk(name("keys"), Vector.empty, new SetClock(Instant.EPOCH))
      bound.map(b =>
        b.described.name -> b.described.spec.args
          .read(ujson.Obj("n" -> 1), Set.empty)
          .map(args =>
            b.run.run(
              args,
              TestCallSlots.First,
              FakeDb.as(Subject.Turn(TestCallSlots.First.turn)),
              desk
            )
          )
      ) ==> Vector(
        ToolName("keys") -> Right(Outcome.Done("theirs-1")),
        ToolName("theirs") -> Right(Outcome.Done("theirs-1"))
      )
    }

    test(
      "a turn is offered each plugin's commands that say so, in the plugins' order, and no other"
    ) {
      def called(n: String): ToolName =
        ToolName.of(n).fold(e => throw new java.lang.AssertionError(e), identity)
      def plugin(p: String, offered: Boolean) = new TestPlugins.Commanding(
        name(p),
        Vector(
          new TestPlugins.Says(called(s"${p}_says"), offered),
          new TestPlugins.Notes(called(s"${p}_notes"), offered),
          new TestPlugins.Says(called(s"${p}_hush"))
        ),
        documents = Some(TestPlugins.Tallies)
      )
      val docs = new InMemoryDocuments
      val clock = new SetClock(Instant.EPOCH)
      val offered: Vector[grit.core.tool.Tool.Offered^{clock}] = PluginBinding
        .commands(
          Vector(
            plugin("b", offered = true),
            plugin("c", offered = false),
            plugin("a", offered = true)
          ),
          p =>
            PluginReads(new InMemoryPlugins().docs(p), docs.keeper(p, TestPlugins.Tallies.terms)),
          docs.keeper,
          p => new InMemoryClaims().of(p, _ => None),
          new Askers {
            def of(turn: TurnRef)(using Tx^): Either[StoreError, Option[Principal]] = Right(None)
          },
          clock
        )
      offered.map(_.name) ==> Vector("b_says", "b_notes", "a_says", "a_notes").map(called)
    }

    test("each plugin's tool writes through its own plugin's desk, of that plugin's jobs alone") {
      val (nudge, standup, other) =
        (
          new TestPlugins.Named("nudge"),
          new TestPlugins.Named("standup"),
          new TestPlugins.Named("other")
        )
      val plugins = Vector(
        new TestPlugins.Booking(name("books"), nudge, Vector(nudge, standup)),
        new TestPlugins.Booking(name("more"), other, Vector(other), ToolName("book_more"))
      )
      val asked = scala.collection.mutable.ArrayBuffer.empty[(PluginName, Vector[JobName])]
      val schedules = new InMemorySchedules()
      val clock = new SetClock(Instant.EPOCH)
      val desks: Desks^{clock} = new Desks {
        def of(plugin: PluginName, jobs: Vector[JobName]): ScheduleDesk^ = {
          asked += plugin -> jobs
          schedules.desk(plugin, jobs, clock)
        }
      }
      val reads = new InMemoryPlugins
      val documents = new InMemoryDocuments
      val bound = PluginBinding
        .bound(plugins, n => PluginReads(reads.docs(n), documents.shelf(n)))
        .fold(u => sys.error(u.toString), identity)
      val box = Toolbox
        .of[caps.CapSet^{desks}](bound.map(_.over(desks))*)
        .fold(d => sys.error(d.toString), identity)
      val outcomes = Vector("book", "book_more").map(tool =>
        box.bind(
          TestTainted.authority(
            AssistantBlock.ToolCall(ToolCallId(tool), tool, ujson.Obj("n" -> 1))
          ),
          Repairs.All
        ) match {
          case Right(r: Bound.Reading) =>
            r.through(FakeDb.as(Subject.Turn(TestCallSlots.First.turn))) match {
              case free: Bound.Free => free(TestCallSlots.First)
              case other => sys.error(s"not free: $other")
            }
          case other => sys.error(s"not reading: $other")
        }
      )
      (outcomes, asked.toVector) ==> (
        Vector(Outcome.Done("booked"), Outcome.Done("booked")),
        Vector(
          name("books") -> Vector(nudge.name, standup.name),
          name("more") -> Vector(other.name)
        )
      )
    }

    test(
      "grit's own tool names are those of every tool a launch offers besides the plugins': its own, the operator's, the administering and the coding tools"
    ) {
      val noOne: Askers = new Askers {
        def of(turn: TurnRef)(using Tx^): Either[StoreError, Option[Principal]] = Right(None)
      }
      val models = new StubModels()
      val entries = new InMemoryEntryStore
      val principals = new InMemoryPrincipals
      val unread = MessageIds.Stores(
        entries,
        new InMemoryRoomReads(entries, c => Origin.Task("t", ConversationId.value(c)), principals),
        Unsearched,
        principals
      )
      val admin: grit.core.admin.Administration^ = grit.core.admin.InMemoryAdministration.none()
      val clock: grit.core.clock.Clock^ = grit.core.clock.Clock.system()
      val administering =
        Launch.administering(grit.kit.deployment.Deployments.accepted(), admin, clock).map(_.name)
      val offered =
        Launch
          .own(Persona.Grit, noOne, unread, NoFiles)
          .map(_.name) ++
          Launch.operator(Unkept, models).map(_.name) ++ administering ++ Coding.hosted.map(_.name)
      (Names.all.toSet, Names.all.size) ==> (offered.toSet, offered.size)
    }
  }
}

/** A file area that keeps nothing, for a test that only names the tools over it. */
private object NoFiles extends grit.core.files.Files {
  import grit.core.files.{Admitted, FileError, Files, Formats, OpenFile}
  def formats: Formats = grit.core.files.PagedText.formats
  def admit(handle: grit.core.id.FileHandle)(using
      grit.core.store.Tx^
  ): Either[FileError, Admitted] =
    Left(FileError.Gone(handle, Files.Expiry))
  def open[A](admitted: Admitted)(body: OpenFile^ => A): Either[FileError, A] =
    Left(FileError.Gone(admitted.record.handle, Files.Expiry))
}
