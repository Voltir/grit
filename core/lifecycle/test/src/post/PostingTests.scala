package grit.lifecycle.post

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.document.{
  DocLabel,
  DocText,
  DocWeight,
  DocumentKeeper,
  DocumentTerms,
  InMemoryDocuments
}
import grit.core.durable.InMemoryDurable
import grit.core.id.{CloseRef, ConversationId, DocKey, EntryId, PluginName}
import grit.core.identity.Account
import grit.core.message.Message
import grit.core.period.{CloseOrdinal, CloseReason, TestClosings}
import grit.core.place.{Namespace, Place}
import grit.core.plugin.{
  CacheDocs,
  CachePosting,
  DocumentPosting,
  Documents,
  InMemoryPlugins,
  Plugin,
  PostRef
}
import grit.core.retention.{Target, Tombstone}
import grit.core.store.{
  ClosedPeriod,
  Entry,
  InMemoryConversationStore,
  InMemoryEntryStore,
  InMemoryPeriodStore,
  InMemoryTombstones,
  Jot,
  Origin,
  Payload,
  StoreError,
  Tx
}
import grit.core.visibility.{Clearance, Label, Subject, TestLabels}
import grit.dbos.sql.TestTx
import grit.lifecycle.close.CloseFixtures.SetClock

import utest.*

object PostingTests extends TestSuite {

  private val c = ConversationId("c1")

  private def name(s: String): PluginName =
    PluginName.of(s).getOrElse(throw new java.lang.AssertionError(s))

  private def ordinal(n: Long): CloseOrdinal =
    CloseOrdinal.of(n).getOrElse(throw new java.lang.AssertionError(n))

  /** Keeps each closed period's prose under its close ordinal, as of `version`; refuses the
    * periods whose prose is in `refused`.
    */
  private final class Recorder(
      val name: PluginName,
      val version: Int,
      refused: Set[String] = Set.empty
  ) extends Plugin {
    override val cache: Option[CachePosting] = Some(new CachePosting {
      def post(closed: ClosedPeriod, docs: CacheDocs)(using Tx^): Either[StoreError, Unit] =
        if (refused.contains(closed.closing.flows.prose))
          Left(StoreError.Invalid(s"refused ${closed.closing.flows.prose}"))
        else
          docs.put(
            CloseOrdinal.value(closed.order).toString,
            ujson.Str(s"v$version ${closed.closing.flows.prose}")
          )
    })
  }

  /** Keeps one document per closed period, keyed by its close ordinal, holding its prose;
    * refuses the periods whose prose is in `refused`. With `cached`, also keeps a cache that
    * refuses the periods whose prose is in `cacheRefused`.
    */
  private final class Keeper(
      val name: PluginName,
      refused: Set[String] = Set.empty,
      cached: Boolean = false,
      cacheRefused: Set[String] = Set.empty
  ) extends Plugin {
    val version: Int = 1
    override val cache: Option[CachePosting] =
      Option.when(cached)(new CachePosting {
        def post(closed: ClosedPeriod, docs: CacheDocs)(using Tx^): Either[StoreError, Unit] =
          if (cacheRefused.contains(closed.closing.flows.prose))
            Left(StoreError.Invalid(s"cache refused ${closed.closing.flows.prose}"))
          else docs.put(CloseOrdinal.value(closed.order).toString, ujson.Str("cached"))
      })
    override val documents: Option[Documents] = Some(new Documents with DocumentPosting {
      val posting: Option[DocumentPosting] = Some(this)
      val terms: DocumentTerms =
        DocLabel
          .of("prose")
          .flatMap(DocumentTerms.of(_, DocWeight.Unscaled, 1.day, 100))
          .fold(sys.error, identity)
      def post(closed: ClosedPeriod, keeper: DocumentKeeper)(using
          Tx^
      ): Either[StoreError, Unit] = {
        val prose = closed.closing.flows.prose
        if (refused.contains(prose)) Left(StoreError.Invalid(s"refused $prose"))
        else
          (for {
            key <- DocKey.of(CloseOrdinal.value(closed.order).toString)
            text <- DocText.of(prose)
          } yield (key, text)) match {
            case Left(why) => Left(StoreError.Invalid(why))
            case Right((key, text)) =>
              keeper.write(key, Label.Public, Here, text, ujson.Obj(), closed.at).map(_ => ())
          }
      }
    })
  }

  private val Here: Place = Place.under(Namespace.Task, Vector("posting"))

  /** Contributes nothing to posting: a plugin of tools alone. */
  private final class Unposted(val name: PluginName) extends Plugin {
    val version: Int = 1
  }

  /** Opens each write at what its subject reads, as an opener resolves a conversation's
    * ([[Subject.Conversation]]) from `conversations`; anything else, which posting never opens
    * but publicly, reads what is public.
    */
  private final class SubjectJot(conversations: InMemoryConversationStore) extends Jot {
    def write[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake(clearance(subject)))

    private def clearance(subject: Subject): Clearance = subject match {
      case Subject.Conversation(id) =>
        conversations.all
          .find(_.id == id)
          .fold(Clearance.of(Label.Public))(c => Clearance.inRoom(c.origin.room, c.label, c.label))
      case Subject.Public | Subject.Turn(_) => Clearance.of(Label.Public)
    }
  }

  /** `n` periods, each one turn, closed in order with prose `p1`, `p2`, ...: of `c`, public,
    * but those numbered in `trialled`, which are of a `{trial}` conversation of their own room.
    */
  private final class World(n: Int, trialled: Set[Int] = Set.empty) {
    val conversations = new InMemoryConversationStore
    private def made(origin: Origin, label: Label): ConversationId =
      conversations
        .findOrCreate(origin, Account.Local, label)(using TestTx.fake)
        .fold(e => sys.error(s"$e"), _.id)
    assert(made(Origin.Task("posting", "public"), Label.Public) == c)
    val trial: ConversationId = made(Origin.Slack("T1", "C9", "1.0"), TestLabels.Trial)
    val entries = new InMemoryEntryStore(id => conversations.all.find(_.id == id))
    val periods = new InMemoryPeriodStore(entries)
    val documents = new InMemoryDocuments
    val tombstones: InMemoryTombstones = documents.tombstones
    val plugins = new InMemoryPlugins(tombstones)
    locally {
      given Tx = TestTx.fake
      for (i <- 1 to n) {
        val in = if (trialled.contains(i)) trial else c
        val next = entries.lockNext(in).getOrElse(sys.error("in-memory"))
        val period =
          periods.openFor(in, next.turnSeq, Instant.EPOCH).fold(e => sys.error(s"$e"), _.ref)
        entries.insert(
          Entry(
            EntryId(s"m$i"),
            in,
            next.turnSeq,
            None,
            next.seq,
            Payload.Message(Message.User("hi")),
            Instant.EPOCH
          )
        )
        val closing = TestClosings.prose(s"p$i")
        periods.seal(
          CloseRef(period, next.turnSeq, Instant.EPOCH),
          CloseReason.Lapsed,
          closing,
          Instant.EPOCH
        )
      }
    }
    def run(ps: Vector[Plugin], ref: PostRef): String =
      new InMemoryDurable().run(ref.workflowId)(
        Posting.body(
          ps,
          PostEnv(
            periods,
            plugins.cursors,
            plugins.posting,
            documents.keeper,
            tombstones,
            new SubjectJot(conversations),
            new SetClock(Instant.EPOCH)
          )
        )
      )
    def cursor(p: Plugin): CloseOrdinal =
      plugins.cursors
        .start(p.name, p.version, Instant.EPOCH)(using TestTx.fake)
        .getOrElse(sys.error("cursor"))

    /** `p`'s current documents' keys and texts, oldest written first. */
    def kept(p: Plugin): Vector[(String, String)] =
      documents
        .shelf(p.name)
        .newest(1000)(using TestTx.fake)
        .getOrElse(Vector.empty)
        .reverse
        .map(d => DocKey.value(d.key) -> DocText.value(d.text))

    /** `p`'s cache documents, oldest key first, read at every label the world keeps them at. */
    def docs(p: Plugin): Vector[(String, ujson.Value)] =
      plugins
        .docs(p.name)
        .newest("", 1000)(using TestTx.fake(Clearance.of(TestLabels.Trialled.compartments.top)))
        .getOrElse(Vector.empty)
        .reverse
  }

  val tests = Tests {
    test("each closed period is posted once, in close order, the cursor moved with it") {
      val w = new World(3)
      val digest = new Recorder(name("digest"), 1)
      w.run(Vector(digest), PostRef(digest.name, 1, CloseOrdinal.Start, 0)) ==> "posted 3"
      w.docs(digest) ==> Vector(
        "1" -> ujson.Str("v1 p1"),
        "2" -> ujson.Str("v1 p2"),
        "3" -> ujson.Str("v1 p3")
      )
      w.cursor(digest) ==> ordinal(3)
      w.run(Vector(digest), PostRef(digest.name, 1, ordinal(3), 0)) ==> "posted 0"
    }

    test(
      "a {trial} closing between two public ones is posted, in order, and the cursor never skips it"
    ) {
      val w = new World(3, trialled = Set(2))
      val digest = new Recorder(name("digest"), 1)
      w.run(Vector(digest), PostRef(digest.name, 1, CloseOrdinal.Start, 0)) ==> "posted 3"
      w.docs(digest).map(_._1) ==> Vector("1", "2", "3")
      w.cursor(digest) ==> ordinal(3)
    }

    test(
      "a run that moves the cursor marks the runs from its start, and one that moves none, none"
    ) {
      val w = new World(3)
      val digest = new Recorder(name("digest"), 1)
      val from = PostRef(digest.name, 1, CloseOrdinal.Start, 0)
      w.run(Vector(digest), from) ==> "posted 3"
      w.tombstones.pending ==>
        Vector(Tombstone(Target.PostRuns(digest.name, 1, CloseOrdinal.Start), Instant.EPOCH))
      w.run(Vector(digest), PostRef(digest.name, 1, ordinal(3), 0)) ==> "posted 0"
      w.tombstones.pending.map(_.target) ==>
        Vector(Target.PostRuns(digest.name, 1, CloseOrdinal.Start))
    }

    test("a Left ends the run, the cursor before the period refused") {
      val w = new World(3)
      val picky = new Recorder(name("picky"), 1, refused = Set("p2"))
      w.run(
        Vector(picky),
        PostRef(picky.name, 1, CloseOrdinal.Start, 0)
      ) ==> "posted 1; stopped: refused p2"
      w.cursor(picky) ==> ordinal(1)
    }

    test("a run posts at most MaxPerRun periods, and the next run goes on from its cursor") {
      val w = new World(Posting.MaxPerRun + 1)
      val digest = new Recorder(name("digest"), 1)
      w.run(Vector(digest), PostRef(digest.name, 1, CloseOrdinal.Start, 0)) ==>
        s"posted ${Posting.MaxPerRun}; more to come"
      w.run(Vector(digest), PostRef(digest.name, 1, w.cursor(digest), 0)) ==> "posted 1"
    }

    test("a new version posts every closed period again") {
      val w = new World(2)
      w.run(
        Vector(new Recorder(name("digest"), 1)),
        PostRef(name("digest"), 1, CloseOrdinal.Start, 0)
      )
      val v2 = new Recorder(name("digest"), 2)
      w.run(Vector(v2), PostRef(v2.name, 2, CloseOrdinal.Start, 0)) ==> "posted 2"
      w.docs(v2) ==> Vector("1" -> ujson.Str("v2 p1"), "2" -> ujson.Str("v2 p2"))
    }

    test("a plugin's documents are posted each closed period, with the cursor's move") {
      val w = new World(2)
      val keeper = new Keeper(name("keeper"))
      w.run(Vector(keeper), PostRef(keeper.name, 1, CloseOrdinal.Start, 0)) ==> "posted 2"
      w.kept(keeper) ==> Vector("1" -> "p1", "2" -> "p2")
      w.cursor(keeper) ==> ordinal(2)
    }

    test("documents are posted after the cache; a refusal by either leaves the cursor before") {
      val w = new World(3)
      val cacheFirst = new Keeper(name("cache-first"), cached = true, cacheRefused = Set("p2"))
      w.run(Vector(cacheFirst), PostRef(cacheFirst.name, 1, CloseOrdinal.Start, 0)) ==>
        "posted 1; stopped: cache refused p2"
      w.kept(cacheFirst) ==> Vector("1" -> "p1")
      w.cursor(cacheFirst) ==> ordinal(1)
      val refusing = new Keeper(name("refusing"), refused = Set("p3"), cached = true)
      w.run(Vector(refusing), PostRef(refusing.name, 1, CloseOrdinal.Start, 0)) ==>
        "posted 2; stopped: refused p3"
      w.cursor(refusing) ==> ordinal(2)
    }

    test("a plugin with nothing to post is never posted, and no cursor starts for it") {
      val w = new World(1)
      val tools = new Unposted(name("tools"))
      w.run(Vector(tools), PostRef(tools.name, 1, CloseOrdinal.Start, 0)) ==>
        "plugin tools posts nothing"
      w.plugins.cursors.stored()(using TestTx.fake) ==> Right(Vector())
    }

    test("a run of a plugin not enabled, or of another version, posts nothing") {
      val w = new World(1)
      val digest = new Recorder(name("digest"), 2)
      w.run(
        Vector(digest),
        PostRef(name("wiki"), 1, CloseOrdinal.Start, 0)
      ) ==> "no plugin wiki at version 1"
      w.run(
        Vector(digest),
        PostRef(name("digest"), 1, CloseOrdinal.Start, 0)
      ) ==> "no plugin digest at version 1"
      w.docs(digest) ==> Vector()
    }
  }
}
