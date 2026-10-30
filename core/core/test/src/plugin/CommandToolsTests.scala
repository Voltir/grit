package grit.core.plugin

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.claim.{Claims, InMemoryClaims}
import grit.core.clock.SetClock
import grit.core.document.{
  DocLabel,
  DocText,
  DocWeight,
  DocumentKeeper,
  DocumentTerms,
  InMemoryDocuments
}
import grit.core.flow.TestTainted
import grit.core.id.{DocKey, PluginName, TestCallSlots, ToolCallId}
import grit.core.identity.{Principal, TestAccounts}
import grit.core.message.AssistantBlock
import grit.core.need.PluginReads
import grit.core.place.Place
import grit.core.store.{Askers, StoreError, Tx}
import grit.core.tool.{Args, Bound, Field, Outcome, Repairs, ToolName, ToolSpec, Toolbox}
import grit.core.visibility.TestLabels.place
import grit.core.visibility.{Cap, Clearance, Compartments, Integrity, Label, Visibility}
import grit.dbos.sql.TestTx

import utest.*

/** [[CommandTools]]: a plugin's command as a turn's tool, run for the person the turn answers. */
object CommandToolsTests extends TestSuite {

  private def got[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val plugin = got(PluginName.of("notes"))
  private val terms =
    got(DocumentTerms.of(got(DocLabel.of("notes")), DocWeight.Unscaled, 30.days, 100))
  private val key = got(DocKey.of("note"))
  private val at = Instant.parse("2026-10-30T12:00:00Z")

  private val person: Principal.Person =
    TestAccounts.principal(TestAccounts.account("slack:T1/U1")) match {
      case p: Principal.Person => p
      case Principal.Grit => throw new java.lang.AssertionError("not a person")
    }

  /** The room the turn's transactions are opened in, capped at outside. */
  private val room = place("slack:T1/C1")
  private val capped = got(
    Visibility
      .of(
        Compartments.Shipped,
        Visibility.Shipped.rooms,
        Vector.empty,
        Vector.empty,
        caps = Vector(Cap(room, Integrity.Outside))
      )
      .left
      .map(_.toString)
  )
  private val inRoom: Tx = TestTx.fake(Clearance.inRoom(room, Label.Public, Label.Public), capped)

  private val said = Args.of((text = Field.text("What."))).map(a => Word(a.text))
  final case class Word(text: String) extends caps.Pure

  /** Keeps `text` under [[key]], believed at authority but for what its keeper allows. */
  private object Note extends KeepingCommand[Word] {
    val spec: ToolSpec[Word] = ToolSpec(ToolName("note"), "Keeps a note.", said)
    def run(args: Word, by: CommandBy, own: DocumentKeeper, claims: Claims, at: Instant)(using
        Tx^
    ): Either[StoreError, CommandOutcome] =
      own
        .write(
          key,
          Label.Public,
          Integrity.Authority,
          Place.Everywhere,
          got(DocText.of(args.text)),
          ujson.Obj(),
          None,
          at
        )
        .map(_ => CommandOutcome.Done(s"kept ${args.text}"))
  }

  /** Answers the note under [[key]], or that there is none. */
  private object Read extends ReadingCommand[Word] {
    val spec: ToolSpec[Word] = ToolSpec(ToolName("read"), "Reads the note.", said)
    def run(args: Word, by: CommandBy, own: PluginReads, at: Instant)(using
        Tx^
    ): Either[StoreError, CommandOutcome] =
      own.documents
        .current(key, Label.Public)
        .map(found => CommandOutcome.Done(found.fold("none")(d => DocText.value(d.text))))
  }

  private def asking(who: Option[Principal]): Askers = new Askers {
    def of(turn: grit.core.id.TurnRef)(using Tx^): Either[StoreError, Option[Principal]] =
      Right(who)
  }

  /** `commanded` as a turn's tool over `docs`, its turn answering `who`, called with `text`
    * stamped at `stamp` and run in `tx`.
    */
  private def call(
      docs: InMemoryDocuments,
      commanded: Commanded,
      who: Option[Principal],
      stamp: Integrity,
      tx: Tx
  ): Either[StoreError, Outcome] = {
    val clock = new SetClock(at)
    val box = Toolbox
      .of[caps.CapSet^{clock}](tool(docs, commanded, who, clock))
      .fold(d => throw new java.lang.AssertionError(d.toString), identity)
    val sent: AssistantBlock.ToolCall =
      AssistantBlock.ToolCall(
        ToolCallId("c"),
        ToolName.value(commanded.command.spec.name),
        ujson.Obj("text" -> "hi")
      )
    box.bind(TestTainted.at(sent, stamp), Repairs.All) match {
      case Right(k: Bound.Keeping) => k(TestCallSlots.First)(using tx)
      case Right(r: Bound.Reading) =>
        r.through(TestReads(tx)) match {
          case free: Bound.Free => Right(free(TestCallSlots.First))
          case other => throw new java.lang.AssertionError(s"not free: $other")
        }
      case other => throw new java.lang.AssertionError(s"not a store tool: $other")
    }
  }

  /** `commanded` as a turn's tool over `docs`, its turn answering `who`, dated by `clock`. */
  private def tool(
      docs: InMemoryDocuments,
      commanded: Commanded,
      who: Option[Principal],
      clock: SetClock
  ): grit.core.tool.Tool.Offered^{clock} =
    CommandTools.of(
      commanded,
      p => PluginReads(new InMemoryPlugins().docs(p), docs.keeper(p, terms)),
      docs.keeper,
      p => new InMemoryClaims().of(p, _ => None),
      asking(who),
      clock
    )

  /** Reads that run each body in `tx`. */
  private def TestReads(tx: Tx): grit.core.store.Reads = new grit.core.store.Reads {
    def read[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] = body(using tx)
  }

  private def kept(docs: InMemoryDocuments): Option[Integrity] =
    docs
      .keeper(plugin, terms)
      .current(key, Label.Public)(using inRoom)
      .toOption
      .flatten
      .map(_.integrity)

  val tests = Tests {
    test("a turn's keep is believed at its call's stamp met with its room's cap") {
      val docs = new InMemoryDocuments
      val keeping = Commanded.Keeping(plugin, terms, Note)
      (call(docs, keeping, Some(person), Integrity.Member, inRoom), kept(docs)) ==> (
        Right(Outcome.Done("kept hi")),
        Some(Integrity.Outside)
      )
    }

    test("a keeping command's tool requires outside, and a reading one's nothing") {
      def requires(commanded: Commanded): Integrity =
        tool(new InMemoryDocuments, commanded, None, new SetClock(at)).entry.requires
      (
        requires(Commanded.Keeping(plugin, terms, Note)),
        requires(Commanded.Reading(plugin, Read))
      ) ==> (Integrity.Outside, Integrity.Untrusted)
    }

    test("a turn that answers no person, or keeps in no room, runs nothing") {
      val docs = new InMemoryDocuments
      val keeping = Commanded.Keeping(plugin, terms, Note)
      val roomless: Tx = TestTx.fake(Clearance.of(Label.Public), capped)
      (
        call(docs, keeping, Some(Principal.Grit), Integrity.Member, inRoom),
        call(docs, keeping, None, Integrity.Member, inRoom),
        call(docs, keeping, Some(person), Integrity.Member, roomless),
        kept(docs)
      ) ==> (
        Right(Outcome.Failed(CommandTools.NoPerson)),
        Right(Outcome.Failed(CommandTools.NoPerson)),
        Right(Outcome.Failed(CommandTools.NoRoom)),
        None
      )
    }

    test("a read is believed at the least it read, and as grit's own words when it read nothing") {
      val docs = new InMemoryDocuments
      val reading = Commanded.Reading(plugin, Read)
      val before = call(docs, reading, Some(person), Integrity.Member, inRoom)
      val _ =
        call(docs, Commanded.Keeping(plugin, terms, Note), Some(person), Integrity.Member, inRoom)
      val after = call(docs, reading, Some(person), Integrity.Member, inRoom)
      (before, after) ==> (
        Right(Outcome.Done("none")),
        Right(Outcome.Done("hi", Some(Integrity.Outside)))
      )
    }
  }
}
