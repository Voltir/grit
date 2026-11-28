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
import grit.core.id.{DocKey, PluginName, TestCallSlots, ToolCallId, TurnRef}
import grit.core.identity.{Principal, TestAccounts}
import grit.core.message.AssistantBlock
import grit.core.need.PluginReads
import grit.core.place.Place
import grit.core.store.{Askers, Reads, StoreError, Tx}
import grit.core.tool.{Args, Bound, Field, Outcome, Repairs, Tool, ToolName, ToolSpec, Toolbox}
import grit.core.visibility.TestLabels.place
import grit.core.visibility.{Clearance, Integrity, Label}
import grit.dbos.sql.TestTx

import utest.*

/** [[CommandTools]]: a command as a turn's tool runs only for a person the turn answers. */
object HeldCommandToolsTests extends TestSuite {

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

  /** A transaction in a room, so that a keep refused here is refused for want of a person. */
  private val inRoom: Tx =
    TestTx.fake(Clearance.inRoom(place("slack:T1/C1"), Label.Public, Label.Public))

  final case class Word(text: String) extends caps.Pure
  private val said = Args.of((text = Field.text("What."))).map(a => Word(a.text))

  /** Keeps `text` under [[key]]. */
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

  /** Answers "read", reading nothing: any other answer is its run's. */
  private object Read extends ReadingCommand[Word] {
    val spec: ToolSpec[Word] = ToolSpec(ToolName("read"), "Reads.", said)
    def run(args: Word, by: CommandBy, own: PluginReads, at: Instant)(using
        Tx^
    ): Either[StoreError, CommandOutcome] = Right(CommandOutcome.Done("read"))
  }

  private def asking(who: Option[Principal]): Askers = new Askers {
    def of(turn: TurnRef)(using Tx^): Either[StoreError, Option[Principal]] = Right(who)
  }

  /** Reads that run each body in `tx`. */
  private def readsIn(tx: Tx): Reads = new Reads {
    def read[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] = body(using tx)
  }

  /** `commanded`, named `name`, as a turn's tool over `docs`, its turn answering `who`, called
    * stamped at member and run in `tx`.
    */
  private def call(
      docs: InMemoryDocuments,
      commanded: Commanded,
      name: ToolName,
      who: Option[Principal],
      tx: Tx
  ): Either[StoreError, Outcome] = {
    val clock = new SetClock(at)
    val tool: Tool.Offered^{clock} = CommandTools.of(
      commanded,
      p => PluginReads(new InMemoryPlugins().docs(p), docs.keeper(p, terms)),
      docs.keeper,
      p => new InMemoryClaims().of(p, _ => None),
      asking(who),
      clock
    )
    val box = Toolbox
      .of[caps.CapSet^{clock}](tool)
      .fold(d => throw new java.lang.AssertionError(d.toString), identity)
    val sent: AssistantBlock.ToolCall =
      AssistantBlock.ToolCall(ToolCallId("c"), ToolName.value(name), ujson.Obj("text" -> "hi"))
    box.bind(TestTainted.at(sent, Integrity.Member), Repairs.All) match {
      case Right(k: Bound.Keeping) => k(TestCallSlots.First)(using tx)
      case Right(r: Bound.Reading) =>
        r.through(readsIn(tx)) match {
          case free: Bound.Free => Right(free(TestCallSlots.First))
          case other => throw new java.lang.AssertionError(s"not free: $other")
        }
      case other => throw new java.lang.AssertionError(s"not a store tool: $other")
    }
  }

  private def kept(docs: InMemoryDocuments): Option[String] =
    docs
      .keeper(plugin, terms)
      .current(key, Label.Public)(using inRoom)
      .toOption
      .flatten
      .map(d => DocText.value(d.text))

  val tests = Tests {
    test("a keep called by a turn answering no person is failed with NoPerson and keeps nothing") {
      val docs = new InMemoryDocuments
      def keep(who: Option[Principal]) =
        call(docs, Commanded.Keeping(plugin, terms, Note), Note.spec.name, who, inRoom)
      val refused = Vector(keep(None), keep(Some(Principal.Grit)))
      val keptBefore = kept(docs)
      val _ = keep(Some(person))
      (refused, keptBefore, kept(docs)) ==> (
        Vector.fill(2)(Right(Outcome.Failed(CommandTools.NoPerson))),
        None,
        Some("hi")
      )
    }

    test("a read called by a turn answering no person is failed with NoPerson, not answered") {
      def read(who: Option[Principal]) =
        call(new InMemoryDocuments, Commanded.Reading(plugin, Read), Read.spec.name, who, inRoom)
      Vector(read(None), read(Some(Principal.Grit))) ==>
        Vector.fill(2)(Right(Outcome.Failed(CommandTools.NoPerson)))
    }
  }
}
