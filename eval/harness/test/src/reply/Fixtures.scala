package grit.eval.harness.reply

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, EntrySeq, ToolCallId, TurnRef, TurnSeq, WorkflowId}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.place.Place
import grit.core.store.{Entry, Focus, Nearby, Payload, Speakers}
import grit.dbos.engine.Build
import grit.eval.harness.capture.{CaseId, Ended, Part, Parts, Said, Support, TurnCase}
import grit.turn.{Turn, TurnOffer, TurnRecord}

/** A thread `c1` whose turn 2 answers "when does the deploy move?" from two nearby sections
  * (`c2`, then `c3`, less supported), beside its own earlier turn; and the tool results,
  * reasoning, query and thread messages a review must never show, each holding a marker.
  * Every message here is synthetic.
  */
object Fixtures {

  def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  val c1: ConversationId = ConversationId("c1")
  val c2: ConversationId = ConversationId("c2")
  val c3: ConversationId = ConversationId("c3")
  val at: Instant = Instant.parse("2026-10-02T17:05:00Z")
  private val place = right(Place.read("slack:T1/C2"))

  /** Text a review must never show: each kind's own marker. */
  val Markers: Vector[String] =
    Vector("TOOL-MARKER", "REASON-MARKER", "QUERY-MARKER", "THREAD-MARKER")

  val asked = "when does the deploy move?"
  val replied = "It moved to Friday at 10, after the freeze."
  val near = "Friday at 10 works for the deploy"
  val further = "the freeze ends Thursday"

  def assistant(blocks: AssistantBlock*): Message.Assistant =
    Message.Assistant(blocks.toVector, StopReason.EndTurn, Usage.Zero, "m")

  private def entry(c: ConversationId, turn: Long, seq: Long, p: Payload, id: String = "") =
    Entry(
      EntryId(if (id.isEmpty) s"${ConversationId.value(c)}-$seq" else id),
      c,
      TurnSeq(turn),
      None,
      EntrySeq(seq),
      p,
      at
    )

  /** `c1`'s turn 1, then its turn `seq`, `root`, which replies [[replied]] from a window of
    * turn 1 and two nearby sections; and those sections' entries, `c2`'s first message
    * `nearText`.
    */
  def entries(root: TurnOffer.Root, nearText: String = near, seq: Long = 2): Vector[Entry] = {
    val ref = TurnRef(c1, TurnSeq(seq))
    val call = ToolCallId("call-1")
    Vector(
      // Turn 1: a thread message, its tool loop and its reply.
      entry(c1, 1, 1, Payload.Message(Message.User("THREAD-MARKER: is the freeze on?"))),
      entry(c1, 1, 2, Payload.Message(Message.ToolResult(call, "TOOL-MARKER old", false))),
      entry(
        c1,
        1,
        3,
        Payload.Message(
          assistant(AssistantBlock.Reasoning("REASON-MARKER old", None), AssistantBlock.Text("yes"))
        )
      ),
      // Turn `seq`.
      entry(
        c1,
        seq,
        4,
        root match {
          case TurnOffer.Root.Addressed => Payload.Message(Message.User(asked))
          case TurnOffer.Root.Heard | TurnOffer.Root.Named | TurnOffer.Root.ByName =>
            Payload.Heard(asked)
        }
      ),
      entry(c1, seq, 5, Payload.Query("QUERY-MARKER deploy freeze")),
      entry(
        c1,
        seq,
        6,
        Payload.Window(
          Vector(EntrySeq(1), EntrySeq(2), EntrySeq(3)),
          Vector.empty,
          Vector(
            Nearby.Open(c2, place, Vector(EntrySeq(1), EntrySeq(2))),
            Nearby.Open(c3, place, Vector(EntrySeq(4)))
          )
        ),
        EntryId.value(Turn.windowId(ref))
      ),
      entry(
        c1,
        seq,
        7,
        Payload.Exchange(
          assistant(
            AssistantBlock.Reasoning("REASON-MARKER loop", None),
            AssistantBlock.ToolCall(call, "read", ujson.Obj())
          )
        )
      ),
      entry(
        c1,
        seq,
        8,
        Payload.Result(Message.ToolResult(call, "TOOL-MARKER new", false), "TOOL-MARKER new")
      ),
      entry(
        c1,
        seq,
        9, {
          val m = assistant(
            AssistantBlock.Reasoning("REASON-MARKER reply", None),
            AssistantBlock.Text(replied)
          )
          root match {
            case TurnOffer.Root.Addressed | TurnOffer.Root.ByName => Payload.Message(m)
            case TurnOffer.Root.Heard | TurnOffer.Root.Named => Payload.Draft(m)
          }
        },
        root match {
          case TurnOffer.Root.Addressed | TurnOffer.Root.ByName => EntryId.value(ref.replyId)
          case TurnOffer.Root.Heard | TurnOffer.Root.Named => EntryId.value(ref.draftId)
        }
      ),
      // The nearby sections' conversations.
      entry(c2, 1, 1, Payload.Message(Message.User(nearText))),
      entry(
        c2,
        1,
        2,
        Payload.Message(
          assistant(
            AssistantBlock.Reasoning("REASON-MARKER near", None),
            AssistantBlock.Text("noted")
          )
        )
      ),
      entry(c3, 1, 4, Payload.Heard(further))
    )
  }

  private def support(x: Double) = Some(right(Support.read(x).toRight(s"support $x")))

  /** Turn `seq` of `c1` as captured, said as `said`, its window: `c1`'s earlier turn (the most
    * supported), then `c2`'s section, then `c3`'s.
    */
  def turn(
      seq: Long = 2,
      said: Said = Said.Slack(right(CaseId.read("C1/1000.2"))),
      root: TurnOffer.Root = TurnOffer.Root.Addressed,
      ended: Ended = Ended.Replied(replied.length, passed = false)
  ): TurnCase =
    TurnCase(
      WorkflowId(s"c1:$seq"),
      c1,
      TurnSeq(seq),
      said,
      root,
      Focus.Open,
      at.plusSeconds(seq),
      Build.Unknown,
      None,
      None,
      TurnRecord.Weigh.Unrecorded,
      Some(
        Parts(
          Vector(
            Part(Part.Kind.Open, c2, Vector(EntrySeq(1), EntrySeq(2)), Tokens(30), support(0.5)),
            Part(Part.Kind.Open, c3, Vector(EntrySeq(4)), Tokens(10), support(0.25)),
            Part(
              Part.Kind.Recent,
              c1,
              Vector(EntrySeq(1), EntrySeq(2), EntrySeq(3)),
              Tokens(40),
              support(0.9)
            )
          ),
          Tokens.Zero,
          Tokens.Zero
        )
      ),
      Vector.empty,
      ended,
      Vector.empty,
      None
    )

  val speakers: Speakers = Speakers.none
}
