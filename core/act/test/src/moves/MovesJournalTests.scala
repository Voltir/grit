package grit.act.moves

import grit.core.act.Called
import grit.core.durable.Journaled
import grit.core.edge.RequestState
import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.place.Service
import grit.core.provider.{ModelRequest, ToolSchema, ToolUse}
import grit.core.store.{Payload, PayloadJson}
import grit.core.tool.{Outcome, ToolName}
import grit.core.visibility.{Label, Level}

import utest.*

/** A planner's recorded step outputs and its input digests. Each form is pinned as written, not
  * only round-tripped: a run in flight reads back what an earlier build recorded, and a digest
  * an earlier build recorded is compared with one this build computes, so a changed key or
  * canonical form strands every run in flight.
  */
object MovesJournalTests extends TestSuite {
  import MovesJournal.given

  private def pinned[A](value: A, written: String)(using j: Journaled[A]): Unit = {
    j.encode(value) ==> written
    j.decode(written) ==> Right(value)
  }

  private val message: Message.Assistant = Message.Assistant(
    Vector(AssistantBlock.Text("hello")),
    StopReason.EndTurn,
    Usage(Tokens(10), Tokens(2), Tokens.Zero, Some(BigDecimal("0.001"))),
    "test/summary"
  )

  private val probe: Service =
    Service.of("probe").fold(e => throw new java.lang.AssertionError(e), identity)

  val tests = Tests {
    test(
      "an ask's move step is written as ok with its digest, message, label and the estimate of the request it sent, or refused"
    ) {
      val internal = Label.at(Level.Internal)
      pinned[AskMade](
        AskMade.Made("v1:ab", message, internal, Tokens(42)),
        ujson.write(
          ujson.Obj(
            "ok" -> ujson.Obj(
              "digest" -> "v1:ab",
              "message" -> PayloadJson.write(Payload.Message(message)),
              "at" -> "internal",
              "estimate" -> 42
            )
          )
        )
      )
      pinned[AskMade](
        AskMade.Refused(AskMade.Kind.Capped, "cap", "v1:ab"),
        """{"refused":"capped","why":"cap","digest":"v1:ab"}"""
      )
      pinned[AskMade](
        AskMade.Refused(AskMade.Kind.Model, "down", "v1:ab"),
        """{"refused":"model","why":"down","digest":"v1:ab"}"""
      )
      pinned[AskMade](
        AskMade.Refused(AskMade.Kind.Store, "gone", "v1:ab"),
        """{"refused":"store","why":"gone","digest":"v1:ab"}"""
      )
    }

    test("an ask's record step is written recorded, or with its store's failure") {
      pinned[Either[String, Unit]](Right(()), """{"recorded":true}""")
      pinned[Either[String, Unit]](Left("gone"), """{"store":"gone"}""")
    }

    // A keep carries no digest: its input is its code, which a job's version names. Its value is
    // the keep's own type's journaled string, held as a string, so the moves' form never changes
    // when a plugin's does.
    test("a keep's step is written kept with its value's own journaled string, or store") {
      pinned[Either[String, String]](Right("note"), """{"kept":"note"}""")(using
        MovesJournal.kept[String]
      )
      pinned[Either[String, Either[String, Unit]]](
        Right(Right(())),
        """{"kept":"{\"recorded\":true}"}"""
      )(using MovesJournal.kept[Either[String, Unit]])
      pinned[Either[String, String]](Left("gone"), """{"store":"gone"}""")(using
        MovesJournal.kept[String]
      )
    }

    test("a call's move step is written sent with its digest, or unsent saying why") {
      pinned[CallMade](CallMade.Sent("v1:cd"), """{"sent":{"digest":"v1:cd"}}""")
      CallMade.Kind.values.toVector.map { kind =>
        summon[Journaled[CallMade]].encode(CallMade.Unsent(kind, "w", "v1:cd"))
      } ==> Vector(
        """{"unsent":"unserved","why":"w","digest":"v1:cd"}""",
        """{"unsent":"unadvertised","why":"w","digest":"v1:cd"}""",
        """{"unsent":"refused","why":"w","digest":"v1:cd"}""",
        """{"unsent":"store","why":"w","digest":"v1:cd"}"""
      )
      CallMade.Kind.values.toVector.map(kind =>
        summon[Journaled[CallMade]].decode(
          summon[Journaled[CallMade]].encode(CallMade.Unsent(kind, "w", "v1:cd"))
        )
      ) ==> CallMade.Kind.values.toVector.map(kind => Right(CallMade.Unsent(kind, "w", "v1:cd")))
    }

    test("a call's expire and abandon steps are written with where its request stood") {
      pinned[Either[String, RequestState]](Right(RequestState.Expired), """{"state":"expired"}""")
      pinned[Either[String, RequestState]](Right(RequestState.Claimed), """{"state":"claimed"}""")
      pinned[Either[String, RequestState]](
        Right(RequestState.Answered(Outcome.Done("read"))),
        """{"answered":{"kind":"done","text":"read"}}"""
      )
      pinned[Either[String, RequestState]](Left("gone"), """{"store":"gone"}""")
    }

    test("a call's answer step is written done with its label, failed, interrupted, or store") {
      pinned[Either[String, Called]](
        Right(Called.Done("read", Label.Public)),
        """{"done":{"text":"read","at":"public"}}"""
      )
      pinned[Either[String, Called]](Right(Called.Failed("no")), """{"failed":"no"}""")
      pinned[Either[String, Called]](Right(Called.Interrupted), """{"interrupted":true}""")
      pinned[Either[String, Called]](Left("gone"), """{"store":"gone"}""")
    }

    // Each vector is the SHA-256 of the canonical form spelled out beside it, computed outside
    // grit (`printf '%s' '<form>' | sha256sum`).
    test("an ask's v1 digest is of its system text, messages, tools' names and tool use") {
      // ask10:Say hello.1;u2:hi0;4:auto
      MovesJournal.ask(ModelRequest("Say hello.", Vector(Message.User("hi")))) ==>
        "v1:50383539a7ceebdc8ef75dd02f1d8f9b48fa3993e3f7067c1f96e9b7f7135e20"
      // ask1:s3;u1:qa3;r5:thinkt1:tc2:c110:probe_reado1;4:paths1:ax2:c14:read01;10:probe_read3:off
      val call = ToolCallId("c1")
      MovesJournal.ask(
        ModelRequest(
          "s",
          Vector(
            Message.User("q"),
            Message.Assistant(
              Vector(
                AssistantBlock.Reasoning("think", Some(ujson.Str("opaque"))),
                AssistantBlock.Text("t"),
                AssistantBlock.ToolCall(call, "probe_read", ujson.Obj("path" -> "a"))
              ),
              StopReason.ToolUse,
              Usage.Zero,
              "m"
            ),
            Message.ToolResult(call, "read", isError = false)
          ),
          Vector(ToolSchema("probe_read", "Reads a path.", ujson.Obj())),
          ToolUse.Off
        )
      ) ==> "v1:522fedf1ce2ec8d59b4c9161fac98a23efbd7bcfa20782daeec04f24d7e95075"
      // ask10:Say hello.1;u2:hi0;8:required
      MovesJournal.ask(
        ModelRequest("Say hello.", Vector(Message.User("hi")), use = ToolUse.Required)
      ) ==> "v1:f5299b53da2561d7621e8442dcbd322ca7b0c9e19bd9dc2972026cf0cd05919f"
    }

    test("a call's v1 digest is of its place, tool and arguments, their keys sorted") {
      // call13:service:probe10:probe_reado2;1:nd1:14:paths1:a
      val digest = "v1:0319dd8777604bcd34fab2f4971030dd5956196539cf7b3a73f93a6a6c92899c"
      (
        MovesJournal.call(probe, ToolName("probe_read"), ujson.Obj("path" -> "a", "n" -> 1)),
        MovesJournal.call(probe, ToolName("probe_read"), ujson.Obj("n" -> 1, "path" -> "a"))
      ) ==> (digest, digest)
    }

    test(
      "a recorded digest is compared under its own version, and one of no known version differs"
    ) {
      val request = ModelRequest("Say hello.", Vector(Message.User("hi")))
      val args = ujson.Obj("path" -> "a")
      (
        MovesJournal.sameAsk(MovesJournal.ask(request), request),
        MovesJournal.sameAsk(MovesJournal.ask(request).replace("v1:", "v9:"), request),
        MovesJournal.sameCall(
          MovesJournal.call(probe, ToolName("probe_read"), args),
          probe,
          ToolName("probe_read"),
          args
        ),
        MovesJournal.sameCall(
          MovesJournal.call(probe, ToolName("probe_read"), args),
          probe,
          ToolName("other"),
          args
        )
      ) ==> (true, false, true, false)
    }
  }
}
