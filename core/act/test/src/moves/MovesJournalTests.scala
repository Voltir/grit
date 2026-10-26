package grit.act.moves

import grit.core.act.MovesFixtures.shown
import grit.core.act.{Answer, Called, Declassifying}
import grit.core.classify.{Answer as Judged, Ask, Criterion, Level as Rung, Request, StateJson}
import grit.core.document.{Declassified, DocText, PolicyApplication}
import grit.core.durable.Journaled
import grit.core.edge.RequestState
import grit.core.files.TestPieces
import grit.core.flow.{TestTainted, Trusted}
import grit.core.id.{ConversationId, DocKey, DocumentVersionId, PluginName, ToolCallId}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.place.{Namespace, Place, Service}
import grit.core.provider.{ModelRequest, ToolSchema, ToolUse}
import grit.core.schema.JsonSchema
import grit.core.screen.{Must, Screen}
import grit.core.stitch.{Strand, StrandDigest, StrandWorld}
import grit.core.store.{Payload, PayloadJson}
import grit.core.tool.{Outcome, ToolName}
import grit.core.visibility.{
  DeclassificationPolicy,
  Facts,
  Integrity,
  Label,
  Level,
  Provenance,
  Readers,
  ResultPart,
  TestLabels
}

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

  /** The answer step `written` read back, its run starting at `start`: what it came to, and,
    * when done, what it is believed at.
    */
  private def answer(
      written: String,
      start: Integrity = Integrity.Authority
  ): Either[String, Either[String, (String, Option[Integrity])]] =
    MovesJournal
      .answered(start)
      .decode(written)
      .map(_.map {
        case c @ Called.Done(text, _) => (shown(c), Some(text.integrity))
        case other => (shown(other), None)
      })

  /** The facts a done answer stated; none for any other. */
  private def stated(called: Called): Option[Outcome.Stated] = called match {
    case Called.Done(answer, _) => Some(Trusted.asIs(answer).extract.stated)
    case _ => None
  }

  private val message: Message.Assistant = Message.Assistant(
    Vector(AssistantBlock.Text("hello")),
    StopReason.EndTurn,
    Usage(Tokens(10), Tokens(2), Tokens.Zero, Some(BigDecimal("0.001"))),
    "test/summary"
  )

  /** A schema of `a`, a string, and `b`, an integer, its properties in `order`. */
  private def schema(order: String*): JsonSchema = {
    val types = Map("a" -> "string", "b" -> "integer")
    JsonSchema
      .read(
        ujson.Obj(
          "type" -> "object",
          "properties" -> ujson.Obj.from(
            order.map(p => p -> ujson.Obj("type" -> types.getOrElse(p, "string")))
          ),
          "required" -> ujson.Arr.from(order.map(ujson.Str(_))),
          "additionalProperties" -> false
        )
      )
      .fold(e => throw new java.lang.AssertionError(e.message), identity)
  }

  /** A choice with a described key and a bare one, a yes/no saying what yes means, and a score
    * of two levels, asked about `state` in one request.
    */
  private def judged(state: ujson.Value): Request = {
    given StateJson[ujson.Value] = StateJson.instance(v => v)
    val asked = for {
      choice <- Ask
        .choice[ujson.Value, Int](
          "Pick.",
          Criterion(1, "k1", Some("one")),
          Criterion(2, "k2", None)
        )
        .left
        .map(_.toString)
      score <- Ask
        .score[ujson.Value, Int]("How?", Rung(0, "low"), Rung(1, "high"))
        .left
        .map(_.toString)
    } yield choice.zip(Ask.yesNo[ujson.Value]("Yes?", Some("y"), None)).zip(score)
    asked.map(Request.of(state, _)).fold(e => throw new java.lang.AssertionError(e), identity)
  }

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

    test(
      "an uncleared ask's refusal is written with the label it was made from, and one without it does not read"
    ) {
      pinned[AskMade](
        AskMade.Refused(
          AskMade.Kind.Uncleared,
          "no model cleared for internal",
          "v1:ab",
          Some(Label.at(Level.Internal))
        ),
        """{"refused":"uncleared","why":"no model cleared for internal","digest":"v1:ab","at":"internal"}"""
      )
      summon[Journaled[AskMade]]
        .decode("""{"refused":"uncleared","why":"no model cleared","digest":"v1:ab"}""")
        .isLeft ==> true
    }

    test(
      "a call's cost is written with who served it, and one recorded before reads with nobody"
    ) {
      val served = AskMade.Call("test/summary", message.usage, Tokens(42), Some("acme"))
      pinned[AskMade](
        AskMade.Unshaped("v1:ab", "no call", Vector(served)),
        """{"unshaped":{"digest":"v1:ab","why":"no call","calls":[{"model":"test/summary",""" +
          """"usage":{"input":10,"output":2,"cachedInput":0,"costUsd":"0.001"},"estimate":42,"upstream":"acme"}]}}"""
      )
      pinned[AskMade](
        AskMade.Judged(
          "v1:ab",
          "[]",
          Label.at(Level.Internal),
          served.copy(upstream = Some("service:jev"))
        ),
        """{"judged":{"digest":"v1:ab","answers":[],"model":"test/summary",""" +
          """"usage":{"input":10,"output":2,"cachedInput":0,"costUsd":"0.001"},"estimate":42,""" +
          """"upstream":"service:jev","at":"internal"}}"""
      )
      summon[Journaled[AskMade]].decode(
        """{"unshaped":{"digest":"v1:ab","why":"no call","calls":[{"model":"test/summary",""" +
          """"usage":{"input":10,"output":2,"cachedInput":0,"costUsd":"0.001"},"estimate":42}]}}"""
      ) ==> Right(AskMade.Unshaped("v1:ab", "no call", Vector(served.copy(upstream = None))))
    }

    test(
      "a JSON ask's move step is written shaped with its digest, reply, label, posing level and each call's model, usage and estimate, or unshaped saying why"
    ) {
      val calls = Vector(
        AskMade.Call("test/summary", message.usage, Tokens(42)),
        AskMade.Call("test/summary", Usage(Tokens(3), Tokens(1), Tokens.Zero, None), Tokens(57))
      )
      val written =
        """[{"model":"test/summary","usage":{"input":10,"output":2,"cachedInput":0,"costUsd":"0.001"},"estimate":42},""" +
          """{"model":"test/summary","usage":{"input":3,"output":1,"cachedInput":0},"estimate":57}]"""
      pinned[AskMade](
        AskMade.Shaped(
          "v1:ab",
          """{"b":2,"a":"x"}""",
          Label.at(Level.Internal),
          Integrity.Member,
          calls
        ),
        s"""{"shaped":{"digest":"v1:ab","reply":{"b":2,"a":"x"},"at":"internal","level":"member","calls":$written}}"""
      )
      pinned[AskMade](
        AskMade.Unshaped("v1:ab", "no call of `reply`", calls),
        s"""{"unshaped":{"digest":"v1:ab","why":"no call of `reply`","calls":$written}}"""
      )
    }

    test(
      "a summary's shaped step holds the versions it showed after its calls; one recorded without them showed none"
    ) {
      val versions = Vector(3L, 7L).map(n =>
        DocumentVersionId.of(n).getOrElse(throw new java.lang.AssertionError(s"version $n"))
      )
      val shaped =
        AskMade.Shaped("v2:ab", """{"a":"x"}""", Label.Public, Integrity.Member, Vector())
      pinned[AskMade](
        shaped.withShown(versions),
        """{"shaped":{"digest":"v2:ab","reply":{"a":"x"},"at":"public","level":"member","calls":[],"shown":[3,7]}}"""
      )
      summon[Journaled[AskMade]].decode(
        """{"shaped":{"digest":"v1:ab","reply":{"a":"x"},"at":"public","level":"member","calls":[]}}"""
      ) ==> Right(
        AskMade.Shaped("v1:ab", """{"a":"x"}""", Label.Public, Integrity.Member, Vector())
      )
    }

    test(
      "a judgment's move step is written judged with its digest, answers as written, call and label, or unjudged saying why"
    ) {
      val call = AskMade.Call(
        "jev",
        Usage(Tokens(300), Tokens(0), Tokens.Zero, Some(BigDecimal("0.000013"))),
        Tokens(250)
      )
      val answers =
        """[{"choice":"k1","weights":[{"key":"k1","p":0.75},{"key":"k2","p":0.25}]},{"yes":0.5}]"""
      val cost =
        """"model":"jev","usage":{"input":300,"output":0,"cachedInput":0,"costUsd":"0.000013"},"estimate":250"""
      pinned[AskMade](
        AskMade.Judged("v1:ab", answers, Label.at(Level.Internal), call),
        s"""{"judged":{"digest":"v1:ab","answers":$answers,$cost,"at":"internal"}}"""
      )
      pinned[AskMade](
        AskMade.Unjudged("v1:ab", "chose k9, not an option", call),
        s"""{"unjudged":{"digest":"v1:ab","why":"chose k9, not an option",$cost}}"""
      )
    }

    test(
      "a workflow's judgment step is written with what it made, its calls and the label it was asked under, and one recorded before it held a label reads with none"
    ) {
      given Journaled[Judging.Posed[String]] = MovesJournal.posed[String]
      val call = AskMade.Call("jev", Usage(Tokens(300), Tokens(0), Tokens.Zero, None), Tokens(250))
      val cost =
        """{"model":"jev","usage":{"input":300,"output":0,"cachedInput":0},"estimate":250}"""
      pinned(
        Judging.Posed("kept", Vector(call), Some(Label.at(Level.Internal))),
        s"""{"made":"kept","calls":[$cost],"at":"internal"}"""
      )
      // A record made before the label was kept, as every history of the epoch before holds.
      pinned(Judging.Posed("kept", Vector(), None), """{"made":"kept","calls":[]}""")
    }

    test(
      "an ask step's label reads alike from a workflow's judgment and a job's ask, and is none where it records none"
    ) {
      val internal = Label.at(Level.Internal)
      val judgment =
        MovesJournal.posed[String].encode(Judging.Posed("kept", Vector(), Some(internal)))
      val made = summon[Journaled[AskMade]].encode(
        AskMade.Made("v1:ab", message, Label.at(Level.Public), Tokens(10))
      )
      val unshaped =
        summon[Journaled[AskMade]].encode(AskMade.Unshaped("v1:ab", "no JSON", Vector()))
      val unasked = MovesJournal.posed[String].encode(Judging.Posed("kept", Vector(), None))
      Vector(judgment, made, unshaped, unasked, """{"recorded":true}""").map(
        MovesJournal.askedAt
      ) ==> Vector(Some(internal), Some(Label.at(Level.Public)), None, None, None)
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

    // A recorded step must never hold what only some readers may read.
    test(
      "a call's expire or abandon step that found an answer cut by item records it without its parts"
    ) {
      val part = ResultPart(
        Provenance(probe.place, Integrity.Member),
        Readers.AtPlace,
        "only some may read this"
      )
      summon[Journaled[Either[String, RequestState]]].encode(
        Right(RequestState.Answered(Outcome.Done("read", parts = Some(Vector(part)))))
      ) ==> """{"answered":{"kind":"done","text":"read"}}"""
    }

    test(
      "a call's answer step is written done with its label and integrity, failed, interrupted, or store"
    ) {
      given Journaled[Either[String, Called]] = MovesJournal.answered(Integrity.Authority)
      val done = Called.Done(
        TestTainted.at(Answer("read", Outcome.Stated.None), Integrity.Member),
        Label.Public
      )
      val written = """{"done":{"text":"read","at":"public","integrity":"member"}}"""
      summon[Journaled[Either[String, Called]]].encode(Right(done)) ==> written
      answer(written) ==> Right(Right(("Done(read,public)", Some(Integrity.Member))))
      pinned[Either[String, Called]](Right(Called.Failed("no")), """{"failed":"no"}""")
      pinned[Either[String, Called]](Right(Called.Interrupted), """{"interrupted":true}""")
      pinned[Either[String, Called]](Left("gone"), """{"store":"gone"}""")
    }

    test("a done answer recorded before it held its integrity is read at the run's own") {
      answer("""{"done":{"text":"read","at":"public"}}""", Integrity.Outside) ==>
        Right(Right(("Done(read,public)", Some(Integrity.Outside))))
    }

    // A pin of the recorded form: every run recorded so far holds this form, and replays with
    // no patch only while it reads back as it did and encodes to the bytes it was recorded with.
    test(
      "an old-form done answer reads back with no facts, its text byte-identical, and writes back the same bytes"
    ) {
      val journaled = MovesJournal.answered(Integrity.Authority)
      val written =
        """{"done":{"text":"a \"quoted\" line\nand é","at":"public","integrity":"member"}}"""
      val read = journaled.decode(written)
      (read.map(_.map(shown)), read.map(_.map(stated)), read.map(journaled.encode)) ==> (
        Right(Right("Done(a \"quoted\" line\nand é,public)")),
        Right(Right(Some(Outcome.Stated.None))),
        Right(written)
      )
    }

    test("a done answer records its stated facts only when it has some") {
      val journaled = MovesJournal.answered(Integrity.Authority)
      val listed = Facts.of(ujson.Obj("complete" -> true))
      val file = Facts.of(ujson.Obj("id" -> "f1"))
      def done(s: Outcome.Stated): Either[String, Called] =
        Right(Called.Done(TestTainted.at(Answer("read", s), Integrity.Member), Label.Public))
      val prefix = """{"done":{"text":"read","at":"public","integrity":"member""""
      val all = Vector(
        Outcome.Stated(Some(listed), Vector(file, listed)) ->
          (prefix + ""","stated":{"answer":{"complete":true},"parts":[{"id":"f1"},{"complete":true}]}}}"""),
        Outcome.Stated(None, Vector(file)) -> (prefix + ""","stated":{"parts":[{"id":"f1"}]}}}"""),
        Outcome.Stated(Some(listed), Vector.empty) ->
          (prefix + ""","stated":{"answer":{"complete":true},"parts":[]}}}"""),
        Outcome.Stated.None -> (prefix + "}}")
      )
      all.map((s, _) => journaled.encode(done(s))) ==> all.map(_._2)
      all.map((_, w) => journaled.decode(w).map(_.map(stated))) ==>
        all.map((s, _) => Right(Right(Some(s))))
    }

    // Each vector is the SHA-256 of the canonical form spelled out beside it, computed outside
    // grit (`printf '%s' '<form>' | sha256sum`).
    test(
      "a summary of one strand alone is digested v1, of its root, strand digest, asOf, chars, system text and schema; of more, v2, its documents' budget included; and a changed strand is refused so"
    ) {
      val schema = JsonSchema
        .read(
          ujson.Obj(
            "type" -> "object",
            "properties" -> ujson.Obj("summary" -> ujson.Obj("type" -> "string")),
            "required" -> ujson.Arr("summary"),
            "additionalProperties" -> false
          )
        )
        .fold(e => throw new java.lang.AssertionError(e.message), identity)
      val digest = StrandDigest
        .read("0123456789abcdef")
        .fold(e => throw new java.lang.AssertionError(e), identity)
      val at = java.time.Instant.parse("2026-10-12T09:10:00Z")
      val seen = Strand.Digested(
        Place.under(Namespace.Task, Vector("room")),
        Label.Public,
        ConversationId("c1"),
        Vector(ConversationId("c1")),
        2,
        40,
        at,
        at,
        digest,
        at
      )
      // strand2:c116:0123456789abcdef20:2026-10-12T09:10:00Z1000;10:Summarise.112:{…}
      MovesJournal.strand(seen, "Summarise.", 1000, schema) ==>
        "v1:124bfd546ae887abb0a8afdd91774f22a6644f51e5bf01855aeb4789971ae667"
      assert(
        MovesJournal.sameStrands(
          MovesJournal.strand(seen, "Summarise.", 1000, schema),
          Vector(seen),
          Vector.empty,
          "Summarise.",
          1000,
          0,
          schema
        ),
        !MovesJournal.sameStrands(
          MovesJournal.strand(seen, "Summarise.", 999, schema),
          Vector(seen),
          Vector.empty,
          "Summarise.",
          1000,
          0,
          schema
        )
      )
      // One strand and no documents is digested as v1 was, whatever the documents' budget.
      MovesJournal.strands(Vector(seen), Vector.empty, "Summarise.", 1000, 500, schema) ==>
        MovesJournal.strand(seen, "Summarise.", 1000, schema)
      val version =
        DocumentVersionId.of(7).getOrElse(throw new java.lang.AssertionError("version"))
      val many =
        MovesJournal.strands(Vector(seen), Vector(version), "Summarise.", 1000, 500, schema)
      (
        many,
        MovesJournal.sameStrands(
          many,
          Vector(seen),
          Vector(version),
          "Summarise.",
          1000,
          500,
          schema
        ),
        MovesJournal.sameStrands(
          many,
          Vector(seen),
          Vector(version),
          "Summarise.",
          1000,
          400,
          schema
        ),
        MovesJournal.sameStrands(
          many,
          Vector(seen, seen),
          Vector.empty,
          "Summarise.",
          1000,
          500,
          schema
        )
      ) ==> (
        "v2:d45de61289993f40fdb6057d4b43904f8b6d38fde33b5f81b8a4ad0eccb5a879",
        true,
        false,
        false
      )
      pinned[AskMade](
        AskMade.Refused(AskMade.Kind.Changed, "moved", "v1:ab"),
        """{"refused":"changed","why":"moved","digest":"v1:ab"}"""
      )
    }

    test("a rewrite or check whose source's file had no pieces is written unpieced, with its why") {
      pinned[AskMade](
        AskMade.Refused(AskMade.Kind.Unpieced, "no pieces", "v2:ab"),
        """{"refused":"unpieced","why":"no pieces","digest":"v2:ab"}"""
      )
    }

    test(
      "a rewrite or check whose policy version in force has no checks that read is written unchecked, and reads back as unchecked"
    ) {
      val made =
        AskMade.Refused(Act.unreadKind(Declassifying.Unread.Unchecked), "no checks", "v2:ab")
      (
        pinned[AskMade](made, """{"refused":"unchecked","why":"no checks","digest":"v2:ab"}"""),
        Act.unreadOf(AskMade.Kind.Unchecked)
      ) ==> ((), Some(Declassifying.Unread.Unchecked))
    }

    test("a strand's summary records none of what its members said, and its reply's level") {
      val (world, root) = MovesContract.strand()
      val seen = StrandWorld.right(
        world.shelf.of(Vector(root), StrandWorld.at(600))(using grit.dbos.sql.TestTx.fake)
      )
      val w = new MovesWorld(broken = false, MovesContract.Floor)
      val _ = w.dreaming(MovesContract.limits(1, 0), world)(m =>
        MovesContract.Seen(Vector(MovesContract.summarised(m, "s", seen)))
      )
      val outputs = w.durable.history(MovesWorld.Turn.workflowId).map(_.toString)
      assert(
        outputs.nonEmpty,
        outputs.forall(o => !o.contains("deploy") && !o.contains("stuck")),
        outputs.exists(_.contains("\"level\":\"outside\""))
      )
    }

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

    test(
      "a JSON ask's v1 digest is of its system text, messages and schema as sent, its properties in order"
    ) {
      val asked = Vector(Message.User("hi"))
      // json10:Say hello.1;u2:hi127:{"type":"object","properties":{"a":{"type":"string"},
      //   "b":{"type":"integer"}},"required":["a","b"],"additionalProperties":false}
      // and the same with b before a, in properties and required.
      (
        MovesJournal.json("Say hello.", asked, schema("a", "b")),
        MovesJournal.json("Say hello.", asked, schema("b", "a"))
      ) ==> (
        "v1:3e4db55a8bbd765864607dc393e7e4daa04844d32440b7727d1a74fddd3f413e",
        "v1:ce9b28636014d3a670105b17991e2ea46cde272d44975b8ec0dda8733bd975b3"
      )
    }

    test(
      "a judgment's v1 digest is of its state, its keys in order, and each question's kind, words, and keys, levels or meanings"
    ) {
      // judgeo2;1:bd1:11:as1:x3;c5:Pick.2;2:k1s3:one2:k2ny4:Yes?s1:yns4:How?2;3:low4:high
      // and the same with the state's a before b.
      (
        MovesJournal.judgment(judged(ujson.Obj("b" -> 1, "a" -> "x"))),
        MovesJournal.judgment(judged(ujson.Obj("a" -> "x", "b" -> 1)))
      ) ==> (
        "v1:fe412c8e2e684e93d4c008f8ea2a09f1a0cd827174fb7bca724eb8f1c5010991",
        "v1:d6b59dee7b0a3d0a65058b41912ef4937241a3bfaa7d0c8c00f3777561b78cb6"
      )
    }

    test(
      "a v1 rewrite digest is still compared under v1's form; a new one is v2 and differs by revision"
    ) {
      val source = DocumentVersionId.of(7).getOrElse(sys.error("no version"))
      val applied = PolicyApplication(
        DeclassificationPolicy(
          DeclassificationPolicy.Over.Compartment(TestLabels.compartment("team")),
          1
        ),
        TestPieces.pointer("f"),
        1
      )
      // Recorded by the build before v2, of the same source and application.
      val v1Rewrite = "v1:f5dd4d46732897e2e7253e2142d1ac41ffcf66d8cca45a7b6ebd24bf19698299"
      val (r1, r2) =
        (MovesJournal.rewrite(source, "r1", applied), MovesJournal.rewrite(source, "r2", applied))
      (
        MovesJournal.sameRewrite(v1Rewrite, source, "r1", applied, None),
        r1.take(3),
        r1 == r2,
        MovesJournal.sameRewrite(r1, source, "r1", applied, None),
        MovesJournal.sameRewrite(r1, source, "r2", applied, None)
      ) ==> (true, "v2:", false, true, false)
    }

    test(
      "a retry digest is v3, of its draft and what fired; a first rewrite's digest is no retry's, nor a retry's a first's"
    ) {
      val source = DocumentVersionId.of(7).getOrElse(sys.error("no version"))
      val applied = PolicyApplication(
        DeclassificationPolicy(
          DeclassificationPolicy.Over.Compartment(TestLabels.compartment("team")),
          1
        ),
        TestPieces.pointer("f"),
        1
      )
      val shape = Screen.Shape(2, Vector(Must.Present))
      def again(draft: String, answers: Vector[Double]): Declassifying.Again =
        (for {
          text <- DocText.of(draft).left.map(_.toString)
          screened <- Screen.read(shape, answers.map(Judged.YesNo(_))).left.map(_.said)
          a <- Declassifying.Again.of(text, "m", screened.fired(None)).toRight("no retry")
        } yield a).fold(e => throw new java.lang.AssertionError(e), identity)
      val floor = again("The draft.", Vector(0.5, 0.0, 0.9))
      val digest = MovesJournal.retry(source, "r1", applied, floor)
      val first = MovesJournal.rewrite(source, "r1", applied)
      (
        digest.take(3),
        digest == MovesJournal.retry(
          source,
          "r1",
          applied,
          again("Another.", Vector(0.5, 0.0, 0.9))
        ),
        digest == MovesJournal.retry(
          source,
          "r1",
          applied,
          again("The draft.", Vector(0.0, 0.0, 0.2))
        ),
        MovesJournal.sameRewrite(digest, source, "r1", applied, Some(floor)),
        MovesJournal.sameRewrite(first, source, "r1", applied, Some(floor)),
        MovesJournal.sameRewrite(digest, source, "r1", applied, None)
      ) ==> ("v3:", false, false, true, false, false)
    }

    test(
      "a screening digest is v3, of its kind, revision, attempt and rewrite; a v1 or v2 check digest compares unequal"
    ) {
      val source = DocumentVersionId.of(7).getOrElse(sys.error("no version"))
      val applied = PolicyApplication(
        DeclassificationPolicy(
          DeclassificationPolicy.Over.Compartment(TestLabels.compartment("team")),
          1
        ),
        TestPieces.pointer("f"),
        1
      )
      def text(s: String) = DocText.of(s).getOrElse(sys.error("no text"))
      val rewrite = text("The team meets offsite.")
      val first = Declassified.Attempt.First
      val check = MovesJournal.check(source, "r1", applied, rewrite, first)
      // Recorded by the builds before the screen, of the same source, application and rewrite.
      val v1Check = "v1:1b496f70387cf51430faee0be1acb208efdda5992a0a38627609faf9bd1489a5"
      val v2Check = "v2:" + check.drop(3)
      val others = Vector(
        MovesJournal.check(source, "r2", applied, rewrite, first),
        MovesJournal.check(source, "r1", applied, rewrite, Declassified.Attempt.Second),
        MovesJournal.check(source, "r1", applied, text("The team meets."), first),
        MovesJournal.baseline(source, "r1", applied)
      )
      (
        check.take(3),
        MovesJournal.baseline(source, "r1", applied).take(3),
        MovesJournal.sameScreening(check, check),
        others.filter(MovesJournal.sameScreening(_, check)),
        MovesJournal.sameScreening(v1Check, check),
        MovesJournal.sameScreening(v2Check, check)
      ) ==> ("v3:", "v3:", true, Vector(), false, false)
    }

    test(
      "a screening's step is written with its shape, floor size and each signal's must, beside its answers, and reads back as written; one that read nothing has no shape"
    ) {
      val call = AskMade.Call("jev", Usage.Zero, Tokens(250))
      val judged = AskMade.Judged("v3:ab", """[{"yes":0.1},{"yes":0.2}]""", Label.Public, call)
      val shape = Screen.Shape(2, Vector(Must.Absent, Must.Present))
      pinned[(AskMade, Option[Screen.Shape])](
        (judged, Some(shape)),
        s"""{"screen":${MovesJournal.askMade.encode(
            judged
          )},"shape":{"floor":2,"musts":["absent","present"]}}"""
      )
      pinned[(AskMade, Option[Screen.Shape])](
        (AskMade.Refused(AskMade.Kind.Unchecked, "no checks", "v3:ab"), None),
        """{"screen":{"refused":"unchecked","why":"no checks","digest":"v3:ab"}}"""
      )
    }

    test(
      "a rewrite's step is written with what it read of its source, its version and its file's revision among it, and reads back as written"
    ) {
      val j = summon[Journaled[(AskMade, Option[Act.Sourced])]]
      val from = Declassified.Source(
        DocumentVersionId.of(7).getOrElse(sys.error("no version")),
        PluginName.of("cards").getOrElse(sys.error("no plugin")),
        DocKey.of("k").getOrElse(sys.error("no key")),
        Place.Everywhere,
        Label.Public,
        TestPieces.pointer("f"),
        "r1"
      )
      val step = (
        AskMade.Refused(AskMade.Kind.Model, "down", "v2:ab"),
        Some(Act.Sourced(from, Integrity.Member))
      )
      val written = j.encode(step)
      val source = ujson.read(written)("source").obj
      (source.keys.toVector, source("version"), source("revision"), j.decode(written)) ==> (
        Vector("version", "plugin", "key", "place", "label", "pointer", "revision", "level"),
        ujson.Num(7),
        ujson.Str("r1"),
        Right(step)
      )
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
      "a recorded digest is compared under its own version and kind, and one of no known version or another kind differs"
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
        ),
        MovesJournal.sameJson(
          MovesJournal.json("Say hello.", request.messages, schema("a", "b")),
          "Say hello.",
          request.messages,
          schema("a", "b")
        ),
        MovesJournal.sameJson(
          MovesJournal.ask(request),
          "Say hello.",
          request.messages,
          schema("a")
        ),
        MovesJournal.sameJudgment(
          MovesJournal.judgment(judged(ujson.Obj("a" -> 1))),
          judged(ujson.Obj("a" -> 1))
        ),
        MovesJournal.sameJudgment(
          MovesJournal.judgment(judged(ujson.Obj("a" -> 1))),
          judged(ujson.Obj("a" -> 2))
        )
      ) ==> (true, false, true, false, true, false, true, false)
    }
  }
}
