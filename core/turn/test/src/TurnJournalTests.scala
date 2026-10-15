package grit.turn

import grit.core.allot.{Allotment, Headroom}
import grit.core.context.{AssemblyNote, Window}
import grit.core.durable.Journaled
import grit.core.id.{ConversationId, DocumentVersion, EntryId, EntrySeq, ToolCallId, TurnSeq}
import grit.core.message.{Message, Tokens, Usage}
import grit.core.place.Place
import grit.core.provider.ModelRequest
import grit.core.store.Nearby
import grit.models.StubProvider

import utest.*

object TurnJournalTests extends TestSuite {

  private def roundTrip[A](a: A)(using j: Journaled[A]): Either[String, A] =
    j.decode(j.encode(a))

  val tests = Tests {
    import TurnJournal.given

    test("outputs and failures read back as written") {
      val window: Either[TurnFailure, Window] = Right(Window(Vector(EntrySeq(0), EntrySeq(1))))
      roundTrip(window) ==> Right(window)
      val reply: Either[TurnFailure, Message.Assistant] =
        new StubProvider()
          .complete(ModelRequest("s", Vector(Message.User("hi"))))
          .left
          .map(_ => TurnFailure.Model("unreachable"))
      roundTrip(reply) ==> Right(reply)
      for (
        failure <- Seq(
          TurnFailure.Assembly("a"),
          TurnFailure.Model("m"),
          TurnFailure.Store("s"),
          TurnFailure.ToolsChanged("t")
        )
      ) {
        val id: Either[TurnFailure, EntryId] = Left(failure)
        roundTrip(id) ==> Right(id)
      }
    }

    test(
      "a route reads back as written, a role with no pin cleared absent, and a turn uncleared as its label"
    ) {
      import grit.core.visibility.{Label, Level}
      val routed: Either[TurnFailure, Route.Recorded] =
        Right(
          Route.Recorded(
            Label.at(Level.Internal),
            1,
            None,
            Some(0),
            Headroom.Ample,
            Route.Task.Skipped,
            Allotment.Untouched,
            None
          )
        )
      roundTrip(routed) ==> Right(routed)
      routing.encode(routed) ==> """{"ok":{"classification":"internal","turn":1,"query":0}}"""
      val uncleared: Either[TurnFailure, Route.Recorded] =
        Left(TurnFailure.Uncleared(Label.at(Level.Restricted)))
      (roundTrip(uncleared), routing.encode(uncleared)) ==> (
        Right(uncleared),
        """{"failed":"uncleared","reason":"restricted"}"""
      )
    }

    test(
      "a route recorded among its profile's pins reads back in that profile as routed, and a place it lacks is named"
    ) {
      import grit.core.visibility.{Label, Level}
      val pin = TurnFixtures.TestCatalog.pin
      val route = Route(
        Label.at(Level.Internal),
        pin.turn.first,
        None,
        Some(pin.query.first),
        Headroom.Ample,
        Route.Task.Skipped,
        Allotment.Untouched,
        None
      )
      Route.recorded(route, pin).flatMap(_.in(pin)) ==> Right(route)
      Route.recorded(route, pin).map(_.copy(turn = 3)).flatMap(_.in(pin)) ==>
        Left("the profile has no turn pin at 3")
    }

    test(
      "a route reads back its headroom, its task asked, unanswered or skipped, its allotment and its baseline; one recorded before them reads as skipped, ample, nothing laid over, no baseline"
    ) {
      import grit.core.allot.{Features, TaskKind, Tasking}
      import grit.core.model.{Effort, Grade}
      import grit.core.period.Probability
      import grit.core.triage.Weighing
      import grit.core.visibility.{Label, Level}
      val features = Features
        .of(
          Map(
            TaskKind.Investigation -> Probability.clamped(0.7),
            TaskKind.Chat -> Probability.clamped(0.1)
          ),
          Probability.clamped(0.72)
        )
        .getOrElse(throw new java.lang.AssertionError())
      val usage = Usage(Tokens(556), Tokens(0), Tokens.Zero, Some(BigDecimal("0.00002335")))
      val asked = Tasking.Asked(features, strand = true, "jev-1.13.0", usage, Tokens(560))
      val base =
        Route.Recorded(
          Label.at(Level.Internal),
          0,
          Some(0),
          Some(0),
          Headroom.Ample,
          Route.Task.Skipped,
          Allotment.Untouched,
          None
        )
      val thrice = Allotment.Calls.Scaled.of(3).getOrElse(throw new java.lang.AssertionError())
      val routes: Vector[Either[TurnFailure, Route.Recorded]] = Vector(
        base.copy(
          task = Route.Task.Read(asked),
          allotment = Allotment(Some(Grade.Strong), Allotment.Reasoning.At(Effort.High), thrice)
        ),
        base.copy(
          headroom = Headroom.Low,
          task = Route.Task.Unanswered(Weighing.Unweighed.PlacementLate),
          allotment =
            Allotment(None, Allotment.Reasoning.AtMost(Effort.Medium), Allotment.Calls.Fewest)
        ),
        base.copy(baseline = Some(Grade.Strong)),
        base
      ).map(Right(_))
      routes.map(roundTrip(_)) ==> routes.map(Right(_))
      // As every route recorded before tasks were asked was written.
      val before = ujson.Obj(
        "ok" -> ujson.Obj(
          "classification" -> Label.written(base.classification),
          "turn" -> base.turn
        )
      )
      routing
        .decode(before.render())
        .map(_.map(r => (r.headroom, r.task, r.allotment, r.baseline))) ==>
        Right(Right((Headroom.Ample, Route.Task.Skipped, Allotment.Untouched, None)))
    }

    test("a query's note keeps who served it, and one recorded before reads with none") {
      val note = TurnFixtures.queried.copy(upstream = Some("Fireworks"))
      val noted: Either[TurnFailure, Window] = Right(Window(Vector(EntrySeq(0)), Vector(note)))
      roundTrip(noted) ==> Right(noted)
      val before: Either[TurnFailure, Window] =
        Right(Window(Vector(EntrySeq(0)), Vector(TurnFixtures.queried)))
      // As every query note was recorded before its upstream was.
      window.decode(
        """{"ok":{"entries":[0],"notes":[{"queried":"hello greeting","model":"writer",""" +
          """"usage":{"input":30,"output":4,"cachedInput":0,"costUsd":"0.00001"},"estimate":28}]}}"""
      ) ==> Right(before)
    }

    test("a window's notes read back, and one without notes keeps the bare array") {
      val noted: Either[TurnFailure, Window] = Right(
        Window(
          Vector(EntrySeq(0)),
          Vector(
            AssemblyNote.Queried(
              "postgres sqlite",
              "m",
              Usage(Tokens(3), Tokens(2), Tokens.Zero, Some(BigDecimal("0.00001"))),
              Tokens(4)
            ),
            AssemblyNote.FellBack("why"),
            AssemblyNote.Recalled(Vector(TurnSeq(0), TurnSeq(4)))
          )
        )
      )
      roundTrip(noted) ==> Right(noted)
      val j = summon[Journaled[Either[TurnFailure, Window]]]
      j.encode(Right(Window(Vector(EntrySeq(0))))) ==> """{"ok":[0]}"""
      j.encode(Right(Window(Vector(EntrySeq(0)), Vector(AssemblyNote.FellBack("why"))))) ==>
        """{"ok":{"entries":[0],"notes":[{"fellBack":"why"}]}}"""
      j.encode(
        Right(Window(Vector(EntrySeq(0)), Vector(AssemblyNote.Recalled(Vector(TurnSeq(2))))))
      ) ==>
        """{"ok":{"entries":[0],"notes":[{"recalled":[2]}]}}"""
      assert(j.decode("""{"ok":{"entries":[0],"notes":[{"lunch":1}]}}""").isLeft)
      assert(j.decode("""{"ok":{"entries":[0],"notes":[{"recalled":[-1]}]}}""").isLeft)
    }

    test("a window's nearby sections are recorded after its notes, and read back") {
      val api = Place.read("fs:/home/nick/api").fold(e => sys.error(e), identity)
      val near: Either[TurnFailure, Window] = Right(
        Window(
          Vector(EntrySeq(0)),
          Vector.empty,
          Vector(Nearby.Open(ConversationId("c9"), api, Vector(EntrySeq(5))))
        )
      )
      val j = summon[Journaled[Either[TurnFailure, Window]]]
      j.encode(near) ==>
        """{"ok":{"entries":[0],"notes":[],""" +
        """"nearby":[{"conversation":"c9","place":"fs:/home/nick/api","entries":[5]}]}}"""
      roundTrip(near) ==> Right(near)
    }

    test("a window's documents are recorded by version after its nearby sections, and read back") {
      val versions = Vector(7L, 3L).flatMap(DocumentVersion.of)
      val j = summon[Journaled[Either[TurnFailure, Window]]]
      val documented: Either[TurnFailure, Window] =
        Right(Window(Vector(EntrySeq(0)), Vector.empty, Vector.empty, versions))
      j.encode(documented) ==> """{"ok":{"entries":[0],"notes":[],"documents":[7,3]}}"""
      roundTrip(documented) ==> Right(documented)
      assert(j.decode("""{"ok":{"entries":[0],"notes":[],"documents":[0]}}""").isLeft)
    }

    test(
      "a window with no documents is recorded as before, a bare array or an object without documents"
    ) {
      // Recorded outputs: every window recorded before documents replays as written.
      val api = Place.read("fs:/home/nick/api").fold(e => sys.error(e), identity)
      val j = summon[Journaled[Either[TurnFailure, Window]]]
      j.encode(Right(Window(Vector(EntrySeq(0))))) ==> """{"ok":[0]}"""
      j.encode(
        Right(
          Window(
            Vector(EntrySeq(0)),
            Vector(AssemblyNote.FellBack("why")),
            Vector(Nearby.Open(ConversationId("c9"), api, Vector(EntrySeq(5))))
          )
        )
      ) ==>
        """{"ok":{"entries":[0],"notes":[{"fellBack":"why"}],""" +
        """"nearby":[{"conversation":"c9","place":"fs:/home/nick/api","entries":[5]}]}}"""
      j.decode("""{"ok":{"entries":[0],"notes":[{"fellBack":"why"}]}}""") ==>
        Right(Right(Window(Vector(EntrySeq(0)), Vector(AssemblyNote.FellBack("why")))))
    }

    test("a classification reads back as written") {
      import grit.core.topic.{Placement, TopicEvent, TopicId, Weights}
      val (a, b) = (TopicId("a"), TopicId("b"))
      val choice = Vector(Placement.Chance(None, 0.9), Placement.Chance(Some(a), 0.1))
      val full = TurnTopics.Classification(
        Vector(
          TopicEvent.Opened(b),
          TopicEvent.Placed(
            TurnSeq(3),
            Weights.changed(a, 0.1, choice, Some(b)),
            Placement.Classified(0.1, Placement.Outcome.Changed(choice))
          )
        ),
        Some(TurnTopics.Shown(a, "Knots")),
        Vector(TurnTopics.Shown(b, "new topic")),
        Some(
          (
            "jev-1",
            Usage(Tokens(40), Tokens.Zero, Tokens.Zero, Some(BigDecimal("0.00001"))),
            Tokens(35)
          )
        ),
        None
      )
      val empty =
        TurnTopics.Classification(Vector.empty, None, Vector.empty, None, Some("store: down"))
      roundTrip(full) ==> Right(full)
      roundTrip(empty) ==> Right(empty)
      assert(summon[Journaled[TurnTopics.Classification]].decode("""{"events":[]}""").isLeft)
    }

    test("a round's record keeps its reply and each of its calls' kinds, in its pinned form") {
      import TurnTools.Kind
      val j = summon[Journaled[Either[TurnFailure, TurnRound.Kept]]]
      val post = Place.read("service:github").fold(e => sys.error(e), identity)
      val kept: Either[TurnFailure, TurnRound.Kept] = Right(
        TurnRound.Kept(
          EntryId("call:c1:0:0"),
          Vector(
            Kind.Refused,
            Kind.Free,
            Kind.Gated,
            Kind.Hosted(grit.core.tool.ToolName("post"), true, Some(post)),
            Kind.Hosted(grit.core.tool.ToolName("fetch"), false, None)
          )
        )
      )
      j.encode(kept) ==> """{"ok":{"reply":"call:c1:0:0","kinds":["refused","free","gated",""" +
        """{"hosted":"post","asks":true,"place":"service:github"},""" +
        """{"hosted":"fetch","asks":false,"place":null}]}}"""
      j.decode(j.encode(kept)) ==> Right(kept)
    }

    test("the recorded form is pinned") {
      val j = summon[Journaled[Either[TurnFailure, EntryId]]]
      j.encode(Right(EntryId("reply:c1:0"))) ==> """{"ok":"reply:c1:0"}"""
      j.encode(Left(TurnFailure.Model("down"))) ==> """{"failed":"model","reason":"down"}"""
    }

    test(
      "an offer's root: heard, named and by-name are written, and an offer recorded before roots reads as addressed"
    ) {
      val j = summon[Journaled[Either[TurnFailure, TurnOffer.Recorded]]]
      val tools = grit.core.tool.ToolSet.Empty.id
      val heard: Either[TurnFailure, TurnOffer.Recorded] =
        Right(TurnOffer.Recorded(None, tools, Vector.empty, TurnOffer.Root.Heard))
      val set = grit.core.tool.ToolSetId.value(tools)
      j.encode(heard) ==> s"""{"ok":{"workspace":null,"tools":"$set","prompt":[],"root":"heard"}}"""
      j.decode(j.encode(heard)) ==> Right(heard)
      // A pin of the recorded form: a named root replays as named.
      val named: Either[TurnFailure, TurnOffer.Recorded] =
        Right(TurnOffer.Recorded(None, tools, Vector.empty, TurnOffer.Root.Named))
      j.encode(named) ==> s"""{"ok":{"workspace":null,"tools":"$set","prompt":[],"root":"named"}}"""
      j.decode(j.encode(named)) ==> Right(named)
      val byName: Either[TurnFailure, TurnOffer.Recorded] =
        Right(TurnOffer.Recorded(None, tools, Vector.empty, TurnOffer.Root.ByName))
      j.encode(byName) ==>
        s"""{"ok":{"workspace":null,"tools":"$set","prompt":[],"root":"by-name"}}"""
      j.decode(j.encode(byName)) ==> Right(byName)
      // Every offer recorded before this field was an addressed turn's.
      j.decode(s"""{"ok":{"workspace":null,"tools":"$set","prompt":[]}}""") ==>
        Right(Right(TurnOffer.Recorded(None, tools, Vector.empty, TurnOffer.Root.Addressed)))
    }

    test(
      "an offer's advertised tools are written by name, and an offer recorded before them reads as none"
    ) {
      // Pinned: replay reads this key to rebuild the tools a turn took from an edge's advert.
      val j = summon[Journaled[Either[TurnFailure, TurnOffer.Recorded]]]
      val tools = grit.core.tool.ToolSet.Empty.id
      val set = grit.core.tool.ToolSetId.value(tools)
      val workspace = grit.core.place.Place.read("service:github").toOption
      val took: Either[TurnFailure, TurnOffer.Recorded] = Right(
        TurnOffer.Recorded(
          workspace,
          tools,
          Vector.empty,
          TurnOffer.Root.Addressed,
          Vector(grit.core.tool.ToolName("github_search"))
        )
      )
      j.encode(took) ==>
        s"""{"ok":{"workspace":"service:github","tools":"$set","prompt":[],"advertised":["github_search"]}}"""
      j.decode(j.encode(took)) ==> Right(took)
      j.decode(s"""{"ok":{"workspace":null,"tools":"$set","prompt":[]}}""")
        .map(_.map(_.advertised)) ==>
        Right(Right(Vector.empty))
    }

    test(
      "an offer's reached tools are written with their places, and an offer recorded before them reads as none"
    ) {
      // Pinned: replay reads this key to address each reached tool's calls to its place.
      val j = summon[Journaled[Either[TurnFailure, TurnOffer.Recorded]]]
      val tools = grit.core.tool.ToolSet.Empty.id
      val set = grit.core.tool.ToolSetId.value(tools)
      val workspace = grit.core.place.Place.read("service:github").toOption
      val elsewhere = grit.core.place.Place
        .read("service:elsewhere")
        .fold(e => throw new java.lang.AssertionError(e), identity)
      val took: Either[TurnFailure, TurnOffer.Recorded] = Right(
        TurnOffer.Recorded(
          workspace,
          tools,
          Vector.empty,
          TurnOffer.Root.Addressed,
          reached = Map(grit.core.tool.ToolName("post_x") -> elsewhere)
        )
      )
      j.encode(took) ==>
        s"""{"ok":{"workspace":"service:github","tools":"$set","prompt":[],"reached":{"post_x":"service:elsewhere"}}}"""
      j.decode(j.encode(took)) ==> Right(took)
      j.decode(s"""{"ok":{"workspace":null,"tools":"$set","prompt":[]}}""")
        .map(_.map(_.reached)) ==>
        Right(Right(Map.empty))
    }

    test(
      "an offer's shape is written with its width, whole set and services' verdicts, and an offer recorded before shapes reads as unshaped"
    ) {
      // Pinned: replay reads the width back for the turn's window, and the harness each verdict.
      val j = summon[Journaled[Either[TurnFailure, TurnOffer.Recorded]]]
      val tools = grit.core.tool.ToolSet.Empty.id
      val set = grit.core.tool.ToolSetId.value(tools)
      def service(name: String) =
        grit.core.place.Service.of(name).fold(e => throw new java.lang.AssertionError(e), identity)
      val repo = grit.core.id.CorpusName
        .of("repo")
        .fold(e => throw new java.lang.AssertionError(e), identity)
      val read = grit.core.triage.Reading.Yes(
        grit.core.id.QuestionName.per(grit.core.triage.Tags.V2.sourcePrefix, repo)
      )
      val p = grit.core.period.Probability.clamped
      val failed = grit.core.triage.Gate.Checked.Fails(
        grit.core.triage.Gate.Failed(grit.core.triage.Bound.AtLeast(read, p(0.25)), p(0.125)),
        Vector.empty
      )
      import grit.core.recipe.ServiceOffer
      val shaped: Either[TurnFailure, TurnOffer.Recorded] = Right(
        TurnOffer.Recorded(
          None,
          tools,
          Vector.empty,
          TurnOffer.Root.Heard,
          shaped = Some(
            TurnShape(
              grit.core.context.Width.Within(Tokens(9000), 4),
              tools,
              Vector(
                TurnShape.Took(
                  ServiceOffer(
                    service("github"),
                    Vector(repo),
                    ServiceOffer.Verdict.Checked(failed)
                  ),
                  TurnShape.Via.Workspace,
                  Vector(grit.core.tool.ToolName("github_search"))
                ),
                TurnShape.Took(
                  ServiceOffer(service("elsewhere"), Vector.empty, ServiceOffer.Verdict.Ungated),
                  TurnShape.Via.Reached,
                  Vector.empty
                ),
                TurnShape.Took(
                  ServiceOffer(service("docs"), Vector(repo), ServiceOffer.Verdict.Unweighed),
                  TurnShape.Via.Reached,
                  Vector.empty
                )
              )
            )
          )
        )
      )
      j.encode(shaped) ==>
        s"""{"ok":{"workspace":null,"tools":"$set","prompt":[],"root":"heard","shaped":{""" +
        s""""width":{"budget":9000,"hits":4},"whole":"$set","services":[""" +
        """{"service":"github","via":"workspace","tools":["github_search"],"sources":["repo"],"verdict":{"checked":""" +
        """{"fails":[{"reading":{"reads":"yes","name":"source:repo"},"bound":"at_least","p":0.25,"read":0.125}]}}},""" +
        """{"service":"elsewhere","via":"reached","tools":[],"sources":[],"verdict":"ungated"},""" +
        """{"service":"docs","via":"reached","tools":[],"sources":["repo"],"verdict":"unweighed"}]}}}"""
      j.decode(j.encode(shaped)) ==> Right(shaped)
      val deployed = shaped.map(r =>
        r.copy(shaped =
          r.shaped.map(_.copy(width = grit.core.context.Width.Deployed, services = Vector.empty))
        )
      )
      j.encode(deployed) ==>
        s"""{"ok":{"workspace":null,"tools":"$set","prompt":[],"root":"heard","shaped":{"width":"deployed","whole":"$set","services":[]}}}"""
      j.decode(j.encode(deployed)) ==> Right(deployed)
      j.decode(s"""{"ok":{"workspace":null,"tools":"$set","prompt":[]}}""")
        .map(_.map(_.shaped)) ==>
        Right(Right(None))
    }

    test("a gate is recorded in its pinned form, hidden items and every ref kind among it") {
      // The recorded form of gate:n, which every later build of the epoch must read back.
      val j = summon[Journaled[Either[TurnFailure, Gated]]]
      val pinned =
        """{"ok":{"level":"member","decided":[""" +
          """{"item":{"ref":{"kind":"result","call":"c1"},"integrity":"untrusted","source":"github_get_issue"},""" +
          """"fate":{"kind":"hidden","name":"h1","costs":["slack_post"]}},""" +
          """{"item":{"ref":{"kind":"own","seq":41},"integrity":"outside","source":"a guest"},""" +
          """"fate":{"kind":"lowered"}},""" +
          """{"item":{"ref":{"kind":"nearby","conversation":"c2","seq":4},"integrity":"outside","source":"Ann"},""" +
          """"fate":{"kind":"lowered"}},""" +
          """{"item":{"ref":{"kind":"document","version":3},"integrity":"authority","source":"notes"},""" +
          """"fate":{"kind":"lowered"}}],""" +
          """"withheld":[{"tool":"slack_post","requires":"member"}]}}"""
      val three =
        DocumentVersion.of(3).getOrElse(throw new java.lang.AssertionError("3 is a version"))
      val gated = j.decode(pinned)
      gated.map(_.map(_.decided.map(_.item.ref))) ==> Right(
        Right(
          Vector(
            ItemRef.Result(ToolCallId("c1")),
            ItemRef.Own(EntrySeq(41)),
            ItemRef.Nearby(ConversationId("c2"), EntrySeq(4)),
            ItemRef.Document(three)
          )
        )
      )
      gated.map(j.encode) ==> Right(pinned)
      assert(j.decode(pinned.replace("\"kind\":\"own\"", "\"kind\":\"mine\"")).isLeft)
      assert(j.decode(pinned.replace("\"name\":\"h1\"", "\"name\":\"h0\"")).isLeft)
    }

    test("a record in neither shape is rejected") {
      val j = summon[Journaled[Either[TurnFailure, EntryId]]]
      assert(j.decode("""{"ok":"x","failed":"model","reason":"r"}""").isLeft)
      assert(j.decode("""{"failed":"lunch","reason":"r"}""").isLeft)
      assert(j.decode("""[1]""").isLeft)
    }
  }
}
