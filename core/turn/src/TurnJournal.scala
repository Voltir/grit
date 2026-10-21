package grit.turn

import grit.core.agenda.{Agenda, AgendaJson}
import grit.core.allot.{Allotment, Features, Headroom, TaskKind, Tasking}
import grit.core.context.{AssemblyNote, Querying, Results, Retrieved, Width, Window}
import grit.core.durable.Journaled
import grit.core.edge.{OutcomeJson, RequestState}
import grit.core.either.Traversal.sequence
import grit.core.id.{
  ConversationId,
  CorpusName,
  DocumentVersionId,
  EntryId,
  EntrySeq,
  ToolCallId,
  TurnSeq
}
import grit.core.message.{Message, Tokens}
import grit.core.model.{CatalogJson, Grade, TurnProfileId}
import grit.core.period.Probability
import grit.core.place.{Place, Service}
import grit.core.prompt.FragmentId
import grit.core.recipe.ServiceOffer
import grit.core.speech.{Outcome, SpeechJson}
import grit.core.store.{Nearby, Payload, PayloadJson}
import grit.core.tool.{ToolName, ToolSetId}
import grit.core.triage.{GateJson, Tags, TagsJson, Weighing}
import grit.core.visibility.{Integrity, Label}

/** How the turn's step outputs are recorded: `{"ok": value}` or
  * `{"failed": kind, "reason": text}`, an uncleared turn's reason its classification's
  * written label. In-flight turns must read back what an earlier build wrote: an encoding
  * only grows, a field absent reading as it did before it existed, and any other change is a
  * patch or a new epoch (ADR 0004).
  */
private[turn] object TurnJournal {

  /** A window with no notes, nearby sections or documents is the bare array of its entries'
    * seqs ([[PayloadJson.writeSeqs]]); one with any is `{"entries": [...], "notes": [...]}`,
    * with `"nearby": [...]` too when it has sections ([[PayloadJson.writeNearby]]), and
    * `"documents": [...]` when it has documents ([[PayloadJson.writeVersions]]).
    */
  given window: Journaled[Either[TurnFailure, Window]] =
    outcome(
      w => {
        val ids = PayloadJson.writeSeqs(w.entries)
        if (w.notes.isEmpty && w.nearby.isEmpty && w.documents.isEmpty) ids
        else {
          val o = ujson.Obj("entries" -> ids, "notes" -> ujson.Arr.from(w.notes.map(writeNote)))
          if (w.nearby.nonEmpty) o("nearby") = ujson.Arr.from(w.nearby.map(PayloadJson.writeNearby))
          if (w.documents.nonEmpty) o("documents") = PayloadJson.writeVersions(w.documents)
          o
        }
      },
      v =>
        v match {
          case ujson.Arr(_) => PayloadJson.readSeqs(v).map(Window(_))
          case o: ujson.Obj =>
            for {
              ids <- o.value
                .get("entries")
                .toRight("window: missing entries")
                .flatMap(PayloadJson.readSeqs)
              raw <- o.value.get("notes").flatMap(_.arrOpt).toRight("window: missing notes")
              notes <- raw.toVector
                .foldLeft[Either[String, Vector[AssemblyNote]]](Right(Vector.empty)) { (acc, n) =>
                  acc.flatMap(ns => readNote(n).map(ns :+ _))
                }
              nearby <- o.value.get("nearby") match {
                case None => Right(Vector.empty)
                case Some(ujson.Arr(items)) =>
                  items.toVector.foldLeft[Either[String, Vector[Nearby]]](Right(Vector.empty)) {
                    (acc, n) => acc.flatMap(ns => PayloadJson.readNearby(n).map(ns :+ _))
                  }
                case Some(_) => Left("window: nearby is not an array")
              }
              documents <- o.value
                .get("documents")
                .fold(Right(Vector.empty))(PayloadJson.readVersions)
            } yield Window(ids, notes, nearby, documents)
          case _ => Left("window: expected an array or an object")
        }
    )

  private def writeNote(n: AssemblyNote): ujson.Value = n match {
    case AssemblyNote.Queried(query, model, usage, estimate, upstream) =>
      val o = ujson.Obj(
        "queried" -> query,
        "model" -> model,
        "usage" -> PayloadJson.writeUsage(usage),
        "estimate" -> Tokens.value(estimate).toDouble
      )
      upstream.foreach(u => o("upstream") = u)
      o
    case AssemblyNote.FellBack(reason) => ujson.Obj("fellBack" -> reason)
    case AssemblyNote.Recalled(turns) =>
      ujson.Obj("recalled" -> ujson.Arr.from(turns.map(t => ujson.Num(TurnSeq.value(t).toDouble))))
  }

  private def readNote(v: ujson.Value): Either[String, AssemblyNote] = v match {
    case o: ujson.Obj =>
      (o.value.get("queried"), o.value.get("fellBack"), o.value.get("recalled")) match {
        case (Some(ujson.Str(query)), None, None) =>
          for {
            model <- o.value.get("model").flatMap(_.strOpt).toRight("note: missing model")
            usage <- o.value
              .get("usage")
              .toRight("note: missing usage")
              .flatMap(PayloadJson.readUsage)
            estimate <- o.value
              .get("estimate")
              .collect { case ujson.Num(n) if n.isWhole && n >= 0 => Tokens(n.toLong) }
              .toRight("note: bad estimate")
            upstream = o.value.get("upstream").flatMap(_.strOpt)
          } yield AssemblyNote.Queried(query, model, usage, estimate, upstream)
        case (None, Some(ujson.Str(reason)), None) => Right(AssemblyNote.FellBack(reason))
        case (None, None, Some(ujson.Arr(turns))) =>
          val seqs = turns.toVector.collect {
            case ujson.Num(n) if n.isWhole && n >= 0 => TurnSeq(n.toLong)
          }
          if (seqs.size == turns.size) Right(AssemblyNote.Recalled(seqs))
          else Left("note: a recalled turn is not a non-negative whole number")
        case _ => Left("note: expected queried, fellBack or recalled")
      }
    case _ => Left("note: expected an object")
  }

  /** The `classify` step's output: `{"events": [...], "current": {id, key} | null,
    * "earlier": [{id, key}], "cost": {model, usage, estimate} | null, "note": text | null}`.
    */
  given classification: Journaled[TurnAgenda.Classification] =
    Journaled.json[TurnAgenda.Classification](
      c =>
        ujson.Obj(
          "events" -> ujson.Arr.from(c.events.map(AgendaJson.write)),
          "current" -> c.current.fold[ujson.Value](ujson.Null)(writeShown),
          "earlier" -> ujson.Arr.from(c.earlier.map(writeShown)),
          "cost" -> c.cost.fold[ujson.Value](ujson.Null) { (model, usage, estimate) =>
            ujson.Obj(
              "model" -> model,
              "usage" -> PayloadJson.writeUsage(usage),
              "estimate" -> Tokens.value(estimate).toDouble
            )
          },
          "note" -> c.note.fold[ujson.Value](ujson.Null)(ujson.Str(_))
        ),
      v =>
        for {
          o <- v.objOpt.toRight("classification: expected an object")
          events <- o
            .get("events")
            .flatMap(_.arrOpt)
            .toRight("classification: missing events")
            .flatMap(es => sequence(es.toVector.map(AgendaJson.read)))
          current <- o.get("current") match {
            case None | Some(ujson.Null) => Right(None)
            case Some(s) => readShown(s).map(Some(_))
          }
          earlier <- o
            .get("earlier")
            .flatMap(_.arrOpt)
            .toRight("classification: missing earlier")
            .flatMap(es => sequence(es.toVector.map(readShown)))
          cost <- o.get("cost") match {
            case None | Some(ujson.Null) => Right(None)
            case Some(c: ujson.Obj) =>
              for {
                model <- c.value.get("model").flatMap(_.strOpt).toRight("cost: missing model")
                usage <- c.value
                  .get("usage")
                  .toRight("cost: missing usage")
                  .flatMap(PayloadJson.readUsage)
                estimate <- c.value
                  .get("estimate")
                  .collect { case ujson.Num(n) if n.isWhole && n >= 0 => Tokens(n.toLong) }
                  .toRight("cost: bad estimate")
              } yield Some((model, usage, estimate))
            case Some(_) => Left("classification: cost is not an object")
          }
          note <- o.get("note") match {
            case None | Some(ujson.Null) => Right(None)
            case Some(ujson.Str(n)) => Right(Some(n))
            case Some(_) => Left("classification: note is not a string")
          }
        } yield TurnAgenda.Classification(events, current, earlier, cost, note)
    )

  private def writeShown(s: TurnAgenda.Shown): ujson.Value =
    ujson.Obj("id" -> Agenda.ItemId.value(s.id), "key" -> s.key)

  private def readShown(v: ujson.Value): Either[String, TurnAgenda.Shown] =
    (
      v.objOpt.flatMap(_.get("id")).flatMap(_.strOpt),
      v.objOpt.flatMap(_.get("key")).flatMap(_.strOpt)
    ) match {
      case (Some(id), Some(key)) => Right(TurnAgenda.Shown(Agenda.ItemId(id), key))
      case _ => Left("a topic shown: expected {id, key}")
    }

  given reply: Journaled[Either[TurnFailure, Message.Assistant]] =
    outcome(
      m => PayloadJson.write(Payload.Message(m)),
      v =>
        PayloadJson.read(v).flatMap {
          case Payload.Message(m: Message.Assistant) => Right(m)
          case _ => Left("reply: expected an assistant message")
        }
    )

  given entryId: Journaled[Either[TurnFailure, EntryId]] =
    outcome(
      id => ujson.Str(EntryId.value(id)),
      v => v.strOpt.map(EntryId(_)).toRight("entry id: expected a string")
    )

  /** An `append` step's output: the reply's entry id alone when nothing was followed, as it
    * was written before follows were recorded; else `{"reply": entry id, "followed": [...]}`
    * ([[PayloadJson.writeVersions]]).
    */
  given appended: Journaled[Either[TurnFailure, TurnSteps.Appended]] =
    outcome(
      a =>
        if (a.followed.isEmpty) ujson.Str(EntryId.value(a.reply))
        else
          ujson.Obj(
            "reply" -> EntryId.value(a.reply),
            "followed" -> PayloadJson.writeVersions(a.followed)
          ),
      v =>
        v match {
          case ujson.Str(id) => Right(TurnSteps.Appended(EntryId(id), Vector.empty))
          case o: ujson.Obj =>
            for {
              reply <- o.value.get("reply").flatMap(_.strOpt).toRight("appended: no reply")
              followed <- o.value
                .get("followed")
                .toRight("appended: no followed")
                .flatMap(PayloadJson.readVersions)
            } yield TurnSteps.Appended(EntryId(reply), followed)
          case _ => Left("appended: expected an entry id or an object")
        }
    )

  /** A `ask:n:record` step's output: `{"reply": entry id, "kinds": [kind, …]}`, each call's
    * kind in order: `"refused"`, `"free"`, `"gated"`, or `{"hosted": tool, "asks": boolean,
    * "place": place written | null}`.
    */
  given kept: Journaled[Either[TurnFailure, TurnRound.Kept]] =
    outcome(
      k =>
        ujson.Obj(
          "reply" -> EntryId.value(k.reply),
          "kinds" -> ujson.Arr.from(k.kinds.map(writeKind))
        ),
      v =>
        for {
          o <- v.objOpt.toRight("kept: expected an object")
          reply <- o.get("reply").flatMap(_.strOpt).toRight("kept: no reply").map(EntryId(_))
          kinds <- o
            .get("kinds")
            .flatMap(_.arrOpt)
            .toRight("kept: no kinds")
            .flatMap(ks => sequence(ks.toVector.map(readKind)))
        } yield TurnRound.Kept(reply, kinds)
    )

  private def writeKind(kind: TurnTools.Kind): ujson.Value = kind match {
    case TurnTools.Kind.Refused => "refused"
    case TurnTools.Kind.Free => "free"
    case TurnTools.Kind.Gated => "gated"
    case TurnTools.Kind.Hosted(tool, asks, place) =>
      ujson.Obj(
        "hosted" -> ToolName.value(tool),
        "asks" -> asks,
        "place" -> place.fold[ujson.Value](ujson.Null)(p => ujson.Str(p.written))
      )
  }

  private def readKind(v: ujson.Value): Either[String, TurnTools.Kind] = v match {
    case ujson.Str("refused") => Right(TurnTools.Kind.Refused)
    case ujson.Str("free") => Right(TurnTools.Kind.Free)
    case ujson.Str("gated") => Right(TurnTools.Kind.Gated)
    case ujson.Obj(o) =>
      for {
        name <- o.get("hosted").flatMap(_.strOpt).toRight("kind: no hosted tool")
        tool <- ToolName.of(name).left.map(e => s"kind: $e")
        asks <- o.get("asks").flatMap(_.boolOpt).toRight("kind: no asks")
        place <- o.get("place") match {
          case Some(ujson.Null) => Right(None)
          case Some(p) =>
            p.strOpt.toRight("kind: place is not text").flatMap(Place.read).map(Some(_))
          case None => Left("kind: no place")
        }
      } yield TurnTools.Kind.Hosted(tool, asks, place)
    case other => Left(s"kind: unknown ${other.render()}")
  }

  /** A `record-window` step's output: `{"budget": calls, "answer-within": milliseconds}`. */
  given limits: Journaled[Either[TurnFailure, TurnRound.Limits]] =
    outcome(
      l =>
        ujson.Obj("budget" -> l.budget.calls, "answer-within" -> l.answerWithin.toMillis.toDouble),
      v =>
        for {
          o <- v.objOpt.toRight("limits: expected an object")
          calls <- o.get("budget").flatMap(_.numOpt).toRight("limits: no budget")
          budget <- TurnLoop.Rounds.of(calls.toInt).left.map(e => s"limits: $e")
          within <- o.get("answer-within").flatMap(_.numOpt).toRight("limits: no answer-within")
        } yield TurnRound.Limits(
          budget,
          scala.concurrent.duration.FiniteDuration(within.toLong, "ms")
        )
    )

  /** A `call:n:j` step's output: `{"result": entry id, "failed": boolean}`. */
  given settled: Journaled[Either[TurnFailure, TurnTools.Settled]] =
    outcome(
      s => ujson.Obj("result" -> EntryId.value(s.result), "failed" -> s.failed),
      v =>
        (
          v.objOpt.flatMap(_.get("result")).flatMap(_.strOpt),
          v.objOpt.flatMap(_.get("failed")).flatMap(_.boolOpt)
        ) match {
          case (Some(id), Some(failed)) => Right(TurnTools.Settled(EntryId(id), failed))
          case _ => Left("settled: expected {result, failed}")
        }
    )

  /** A `route` step's output: `{"classification": label written, "turn": place, "summary":
    * place, "query": place}`, each pin by its place among its role's in the turn's profile
    * ([[Route.Recorded]]); `summary` or `query` absent when none of that role's pins is
    * cleared.
    */
  given routing: Journaled[Either[TurnFailure, Route.Recorded]] =
    outcome(
      r => {
        val o = ujson.Obj(
          "classification" -> Label.written(r.classification),
          "turn" -> r.turn
        )
        r.summary.foreach(p => o("summary") = p)
        r.query.foreach(p => o("query") = p)
        // Each written only when it is not what a route recorded before it decided.
        if (r.headroom == Headroom.Low) o("headroom") = "low"
        writeTask(r.task).foreach(t => o("task") = t)
        if (r.allotment != Allotment.Untouched) o("allotment") = writeAllotment(r.allotment)
        r.baseline.foreach(g => o("baseline") = g.word)
        o
      },
      v => {
        def place(p: ujson.Value, key: String): Either[String, Int] =
          p.numOpt
            .filter(n => n >= 0 && n == n.toInt)
            .map(_.toInt)
            .toRight(s"route: $key is no place")
        def optional(o: collection.Map[String, ujson.Value], key: String) =
          o.get(key) match {
            case None => Right(None)
            case Some(p) => place(p, key).map(Some(_))
          }
        for {
          o <- v.objOpt.toRight("route: expected an object")
          classification <- o
            .get("classification")
            .flatMap(_.strOpt)
            .toRight("route: missing classification")
            .flatMap(Label.read)
          turn <- o.get("turn").toRight("route: missing turn").flatMap(place(_, "turn"))
          summary <- optional(o, "summary")
          query <- optional(o, "query")
          headroom <- o.get("headroom") match {
            case None => Right(Headroom.Ample)
            case Some(ujson.Str("low")) => Right(Headroom.Low)
            case Some(_) => Left("route: headroom is not low")
          }
          task <- o.get("task").fold(Right(Route.Task.Skipped))(readTask)
          allotment <- o.get("allotment").fold(Right(Allotment.Untouched))(readAllotment)
          baseline <- o.get("baseline").fold(Right(None)) { v =>
            v.strOpt
              .flatMap(w => Grade.values.find(_.word == w))
              .map(Some(_))
              .toRight("route: baseline names no grade")
          }
        } yield Route.Recorded(
          classification,
          turn,
          summary,
          query,
          headroom,
          task,
          allotment,
          baseline
        )
      }
    )

  /** A route's `task`: `{"asked": "read", "kinds": {kind: p, …}, "hard", "strand", "model",
    * "usage", "estimate"}`, or `{"asked": "unanswered", "why": kind}` by [[unweighedKinds]];
    * nothing when skipped.
    */
  private def writeTask(t: Route.Task): Option[ujson.Value] = t match {
    case Route.Task.Skipped => None
    case Route.Task.Read(a) =>
      Some(
        ujson.Obj(
          "asked" -> "read",
          "kinds" -> ujson.Obj.from(
            a.features.kinds.map((k, p) => k.word -> ujson.Num(Probability.value(p)))
          ),
          "hard" -> Probability.value(a.features.hard),
          "strand" -> a.strand,
          "model" -> a.model,
          "usage" -> PayloadJson.writeUsage(a.usage),
          "estimate" -> Tokens.value(a.estimate).toDouble
        )
      )
    case Route.Task.Unanswered(why) =>
      Some(ujson.Obj("asked" -> "unanswered", "why" -> unweighedKind(why)))
  }

  private def readTask(v: ujson.Value): Either[String, Route.Task] = {
    def probability(n: ujson.Value, at: String) =
      n.numOpt.flatMap(Probability.of).toRight(s"route.task: $at is not a probability")
    for {
      o <- v.objOpt.toRight("route.task: expected an object")
      task <- o.get("asked").flatMap(_.strOpt) match {
        case Some("unanswered") =>
          o.get("why")
            .flatMap(_.strOpt)
            .flatMap(k => unweighedKinds.collectFirst { case (why, `k`) => why })
            .toRight("route.task: why is not a known kind")
            .map(Route.Task.Unanswered(_))
        case Some("read") =>
          for {
            kinds <- o.get("kinds").flatMap(_.objOpt).toRight("route.task: no kinds")
            weights <- kinds.toVector.foldLeft[Either[String, Map[TaskKind, Probability]]](
              Right(Map.empty)
            ) { case (acc, (word, p)) =>
              for {
                done <- acc
                kind <- TaskKind.values
                  .find(_.word == word)
                  .toRight(s"route.task: no kind $word")
                weight <- probability(p, word)
              } yield done + (kind -> weight)
            }
            hard <- o.get("hard").toRight("route.task: no hard").flatMap(probability(_, "hard"))
            features <- Features.of(weights, hard).toRight("route.task: no kind weighs anything")
            strand <- o.get("strand").flatMap(_.boolOpt).toRight("route.task: no strand")
            model <- o.get("model").flatMap(_.strOpt).toRight("route.task: no model")
            usage <- o.get("usage").toRight("route.task: no usage").flatMap(PayloadJson.readUsage)
            estimate <- o
              .get("estimate")
              .collect { case ujson.Num(n) if n.isWhole && n >= 0 => Tokens(n.toLong) }
              .toRight("route.task: no whole estimate")
          } yield Route.Task.Read(Tasking.Asked(features, strand, model, usage, estimate))
        case _ => Left("route.task: asked is neither read nor unanswered")
      }
    } yield task
  }

  /** A route's `allotment`: `{"grade": word | null, "reasoning": {"at": e} | {"at-most": e} |
    * null, "calls": {"times": n} | "fewest" | null}`, each null its untouched value.
    */
  private def writeAllotment(a: Allotment): ujson.Value =
    ujson.Obj(
      "grade" -> a.grade.fold[ujson.Value](ujson.Null)(g => ujson.Str(g.word)),
      "reasoning" -> (a.reasoning match {
        case Allotment.Reasoning.Own => ujson.Null
        case Allotment.Reasoning.At(e) => ujson.Obj("at" -> CatalogJson.effort(e))
        case Allotment.Reasoning.AtMost(e) => ujson.Obj("at-most" -> CatalogJson.effort(e))
      }),
      "calls" -> (a.calls match {
        case Allotment.Calls.Default => ujson.Null
        case Allotment.Calls.Scaled(n) => ujson.Obj("times" -> n)
        case Allotment.Calls.Fewest => ujson.Str("fewest")
      })
    )

  private def readAllotment(v: ujson.Value): Either[String, Allotment] = {
    def effort(n: ujson.Value) = CatalogJson.readEffort(n).left.map(why => s"route.allotment: $why")
    for {
      o <- v.objOpt.toRight("route.allotment: expected an object")
      grade <- o.get("grade") match {
        case None | Some(ujson.Null) => Right(None)
        case Some(g) =>
          g.strOpt
            .flatMap(w => Grade.values.find(_.word == w))
            .toRight("route.allotment: not a grade")
            .map(Some(_))
      }
      reasoning <- o.get("reasoning") match {
        case None | Some(ujson.Null) => Right(Allotment.Reasoning.Own)
        case Some(r: ujson.Obj) if r.value.contains("at") =>
          effort(r.value("at")).map(Allotment.Reasoning.At(_))
        case Some(r: ujson.Obj) if r.value.contains("at-most") =>
          effort(r.value("at-most")).map(Allotment.Reasoning.AtMost(_))
        case Some(_) => Left("route.allotment: reasoning is not at or at-most")
      }
      calls <- o.get("calls") match {
        case None | Some(ujson.Null) => Right(Allotment.Calls.Default)
        case Some(ujson.Str("fewest")) => Right(Allotment.Calls.Fewest)
        case Some(c: ujson.Obj) =>
          c.value
            .get("times")
            .collect { case ujson.Num(n) if n.isWhole => n.toInt }
            .flatMap(Allotment.Calls.Scaled.of)
            .toRight("route.allotment: times is not a whole number of at least 1")
        case Some(_) => Left("route.allotment: calls is not times or fewest")
      }
    } yield Allotment(grade, reasoning, calls)
  }

  /** A turn's pinned profile, by its id: the profile is kept in the profile store. */
  given profile: Journaled[Either[TurnFailure, TurnProfileId]] =
    outcome(
      id => ujson.Str(TurnProfileId.value(id)),
      v => v.strOpt.map(TurnProfileId(_)).toRight("profile id: expected a string")
    )

  /** An `offer` step's output: `{"workspace": place written | null, "tools": set id,
    * "prompt": [fragment ids]}`, `"root": "heard"` for a turn rooted on a heard message, and
    * `"advertised": [tool names]` when it took tools from an edge's advert, and `"reached":
    * {tool name: place written}` when it took tools from a reached service's. References
    * only: the texts are kept by id. No `root` reads as addressed and no `advertised` or
    * `reached` as none, so every offer recorded before them reads as it did; and `"shaped"`
    * ([[writeShape]]) when the offer was shaped by a recipe, none reading as unshaped.
    */
  given recordedOffer: Journaled[Either[TurnFailure, TurnOffer.Recorded]] =
    outcome(writeRecorded, readRecorded)

  private def writeRecorded(r: TurnOffer.Recorded): ujson.Obj = {
    val o = ujson.Obj(
      "workspace" -> r.workspace.fold[ujson.Value](ujson.Null)(p => ujson.Str(p.written)),
      "tools" -> ToolSetId.value(r.tools),
      "prompt" -> ujson.Arr.from(r.prompt.map(id => ujson.Str(FragmentId.value(id))))
    )
    // Written only for a heard root, so an addressed turn's offer keeps its earlier form.
    r.root match {
      case TurnOffer.Root.Heard => o("root") = "heard"
      case TurnOffer.Root.Named => o("root") = "named"
      case TurnOffer.Root.ByName => o("root") = "by-name"
      case TurnOffer.Root.Addressed => ()
    }
    // Written only when some were taken, so every other offer keeps its earlier form.
    if (r.advertised.nonEmpty)
      o("advertised") = ujson.Arr.from(r.advertised.map(n => ujson.Str(ToolName.value(n))))
    // Written only when some were taken, so every other offer keeps its earlier form.
    if (r.reached.nonEmpty)
      o("reached") = ujson.Obj.from(
        r.reached.toVector
          .sortBy((n, _) => ToolName.value(n))
          .map((n, p) => ToolName.value(n) -> ujson.Str(p.written))
      )
    // Written only when shaped, so every offer recorded before shapes keeps its form.
    r.shaped.foreach(shape => o("shaped") = writeShape(shape))
    o
  }

  private def readRecorded(v: ujson.Value): Either[String, TurnOffer.Recorded] =
    for {
      o <- v.objOpt.toRight("offer: expected an object")
      workspace <- o.get("workspace") match {
        case Some(ujson.Null) | None => Right(None)
        case Some(w) =>
          w.strOpt.toRight("offer: workspace is not a string").flatMap(Place.read).map(Some(_))
      }
      tools <- o.get("tools").flatMap(_.strOpt).toRight("offer: no tools").flatMap(ToolSetId.of)
      prompt <- o
        .get("prompt")
        .flatMap(_.arrOpt)
        .toRight("offer: no prompt")
        .flatMap(_.toVector.foldLeft[Either[String, Vector[FragmentId]]](Right(Vector.empty)) {
          (acc, id) =>
            acc.flatMap(done =>
              id.strOpt
                .toRight("offer: an id is not a string")
                .flatMap(FragmentId.of)
                .map(done :+ _)
            )
        })
      root <- o.get("root") match {
        case None => Right(TurnOffer.Root.Addressed)
        case Some(ujson.Str("heard")) => Right(TurnOffer.Root.Heard)
        case Some(ujson.Str("named")) => Right(TurnOffer.Root.Named)
        case Some(ujson.Str("by-name")) => Right(TurnOffer.Root.ByName)
        case Some(other) => Left(s"offer: unknown root ${other.render()}")
      }
      advertised <- o.get("advertised") match {
        case None => Right(Vector.empty)
        case Some(names) =>
          names.arrOpt
            .toRight("offer: advertised is not an array")
            .flatMap(
              _.toVector.foldLeft[Either[String, Vector[ToolName]]](Right(Vector.empty)) {
                (acc, n) =>
                  acc.flatMap(done =>
                    n.strOpt
                      .toRight("offer: an advertised name is not a string")
                      .flatMap(ToolName.of(_).left.map(e => s"offer: $e"))
                      .map(done :+ _)
                  )
              }
            )
      }
      reached <- o.get("reached") match {
        case None => Right(Map.empty[ToolName, Place])
        case Some(places) =>
          places.objOpt
            .toRight("offer: reached is not an object")
            .flatMap(
              _.toVector.foldLeft[Either[String, Map[ToolName, Place]]](Right(Map.empty)) {
                case (acc, (n, p)) =>
                  acc.flatMap(done =>
                    for {
                      name <- ToolName.of(n).left.map(e => s"offer: $e")
                      written <- p.strOpt.toRight("offer: a reached place is not a string")
                      place <- Place.read(written).left.map(e => s"offer: $e")
                    } yield done.updated(name, place)
                  )
              }
            )
      }
      shaped <- o.get("shaped") match {
        case None => Right(None)
        case Some(shape) => readShape(shape).map(Some(_))
      }
    } yield TurnOffer.Recorded(workspace, tools, prompt, root, advertised, reached, shaped)

  /** `{"width", "whole": set id, "services": [...]}`: the width `"deployed"` or `{"budget",
    * "hits"}`; each service `{"service", "via": "workspace" | "reached", "tools": [names],
    * "sources": [names], "verdict"}`, its verdict `"ungated"`, `"unweighed"` or `{"checked":
    * result}` ([[GateJson.writeChecked]]).
    */
  private def writeShape(shape: TurnShape): ujson.Value =
    ujson.Obj(
      "width" -> (shape.width match {
        case Width.Deployed => ujson.Str("deployed")
        case Width.Within(budget, hits) =>
          ujson.Obj("budget" -> Tokens.value(budget).toDouble, "hits" -> hits)
      }),
      "whole" -> ToolSetId.value(shape.whole),
      "services" -> ujson.Arr.from(shape.services.map { took =>
        ujson.Obj(
          "service" -> took.offer.service.name,
          "via" -> (took.via match {
            case TurnShape.Via.Workspace => "workspace"
            case TurnShape.Via.Reached => "reached"
          }),
          "tools" -> ujson.Arr.from(took.tools.map(n => ujson.Str(ToolName.value(n)))),
          "sources" -> ujson.Arr.from(
            took.offer.sources.map(n => ujson.Str(CorpusName.value(n)))
          ),
          "verdict" -> (took.offer.verdict match {
            case ServiceOffer.Verdict.Ungated => ujson.Str("ungated")
            case ServiceOffer.Verdict.Unweighed => ujson.Str("unweighed")
            case ServiceOffer.Verdict.Checked(result) =>
              ujson.Obj("checked" -> GateJson.writeChecked(result))
          })
        )
      })
    )

  private def readShape(v: ujson.Value): Either[String, TurnShape] =
    for {
      o <- v.objOpt.toRight("shape: expected an object")
      width <- o.get("width") match {
        case Some(ujson.Str("deployed")) => Right(Width.Deployed)
        case Some(w: ujson.Obj) =>
          (
            w.value.get("budget").flatMap(_.numOpt).filter(n => n.isWhole && n >= 0),
            w.value.get("hits").flatMap(_.numOpt).filter(n => n.isValidInt)
          ) match {
            case (Some(budget), Some(hits)) =>
              Right(Width.Within(Tokens(budget.toLong), hits.toInt))
            case _ => Left("shape: width is not {budget, hits}")
          }
        case _ => Left("shape: no width")
      }
      whole <- o.get("whole").flatMap(_.strOpt).toRight("shape: no whole").flatMap(ToolSetId.of)
      services <- o
        .get("services")
        .flatMap(_.arrOpt)
        .toRight("shape: no services")
        .flatMap(items => sequence(items.toVector.map(readTook)))
    } yield TurnShape(width, whole, services)

  private def readTook(v: ujson.Value): Either[String, TurnShape.Took] =
    for {
      o <- v.objOpt.toRight("shape: a service is not an object")
      service <- o
        .get("service")
        .flatMap(_.strOpt)
        .toRight("shape: a service has no name")
        .flatMap(Service.of(_).left.map(e => s"shape: $e"))
      via <- o.get("via") match {
        case Some(ujson.Str("workspace")) => Right(TurnShape.Via.Workspace)
        case Some(ujson.Str("reached")) => Right(TurnShape.Via.Reached)
        case _ => Left("shape: via is neither workspace nor reached")
      }
      tools <- strings(o, "tools").flatMap(ns =>
        sequence(ns.map(ToolName.of(_).left.map(e => s"shape: $e")))
      )
      sources <- strings(o, "sources").flatMap(ns =>
        sequence(ns.map(CorpusName.of(_).left.map(e => s"shape: $e")))
      )
      verdict <- o.get("verdict") match {
        case Some(ujson.Str("ungated")) => Right(ServiceOffer.Verdict.Ungated)
        case Some(ujson.Str("unweighed")) => Right(ServiceOffer.Verdict.Unweighed)
        case Some(c: ujson.Obj) if c.value.contains("checked") =>
          c.value
            .get("checked")
            .toRight("shape: no checked")
            .flatMap(GateJson.readChecked)
            .map(ServiceOffer.Verdict.Checked(_))
        case _ => Left("shape: verdict is not ungated, unweighed or {checked}")
      }
    } yield TurnShape.Took(ServiceOffer(service, sources, verdict), via, tools)

  /** The strings of the array under `key`; `Left` when it is not one of strings. */
  private def strings(
      o: collection.Map[String, ujson.Value],
      key: String
  ): Either[String, Vector[String]] =
    o.get(key)
      .flatMap(_.arrOpt)
      .toRight(s"shape: $key is not an array")
      .flatMap(items =>
        sequence(items.toVector.map(_.strOpt.toRight(s"shape: a name in $key is not a string")))
      )

  /** A `weigh` step's output: `null` when the root was not weighed; `{"kept": tags}`, the tags
    * live triage kept for it ([[TagsJson.write]]); `{"asked": tags, "estimate"}`, what live
    * triage's set answered when the turn asked it, and the estimate of that request's input;
    * or `{"failed": kind}`, why asking it failed, by [[unweighedKinds]].
    */
  given weighed: Journaled[Option[TurnWeighing.Weighed]] =
    Journaled.json[Option[TurnWeighing.Weighed]](
      {
        case None => ujson.Null
        case Some(TurnWeighing.Weighed.Kept(t)) => ujson.Obj("kept" -> TagsJson.write(t))
        case Some(TurnWeighing.Weighed.Asked(Weighing.Weighed(t, estimate))) =>
          ujson.Obj(
            "asked" -> TagsJson.write(t),
            "estimate" -> Tokens.value(estimate).toDouble
          )
        case Some(TurnWeighing.Weighed.Failed(why)) => ujson.Obj("failed" -> unweighedKind(why))
      },
      {
        case ujson.Null => Right(None)
        case o: ujson.Obj if o.value.contains("kept") =>
          o.value
            .get("kept")
            .toRight("weighed: expected {kept}")
            .flatMap(TagsJson.read)
            .map(t => Some(TurnWeighing.Weighed.Kept(t)))
        case o: ujson.Obj if o.value.contains("failed") =>
          o.value
            .get("failed")
            .flatMap(_.strOpt)
            .flatMap(k => unweighedKinds.collectFirst { case (why, `k`) => why })
            .toRight("weighed: failed is not a known kind")
            .map(why => Some(TurnWeighing.Weighed.Failed(why)))
        case o: ujson.Obj =>
          for {
            tags <- o.value.get("asked").toRight("weighed: expected {kept}, {asked} or {failed}")
            read <- TagsJson.read(tags)
            weighed <- read match {
              case w: Tags.Weighed => Right(w)
              case Tags.Unanswered(_) => Left("weighed: asked tags are unanswered")
            }
            estimate <- o.value
              .get("estimate")
              .collect { case ujson.Num(n) if n.isWhole && n >= 0 => Tokens(n.toLong) }
              .toRight("weighed: asked without a whole estimate")
          } yield Some(TurnWeighing.Weighed.Asked(Weighing.Weighed(weighed, estimate)))
        case _ => Left("weighed: expected null, {kept}, {asked} or {failed}")
      }
    )

  /** Each way a weighing fails, as a `weigh` step's `failed` records it: `message`,
    * `placement`, `placement-timeout`, `classifier`, `unreadable`, `timeout`. Never an error's
    * text.
    */
  val unweighedKinds: Vector[(Weighing.Unweighed, String)] =
    Weighing.Unweighed.values.toVector.map(why => why -> unweighedKind(why))

  private def unweighedKind(why: Weighing.Unweighed): String = why match {
    case Weighing.Unweighed.Unread => "message"
    case Weighing.Unweighed.PlacementFailed => "placement"
    case Weighing.Unweighed.PlacementLate => "placement-timeout"
    case Weighing.Unweighed.Unavailable => "classifier"
    case Weighing.Unweighed.Unreadable => "unreadable"
    case Weighing.Unweighed.Late => "timeout"
  }

  /** A step's output that is yes or no: a send or `dispatch` step's, whether its requests were
    * sent to a serving edge (`true`) or no edge was serving the workspace (`false`); a
    * `record-failure` step's, whether a failure was kept for the turn's heard message.
    */
  given yesNo: Journaled[Either[TurnFailure, Boolean]] =
    outcome(b => ujson.Bool(b), v => v.boolOpt.toRight("sent: expected a boolean"))

  /** An `expire` or `abandon` step's output: `"expired"`, `"claimed"`, or `{"answered":
    * outcome}` ([[OutcomeJson]]).
    */
  given requestState: Journaled[Either[TurnFailure, RequestState]] =
    outcome(
      {
        case RequestState.Expired => ujson.Str("expired")
        case RequestState.Claimed => ujson.Str("claimed")
        case RequestState.Answered(o) => ujson.Obj("answered" -> OutcomeJson.write(o))
      },
      v =>
        v.strOpt match {
          case Some("expired") => Right(RequestState.Expired)
          case Some("claimed") => Right(RequestState.Claimed)
          case Some(other) => Left(s"standing: no state $other")
          case None =>
            v.objOpt
              .flatMap(_.get("answered"))
              .toRight("standing: expected a state")
              .flatMap(OutcomeJson.read)
              .map(RequestState.Answered(_))
        }
    )

  /** A `judge` step's output: `{"judgement": "passed" | "nothing_recalled"}`,
    * `{"judgement": "unjudged", "why": ...}`, or `{"judgement": "scored", "judged": ...,
    * "estimated": tokens}` ([[SpeechJson.writeJudged]]).
    */
  given judgement: Journaled[TurnJudge.Judgement] =
    Journaled.json[TurnJudge.Judgement](
      {
        case TurnJudge.Judgement.Passed => ujson.Obj("judgement" -> "passed")
        case TurnJudge.Judgement.NothingRecalled => ujson.Obj("judgement" -> "nothing_recalled")
        case TurnJudge.Judgement.Unjudged(why) => ujson.Obj("judgement" -> "unjudged", "why" -> why)
        case TurnJudge.Judgement.Scored(j, estimated) =>
          ujson.Obj(
            "judgement" -> "scored",
            "judged" -> SpeechJson.writeJudged(j),
            "estimated" -> Tokens.value(estimated).toDouble
          )
      },
      v =>
        v.objOpt.flatMap(_.get("judgement")).flatMap(_.strOpt) match {
          case Some("passed") => Right(TurnJudge.Judgement.Passed)
          case Some("nothing_recalled") => Right(TurnJudge.Judgement.NothingRecalled)
          case Some("unjudged") =>
            v.objOpt
              .flatMap(_.get("why"))
              .flatMap(_.strOpt)
              .toRight("judgement: no why")
              .map(TurnJudge.Judgement.Unjudged(_))
          case Some("scored") =>
            for {
              o <- v.objOpt.toRight("judgement: expected an object")
              j <- o.get("judged").toRight("judgement: no judged").flatMap(SpeechJson.readJudged)
              n <- o
                .get("estimated")
                .flatMap(_.numOpt)
                .filter(_.isWhole)
                .toRight("judgement: no estimate")
            } yield TurnJudge.Judgement.Scored(j, Tokens(n.toLong))
          case _ => Left("judgement: expected a judgement")
        }
    )

  /** An `ask:query` step's outcome: `{"drawn": [note, ...]}`, no search and the window's notes,
    * or `{"written": text, "queried": note}`, each note as a window's notes are written.
    */
  given querying: Journaled[Either[TurnFailure, Querying]] =
    outcome(
      {
        case Querying.Drawn(notes) => ujson.Obj("drawn" -> ujson.Arr.from(notes.map(writeNote)))
        case Querying.Written(text, queried) =>
          ujson.Obj("written" -> text, "queried" -> writeNote(queried))
      },
      v => {
        val o = v.objOpt.getOrElse(ujson.Obj().value)
        (o.get("drawn"), o.get("written"), o.get("queried")) match {
          case (Some(ujson.Arr(notes)), None, None) =>
            sequence(notes.toVector.map(readNote)).map(Querying.Drawn(_))
          case (None, Some(ujson.Str(text)), Some(note)) =>
            readNote(note).flatMap {
              case q: AssemblyNote.Queried => Right(Querying.Written(text, q))
              case _ => Left("querying: a written query's note is not its query's")
            }
          case _ => Left("querying: expected drawn, or written with its note")
        }
      }
    )

  /** A `search` step's outcome: `{"ranked": [{found, "score": s}, ...], "beyond": n}`, each
    * found candidate's fields as [[writeFound]] writes them.
    */
  given results: Journaled[Either[TurnFailure, Results]] =
    outcome[Results](
      r =>
        ujson.Obj(
          "ranked" -> ujson.Arr.from(
            r.ranked.map((f, s) => writeFound(f, "score" -> ujson.Num(s)))
          ),
          "beyond" -> r.beyond.toDouble
        ),
      v =>
        for {
          o <- v.objOpt.toRight("results: expected an object")
          ranked <- o.get("ranked").flatMap(_.arrOpt).toRight("results: no ranked")
          found <- sequence(ranked.toVector.map { item =>
            for {
              f <- readFound(item)
              s <- item.objOpt
                .flatMap(_.get("score"))
                .flatMap(_.numOpt)
                .toRight("results: no score")
            } yield (f, s)
          })
          beyond <- o
            .get("beyond")
            .flatMap(_.numOpt)
            .filter(n => n.isWhole && n >= 0)
            .toRight("results: no beyond")
        } yield Results(found, beyond.toInt)
    )

  /** An `ask:relevance-n` step's verdicts, in order: `[{found, "passed": p} | {found,
    * "rejected": p} | {found, "unjudged": why}, ...]`, each found candidate's fields as
    * [[writeFound]] writes them.
    */
  given verdicts: Journaled[Vector[(Retrieved, TurnRelevance.Verdict)]] =
    Journaled.json[Vector[(Retrieved, TurnRelevance.Verdict)]](
      vs => ujson.Arr.from(vs.map((f, verdict) => writeFound(f, writeVerdict(verdict)))),
      v =>
        v.arrOpt
          .toRight("verdicts: expected an array")
          .flatMap(items =>
            sequence(items.toVector.map { item =>
              readFound(item)
                .flatMap(f => readVerdict(item).left.map(why => s"verdicts: $why").map(f -> _))
            })
          )
    )

  /** An `ask:needs` step's verdict: `{"passed": p}`, `{"rejected": p}` or `{"unjudged": why}`,
    * the fields each of [[verdicts]]' carries.
    */
  given verdict: Journaled[TurnRelevance.Verdict] =
    Journaled.json[TurnRelevance.Verdict](
      v => ujson.Obj(writeVerdict(v)),
      v => readVerdict(v).left.map(why => s"verdict: $why")
    )

  private def writeVerdict(verdict: TurnRelevance.Verdict): (String, ujson.Value) =
    verdict match {
      case TurnRelevance.Verdict.Passed(p) => "passed" -> ujson.Num(Probability.value(p))
      case TurnRelevance.Verdict.Rejected(p) => "rejected" -> ujson.Num(Probability.value(p))
      case TurnRelevance.Verdict.Unjudged(why) => "unjudged" -> ujson.Str(why)
    }

  private def readVerdict(v: ujson.Value): Either[String, TurnRelevance.Verdict] = {
    val o = v.objOpt.getOrElse(ujson.Obj().value)
    def p(key: String) =
      o.get(key).flatMap(_.numOpt).flatMap(Probability.of).toRight(s"$key is not a probability")
    if (o.contains("passed")) p("passed").map(TurnRelevance.Verdict.Passed(_))
    else if (o.contains("rejected")) p("rejected").map(TurnRelevance.Verdict.Rejected(_))
    else
      o.get("unjudged")
        .flatMap(_.strOpt)
        .toRight("expected a verdict")
        .map(TurnRelevance.Verdict.Unjudged(_))
  }

  /** A found candidate as an object's fields, then `more`: `{"own": seq}`, `{"near":
    * conversation, "turn": seq}`, `{"closed": conversation}` or `{"doc": version}`.
    */
  private def writeFound(found: Retrieved, more: (String, ujson.Value)*): ujson.Obj = {
    val fields: Vector[(String, ujson.Value)] = found match {
      case Retrieved.Own(t) => Vector("own" -> ujson.Num(TurnSeq.value(t).toDouble))
      case Retrieved.Near(c, t) =>
        Vector(
          "near" -> ujson.Str(ConversationId.value(c)),
          "turn" -> ujson.Num(TurnSeq.value(t).toDouble)
        )
      case Retrieved.Closed(c) => Vector("closed" -> ujson.Str(ConversationId.value(c)))
      case Retrieved.Doc(d) => Vector("doc" -> ujson.Num(DocumentVersionId.value(d).toDouble))
    }
    ujson.Obj.from(fields ++ more)
  }

  /** The found candidate `v`'s fields name ([[writeFound]]), or why none. */
  private def readFound(v: ujson.Value): Either[String, Retrieved] = {
    val o = v.objOpt.getOrElse(ujson.Obj().value)
    def seq(key: String): Either[String, TurnSeq] =
      o.get(key)
        .flatMap(_.numOpt)
        .filter(_.isWhole)
        .map(n => TurnSeq(n.toLong))
        .toRight(s"found: no $key")
    def conversation(key: String): Option[ConversationId] =
      o.get(key).flatMap(_.strOpt).map(ConversationId(_))
    if (o.contains("own")) seq("own").map(Retrieved.Own(_))
    else if (o.contains("near"))
      conversation("near")
        .toRight("found: no near")
        .flatMap(c => seq("turn").map(Retrieved.Near(c, _)))
    else if (o.contains("closed"))
      conversation("closed").toRight("found: no closed").map(Retrieved.Closed(_))
    else
      o.get("doc")
        .flatMap(_.numOpt)
        .filter(_.isWhole)
        .flatMap(n => DocumentVersionId.of(n.toLong))
        .toRight("found: expected an own, near, closed or doc candidate")
        .map(Retrieved.Doc(_))
  }

  /** A named turn's `judge` step, as recorded before named drafts went unjudged: the call
    * when it scored the draft, `None` otherwise; its scores are not read. `None` is written
    * as an unjudged judgement, a call as a scored one without scores.
    */
  given namedJudge: Journaled[Option[TurnJudge.Call]] =
    Journaled.json[Option[TurnJudge.Call]](
      {
        case None => ujson.Obj("judgement" -> "unjudged", "why" -> "named drafts are not judged")
        case Some(c) =>
          ujson.Obj(
            "judgement" -> "scored",
            "judged" -> ujson.Obj("model" -> c.model, "usage" -> PayloadJson.writeUsage(c.usage)),
            "estimated" -> Tokens.value(c.estimated).toDouble
          )
      },
      v =>
        v.objOpt.flatMap(_.get("judgement")).flatMap(_.strOpt) match {
          case Some("scored") =>
            for {
              o <- v.objOpt.toRight("judgement: expected an object")
              j <- o.get("judged").flatMap(_.objOpt).toRight("judgement: no judged")
              model <- j.get("model").flatMap(_.strOpt).toRight("judgement: no model")
              usage <- j.get("usage").toRight("judgement: no usage").flatMap(PayloadJson.readUsage)
              n <- o
                .get("estimated")
                .flatMap(_.numOpt)
                .filter(_.isWhole)
                .toRight("judgement: no estimate")
            } yield Some(TurnJudge.Call(model, usage, Tokens(n.toLong)))
          case Some("passed" | "nothing_recalled" | "unjudged") => Right(None)
          case _ => Left("judgement: expected a judgement")
        }
    )

  /** A `stitch` step's output: the conversation's first message and where it was placed, or
    * nothing asked ([[grit.core.stitch.StitchJson.writeKept]]).
    */
  given stitched: Journaled[Either[TurnFailure, Option[(EntryId, grit.core.stitch.Placed)]]] =
    outcome(grit.core.stitch.StitchJson.writeKept, grit.core.stitch.StitchJson.readKept)

  /** A `stitched` step's output: what the placement it waited for did, or `null` when it
    * waited for none.
    */
  given placedWaited: Journaled[Either[TurnFailure, Option[String]]] =
    outcome(
      _.fold[ujson.Value](ujson.Null)(ujson.Str(_)),
      {
        case ujson.Null => Right(None)
        case ujson.Str(what) => Right(Some(what))
        case _ => Left("placed: expected a string or null")
      }
    )

  /** A `record-speech` step's output: what became of the draft ([[SpeechJson.writeOutcome]]). */
  given speechSettled: Journaled[Either[TurnFailure, Outcome]] =
    outcome(SpeechJson.writeOutcome, SpeechJson.readOutcome)

  /** A `gate:n` step's output: `{"level", "decided": [{"item", "fate"}], "withheld": [{"tool",
    * "requires"}]}`, each integrity written ([[Integrity.written]]); an item
    * `{"ref", "integrity", "source"}`, its ref `{"kind": "own", "seq"}`,
    * `{"kind": "nearby", "conversation", "seq"}`, `{"kind": "document", "version"}` or
    * `{"kind": "result", "call"}`; a fate `{"kind": "lowered"}` or
    * `{"kind": "hidden", "name", "costs": [tool names]}`.
    */
  given gated: Journaled[Either[TurnFailure, Gated]] =
    outcome(writeGated, readGated)

  private def writeGated(g: Gated): ujson.Value =
    ujson.Obj(
      "level" -> Integrity.written(g.level),
      "decided" -> ujson.Arr.from(g.decided.map { d =>
        val ref = d.item.ref match {
          case ItemRef.Own(seq) =>
            ujson.Obj("kind" -> "own", "seq" -> ujson.Num(EntrySeq.value(seq).toDouble))
          case ItemRef.Nearby(conversation, seq) =>
            ujson.Obj(
              "kind" -> "nearby",
              "conversation" -> ConversationId.value(conversation),
              "seq" -> ujson.Num(EntrySeq.value(seq).toDouble)
            )
          case ItemRef.Document(version) =>
            ujson.Obj(
              "kind" -> "document",
              "version" -> ujson.Num(DocumentVersionId.value(version).toDouble)
            )
          case ItemRef.Result(call) =>
            ujson.Obj("kind" -> "result", "call" -> ToolCallId.value(call))
        }
        val fate = d.fate match {
          case Fate.Lowered => ujson.Obj("kind" -> "lowered")
          case Fate.Hidden(name, costs) =>
            ujson.Obj(
              "kind" -> "hidden",
              "name" -> HiddenName.value(name),
              "costs" -> ujson.Arr.from(costs.map(ToolName.value))
            )
        }
        ujson.Obj(
          "item" -> ujson.Obj(
            "ref" -> ref,
            "integrity" -> Integrity.written(d.item.integrity),
            "source" -> d.item.source
          ),
          "fate" -> fate
        )
      }),
      "withheld" -> ujson.Arr.from(g.withheld.map { w =>
        ujson.Obj("tool" -> ToolName.value(w.tool), "requires" -> Integrity.written(w.requires))
      })
    )

  private def readGated(v: ujson.Value): Either[String, Gated] = {
    def field(o: collection.Map[String, ujson.Value], key: String): Either[String, ujson.Value] =
      o.get(key).toRight(s"gate: no $key")
    def obj(v: ujson.Value, what: String): Either[String, collection.Map[String, ujson.Value]] =
      v.objOpt.toRight(s"gate: $what is not an object")
    def str(o: collection.Map[String, ujson.Value], key: String): Either[String, String] =
      field(o, key).flatMap(_.strOpt.toRight(s"gate: $key is not a string"))
    def long(o: collection.Map[String, ujson.Value], key: String): Either[String, Long] =
      field(o, key).flatMap(
        _.numOpt.filter(n => n.isWhole).map(_.toLong).toRight(s"gate: $key is not a whole number")
      )
    def integrity(o: collection.Map[String, ujson.Value], key: String): Either[String, Integrity] =
      str(o, key).flatMap(Integrity.read)
    def tool(text: String): Either[String, ToolName] = ToolName.of(text)
    def arr(
        o: collection.Map[String, ujson.Value],
        key: String
    ): Either[String, Vector[ujson.Value]] =
      field(o, key).flatMap(_.arrOpt.map(_.toVector).toRight(s"gate: $key is not an array"))
    def ref(v: ujson.Value): Either[String, ItemRef] =
      obj(v, "a ref").flatMap { o =>
        str(o, "kind").flatMap {
          case "own" => long(o, "seq").map(s => ItemRef.Own(EntrySeq(s)))
          case "nearby" =>
            for {
              c <- str(o, "conversation")
              s <- long(o, "seq")
            } yield ItemRef.Nearby(ConversationId(c), EntrySeq(s))
          case "document" =>
            long(o, "version").flatMap(n =>
              DocumentVersionId.of(n).map(ItemRef.Document(_)).toRight(s"gate: version $n")
            )
          case "result" => str(o, "call").map(c => ItemRef.Result(ToolCallId(c)))
          case other => Left(s"gate: no ref kind $other")
        }
      }
    def fate(v: ujson.Value): Either[String, Fate] =
      obj(v, "a fate").flatMap { o =>
        str(o, "kind").flatMap {
          case "lowered" => Right(Fate.Lowered)
          case "hidden" =>
            for {
              name <- str(o, "name").flatMap(HiddenName.read)
              costs <- arr(o, "costs").flatMap(cs =>
                sequence(cs.map(_.strOpt.toRight("gate: a cost is not a string").flatMap(tool)))
              )
            } yield Fate.Hidden(name, costs)
          case other => Left(s"gate: no fate kind $other")
        }
      }
    def decided(v: ujson.Value): Either[String, Decided] =
      for {
        o <- obj(v, "a decided item")
        i <- field(o, "item").flatMap(obj(_, "an item"))
        r <- field(i, "ref").flatMap(ref)
        at <- integrity(i, "integrity")
        source <- str(i, "source")
        f <- field(o, "fate").flatMap(fate)
      } yield Decided(Item(r, at, source), f)
    def withheld(v: ujson.Value): Either[String, Withheld] =
      for {
        o <- obj(v, "a withheld tool")
        t <- str(o, "tool").flatMap(tool)
        requires <- integrity(o, "requires")
      } yield Withheld(t, requires)
    for {
      o <- obj(v, "the gate")
      level <- integrity(o, "level")
      ds <- arr(o, "decided").flatMap(ds => sequence(ds.map(decided)))
      ws <- arr(o, "withheld").flatMap(ws => sequence(ws.map(withheld)))
    } yield Gated(level, ds, ws)
  }

  private def outcome[A](
      write: A -> ujson.Value,
      read: ujson.Value -> Either[String, A]
  ): Journaled[Either[TurnFailure, A]] =
    Journaled.json[Either[TurnFailure, A]](
      {
        case Right(a) => ujson.Obj("ok" -> write(a))
        case Left(failure) =>
          val (kind, reason) = failure match {
            case TurnFailure.Assembly(r) => ("assembly", r)
            case TurnFailure.Model(r) => ("model", r)
            case TurnFailure.Store(r) => ("store", r)
            case TurnFailure.ToolsChanged(r) => ("tools-changed", r)
            case TurnFailure.Uncleared(c) => ("uncleared", Label.written(c))
          }
          ujson.Obj("failed" -> kind, "reason" -> reason)
      },
      v =>
        v.objOpt match {
          case None => Left("expected an object")
          case Some(o) =>
            (o.get("ok"), o.get("failed"), failed(o)) match {
              case (Some(value), None, None) if !o.contains("reason") => read(value).map(Right(_))
              case (None, Some(_), Some(failure)) => Right(Left(failure))
              case _ => Left("expected {ok} or {failed, reason}")
            }
        }
    )

  /** The failure an outcome's output records (`{"failed": kind, "reason": text}`); `None` for
    * any other output, an `{"ok": ...}` included.
    */
  def failure(output: String): Option[TurnFailure] =
    scala.util
      .Try(ujson.read(output))
      .toOption
      .flatMap(_.objOpt)
      .flatMap(o => Option.when(!o.contains("ok"))(o).flatMap(failed))

  private def failed(o: collection.Map[String, ujson.Value]): Option[TurnFailure] =
    (o.get("failed").flatMap(_.strOpt), o.get("reason").flatMap(_.strOpt)) match {
      case (Some("assembly"), Some(r)) => Some(TurnFailure.Assembly(r))
      case (Some("model"), Some(r)) => Some(TurnFailure.Model(r))
      case (Some("store"), Some(r)) => Some(TurnFailure.Store(r))
      case (Some("tools-changed"), Some(r)) => Some(TurnFailure.ToolsChanged(r))
      case (Some("uncleared"), Some(r)) => Label.read(r).toOption.map(TurnFailure.Uncleared(_))
      case _ => None
    }
}
