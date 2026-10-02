package grit.turn

import grit.core.context.{AssemblyNote, Window}
import grit.core.durable.Journaled
import grit.core.edge.{OutcomeJson, RequestState}
import grit.core.id.{EntryId, TurnSeq}
import grit.core.message.{Message, Tokens}
import grit.core.model.TurnProfileId
import grit.core.place.Place
import grit.core.prompt.FragmentId
import grit.core.speech.{Outcome, SpeechJson}
import grit.core.store.{Nearby, Payload, PayloadJson}
import grit.core.tool.{ToolName, ToolSetId}
import grit.core.topic.{TopicId, TopicJson}

/** How the turn's step outputs are recorded: `{"ok": value}` or
  * `{"failed": kind, "reason": text}`. In-flight turns must read back what an earlier
  * build wrote, so change these only with the workflow version (`workflow-versioning`).
  */
private[turn] object TurnJournal {

  /** A window with no notes and no nearby sections is the bare array of its entries' seqs
    * ([[PayloadJson.writeSeqs]]); one with either is `{"entries": [...], "notes": [...]}`, with
    * `"nearby": [...]` too when it has sections ([[PayloadJson.writeNearby]]).
    */
  given window: Journaled[Either[TurnFailure, Window]] =
    outcome(
      w => {
        val ids = PayloadJson.writeSeqs(w.entries)
        if (w.notes.isEmpty && w.nearby.isEmpty) ids
        else {
          val o = ujson.Obj("entries" -> ids, "notes" -> ujson.Arr.from(w.notes.map(writeNote)))
          if (w.nearby.nonEmpty) o("nearby") = ujson.Arr.from(w.nearby.map(PayloadJson.writeNearby))
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
            } yield Window(ids, notes, nearby)
          case _ => Left("window: expected an array or an object")
        }
    )

  private def writeNote(n: AssemblyNote): ujson.Value = n match {
    case AssemblyNote.Queried(query, model, usage, estimate) =>
      ujson.Obj(
        "queried" -> query,
        "model" -> model,
        "usage" -> PayloadJson.writeUsage(usage),
        "estimate" -> Tokens.value(estimate).toDouble
      )
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
          } yield AssemblyNote.Queried(query, model, usage, estimate)
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
  given classification: Journaled[TurnTopics.Classification] =
    Journaled.json[TurnTopics.Classification](
      c =>
        ujson.Obj(
          "events" -> ujson.Arr.from(c.events.map(TopicJson.write)),
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
            .flatMap(es => sequence(es.toVector.map(TopicJson.read)))
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
        } yield TurnTopics.Classification(events, current, earlier, cost, note)
    )

  private def writeShown(s: TurnTopics.Shown): ujson.Value =
    ujson.Obj("id" -> TopicId.value(s.id), "key" -> s.key)

  private def readShown(v: ujson.Value): Either[String, TurnTopics.Shown] =
    (
      v.objOpt.flatMap(_.get("id")).flatMap(_.strOpt),
      v.objOpt.flatMap(_.get("key")).flatMap(_.strOpt)
    ) match {
      case (Some(id), Some(key)) => Right(TurnTopics.Shown(TopicId(id), key))
      case _ => Left("a topic shown: expected {id, key}")
    }

  private def sequence[A](as: Vector[Either[String, A]]): Either[String, Vector[A]] =
    as.foldLeft[Either[String, Vector[A]]](Right(Vector.empty))((acc, a) =>
      acc.flatMap(done => a.map(done :+ _))
    )

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

  /** A `tool:n:j` step's output: `{"result": entry id, "failed": boolean}`. */
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
    * `reached` as none, so every offer recorded before them reads as it did.
    */
  given recordedOffer: Journaled[Either[TurnFailure, TurnOffer.Recorded]] =
    outcome(
      r => {
        val o = ujson.Obj(
          "workspace" -> r.workspace.fold[ujson.Value](ujson.Null)(p => ujson.Str(p.written)),
          "tools" -> ToolSetId.value(r.tools),
          "prompt" -> ujson.Arr.from(r.prompt.map(id => ujson.Str(FragmentId.value(id))))
        )
        // Written only for a heard root, so an addressed turn's offer keeps its earlier form.
        r.root match {
          case TurnOffer.Root.Heard => o("root") = "heard"
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
        o
      },
      v =>
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
        } yield TurnOffer.Recorded(workspace, tools, prompt, root, advertised, reached)
    )

  /** A `dispatch` step's output: whether its requests were sent to a serving edge (`true`),
    * or no edge was serving the workspace (`false`).
    */
  given dispatchedTo: Journaled[Either[TurnFailure, Boolean]] =
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

  /** A `stitch` step's output: the conversation's first message and where it was placed, or
    * nothing asked ([[grit.core.stitch.StitchJson.writeKept]]).
    */
  given stitched: Journaled[Either[TurnFailure, Option[(EntryId, grit.core.stitch.Placed)]]] =
    outcome(grit.core.stitch.StitchJson.writeKept, grit.core.stitch.StitchJson.readKept)

  /** A `record-speech` step's output: what became of the draft ([[SpeechJson.writeOutcome]]). */
  given speechSettled: Journaled[Either[TurnFailure, Outcome]] =
    outcome(SpeechJson.writeOutcome, SpeechJson.readOutcome)

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
          }
          ujson.Obj("failed" -> kind, "reason" -> reason)
      },
      v =>
        v.objOpt match {
          case None => Left("expected an object")
          case Some(o) =>
            (
              o.get("ok"),
              o.get("failed").flatMap(_.strOpt),
              o.get("reason").flatMap(_.strOpt)
            ) match {
              case (Some(value), None, None) => read(value).map(Right(_))
              case (None, Some("assembly"), Some(r)) => Right(Left(TurnFailure.Assembly(r)))
              case (None, Some("model"), Some(r)) => Right(Left(TurnFailure.Model(r)))
              case (None, Some("store"), Some(r)) => Right(Left(TurnFailure.Store(r)))
              case _ => Left("expected {ok} or {failed, reason}")
            }
        }
    )
}
