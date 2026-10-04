package grit.eval.harness.corpus

import scala.collection.immutable.VectorMap

import grit.core.context.Width
import grit.core.id.{ConversationId, EntryId, EntrySeq, KnowledgeSourceName, TurnSeq, WorkflowId}
import grit.core.message.{Tokens, Usage}
import grit.core.period.Probability
import grit.core.place.{Place, Service}
import grit.core.prompt.Layer
import grit.core.recipe.ServiceOffer
import grit.core.store.Focus
import grit.core.tool.{ToolName, ToolSetId}
import grit.core.triage.{GateJson, Tags, TagsJson, Weighing}
import grit.turn.{TurnOffer, TurnRecord, TurnShape, TurnWeighing}

import Fields.{each, opt}

/** A turn of a corpus as JSON, one line of `turns.jsonl` each, its fields in one fixed order,
  * so equal turns write equal bytes. A read is `Left` naming the first field missing or not of
  * its form; a line written before turns' shapes were captured, which has no `weighed`, is
  * refused, naming the corpus to recapture.
  */
object TurnJson {

  def write(t: TurnCase): ujson.Value = ujson.Obj(
    "workflow" -> WorkflowId.value(t.workflow),
    "conversation" -> ConversationId.value(t.conversation),
    "turn" -> TurnSeq.value(t.turn).toDouble,
    "said" -> (t.said match {
      case Said.Slack(at) => ujson.Obj("slack" -> at.written)
      case Said.Tui(entry) => ujson.Obj("tui" -> EntryId.value(entry))
      case Said.Task(entry) => ujson.Obj("task" -> EntryId.value(entry))
    }),
    "root" -> t.root.toString.toLowerCase,
    "focus" -> t.focus.toString.toLowerCase,
    "started" -> t.started.toString,
    "build" -> CorpusJson.writeBuild(t.build),
    "triage" -> t.triage.fold[ujson.Value](ujson.Null)(CorpusJson.writeLive),
    "offered" -> t.offered.fold[ujson.Value](ujson.Null)(writeOffered),
    "weighed" -> writeWeigh(t.weighed),
    "window" -> t.window.fold[ujson.Value](ujson.Null)(writeParts),
    "rounds" -> ujson.Arr.from(t.rounds.map(r => ujson.Arr.from(r.calls.map(writeCall)))),
    "ended" -> (t.ended match {
      case Ended.Replied(length, passed) =>
        ujson.Obj("replied" -> ujson.Obj("length" -> length, "passed" -> passed))
      case Ended.Failed(step, why) =>
        ujson.Obj("failed" -> ujson.Obj("step" -> step, "why" -> why.toString.toLowerCase))
      case Ended.Unfinished(status) => ujson.Obj("unfinished" -> status)
    }),
    "spend" -> ujson.Arr.from(t.spend.map(writeSpent)),
    "speech" -> t.speech.fold[ujson.Value](ujson.Null)(writeDrafted)
  )

  def read(v: ujson.Value): Either[String, TurnCase] = {
    val f = Fields("turn", v)
    for {
      workflow <- f.str("workflow")
      conversation <- f.str("conversation")
      turn <- f.long("turn")
      said <- f.obj("said").flatMap(readSaid)
      root <- f
        .str("root")
        .flatMap(r =>
          TurnOffer.Root.values.find(_.toString.toLowerCase == r).toRight(s"turn: root $r")
        )
      focus <- f
        .str("focus")
        .flatMap(r => Focus.values.find(_.toString.toLowerCase == r).toRight(s"turn: focus $r"))
      started <- f.instant("started")
      build <- f.field("build").flatMap(CorpusJson.readBuild("turn", _))
      triage <- f.optional("triage").flatMap(opt(_)(CorpusJson.readLive))
      weighed <- f
        .field("weighed")
        .left
        .map(_ => "turn: no weighed: written before shapes were captured; recapture the corpus")
        .flatMap(readWeigh)
      offered <- f.optional("offered").flatMap(opt(_)(readOffered))
      window <- f.optional("window").flatMap(opt(_)(readParts))
      rounds <- f
        .arr("rounds")
        .flatMap(each(_)(r => r.arrOpt.toRight("turn: a round is not an array")))
        .flatMap(each(_)(calls => each(calls.toVector)(readCall).map(Round(_))))
      ended <- f.obj("ended").flatMap(readEnded)
      spend <- f.arr("spend").flatMap(each(_)(readSpent))
      speech <- f.optional("speech").flatMap(opt(_)(readDrafted))
    } yield TurnCase(
      WorkflowId(workflow),
      ConversationId(conversation),
      TurnSeq(turn),
      said,
      root,
      focus,
      started,
      build,
      triage,
      offered,
      weighed,
      window,
      rounds,
      ended,
      spend,
      speech
    )
  }

  private def readSaid(v: ujson.Value): Either[String, Said] = {
    val f = Fields("said", v)
    v.objOpt.map(_.keys.toVector) match {
      case Some(Vector("slack")) => f.str("slack").flatMap(CaseId.read).map(Said.Slack(_))
      case Some(Vector("tui")) => f.str("tui").map(e => Said.Tui(EntryId(e)))
      case Some(Vector("task")) => f.str("task").map(e => Said.Task(EntryId(e)))
      case _ => Left("said: neither slack, tui nor task")
    }
  }

  private def writeOffered(o: Offered): ujson.Value = ujson.Obj(
    "tools" -> ujson.Arr.from(o.tools.map(n => ujson.Str(ToolName.value(n)))),
    "set" -> ToolSetId.value(o.set),
    "schema" -> tokens(o.schema),
    "prompt" -> ujson.Obj.from(o.prompt.toVector.map((l, n) => l.key -> tokens(n))),
    "workspace" -> o.workspace.fold[ujson.Value](ujson.Null)(p => ujson.Str(p.written)),
    "reached" -> ujson.Arr.from(o.reached.map(p => ujson.Str(p.written))),
    "shape" -> o.shape.fold[ujson.Value](ujson.Null)(writeShape)
  )

  private def readOffered(v: ujson.Value): Either[String, Offered] = {
    val f = Fields("offered", v)
    for {
      tools <- f
        .arr("tools")
        .flatMap(each(_)(t => Fields.str("offered: a tool", t).flatMap(ToolName.of)))
      set <- f.str("set").flatMap(ToolSetId.of)
      schema <- f.long("schema").map(Tokens(_))
      prompt <- f
        .obj("prompt")
        .flatMap(p =>
          each(p.obj.toVector) { (k, n) =>
            for {
              layer <- Layer.of(k).toRight(s"offered: no layer $k")
              count <- n.numOpt.filter(_.isWhole).toRight(s"offered: $k is not whole")
            } yield layer -> Tokens(count.toLong)
          }
        )
      workspace <- f
        .optional("workspace")
        .flatMap(opt(_)(w => Fields.str("offered: workspace", w).flatMap(Place.read)))
      reached <- f
        .arr("reached")
        .flatMap(each(_)(r => Fields.str("offered: reached", r).flatMap(Place.read)))
      shape <- f.optional("shape").flatMap(opt(_)(readShape))
    } yield Offered(tools, set, schema, VectorMap.from(prompt), workspace, reached, shape)
  }

  /** `{"width", "whole", "services": [...]}`: the width `"deployed"` or `{"budget", "hits"}`;
    * each service `{"service", "via": "workspace" | "reached", "tools", "sources", "verdict"}`,
    * its verdict `"ungated"`, `"unweighed"` or `{"checked": result}`.
    */
  private def writeShape(shape: TurnShape): ujson.Value = ujson.Obj(
    "width" -> (shape.width match {
      case Width.Deployed => ujson.Str("deployed")
      case Width.Within(budget, hits) => ujson.Obj("budget" -> tokens(budget), "hits" -> hits)
    }),
    "whole" -> ToolSetId.value(shape.whole),
    "services" -> ujson.Arr.from(shape.services.map { took =>
      ujson.Obj(
        "service" -> took.offer.service.name,
        "via" -> took.via.toString.toLowerCase,
        "tools" -> ujson.Arr.from(took.tools.map(n => ujson.Str(ToolName.value(n)))),
        "sources" -> ujson.Arr.from(
          took.offer.sources.map(n => ujson.Str(KnowledgeSourceName.value(n)))
        ),
        "verdict" -> (took.offer.verdict match {
          case ServiceOffer.Verdict.Ungated => ujson.Str("ungated")
          case ServiceOffer.Verdict.Unweighed => ujson.Str("unweighed")
          case ServiceOffer.Verdict.Checked(c) => ujson.Obj("checked" -> GateJson.writeChecked(c))
        })
      )
    })
  )

  private def readShape(v: ujson.Value): Either[String, TurnShape] = {
    val f = Fields("shape", v)
    for {
      width <- f.field("width").flatMap {
        case ujson.Str("deployed") => Right(Width.Deployed)
        case w: ujson.Obj =>
          val wf = Fields("shape: width", w)
          for {
            budget <- wf.long("budget")
            hits <- wf.int("hits")
          } yield Width.Within(Tokens(budget), hits)
        case _ => Left("shape: width is neither deployed nor {budget, hits}")
      }
      whole <- f.str("whole").flatMap(ToolSetId.of)
      services <- f.arr("services").flatMap(each(_)(readTook))
    } yield TurnShape(width, whole, services)
  }

  private def readTook(v: ujson.Value): Either[String, TurnShape.Took] = {
    val f = Fields("shape: service", v)
    for {
      service <- f.str("service").flatMap(Service.of)
      via <- f
        .str("via")
        .flatMap(w =>
          TurnShape.Via.values.find(_.toString.toLowerCase == w).toRight(s"shape: via $w")
        )
      tools <- f
        .arr("tools")
        .flatMap(each(_)(t => Fields.str("shape: a tool", t).flatMap(ToolName.of)))
      sources <- f
        .arr("sources")
        .flatMap(each(_)(n => Fields.str("shape: a source", n).flatMap(KnowledgeSourceName.of)))
      verdict <- f.field("verdict").flatMap {
        case ujson.Str("ungated") => Right(ServiceOffer.Verdict.Ungated)
        case ujson.Str("unweighed") => Right(ServiceOffer.Verdict.Unweighed)
        case c: ujson.Obj =>
          Fields("shape: verdict", c)
            .field("checked")
            .flatMap(GateJson.readChecked)
            .map(ServiceOffer.Verdict.Checked(_))
        case _ => Left("shape: verdict is neither ungated, unweighed nor {checked}")
      }
    } yield TurnShape.Took(ServiceOffer(service, sources, verdict), via, tools)
  }

  /** `"unrecorded"` for no `weigh` step; else what it recorded: `null` for nothing, `{"kept":
    * tags}`, `{"asked": tags, "estimate"}` or `{"failed": kind}`, the kind
    * [[Weighing.Unweighed]]'s case in lower case.
    */
  private def writeWeigh(w: TurnRecord.Weigh): ujson.Value = w match {
    case TurnRecord.Weigh.Unrecorded => ujson.Str("unrecorded")
    case TurnRecord.Weigh.Recorded(None) => ujson.Null
    case TurnRecord.Weigh.Recorded(Some(TurnWeighing.Weighed.Kept(tags))) =>
      ujson.Obj("kept" -> TagsJson.write(tags))
    case TurnRecord.Weigh.Recorded(Some(TurnWeighing.Weighed.Asked(asked))) =>
      ujson.Obj("asked" -> TagsJson.write(asked.tags), "estimate" -> tokens(asked.estimate))
    case TurnRecord.Weigh.Recorded(Some(TurnWeighing.Weighed.Failed(why))) =>
      ujson.Obj("failed" -> why.toString.toLowerCase)
  }

  private def readWeigh(v: ujson.Value): Either[String, TurnRecord.Weigh] = {
    val f = Fields("weighed", v)
    def recorded(w: TurnWeighing.Weighed) = TurnRecord.Weigh.Recorded(Some(w))
    v match {
      case ujson.Str("unrecorded") => Right(TurnRecord.Weigh.Unrecorded)
      case ujson.Null => Right(TurnRecord.Weigh.Recorded(None))
      case o: ujson.Obj =>
        o.value.keys.toVector.sorted match {
          case Vector("kept") =>
            f.field("kept").flatMap(TagsJson.read).map(t => recorded(TurnWeighing.Weighed.Kept(t)))
          case Vector("asked", "estimate") =>
            for {
              tags <- f.field("asked").flatMap(TagsJson.read).flatMap {
                case w: Tags.Weighed => Right(w)
                case Tags.Unanswered(_) => Left("weighed: asked tags are unanswered")
              }
              estimate <- f.long("estimate")
            } yield recorded(TurnWeighing.Weighed.Asked(Weighing.Weighed(tags, Tokens(estimate))))
          case Vector("failed") =>
            f.str("failed")
              .flatMap(k =>
                Weighing.Unweighed.values
                  .find(_.toString.toLowerCase == k)
                  .toRight(s"weighed: failed $k")
              )
              .map(why => recorded(TurnWeighing.Weighed.Failed(why)))
          case _ => Left("weighed: neither kept, asked nor failed")
        }
      case _ => Left("weighed: neither unrecorded, null nor an object")
    }
  }

  private def writeParts(p: Parts): ujson.Value = ujson.Obj(
    "parts" -> ujson.Arr.from(p.parts.map { part =>
      ujson.Obj(
        "kind" -> Part.Kind.written(part.kind),
        "conversation" -> ConversationId.value(part.conversation),
        "seqs" -> ujson.Arr.from(part.seqs.map(s => ujson.Num(EntrySeq.value(s).toDouble))),
        "tokens" -> tokens(part.tokens),
        "support" -> part.support.fold[ujson.Value](ujson.Null)(s => ujson.Num(s.value))
      )
    }),
    "gaps" -> tokens(p.gaps),
    "own" -> tokens(p.own)
  )

  private def readParts(v: ujson.Value): Either[String, Parts] = {
    val f = Fields("window", v)
    for {
      parts <- f
        .arr("parts")
        .flatMap(each(_) { p =>
          val pf = Fields("window: part", p)
          for {
            kind <- pf.str("kind").flatMap(k => Part.Kind.read(k).toRight(s"window: no kind $k"))
            conversation <- pf.str("conversation")
            seqs <- pf
              .arr("seqs")
              .flatMap(
                each(_)(s => s.numOpt.filter(_.isWhole).toRight("window: a seq is not whole"))
              )
            count <- pf.long("tokens")
            support <- pf
              .optional("support")
              .flatMap(opt(_)(s => s.numOpt.flatMap(Support.read).toRight("window: support")))
          } yield Part(
            kind,
            ConversationId(conversation),
            seqs.map(s => EntrySeq(s.toLong)),
            Tokens(count),
            support
          )
        })
      gaps <- f.long("gaps")
      own <- f.long("own")
    } yield Parts(parts, Tokens(gaps), Tokens(own))
  }

  private def writeCall(c: Call): ujson.Value = {
    val (settled, length) = c.settled match {
      case Settled.Ok(n) => ("ok", Some(n))
      case Settled.Failed(n) => ("failed", Some(n))
      case Settled.Expired => ("expired", None)
      case Settled.Abandoned => ("abandoned", None)
      case Settled.Unsettled => ("unsettled", None)
    }
    ujson.Obj(
      "tool" -> (c.tool match {
        case Called.Tool(n) => ujson.Str(ToolName.value(n))
        case Called.Topic | Called.Unnamed => ujson.Null
      }),
      "topic" -> (c.tool == Called.Topic),
      "settled" -> settled,
      "length" -> length.fold[ujson.Value](ujson.Null)(ujson.Num(_))
    )
  }

  private def readCall(v: ujson.Value): Either[String, Call] = {
    val f = Fields("call", v)
    for {
      tool <- f
        .optional("tool")
        .flatMap(opt(_)(t => Fields.str("call: tool", t).flatMap(ToolName.of)))
      topic <- f.added("topic").flatMap(opt(_)(t => t.boolOpt.toRight("call: topic")))
      called <- (tool, topic.contains(true)) match {
        case (Some(n), true) => Left(s"call: ${ToolName.value(n)} is a topic call")
        case (Some(n), false) => Right(Called.Tool(n))
        case (None, true) => Right(Called.Topic)
        case (None, false) => Right(Called.Unnamed)
      }
      settled <- f.str("settled")
      length <- f
        .optional("length")
        .flatMap(opt(_)(n => n.numOpt.filter(_.isWhole).map(_.toInt).toRight("call: length")))
      s <- (settled, length) match {
        case ("ok", Some(n)) => Right(Settled.Ok(n))
        case ("failed", Some(n)) => Right(Settled.Failed(n))
        case ("expired", None) => Right(Settled.Expired)
        case ("abandoned", None) => Right(Settled.Abandoned)
        case ("unsettled", None) => Right(Settled.Unsettled)
        case _ => Left(s"call: settled $settled")
      }
    } yield Call(called, s)
  }

  private def readEnded(v: ujson.Value): Either[String, Ended] = {
    val f = Fields("ended", v)
    v.objOpt.map(_.keys.toVector) match {
      case Some(Vector("replied")) =>
        f.obj("replied").flatMap { r =>
          val rf = Fields("ended: replied", r)
          for {
            length <- rf.int("length")
            passed <- rf.bool("passed")
          } yield Ended.Replied(length, passed)
        }
      case Some(Vector("failed")) =>
        f.obj("failed").flatMap { r =>
          val rf = Fields("ended: failed", r)
          for {
            step <- rf.str("step")
            why <- rf
              .str("why")
              .flatMap(w =>
                Ended.Why.values.find(_.toString.toLowerCase == w).toRight(s"ended: why $w")
              )
          } yield Ended.Failed(step, why)
        }
      case Some(Vector("unfinished")) => f.str("unfinished").map(Ended.Unfinished(_))
      case _ => Left("ended: neither replied, failed nor unfinished")
    }
  }

  private def writeSpent(s: Spent): ujson.Value = ujson.Obj(
    "role" -> s.role.fold[ujson.Value](ujson.Null)(r => ujson.Str(role(r))),
    "model" -> s.model,
    "input" -> tokens(s.usage.input),
    "output" -> tokens(s.usage.output),
    "cached" -> tokens(s.usage.cachedInput),
    "cost_usd" -> s.usage.costUsd.fold[ujson.Value](ujson.Null)(c => ujson.Str(c.toString)),
    "estimated" -> tokens(s.estimated)
  )

  private def readSpent(v: ujson.Value): Either[String, Spent] = {
    val f = Fields("spent", v)
    for {
      r <- f.optional("role").flatMap(opt(_)(r => Fields.str("spent: role", r).flatMap(readRole)))
      model <- f.str("model")
      input <- f.long("input")
      output <- f.long("output")
      cached <- f.long("cached")
      cost <- f
        .optional("cost_usd")
        .flatMap(
          opt(_)(c =>
            Fields
              .str("spent: cost_usd", c)
              .flatMap(t => scala.util.Try(BigDecimal(t)).toOption.toRight("spent: cost_usd"))
          )
        )
      estimated <- f.long("estimated")
    } yield Spent(
      r,
      model,
      Usage(Tokens(input), Tokens(output), Tokens(cached), cost),
      Tokens(estimated)
    )
  }

  /** A role's written name: `query`, `topic`, `round:<n>`, `reply`, `judge`, `summary` or
    * `weigh`.
    */
  private def role(r: TurnRecord.Role): String = r match {
    case TurnRecord.Role.Round(n) => s"round:$n"
    case other => other.toString.toLowerCase
  }

  private def readRole(name: String): Either[String, TurnRecord.Role] =
    name.split(':') match {
      case Array("round", n) =>
        n.toIntOption.filter(_ >= 0).map(TurnRecord.Role.Round(_)).toRight(s"spent: role $name")
      case _ =>
        Vector(
          TurnRecord.Role.Query,
          TurnRecord.Role.Topic,
          TurnRecord.Role.Reply,
          TurnRecord.Role.Judge,
          TurnRecord.Role.Summary,
          TurnRecord.Role.Weigh
        ).find(role(_) == name).toRight(s"spent: role $name")
    }

  private def writeDrafted(d: Drafted): ujson.Value = {
    def p(o: Option[Probability]) =
      o.fold[ujson.Value](ujson.Null)(x => ujson.Num(Probability.value(x)))
    ujson.Obj(
      "outcome" -> d.outcome.toString,
      "grounded" -> p(d.grounded),
      "worth" -> p(d.worth),
      "post_at" -> p(d.postAt)
    )
  }

  private def readDrafted(v: ujson.Value): Either[String, Drafted] = {
    val f = Fields("speech", v)
    def p(k: String) =
      f.optional(k).flatMap(opt(_)(x => x.numOpt.flatMap(Probability.of).toRight(s"speech: $k")))
    for {
      outcome <- f
        .str("outcome")
        .flatMap(o => Drafted.Kind.values.find(_.toString == o).toRight(s"speech: outcome $o"))
      grounded <- p("grounded")
      worth <- p("worth")
      postAt <- p("post_at")
    } yield Drafted(outcome, grounded, worth, postAt)
  }

  private def tokens(t: Tokens): ujson.Value = ujson.Num(Tokens.value(t).toDouble)
}
