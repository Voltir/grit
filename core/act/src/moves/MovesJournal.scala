package grit.act.moves

import grit.act.phase.Faults
import grit.core.act.Called
import grit.core.classify.{Question, Request}
import grit.core.durable.Journaled
import grit.core.edge.{OutcomeJson, RequestState}
import grit.core.message.{AssistantBlock, Message, Tokens, Usage}
import grit.core.place.Service
import grit.core.provider.{ModelRequest, ToolUse}
import grit.core.schema.JsonSchema
import grit.core.store.{Payload, PayloadJson}
import grit.core.tool.ToolName
import grit.core.visibility.Label

/** What an ask's `move:{n}` step came to. */
private[moves] enum AskMade {

  /** The model answered `message`; its request, estimated at `estimate`, held nothing above
    * `at`.
    */
  case Made(digest: String, message: Message.Assistant, at: Label, estimate: Tokens)

  /** No answer, `why`: the allowance did not admit it, no catalog read or the provider failed,
    * or the store failed.
    */
  case Refused(kind: AskMade.Kind, why: String, digest: String)

  /** A JSON ask's reply conformed and read: `reply`, its conforming JSON written compactly;
    * what it was asked held nothing above `at`; `calls`, its model calls in order.
    */
  case Shaped(digest: String, reply: String, at: Label, calls: Vector[AskMade.Call])

  /** A JSON ask's reply did not read, `why`, after `calls`, its model calls in order. */
  case Unshaped(digest: String, why: String, calls: Vector[AskMade.Call])

  /** A judgment's answers read: `answers`, the classifier's answers as
    * [[grit.core.classify.AnswersJson]] writes them, in a JSON array written compactly; what it
    * was asked held nothing above `at`; `call`, the classifier's call.
    */
  case Judged(digest: String, answers: String, at: Label, call: AskMade.Call)

  /** A judgment's answers did not read, `why`, after `call`, the classifier's call. */
  case Unjudged(digest: String, why: String, call: AskMade.Call)

  def digest: String

  /** Each model call this record holds, in order: what its record step records. */
  def spent: Vector[AskMade.Call] = this match {
    case Made(_, message, _, estimate) =>
      Vector(AskMade.Call(message.model, message.usage, estimate))
    case Refused(_, _, _) => Vector()
    case Shaped(_, _, _, calls) => calls
    case Unshaped(_, _, calls) => calls
    case Judged(_, _, _, call) => Vector(call)
    case Unjudged(_, _, call) => Vector(call)
  }
}

private[moves] object AskMade {
  enum Kind(val key: String) {
    case Capped extends Kind("capped")
    case Model extends Kind("model")
    case Store extends Kind("store")
  }

  /** One model's or classifier's call's cost: `usage` of `model`, its request estimated at
    * `estimate`.
    */
  final case class Call(model: String, usage: Usage, estimate: Tokens)
}

/** What a call's `move:{n}` step came to. */
private[moves] enum CallMade {

  /** Its request was written for the live edge serving its place. */
  case Sent(digest: String)

  /** Nothing was sent to an edge, `why`. */
  case Unsent(kind: CallMade.Kind, why: String, digest: String)

  def digest: String
}

private[moves] object CallMade {
  enum Kind(val key: String) {

    /** No live edge serves its place. */
    case Unserved extends Kind("unserved")

    /** The edge serving its place does not advertise its tool. */
    case Unadvertised extends Kind("unadvertised")

    /** It has nobody to be made for, its tool asks first, its arguments do not bind, or the
      * acting may not send it (its request then written answered with the refusal).
      */
    case Refused extends Kind("refused")

    /** The store failed. */
    case Store extends Kind("store")
  }
}

/** How a planner's step outputs are recorded, and the digests of its moves' inputs. A run in
  * flight must read back what an earlier build wrote, so a form here is never changed, only
  * added to.
  *   - an ask's `move:{n}`: `{"ok":{"digest":d,"message":m,"at":l,"estimate":e}}`, `m` a message
  *     as an entry holds it ([[grit.core.store.PayloadJson]]), `l` a label's written form, `e`
  *     its request's estimated tokens; or `{"refused":"capped"|"model"|"store","why":w,"digest":d}`;
  *   - a JSON ask's `move:{n}`: `{"shaped":{"digest":d,"reply":j,"at":l,"calls":[c…]}}`, `j`
  *     its conforming JSON, or `{"unshaped":{"digest":d,"why":w,"calls":[c…]}}`, or refused as
  *     above; each `c` is `{"model":m,"usage":u,"estimate":e}`, `u` a usage as an entry holds
  *     it ([[grit.core.store.PayloadJson.writeUsage]]);
  *   - a judgment's `move:{n}`:
  *     `{"judged":{"digest":d,"answers":[a…],"model":m,"usage":u,"estimate":e,"at":l}}`, each `a`
  *     an answer as [[grit.core.classify.AnswersJson]] writes it, or
  *     `{"unjudged":{"digest":d,"why":w,"model":m,"usage":u,"estimate":e}}`, or refused as
  *     above;
  *   - `move:{n}:record`, whatever the ask's shape: `{"recorded":true}` or `{"store":w}`;
  *   - a call's `move:{n}`: `{"sent":{"digest":d}}`, or
  *     `{"unsent":"unserved"|"unadvertised"|"refused"|"store","why":w,"digest":d}`;
  *   - `move:{n}:expire` and `move:{n}:abandon`: `{"state":"expired"|"claimed"}`,
  *     `{"answered":o}` (an [[grit.core.edge.OutcomeJson]]), or `{"store":w}`;
  *   - `move:{n}:answer`: `{"done":{"text":t,"at":l}}`, `{"failed":w}`, `{"interrupted":true}`
  *     or `{"store":w}`;
  *   - a keep's `move:{n}`: `{"kept":k}`, `k` its body's result in that result's own journaled
  *     form as a string, or `{"store":w}` ([[kept]]).
  *
  * A digest is `v1:` and the lowercase hex SHA-256 of a canonical form of the move's input
  * that this object owns, never an engine codec's, so a change to those between builds cannot
  * make a run in flight diverge. A form is never changed: a new one is added under the next
  * version, and a recorded digest is compared under its own ([[sameAsk]], [[sameJson]],
  * [[sameJudgment]], [[sameCall]]). Each kind's form starts with its own word, so two kinds never share a digest.
  */
object MovesJournal {

  /** The digest of an ask of `request`, v1: its system text; then each message in order (a
    * user's text; an assistant's blocks' text, reasoning text and tool calls, each call's id,
    * name and arguments; a tool result's call id, content and whether it is an error); then
    * its tools' names in order, and whether it may use them.
    */
  def ask(request: ModelRequest): String = v1(askForm(request))

  /** The digest of a JSON ask of `system` then `messages` held to `schema`, v1: its system text
    * and each message as [[ask]] writes them, then the schema as a provider is sent it
    * ([[grit.core.schema.JsonSchema.json]]), its keys in the order written there: a model reads
    * and writes a schema's properties in order, so two schemas differing only in that order
    * differ.
    */
  def json(system: String, messages: Vector[Message], schema: JsonSchema): String =
    v1(jsonForm(system, messages, schema))

  /** The digest of a judgment of `request`, v1: its state, each object's keys in the order
    * written; then each question in order, its kind, instructions, and its keys with their
    * descriptions, its levels, or what yes and no mean: everything the classifier is sent.
    */
  def judgment(request: Request): String = v1(judgmentForm(request))

  /** The digest of a call of `tool` at `service`'s place with `arguments`, v1: the place's
    * written form, the tool's name, and the arguments with each object's keys sorted.
    */
  def call(service: Service, tool: ToolName, arguments: ujson.Obj): String =
    v1(callForm(service, tool, arguments))

  /** Whether `recorded`, an ask's digest from an earlier run, is of `request`, under the
    * version it was recorded at; false for a version this build does not know.
    */
  def sameAsk(recorded: String, request: ModelRequest): Boolean = recorded match {
    case s"v1:$_" => recorded == ask(request)
    case _ => false
  }

  /** As [[sameAsk]], for a JSON ask. */
  def sameJson(
      recorded: String,
      system: String,
      messages: Vector[Message],
      schema: JsonSchema
  ): Boolean = recorded match {
    case s"v1:$_" => recorded == json(system, messages, schema)
    case _ => false
  }

  /** As [[sameAsk]], for a judgment. */
  def sameJudgment(recorded: String, request: Request): Boolean = recorded match {
    case s"v1:$_" => recorded == judgment(request)
    case _ => false
  }

  /** As [[sameAsk]], for a call. */
  def sameCall(recorded: String, service: Service, tool: ToolName, arguments: ujson.Obj): Boolean =
    recorded match {
      case s"v1:$_" => recorded == call(service, tool, arguments)
      case _ => false
    }

  /** v1's canonical form of an ask of `request`, which [[ask]] hashes. */
  private[moves] def askForm(request: ModelRequest): String = {
    val use = request.use match {
      case ToolUse.Auto => "auto"
      case ToolUse.Off => "off"
      case ToolUse.Required => "required"
    }
    "ask" + text(request.system) + count(request.messages.size) +
      request.messages.map(message).mkString + count(request.tools.size) +
      request.tools.map(t => text(t.name)).mkString + text(use)
  }

  /** v1's canonical form of a JSON ask, which [[json]] hashes. */
  private[moves] def jsonForm(
      system: String,
      messages: Vector[Message],
      schema: JsonSchema
  ): String =
    "json" + text(system) + count(messages.size) + messages.map(message).mkString +
      text(ujson.write(schema.json))

  /** v1's canonical form of a judgment, which [[judgment]] hashes. */
  private[moves] def judgmentForm(request: Request): String =
    "judge" + written(request.state, sorted = false) + count(request.questions.size) +
      request.questions.map(question).mkString

  /** v1's canonical form of a call, which [[call]] hashes. */
  private[moves] def callForm(service: Service, tool: ToolName, arguments: ujson.Obj): String =
    "call" + text(service.place.written) + text(ToolName.value(tool)) + canonical(arguments)

  private def v1(form: String): String = {
    val sha = java.security.MessageDigest.getInstance("SHA-256")
    "v1:" + sha.digest(form.getBytes("UTF-8")).map(b => f"$b%02x").mkString
  }

  private def text(s: String): String = s"${s.length}:$s"

  private def count(n: Int): String = s"$n;"

  private def message(m: Message): String = m match {
    case Message.User(t) => "u" + text(t)
    case Message.Assistant(blocks, _, _, _, _) =>
      "a" + count(blocks.size) + blocks.map {
        case AssistantBlock.Text(t) => "t" + text(t)
        case AssistantBlock.Reasoning(t, _) => "r" + text(t)
        case AssistantBlock.ToolCall(id, name, arguments) =>
          "c" + text(grit.core.id.ToolCallId.value(id)) + text(name) + canonical(arguments)
      }.mkString
    case Message.ToolResult(id, content, isError) =>
      "x" + text(grit.core.id.ToolCallId.value(id)) + text(content) + (if (isError) "1" else "0")
  }

  private def question(q: Question): String = q match {
    case c: Question.Choice =>
      "c" + text(c.instructions) + count(c.keys.size) +
        c.keys.map(k => text(k.name) + optional(k.description)).mkString
    case Question.YesNo(instructions, yes, no) =>
      "y" + text(instructions) + optional(yes) + optional(no)
    case s: Question.Score =>
      "s" + text(s.instructions) + count(s.levels.size) + s.levels.map(text).mkString
  }

  private def optional(s: Option[String]): String = s.fold("n")(t => "s" + text(t))

  /** `v`, each object's keys sorted. */
  private def canonical(v: ujson.Value): String = written(v, sorted = true)

  /** `v`, each object's keys sorted when `sorted`, else in the order written. */
  private def written(v: ujson.Value, sorted: Boolean): String = v match {
    case ujson.Null => "n"
    case ujson.True => "t"
    case ujson.False => "f"
    case ujson.Num(d) =>
      val written =
        if (d.isWhole && math.abs(d) < 9.007199254740992e15) d.toLong.toString
        else java.lang.Double.toString(d)
      "d" + text(written)
    case ujson.Str(s) => "s" + text(s)
    case ujson.Arr(items) => "a" + count(items.size) + items.map(written(_, sorted)).mkString
    case ujson.Obj(fields) =>
      val keyed = if (sorted) fields.toVector.sortBy(_._1) else fields.toVector
      "o" + count(fields.size) + keyed.map((k, value) => text(k) + written(value, sorted)).mkString
  }

  given askMade: Journaled[AskMade] = Journaled.json(writeAsk, readAsk)

  given recorded: Journaled[Either[String, Unit]] = Journaled.json(
    {
      case Right(()) => ujson.Obj("recorded" -> true)
      case Left(why) => ujson.Obj("store" -> why)
    },
    v =>
      fields(v, "record").flatMap { o =>
        (o.get("recorded").flatMap(_.boolOpt), o.get("store").flatMap(_.strOpt)) match {
          case (Some(true), _) => Right(Right(()))
          case (_, Some(why)) => Right(Left(why))
          case _ => Left("record: expected recorded or store")
        }
      }
  )

  given callMade: Journaled[CallMade] = Journaled.json(writeCall, readCall)

  /** A keep's step: what its body returned, in `A`'s own journaled form, or why nothing was
    * kept. Carries no digest: a keep's input is its code.
    */
  def kept[A](using a: Journaled[A]): Journaled[Either[String, A]] = Journaled.json(
    {
      case Right(value) => ujson.Obj("kept" -> a.encode(value))
      case Left(why) => ujson.Obj("store" -> why)
    },
    v =>
      fields(v, "keep").flatMap { o =>
        (o.get("kept").flatMap(_.strOpt), o.get("store").flatMap(_.strOpt)) match {
          case (Some(value), _) => a.decode(value).map(Right(_))
          case (_, Some(why)) => Right(Left(why))
          case _ => Left("keep: expected kept or store")
        }
      }
  )

  given standing: Journaled[Either[String, RequestState]] = Journaled.json(
    {
      case Right(RequestState.Expired) => ujson.Obj("state" -> "expired")
      case Right(RequestState.Claimed) => ujson.Obj("state" -> "claimed")
      case Right(RequestState.Answered(o)) => ujson.Obj("answered" -> OutcomeJson.write(o))
      case Left(why) => ujson.Obj("store" -> why)
    },
    v =>
      fields(v, "wait").flatMap { o =>
        (
          o.get("state").flatMap(_.strOpt),
          o.get("answered"),
          o.get("store").flatMap(_.strOpt)
        ) match {
          case (Some("expired"), _, _) => Right(Right(RequestState.Expired))
          case (Some("claimed"), _, _) => Right(Right(RequestState.Claimed))
          case (_, Some(a), _) => OutcomeJson.read(a).map(o => Right(RequestState.Answered(o)))
          case (_, _, Some(why)) => Right(Left(why))
          case _ => Left("wait: expected a state, an answer or store")
        }
      }
  )

  given answered: Journaled[Either[String, Called]] = Journaled.json(
    {
      case Right(Called.Done(t, at)) =>
        ujson.Obj("done" -> ujson.Obj("text" -> t, "at" -> Label.written(at)))
      case Right(Called.Failed(why)) => ujson.Obj("failed" -> why)
      case Right(Called.Interrupted) => ujson.Obj("interrupted" -> true)
      case Left(why) => ujson.Obj("store" -> why)
    },
    v =>
      fields(v, "answer").flatMap { o =>
        (
          o.get("done"),
          o.get("failed").flatMap(_.strOpt),
          o.get("interrupted"),
          o.get("store")
        ) match {
          case (Some(done), _, _, _) =>
            for {
              d <- fields(done, "answer.done")
              t <- str(d, "text")
              l <- str(d, "at").flatMap(Label.read)
            } yield Right(Called.Done(t, l))
          case (_, Some(why), _, _) => Right(Right(Called.Failed(why)))
          case (_, _, Some(ujson.True), _) => Right(Right(Called.Interrupted))
          case (_, _, _, Some(ujson.Str(why))) => Right(Left(why))
          case _ => Left("answer: expected done, failed, interrupted or store")
        }
      }
  )

  /** A phase's store failure, as a planner's steps record it: its reason. */
  given faults: Faults[String] with {
    def store(reason: String): String = reason
  }

  private def writeAsk(made: AskMade): ujson.Value = made match {
    case AskMade.Made(digest, message, at, estimate) =>
      ujson.Obj(
        "ok" -> ujson.Obj(
          "digest" -> digest,
          "message" -> PayloadJson.write(Payload.Message(message)),
          "at" -> Label.written(at),
          "estimate" -> ujson.Num(Tokens.value(estimate).toDouble)
        )
      )
    case AskMade.Refused(kind, why, digest) =>
      ujson.Obj("refused" -> kind.key, "why" -> why, "digest" -> digest)
    case AskMade.Shaped(digest, reply, at, calls) =>
      ujson.Obj(
        "shaped" -> ujson.Obj(
          "digest" -> digest,
          "reply" -> ujson.read(reply),
          "at" -> Label.written(at),
          "calls" -> ujson.Arr.from(calls.map(writeCost))
        )
      )
    case AskMade.Unshaped(digest, why, calls) =>
      ujson.Obj(
        "unshaped" -> ujson.Obj(
          "digest" -> digest,
          "why" -> why,
          "calls" -> ujson.Arr.from(calls.map(writeCost))
        )
      )
    case AskMade.Judged(digest, answers, at, call) =>
      ujson.Obj(
        "judged" -> ujson.Obj.from(
          Vector("digest" -> ujson.Str(digest), "answers" -> ujson.read(answers)) ++
            costFields(call) :+ ("at" -> Label.written(at))
        )
      )
    case AskMade.Unjudged(digest, why, call) =>
      ujson.Obj(
        "unjudged" -> ujson.Obj.from(
          Vector("digest" -> ujson.Str(digest), "why" -> ujson.Str(why)) ++ costFields(call)
        )
      )
  }

  private def writeCost(c: AskMade.Call): ujson.Value = ujson.Obj.from(costFields(c))

  /** `c`'s fields, in the order every form writes them. */
  private def costFields(c: AskMade.Call): Vector[(String, ujson.Value)] =
    Vector(
      "model" -> ujson.Str(c.model),
      "usage" -> PayloadJson.writeUsage(c.usage),
      "estimate" -> ujson.Num(Tokens.value(c.estimate).toDouble)
    )

  private def readAsk(v: ujson.Value): Either[String, AskMade] =
    fields(v, "ask").flatMap { o =>
      (o.get("ok"), o.get("refused").flatMap(_.strOpt), o.get("shaped"), o.get("unshaped")) match {
        case _ if o.contains("judged") =>
          for {
            f <- fields(o("judged"), "ask.judged")
            digest <- str(f, "digest")
            answers <- f.get("answers").toRight("ask: no answers").map(ujson.write(_))
            at <- str(f, "at").flatMap(Label.read)
            call <- cost(f)
          } yield AskMade.Judged(digest, answers, at, call)
        case _ if o.contains("unjudged") =>
          for {
            f <- fields(o("unjudged"), "ask.unjudged")
            digest <- str(f, "digest")
            why <- str(f, "why")
            call <- cost(f)
          } yield AskMade.Unjudged(digest, why, call)
        case (_, _, Some(shaped), _) =>
          for {
            f <- fields(shaped, "ask.shaped")
            digest <- str(f, "digest")
            reply <- f.get("reply").toRight("ask: no reply").map(ujson.write(_))
            at <- str(f, "at").flatMap(Label.read)
            calls <- costs(f)
          } yield AskMade.Shaped(digest, reply, at, calls)
        case (_, _, _, Some(unshaped)) =>
          for {
            f <- fields(unshaped, "ask.unshaped")
            digest <- str(f, "digest")
            why <- str(f, "why")
            calls <- costs(f)
          } yield AskMade.Unshaped(digest, why, calls)
        case (Some(ok), _, _, _) =>
          for {
            f <- fields(ok, "ask.ok")
            digest <- str(f, "digest")
            message <- f
              .get("message")
              .toRight("ask: no message")
              .flatMap(PayloadJson.read)
              .flatMap {
                case Payload.Message(m: Message.Assistant) => Right(m)
                case _ => Left("ask: expected an assistant message")
              }
            at <- str(f, "at").flatMap(Label.read)
            estimate <- f
              .get("estimate")
              .flatMap(_.numOpt)
              .filter(n => n.isWhole && n >= 0)
              .toRight("ask: no estimate")
          } yield AskMade.Made(digest, message, at, Tokens(estimate.toLong))
        case (_, Some(key), _, _) =>
          for {
            kind <- AskMade.Kind.values.find(_.key == key).toRight(s"ask: no refusal $key")
            why <- str(o, "why")
            digest <- str(o, "digest")
          } yield AskMade.Refused(kind, why, digest)
        case _ => Left("ask: expected ok, refused, shaped, unshaped, judged or unjudged")
      }
    }

  /** A JSON ask's recorded calls, under `calls`. */
  private def costs(f: collection.Map[String, ujson.Value]): Either[String, Vector[AskMade.Call]] =
    f.get("calls")
      .flatMap(_.arrOpt)
      .toRight("ask: no calls")
      .flatMap(_.toVector.foldLeft[Either[String, Vector[AskMade.Call]]](Right(Vector())) {
        (acc, c) => acc.flatMap(done => fields(c, "ask.calls").flatMap(cost).map(done :+ _))
      })

  /** One recorded call, from its fields `model`, `usage` and `estimate` in `o`. */
  private def cost(o: collection.Map[String, ujson.Value]): Either[String, AskMade.Call] =
    for {
      model <- str(o, "model")
      usage <- o.get("usage").toRight("ask: no usage").flatMap(PayloadJson.readUsage)
      estimate <- whole(o, "estimate")
    } yield AskMade.Call(model, usage, Tokens(estimate))

  /** A count of tokens under `key`. */
  private def whole(o: collection.Map[String, ujson.Value], key: String): Either[String, Long] =
    o.get(key)
      .flatMap(_.numOpt)
      .filter(n => n.isWhole && n >= 0)
      .map(_.toLong)
      .toRight(s"expected a count '$key'")

  private def writeCall(made: CallMade): ujson.Value = made match {
    case CallMade.Sent(digest) => ujson.Obj("sent" -> ujson.Obj("digest" -> digest))
    case CallMade.Unsent(kind, why, digest) =>
      ujson.Obj("unsent" -> kind.key, "why" -> why, "digest" -> digest)
  }

  private def readCall(v: ujson.Value): Either[String, CallMade] =
    fields(v, "call").flatMap { o =>
      (o.get("sent"), o.get("unsent").flatMap(_.strOpt)) match {
        case (Some(sent), _) =>
          fields(sent, "call.sent").flatMap(str(_, "digest")).map(CallMade.Sent(_))
        case (_, Some(key)) =>
          for {
            kind <- CallMade.Kind.values.find(_.key == key).toRight(s"call: no kind $key")
            why <- str(o, "why")
            digest <- str(o, "digest")
          } yield CallMade.Unsent(kind, why, digest)
        case _ => Left("call: expected sent or unsent")
      }
    }

  private def fields(
      v: ujson.Value,
      step: String
  ): Either[String, collection.Map[String, ujson.Value]] =
    v.objOpt.toRight(s"$step: expected an object")

  private def str(o: collection.Map[String, ujson.Value], key: String): Either[String, String] =
    o.get(key).flatMap(_.strOpt).toRight(s"expected a string '$key'")
}
