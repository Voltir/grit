package grit.act.moves

import grit.act.phase.Faults
import grit.core.act.Called
import grit.core.durable.Journaled
import grit.core.edge.{OutcomeJson, RequestState}
import grit.core.message.{AssistantBlock, Message}
import grit.core.place.Service
import grit.core.provider.{ModelRequest, ToolUse}
import grit.core.store.{Payload, PayloadJson}
import grit.core.tool.ToolName
import grit.core.visibility.Label

/** What an ask's `move:{n}` step came to. */
private[moves] enum AskMade {

  /** The model answered `message`; its request held nothing above `at`. */
  case Made(digest: String, message: Message.Assistant, at: Label)

  /** No answer, `why`: the allowance did not admit it, no catalog read or the provider failed,
    * or the store failed.
    */
  case Refused(kind: AskMade.Kind, why: String, digest: String)

  def digest: String
}

private[moves] object AskMade {
  enum Kind(val key: String) {
    case Capped extends Kind("capped")
    case Model extends Kind("model")
    case Store extends Kind("store")
  }
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
  *   - an ask's `move:{n}`: `{"ok":{"digest":d,"message":m,"at":l}}`, `m` a message as an entry
  *     holds it ([[grit.core.store.PayloadJson]]), `l` a label's written form; or
  *     `{"refused":"capped"|"model"|"store","why":w,"digest":d}`;
  *   - `move:{n}:record`: `{"recorded":true}` or `{"store":w}`;
  *   - a call's `move:{n}`: `{"sent":{"digest":d}}`, or
  *     `{"unsent":"unserved"|"unadvertised"|"refused"|"store","why":w,"digest":d}`;
  *   - `move:{n}:expire` and `move:{n}:abandon`: `{"state":"expired"|"claimed"}`,
  *     `{"answered":o}` (an [[grit.core.edge.OutcomeJson]]), or `{"store":w}`;
  *   - `move:{n}:answer`: `{"done":{"text":t,"at":l}}`, `{"failed":w}`, `{"interrupted":true}`
  *     or `{"store":w}`.
  *
  * A digest is `v1:` and the lowercase hex SHA-256 of a canonical form of the move's input
  * that this object owns, never an engine codec's, so a change to those between builds cannot
  * make a run in flight diverge. A form is never changed: a new one is added under the next
  * version, and a recorded digest is compared under its own ([[sameAsk]], [[sameCall]]).
  */
object MovesJournal {

  /** The digest of an ask of `request`, v1: its system text; then each message in order (a
    * user's text; an assistant's blocks' text, reasoning text and tool calls, each call's id,
    * name and arguments; a tool result's call id, content and whether it is an error); then
    * its tools' names in order, and whether it may use them.
    */
  def ask(request: ModelRequest): String = v1(askForm(request))

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
    }
    "ask" + text(request.system) + count(request.messages.size) +
      request.messages.map(message).mkString + count(request.tools.size) +
      request.tools.map(t => text(t.name)).mkString + text(use)
  }

  /** v1's canonical form of a call, which [[call]] hashes. */
  private[moves] def callForm(service: Service, tool: ToolName, arguments: ujson.Obj): String =
    "call" + text(service.place.written) + text(ToolName.value(tool)) + json(arguments)

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
          "c" + text(grit.core.id.ToolCallId.value(id)) + text(name) + json(arguments)
      }.mkString
    case Message.ToolResult(id, content, isError) =>
      "x" + text(grit.core.id.ToolCallId.value(id)) + text(content) + (if (isError) "1" else "0")
  }

  private def json(v: ujson.Value): String = v match {
    case ujson.Null => "n"
    case ujson.True => "t"
    case ujson.False => "f"
    case ujson.Num(d) =>
      val written =
        if (d.isWhole && math.abs(d) < 9.007199254740992e15) d.toLong.toString
        else java.lang.Double.toString(d)
      "d" + text(written)
    case ujson.Str(s) => "s" + text(s)
    case ujson.Arr(items) => "a" + count(items.size) + items.map(json).mkString
    case ujson.Obj(fields) =>
      "o" + count(fields.size) +
        fields.toVector.sortBy(_._1).map((k, value) => text(k) + json(value)).mkString
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
    case AskMade.Made(digest, message, at) =>
      ujson.Obj(
        "ok" -> ujson.Obj(
          "digest" -> digest,
          "message" -> PayloadJson.write(Payload.Message(message)),
          "at" -> Label.written(at)
        )
      )
    case AskMade.Refused(kind, why, digest) =>
      ujson.Obj("refused" -> kind.key, "why" -> why, "digest" -> digest)
  }

  private def readAsk(v: ujson.Value): Either[String, AskMade] =
    fields(v, "ask").flatMap { o =>
      (o.get("ok"), o.get("refused").flatMap(_.strOpt)) match {
        case (Some(ok), _) =>
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
          } yield AskMade.Made(digest, message, at)
        case (_, Some(key)) =>
          for {
            kind <- AskMade.Kind.values.find(_.key == key).toRight(s"ask: no refusal $key")
            why <- str(o, "why")
            digest <- str(o, "digest")
          } yield AskMade.Refused(kind, why, digest)
        case _ => Left("ask: expected ok or refused")
      }
    }

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
