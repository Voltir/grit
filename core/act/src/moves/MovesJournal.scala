package grit.act.moves

import grit.act.phase.Faults
import grit.core.act.{Answer, Called, Pieced}
import grit.core.classify.{Question, Request}
import grit.core.durable.Journaled
import grit.core.edge.{OutcomeJson, RequestState}
import grit.core.flow.{Mint, Trusted}
import grit.core.id.{ConversationId, DocumentVersionId}
import grit.core.message.{AssistantBlock, Message, Tokens, Usage}
import grit.core.place.Service
import grit.core.provider.{ModelRequest, ToolUse}
import grit.core.schema.JsonSchema
import grit.core.stitch.{Strand, StrandDigest}
import grit.core.store.{Payload, PayloadJson}
import grit.core.tool.{Outcome, ToolName}
import grit.core.visibility.{Facts, Integrity, Label}

/** What an `ask:{n}` step recorded, a job's ask or a workflow's judgment alike. */
private[moves] trait Asked {

  /** What its request held nothing above; `None` when it records none. */
  def label: Option[Label]
}

/** What an ask's `ask:{n}` step came to. */
private[moves] enum AskMade extends Asked {

  /** The model answered `message`; its request, estimated at `estimate`, held nothing above
    * `at`.
    */
  case Made(digest: String, message: Message.Assistant, at: Label, estimate: Tokens)

  /** No answer, `why`: the allowance did not admit it, no catalog read or the provider failed,
    * nothing it could go to was cleared for `at`, the store failed, or a strand's summary found
    * its strand's messages changed. `at` is `Some` for an
    * uncleared refusal alone.
    */
  case Refused(kind: AskMade.Kind, why: String, digest: String, at: Option[Label] = None)

  /** A JSON ask's reply conformed and read: `reply`, its conforming JSON written compactly;
    * what it was asked held nothing above `at`; `level`, the integrity of the acting that posed
    * it (a summary's met with every message it read and document it showed), which an
    * extraction of the reply is trusted at ([[grit.core.act.Extraction.Node]]); `calls`, its
    * model calls in order; `shown`, the documents a summary showed, in order.
    */
  case Shaped(
      digest: String,
      reply: String,
      at: Label,
      level: Integrity,
      calls: Vector[AskMade.Call],
      shown: Vector[DocumentVersionId] = Vector.empty
  )

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

  /** This record, a shaped reply holding `shown` as the documents it was shown; any other as
    * it is.
    */
  def withShown(shown: Vector[DocumentVersionId]): AskMade = this match {
    case s: Shaped => s.copy(shown = shown)
    case other => other
  }

  /** Each model call this record holds, in order: what its record step records. */
  def spent: Vector[AskMade.Call] = this match {
    case Made(_, message, _, estimate) =>
      Vector(AskMade.Call(message.model, message.usage, estimate, message.upstream))
    case Refused(_, _, _, _) => Vector()
    case Shaped(_, _, _, _, calls, _) => calls
    case Unshaped(_, _, calls) => calls
    case Judged(_, _, _, call) => Vector(call)
    case Unjudged(_, _, call) => Vector(call)
  }

  def label: Option[Label] = this match {
    case Made(_, _, at, _) => Some(at)
    case Refused(_, _, _, at) => at
    case Shaped(_, _, at, _, _, _) => Some(at)
    case Unshaped(_, _, _) => None
    case Judged(_, _, at, _) => Some(at)
    case Unjudged(_, _, _) => None
  }
}

private[moves] object AskMade {
  enum Kind(val key: String) {
    case Capped extends Kind("capped")
    case Model extends Kind("model")
    case Store extends Kind("store")
    case Uncleared extends Kind("uncleared")

    /** A strand's summary found its strand's messages changed since its scan: nothing was
      * asked.
      */
    case Changed extends Kind("changed")
  }

  /** One model's or classifier's call's cost: `usage` of `model`, its request estimated at
    * `estimate`, served by `upstream` (a reply's as its provider reported it, or the
    * classifier's service; `None`: not reported).
    */
  final case class Call(
      model: String,
      usage: Usage,
      estimate: Tokens,
      upstream: Option[String] = None
  )
}

/** What a call's `call:{n}` step came to. */
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
  *   - an ask's `ask:{n}`: `{"ok":{"digest":d,"message":m,"at":l,"estimate":e}}`, `m` a message
  *     as an entry holds it ([[grit.core.store.PayloadJson]]), `l` a label's written form, `e`
  *     its request's estimated tokens; or
  *     `{"refused":"capped"|"model"|"store"|"changed","why":w,"digest":d}` (`changed` a
  *     strand's summary's alone), or `{"refused":"uncleared","why":w,"digest":d,"at":l}`, `l` the label no model or
  *     classifier it could go to was cleared for;
  *   - a JSON ask's `ask:{n}`: `{"shaped":{"digest":d,"reply":j,"at":l,"level":i,"calls":[c…]}}`,
  *     `j` its conforming JSON, `i` an [[grit.core.visibility.Integrity.written]], or `{"unshaped":{"digest":d,"why":w,"calls":[c…]}}`, or refused as
  *     above; each `c` is `{"model":m,"usage":u,"estimate":e}`, `u` a usage as an entry holds
  *     it ([[grit.core.store.PayloadJson.writeUsage]]), then `"upstream":p` when who served it
  *     was reported;
  *   - a summary's `ask:{n}` ([[grit.core.act.Dreaming.summarise]]): a JSON ask's, under its
  *     [[strands]] digest, its `shaped` holding `"shown":[v…]` after `calls` when it showed
  *     documents, each `v` a version's number (none when absent, as a record made before it
  *     held them); or `{"refused":"changed","why":w,"digest":d}` when a strand's messages
  *     changed;
  *   - a judgment's `ask:{n}`:
  *     `{"judged":{"digest":d,"answers":[a…],"model":m,"usage":u,"estimate":e,"at":l}}` (its
  *     call's `"upstream":p` before `at` when reported), each `a`
  *     an answer as [[grit.core.classify.AnswersJson]] writes it, or
  *     `{"unjudged":{"digest":d,"why":w,"model":m,"usage":u,"estimate":e}}`, or refused as
  *     above;
  *   - a workflow's judgment's `ask:{n}` ([[posed]]): `{"made":b,"calls":[c…],"at":l}`, `b`
  *     what it made in its own journaled form, `at` only when its pose was asked of a
  *     classifier or model (a record without it reads with no label);
  *   - `ask:{n}:record`, whatever the ask's shape: `{"recorded":true}` or `{"store":w}`;
  *   - a call's `call:{n}`: `{"sent":{"digest":d}}`, or
  *     `{"unsent":"unserved"|"unadvertised"|"refused"|"store","why":w,"digest":d}`;
  *   - `call:{n}:expire` and `call:{n}:abandon`: `{"state":"expired"|"claimed"}`,
  *     `{"answered":o}` (an [[grit.core.edge.OutcomeJson]]), or `{"store":w}`;
  *   - `call:{n}:answer`: `{"done":{"text":t,"at":l,"integrity":i,"stated":s}}` (`i` an
  *     [[grit.core.visibility.Integrity.written]]; read at the run's own when it has none, as
  *     a record made before it held one; `s`, `{"answer":f,"parts":[f…]}`, the facts it kept,
  *     `answer` only when it had its own, and `s` only when it kept some: none when absent, so
  *     a record made before it held them reads, and writes back, as it was),
  *     `{"failed":w}`, `{"interrupted":true}` or `{"store":w}` ([[answered]]);
  *   - a keep's `keep:{n}`: `{"kept":k}`, `k` its body's result in that result's own journaled
  *     form as a string, or `{"store":w}` ([[kept]]).
  *
  * A digest is `v1:` (`v2:` for a summary of more than one strand or of documents,
  * [[strands]]) and the lowercase hex SHA-256 of a canonical form of the move's input
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

  /** The digest of a strand's summary, v1: its root, its [[grit.core.stitch.StrandDigest]] and
    * `asOf`, as `seen` holds them; `chars`; the system text; and the schema as [[json]] writes
    * it. What its members said is not in it: the strand's digest stands for that.
    */
  def strand(seen: Strand.Digested, system: String, chars: Int, schema: JsonSchema): String =
    v1(strandForm(seen, system, chars, schema))

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

  /** The digest of a summary of `seen` and `shown`: one strand and no documents, its
    * [[strand]] digest (v1); otherwise v2, of its strands' roots, [[grit.core.stitch.StrandDigest]]s
    * and `asOf`s in order, then the versions of `shown` in order, `chars`, `documentChars`, the
    * system text, and the schema as [[json]] writes it. What it was shown is not in it: the
    * strands' digests and the versions stand for that.
    */
  def strands(
      seen: Vector[Strand.Digested],
      shown: Vector[DocumentVersionId],
      system: String,
      chars: Int,
      documentChars: Int,
      schema: JsonSchema
  ): String = (seen, shown) match {
    case (Vector(one), Vector()) => strand(one, system, chars, schema)
    case _ => v2(strandsForm(seen, shown, system, chars, documentChars, schema))
  }

  /** As [[sameAsk]], for a summary ([[strands]]). */
  def sameStrands(
      recorded: String,
      seen: Vector[Strand.Digested],
      shown: Vector[DocumentVersionId],
      system: String,
      chars: Int,
      documentChars: Int,
      schema: JsonSchema
  ): Boolean = recorded match {
    case s"v1:$_" | s"v2:$_" =>
      recorded == strands(seen, shown, system, chars, documentChars, schema)
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

  /** v1's canonical form of a strand's summary, which [[strand]] hashes. */
  private[moves] def strandForm(
      seen: Strand.Digested,
      system: String,
      chars: Int,
      schema: JsonSchema
  ): String =
    "strand" + text(ConversationId.value(seen.root)) + text(StrandDigest.value(seen.digest)) +
      text(seen.asOf.toString) + count(chars) + text(system) + text(ujson.write(schema.json))

  /** v1's canonical form of a judgment, which [[judgment]] hashes. */
  private[moves] def judgmentForm(request: Request): String =
    "judge" + written(request.state, sorted = false) + count(request.questions.size) +
      request.questions.map(question).mkString

  /** v1's canonical form of a call, which [[call]] hashes. */
  private[moves] def callForm(service: Service, tool: ToolName, arguments: ujson.Obj): String =
    "call" + text(service.place.written) + text(ToolName.value(tool)) + canonical(arguments)

  /** v2's canonical form of a summary, which [[strands]] hashes. */
  private[moves] def strandsForm(
      seen: Vector[Strand.Digested],
      shown: Vector[DocumentVersionId],
      system: String,
      chars: Int,
      documentChars: Int,
      schema: JsonSchema
  ): String =
    "strands" + count(seen.size) + seen.map { s =>
      text(ConversationId.value(s.root)) + text(StrandDigest.value(s.digest)) +
        text(s.asOf.toString)
    }.mkString + count(shown.size) + shown
      .map(v => text(DocumentVersionId.value(v).toString))
      .mkString +
      count(chars) + count(documentChars) + text(system) + text(ujson.write(schema.json))

  private def v1(form: String): String = "v1:" + sha(form)

  private def v2(form: String): String = "v2:" + sha(form)

  private def sha(form: String): String = {
    val sha = java.security.MessageDigest.getInstance("SHA-256")
    sha.digest(form.getBytes("UTF-8")).map(b => f"$b%02x").mkString
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

  /** An engine workflow's judgment's step: what it made, in `B`'s own journaled form, and its
    * answered calls' costs.
    */
  def posed[B](using b: Journaled[B]): Journaled[Judging.Posed[B]] = Journaled.json(
    p => {
      val o =
        ujson.Obj("made" -> b.encode(p.made), "calls" -> ujson.Arr.from(p.calls.map(writeCost)))
      p.label.foreach(l => o("at") = Label.written(l))
      o
    },
    v =>
      fields(v, "judgment").flatMap { o =>
        for {
          made <- o
            .get("made")
            .flatMap(_.strOpt)
            .toRight("judgment: no made")
            .flatMap(b.decode)
          calls <- costs(o)
          // Absent in a record made before judgments kept it.
          label <- o.get("at") match {
            case None => Right(None)
            case Some(l) =>
              l.strOpt.toRight("judgment: at is not text").flatMap(Label.read).map(Some(_))
          }
        } yield Judging.Posed(made, calls, label)
      }
  )

  /** The label an `ask:{n}` step's recorded `output` says its request held nothing above, a
    * job's ask and a workflow's judgment read alike; `None` when it records none (a judgment
    * made with no call asked or recorded before it kept one, a reply that did not read, a
    * refusal before its request was read) or `output` is no ask's record.
    */
  def askedAt(output: String): Option[Label] = {
    val asked: Either[String, Asked] =
      askMade.decode(output).left.flatMap(_ => posed[String].decode(output))
    asked.toOption.flatMap(_.label)
  }

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

  /** What a piece move came to: `{"kept":n,"cut":n}`, `{"unread":why}`, `{"withdrawn":n}`,
    * `{"relabelled":n}` or `{"due":[ref, …]}`.
    */
  given pieced: Journaled[Pieced] = Journaled.json(
    {
      case Pieced.Kept(pieces, cut) => ujson.Obj("kept" -> pieces, "cut" -> cut)
      case Pieced.Unread(why) => ujson.Obj("unread" -> why)
      case Pieced.Withdrawn(files) => ujson.Obj("withdrawn" -> files)
      case Pieced.Relabelled(pieces) => ujson.Obj("relabelled" -> pieces)
      case Pieced.Due(refs) => ujson.Obj("due" -> ujson.Arr.from(refs.map(ujson.Str(_))))
    },
    v =>
      fields(v, "piece").flatMap { o =>
        def count(key: String) = o.get(key).flatMap(_.numOpt).map(_.toInt)
        val unread = o.get("unread").flatMap(_.strOpt)
        (count("kept"), count("cut"), unread, count("withdrawn"), count("relabelled")) match {
          case (Some(kept), Some(cut), _, _, _) => Right(Pieced.Kept(kept, cut))
          case (_, _, Some(why), _, _) => Right(Pieced.Unread(why))
          case (_, _, _, Some(files), _) => Right(Pieced.Withdrawn(files))
          case (_, _, _, _, Some(pieces)) => Right(Pieced.Relabelled(pieces))
          case _ =>
            o.get("due").flatMap(_.arrOpt) match {
              case Some(refs) if refs.forall(_.strOpt.nonEmpty) =>
                Right(Pieced.Due(refs.toVector.flatMap(_.strOpt)))
              case _ => Left("piece: expected kept and cut, unread, withdrawn, relabelled, or due")
            }
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

  /** A call's `call:{n}:answer`, a done one without its integrity read at `start`, what the
    * run starts believed at.
    */
  def answered(start: Integrity): Journaled[Either[String, Called]] = Journaled.json(
    {
      case Right(Called.Done(t, at)) =>
        val answer = Trusted.asIs(t).extract
        val done = ujson.Obj(
          "text" -> answer.text,
          "at" -> Label.written(at),
          "integrity" -> Integrity.written(t.integrity)
        )
        if (answer.stated != Outcome.Stated.None) {
          val stated = ujson.Obj()
          answer.stated.answer.foreach(f => stated("answer") = f.json)
          stated("parts") = ujson.Arr.from(answer.stated.parts.map(_.json))
          done("stated") = stated
        }
        ujson.Obj("done" -> done)
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
              i <- d
                .get("integrity")
                .fold(Right(start))(v =>
                  v.strOpt
                    .toRight("answer.done's integrity is not a string")
                    .flatMap(Integrity.read)
                )
              s <- d.get("stated").fold(Right(Outcome.Stated.None))(stated)
            } yield Right(Called.Done(Mint.tainted(Answer(t, s), i), l))
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
    case AskMade.Refused(kind, why, digest, at) =>
      ujson.Obj.from(
        Vector[(String, ujson.Value)]("refused" -> kind.key, "why" -> why, "digest" -> digest) ++
          at.map(l => "at" -> ujson.Str(Label.written(l)))
      )
    case AskMade.Shaped(digest, reply, at, level, calls, shown) =>
      ujson.Obj(
        "shaped" -> ujson.Obj.from(
          Vector[(String, ujson.Value)](
            "digest" -> digest,
            "reply" -> ujson.read(reply),
            "at" -> Label.written(at),
            "level" -> Integrity.written(level),
            "calls" -> ujson.Arr.from(calls.map(writeCost))
          ) ++ Option.when(shown.nonEmpty)(
            "shown" -> ujson.Arr.from(
              shown.map(v => ujson.Num(DocumentVersionId.value(v).toDouble))
            )
          )
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
    Vector[(String, ujson.Value)](
      "model" -> ujson.Str(c.model),
      "usage" -> PayloadJson.writeUsage(c.usage),
      "estimate" -> ujson.Num(Tokens.value(c.estimate).toDouble)
    ) ++ c.upstream.map(u => "upstream" -> ujson.Str(u))

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
            level <- str(f, "level").flatMap(Integrity.read)
            calls <- costs(f)
            shown <- versions(f)
          } yield AskMade.Shaped(digest, reply, at, level, calls, shown)
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
            at <- o.get("at") match {
              case None if kind == AskMade.Kind.Uncleared =>
                Left("ask: an uncleared refusal names no label")
              case None => Right(None)
              case Some(l) =>
                l.strOpt.toRight("ask: at is not text").flatMap(Label.read).map(Some(_))
            }
          } yield AskMade.Refused(kind, why, digest, at)
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

  /** A summary's shown versions, `shown` in `f`: none when absent, as a record made before it
    * held them.
    */
  private def versions(
      f: collection.Map[String, ujson.Value]
  ): Either[String, Vector[DocumentVersionId]] =
    f.get("shown") match {
      case None => Right(Vector.empty)
      case Some(v) =>
        v.arrOpt
          .toRight("ask: shown is not a list")
          .flatMap(_.toVector.foldLeft[Either[String, Vector[DocumentVersionId]]](Right(Vector())) {
            (acc, n) =>
              acc.flatMap(done =>
                n.numOpt
                  .flatMap(d => DocumentVersionId.of(d.toLong))
                  .toRight(s"ask: shown holds no version: $n")
                  .map(done :+ _)
              )
          })
    }

  /** One recorded call, from its fields `model`, `usage` and `estimate` in `o`. */
  private def cost(o: collection.Map[String, ujson.Value]): Either[String, AskMade.Call] =
    for {
      model <- str(o, "model")
      usage <- o.get("usage").toRight("ask: no usage").flatMap(PayloadJson.readUsage)
      estimate <- whole(o, "estimate")
      upstream <- o.get("upstream") match {
        case None => Right(None)
        case Some(u) => u.strOpt.toRight("ask: upstream is not text").map(Some(_))
      }
    } yield AskMade.Call(model, usage, Tokens(estimate), upstream)

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

  /** A done answer's `stated`: `{"answer"?:f,"parts":[f…]}`. */
  private def stated(v: ujson.Value): Either[String, Outcome.Stated] =
    for {
      o <- fields(v, "answer.done.stated")
      parts <- o
        .get("parts")
        .flatMap(_.arrOpt)
        .toRight("answer.done.stated has no parts array")
    } yield Outcome.Stated(o.get("answer").map(Facts.of), parts.toVector.map(Facts.of))

  private def str(o: collection.Map[String, ujson.Value], key: String): Either[String, String] =
    o.get(key).flatMap(_.strOpt).toRight(s"expected a string '$key'")
}
