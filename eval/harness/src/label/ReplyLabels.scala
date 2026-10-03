package grit.eval.harness.label

import scala.util.Try

import grit.core.id.{ConversationId, EntrySeq, WorkflowId}
import grit.eval.harness.corpus.{Fields, Part}

/** How good a reply is, as a person judged it. */
enum Quality {
  case Good, Bad, Unsure
}

/** A version of the guide a person labels replies by; a label is read only under one it
  * knows.
  */
enum ReplyGuide {
  case V1
}

object ReplyGuide {

  /** The guide a review is written under now. */
  val Current: ReplyGuide = ReplyGuide.V1

  /** `g` as files write it: `reply-v1`. */
  def written(g: ReplyGuide): String = g match {
    case ReplyGuide.V1 => "reply-v1"
  }

  /** The guide written `name`; why not, when it is no version this build knows. */
  def read(name: String): Either[String, ReplyGuide] =
    values.find(written(_) == name).toRight(s"no reply guide $name")
}

/** A window part by ids alone, so a label naming it outlives the review file that showed it. */
enum Locator {

  /** A record grit wrote when a period closed: its conversation, and its closing entry's
    * seq.
    */
  case Record(of: ConversationId, closing: EntrySeq)

  /** Messages as they were said: their conversation, and their seqs in the order shown. */
  case Messages(of: ConversationId, seqs: ::[EntrySeq])
}

object Locator {

  /** `part`'s locator: a record's for a closed section or the window's own record, its
    * messages' for any other kind; `None` when it names no entry.
    */
  def of(part: Part): Option[Locator] = (part.kind, part.seqs.toList) match {
    case (_, Nil) => None
    case (Part.Kind.Closed | Part.Kind.Record, first :: _) =>
      Some(Record(part.conversation, first))
    case (_, first :: rest) => Some(Messages(part.conversation, ::(first, rest)))
  }

  /** Whether `parts` show every entry `l` names: a record's closing entry in a part of its
    * conversation that is a record or a closed section; each of its messages in a part of
    * their conversation, of any kind.
    */
  def held(l: Locator, parts: Vector[Part]): Boolean = l match {
    case Record(of, closing) =>
      parts.exists(p =>
        p.conversation == of && p.seqs.contains(closing) &&
          (p.kind == Part.Kind.Record || p.kind == Part.Kind.Closed)
      )
    case Messages(of, seqs) =>
      seqs.forall(s => parts.exists(p => p.conversation == of && p.seqs.contains(s)))
  }

  /** Whether `parts` show any entry `l` names, each found as [[held]] finds it. */
  def anyHeld(l: Locator, parts: Vector[Part]): Boolean = l match {
    case r: Record => held(r, parts)
    case Messages(of, seqs) => seqs.exists(s => held(Messages(of, ::(s, Nil)), parts))
  }

  /** `l` as files write it: `{"record": {"conversation", "closing"}}` or `{"messages":
    * {"conversation", "seqs"}}`.
    */
  def json(l: Locator): ujson.Value = l match {
    case Record(of, closing) =>
      ujson.Obj(
        "record" -> ujson.Obj(
          "conversation" -> ConversationId.value(of),
          "closing" -> EntrySeq.value(closing).toDouble
        )
      )
    case Messages(of, seqs) =>
      ujson.Obj(
        "messages" -> ujson.Obj(
          "conversation" -> ConversationId.value(of),
          "seqs" -> ujson.Arr.from(seqs.map(s => ujson.Num(EntrySeq.value(s).toDouble)))
        )
      )
  }

  /** The locator `v` holds, as [[json]] writes it; why not, when it is not of that form. */
  def read(v: ujson.Value): Either[String, Locator] = {
    def seq(n: ujson.Value) =
      n.numOpt.filter(x => x.isWhole && x >= 0).map(x => EntrySeq(x.toLong)).toRight("locator: seq")
    v.objOpt.map(_.keys.toVector) match {
      case Some(Vector("record")) =>
        val f = Fields("locator: record", v("record"))
        for {
          of <- f.str("conversation")
          closing <- f.field("closing").flatMap(seq)
        } yield Record(ConversationId(of), closing)
      case Some(Vector("messages")) =>
        val f = Fields("locator: messages", v("messages"))
        for {
          of <- f.str("conversation")
          seqs <- f.arr("seqs").flatMap(Fields.each(_)(seq))
          some <- seqs.toList match {
            case first :: rest => Right(::(first, rest))
            case Nil => Left("locator: messages names no seq")
          }
        } yield Messages(ConversationId(of), some)
      case _ => Left("locator: neither record nor messages")
    }
  }
}

/** Where a reply's answer is, as a person labelled it. */
enum Found {

  /** In the records the review showed at these locators. */
  case Shown(at: ::[Locator])

  /** In the turn's own thread, which the review does not show. */
  case Thread

  /** Somewhere neither the records shown nor the thread. */
  case Elsewhere

  /** Nowhere. */
  case Nowhere

  /** The message asked nothing to be found. */
  case NotAsked
}

/** A person's label of one turn's reply, under `guide`; a field `None` was left blank.
  *
  * @param speak
  *   whether grit should have replied at all
  */
final case class ReplyLabel(
    guide: ReplyGuide,
    reply: Option[Quality],
    answer: Option[Found],
    speak: Option[Boolean]
)

/** Replies' labels, by the workflow of the turn that wrote each reply. */
final case class ReplyLabels(turns: Map[WorkflowId, ReplyLabel]) {

  /** These, with each of `more` in place of any label of the same turn. */
  def merged(more: Vector[(WorkflowId, ReplyLabel)]): ReplyLabels = ReplyLabels(turns ++ more)
}

object ReplyLabels {

  /** No reply labelled. */
  val Empty: ReplyLabels = ReplyLabels(Map.empty)

  /** `labels` as `reply-labels.json` holds them: `{"turns": {<workflow id>: {"guide",
    * "reply", "answer", "speak"}}}`, the turns in id order; `reply` `good`, `bad`, `unsure` or
    * `null`; `answer` `{"shown": [<locator>, …]}` ([[Locator.json]]), `thread`, `elsewhere`,
    * `nowhere`, `not-asked` or `null`; `speak` a boolean or `null`.
    */
  def written(labels: ReplyLabels): String =
    ujson.write(
      ujson.Obj(
        "turns" -> ujson.Obj.from(
          labels.turns.toVector.sortBy((w, _) => WorkflowId.value(w)).map { (w, l) =>
            WorkflowId.value(w) -> ujson.Obj(
              "guide" -> ReplyGuide.written(l.guide),
              "reply" -> l.reply.fold[ujson.Value](ujson.Null)(q => ujson.Str(quality(q))),
              "answer" -> l.answer.fold[ujson.Value](ujson.Null)(found),
              "speak" -> l.speak.fold[ujson.Value](ujson.Null)(ujson.Bool(_))
            )
          }
        )
      ),
      indent = 2
    ) + "\n"

  /** The labels `text` holds, as [[written]] writes them; why not, naming the turn and field,
    * when it is not of that form or names a guide this build does not know.
    */
  def read(text: String): Either[String, ReplyLabels] =
    for {
      root <- Try(ujson.read(text)).toOption.toRight("reply labels: not JSON")
      turns <- Fields("reply labels", root).obj("turns")
      each <- Fields.each(turns.obj.toVector) { (w, v) =>
        val f = Fields(s"reply labels: $w", v)
        def err(why: String) = s"reply labels: $w: $why"
        for {
          guide <- f.str("guide").flatMap(ReplyGuide.read(_).left.map(err))
          reply <- f
            .optional("reply")
            .flatMap(Fields.opt(_)(q => q.strOpt.flatMap(qualityNamed).toRight(err("reply"))))
          answer <- f.optional("answer").flatMap(Fields.opt(_)(readFound(_).left.map(err)))
          speak <- f
            .optional("speak")
            .flatMap(Fields.opt(_)(s => s.boolOpt.toRight(err("speak"))))
        } yield WorkflowId(w) -> ReplyLabel(guide, reply, answer, speak)
      }
    } yield ReplyLabels(each.toMap)

  /** `q` as files write it, lower-case. */
  def quality(q: Quality): String = q.toString.toLowerCase

  /** The quality written `name` ([[quality]]). */
  def qualityNamed(name: String): Option[Quality] = Quality.values.find(quality(_) == name)

  /** The answers that carry nothing, by the names files write them under. */
  val Placed: Map[String, Found] = Map(
    "thread" -> Found.Thread,
    "elsewhere" -> Found.Elsewhere,
    "nowhere" -> Found.Nowhere,
    "not-asked" -> Found.NotAsked
  )

  private def found(f: Found): ujson.Value = f match {
    case Found.Shown(at) => ujson.Obj("shown" -> ujson.Arr.from(at.map(Locator.json)))
    case Found.Thread => ujson.Str("thread")
    case Found.Elsewhere => ujson.Str("elsewhere")
    case Found.Nowhere => ujson.Str("nowhere")
    case Found.NotAsked => ujson.Str("not-asked")
  }

  private def readFound(v: ujson.Value): Either[String, Found] =
    v.strOpt match {
      case Some(name) => Placed.get(name).toRight(s"answer $name")
      case None =>
        for {
          at <- Fields("answer", v).arr("shown")
          located <- Fields.each(at)(Locator.read)
          some <- located.toList match {
            case first :: rest => Right(Found.Shown(::(first, rest)))
            case Nil => Left("answer: shown names no record")
          }
        } yield some
    }
}
