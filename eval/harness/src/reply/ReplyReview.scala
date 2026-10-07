package grit.eval.harness.reply

import java.time.Instant

import scala.util.hashing.MurmurHash3

import grit.core.clock.Fresh
import grit.core.context.Shown
import grit.core.id.{EntryId, EntrySeq, TurnRef, WorkflowId}
import grit.core.message.{AssistantBlock, Message, Tokens}
import grit.core.store.{Entry, Nearby, Payload, Speakers, StoreError}
import grit.dbos.internal.Reader
import grit.eval.harness.capture.{CaseId, Drafted, Ended, Fields, Part, Said, TurnCase}
import grit.eval.harness.label.{Found, Locator, ReplyGuide, ReplyLabel, ReplyLabels}
import grit.turn.{Turn, TurnOffer}

/** A few turns' replies shown to a person to label, text and all, in a file only a person
  * reads; and the labels read back from it once filled.
  */
object ReplyReview {

  /** The most cases one review shows. */
  val Cap: Int = 5

  /** The most records a case shows. */
  val Records: Int = 3

  /** The characters a record is cut at. */
  val Cut: Int = 800

  /** How a review's turns are asked for. */
  enum Ask {

    /** These, each a turn's workflow id or a Slack message's case id, which names the turn
      * that answered it.
      */
    case Named(ids: Vector[String])

    /** Up to `n` turns, none of them labelled yet, chosen `by`. */
    case Pick(n: Int, by: By)
  }

  /** How [[Ask.Pick]] chooses among the turns with a reply to review. */
  enum By {

    /** Addressed turns' replies and heard turns' drafts that were posted, latest first. */
    case Posted

    /** Heard turns' drafts that were not posted, latest first. */
    case Held

    /** Turns that called tools, latest first. */
    case Tools

    /** Turns by their window's tokens, most first. */
    case Widest

    /** Turns in an order `seed` fixes. */
    case Random(seed: Long)
  }

  object By {

    /** The order written `name` (`posted`, `held`, `tools`, `widest` or `random`), a random
      * one seeded from `fresh`; why not, when it is no order's name.
      */
    def named(name: String, fresh: Fresh^): Either[String, By] = name match {
      case "posted" => Right(Posted)
      case "held" => Right(Held)
      case "tools" => Right(Tools)
      case "widest" => Right(Widest)
      case "random" => Right(Random(MurmurHash3.stringHash(fresh.nonce()).toLong))
      case other => Left(s"--by $other: posted, held, tools, widest or random")
    }
  }

  /** The turns one review shows, in the order shown, and how they were picked (`None` when
    * named).
    */
  final case class Picked private[ReplyReview] (turns: Vector[TurnCase], by: Option[By])

  /** Whether `t` has a reply to review: it replied, or drafted, with something to say. */
  def reviewable(t: TurnCase): Boolean = t.ended match {
    case Ended.Replied(_, passed) => !passed
    case Ended.Failed(_, _) | Ended.Unfinished(_) => false
  }

  /** The turns `ask` asks for among `turns`; [[Ask.Pick]] leaves out those `labelled` holds,
    * and picks fewer than `n` when fewer are left. `Left` when it asks for none or more than
    * [[Cap]], names one twice, or names a turn `turns` does not hold, one without a reply to
    * review ([[reviewable]]), or a case id more than one turn answered.
    */
  def pick(turns: Vector[TurnCase], labelled: ReplyLabels, ask: Ask): Either[String, Picked] =
    ask match {
      case Ask.Named(ids) =>
        if (ids.isEmpty || ids.size > Cap) Left(s"name 1 to $Cap turns, not ${ids.size}")
        else if (ids.distinct.size != ids.size) Left("a turn is named twice")
        else
          Fields
            .each(ids)(named(turns, _))
            .flatMap(found =>
              Either.cond(
                found.map(_.workflow).distinct.size == found.size,
                Picked(found, None),
                "a turn is named twice"
              )
            )
      case Ask.Pick(n, by) =>
        if (n < 1 || n > Cap) Left(s"pick 1 to $Cap turns, not $n")
        else {
          val open =
            turns.filter(t => reviewable(t) && !labelled.turns.contains(t.workflow))
          Right(Picked(ordered(open, by).take(n), Some(by)))
        }
    }

  private def named(turns: Vector[TurnCase], id: String): Either[String, TurnCase] = {
    val found = turns.find(t => WorkflowId.value(t.workflow) == id) match {
      case Some(t) => Right(t)
      case None =>
        CaseId.read(id).left.map(_ => s"$id: no turn and no case id").flatMap { at =>
          turns.filter(t => t.said == Said.Slack(at) && reviewable(t)) match {
            case Vector(t) => Right(t)
            case Vector() => Left(s"$id: no turn answered it with a reply")
            case more =>
              Left(s"$id: answered by ${more.map(t => WorkflowId.value(t.workflow)).mkString(" ")}")
          }
        }
    }
    found.filterOrElse(reviewable, s"$id: no reply to review")
  }

  private def ordered(turns: Vector[TurnCase], by: By): Vector[TurnCase] = {
    val stable = turns.sortBy(t => WorkflowId.value(t.workflow))
    def latest(ts: Vector[TurnCase]) = ts.sortBy(_.started).reverse
    def posted(t: TurnCase) = t.root match {
      case TurnOffer.Root.Addressed | TurnOffer.Root.ByName => true
      case TurnOffer.Root.Heard | TurnOffer.Root.Named =>
        t.speech.exists(_.outcome == Drafted.Kind.Posted)
    }
    by match {
      case By.Posted => latest(stable.filter(posted))
      case By.Held => latest(stable.filter(t => !posted(t)))
      case By.Tools => latest(stable.filter(_.rounds.nonEmpty))
      case By.Widest =>
        stable.sortBy(t =>
          -t.window.fold(0L)(w => (w.parts.map(_.tokens) :+ w.gaps :+ w.own).map(Tokens.value).sum)
        )
      case By.Random(seed) => new scala.util.Random(seed).shuffle(stable)
    }
  }

  /** A turn's reply as the review shows it: its text blocks alone. */
  enum Reply {

    /** An addressed turn's reply, or that of a heard message answered as said to grit. */
    case Replied(text: String)

    /** A heard turn's draft, and what became of it; `None` when the turn recorded no outcome. */
    case Draft(text: String, outcome: Option[Drafted.Kind])
  }

  /** A window part shown beside a reply: the part, where it is, and its text as the model was
    * shown it, its reasoning, tool calls and results left out, cut at [[Cut]] characters
    * (`cut` when it was).
    */
  final case class Record private[ReplyReview] (
      part: Part,
      at: Locator,
      text: String,
      cut: Boolean
  )

  /** One turn as the review shows it: the message it answers, its reply, and at most
    * [[Records]] of its window's parts, best supported first, none of them the turn's own
    * thread's turns.
    */
  final case class Reviewed private[ReplyReview] (
      turn: TurnCase,
      asked: String,
      reply: Reply,
      records: Vector[Record]
  )

  /** `t` as the review shows it, read from `entries`, which hold its conversation's and those
    * its window's nearby sections name, the records' speakers named by `speakers`; with no
    * record unless `records`. `Left` naming the turn when its message holds no words, or its
    * reply is not among `entries`.
    */
  def reviewed(
      t: TurnCase,
      entries: Vector[Entry],
      speakers: Speakers,
      records: Boolean
  ): Either[String, Reviewed] = {
    val ref = TurnRef(t.conversation, t.turn)
    val w = WorkflowId.value(t.workflow)
    val mine = entries
      .filter(e => e.conversationId == t.conversation && e.turnSeq == t.turn)
      .sortBy(e => EntrySeq.value(e.seq))
    def assistant(id: EntryId) = mine
      .find(_.id == id)
      .flatMap(e =>
        e.payload match {
          case Payload.Message(m: Message.Assistant) => Some(m)
          case Payload.Draft(m) => Some(m)
          case _ => None
        }
      )
    for {
      asked <- mine.headOption.flatMap(_.payload.said).toRight(s"$w: its message holds no words")
      reply <- (t.root match {
        case TurnOffer.Root.Addressed | TurnOffer.Root.ByName =>
          assistant(ref.replyId).map(m => Reply.Replied(said(m)))
        case TurnOffer.Root.Heard | TurnOffer.Root.Named =>
          assistant(ref.draftId).map(m => Reply.Draft(said(m), t.speech.map(_.outcome)))
      }).toRight(s"$w: no reply recorded")
    } yield {
      val nearby = mine.find(_.id == Turn.windowId(ref)).map(_.payload) match {
        case Some(Payload.Window(_, _, n, _)) => n
        case _ => Vector.empty
      }
      val shown =
        if (!records) Vector.empty
        else
          t.window
            .fold(Vector.empty[Part])(_.parts)
            .sortBy(p => -p.support.fold(-1.0)(_.value))
            .flatMap(p =>
              for {
                at <- Locator.of(p)
                text <- text(p, nearby, entries, speakers)
              } yield {
                val cut = text.codePointCount(0, text.length) > Cut
                Record(
                  p,
                  at,
                  if (cut) text.substring(0, text.offsetByCodePoints(0, Cut)) else text,
                  cut
                )
              }
            )
            .take(Records)
      Reviewed(t, asked, reply, shown)
    }
  }

  /** `t` as [[reviewed]] shows it, its entries read from `reader`'s database. `Left` naming
    * what could not be read, or why [[reviewed]] refused it.
    */
  def read(reader: Reader^, t: TurnCase, records: Boolean): Either[String, Reviewed] = {
    val w = WorkflowId.value(t.workflow)
    def unread(what: String)(e: StoreError) = s"$w: $what unread: ${kind(e)}"
    val ref = TurnRef(t.conversation, t.turn)
    for {
      all <- reader.all.read(reader.entries.list(t.conversation)).left.map(unread("entries"))
      nearby = all.find(_.id == Turn.windowId(ref)).map(_.payload) match {
        case Some(Payload.Window(_, _, n, _)) if records => n
        case _ => Vector.empty
      }
      near <- reader.all
        .read(Nearby.read(nearby, reader.entries))
        .left
        .map(unread("nearby"))
      named <-
        if (!records) Right(Speakers.none)
        else
          reader.all
            .read(reader.principals.speakers((all ++ near).map(_.id)))
            .left
            .map(unread("speakers"))
      shown <- reviewed(t, all ++ near, named, records)
    } yield shown
  }

  /** `p`'s words as the model was shown them: a person's and grit's text, never reasoning, a
    * tool call or result; `None` when it shows none.
    */
  private def text(
      p: Part,
      nearby: Vector[Nearby],
      entries: Vector[Entry],
      speakers: Speakers
  ): Option[String] = {
    val messages = p.kind match {
      // The thread's own turns are never shown.
      case Part.Kind.Recent | Part.Kind.Recalled => Vector.empty
      case Part.Kind.Record =>
        entries
          .filter(e => e.conversationId == p.conversation && p.seqs.contains(e.seq))
          .sortBy(e => EntrySeq.value(e.seq))
          .flatMap(Shown.of(_, speakers))
      case Part.Kind.Open | Part.Kind.Closed | Part.Kind.Along | Part.Kind.Asked =>
        nearby
          .find(n => n.conversation == p.conversation && n.names == p.seqs)
          .flatMap(Shown.section(_, entries, speakers))
          .toVector
    }
    val words = messages.map {
      case Message.User(t) => t
      case m: Message.Assistant => said(m)
      case Message.ToolResult(_, _, _) => ""
    }
    Some(words.filter(_.trim.nonEmpty).mkString("\n\n")).filter(_.nonEmpty)
  }

  private def said(m: Message.Assistant): String =
    m.blocks.collect { case AssistantBlock.Text(t) => t }.mkString

  private def kind(e: StoreError): String = e match {
    case StoreError.DuplicateId(_) => "duplicate id"
    case StoreError.DatabaseError(_) => "database error"
    case StoreError.Invalid(_) => "invalid"
  }

  /** One review file's content: a capture's turns as picked, written at `at` under
    * [[ReplyGuide.Current]].
    */
  final case class Review private[ReplyReview] (
      capture: String,
      by: Option[By],
      at: Instant,
      cases: Vector[Reviewed]
  )

  /** The review of `picked`, each turn as `read` shows it; the first `Left` `read` gives. */
  def review(capture: String, picked: Picked, at: Instant)(
      read: TurnCase => Either[String, Reviewed]
  ): Either[String, Review] =
    Fields.each(picked.turns)(read).map(Review(capture, picked.by, at, _))

  /** `r` as its file holds it, markdown for a person to read: a header naming the guide, then
    * each case under `## <workflow id>`, its words quoted line by line (`> `), its records
    * each under `### record <n>` with its locator, and its label stub, one line a field:
    * `reply:`, `answer:`, `speak:`, `prefer:` and `note:`, blank.
    */
  def render(r: Review): String = {
    def quoted(text: String) =
      text
        .split("\n", -1)
        .map(l => if (l.isEmpty) ">" else s"> ${l.stripSuffix("\r")}")
        .mkString("\n")
    val picked = r.by match {
      case None => "named"
      case Some(By.Random(seed)) => s"random, seed $seed"
      case Some(other) => other.toString.toLowerCase
    }
    val header = Vector(
      s"# Replies to review: capture ${r.capture}",
      "",
      s"guide: ${ReplyGuide.written(ReplyGuide.Current)}",
      s"picked: $picked",
      s"written: ${r.at}",
      "",
      "Fill in each case's label lines, then run `scripts/eval reply-labels` on this file.",
      "A blank line is no label. The values: reply good, bad or unsure; answer shown and the",
      "records' numbers (shown 1 3), thread, elsewhere, nowhere or not-asked; speak yes or no;",
      "prefer stays blank (this review shows one reply); a note is never read.",
      ""
    )
    val cases = r.cases.map { c =>
      val t = c.turn
      val said = t.said match {
        case Said.Slack(at) => s"slack ${at.written}"
        case Said.Tui(e) => s"tui ${EntryId.value(e)}"
        case Said.Task(e) => s"task ${EntryId.value(e)}"
      }
      val (replyText, replied) = c.reply match {
        case Reply.Replied(text) => (text, "replied")
        case Reply.Draft(text, outcome) =>
          (text, s"drafted, ${outcome.fold("no outcome")(_.toString.toLowerCase)}")
      }
      val records = c.records.zipWithIndex.flatMap { (rec, i) =>
        val support = rec.part.support.fold("none")(s => f"${s.value}%.2f")
        Vector(
          s"### record ${i + 1} · ${Part.Kind.written(rec.part.kind)} · support $support",
          "",
          s"locator: ${Locator.json(rec.at).render()}",
          "",
          quoted(rec.text)
        ) ++ (if (rec.cut) Vector("", s"(cut at $Cut characters)") else Vector.empty) :+ ""
      }
      (Vector(
        s"## ${WorkflowId.value(t.workflow)}",
        "",
        s"said: $said · ${t.root.toString.toLowerCase} · $replied",
        "",
        "**Asked**",
        "",
        quoted(c.asked),
        "",
        "**Reply**",
        "",
        quoted(replyText),
        ""
      ) ++ records ++ Vector("reply:", "answer:", "speak:", "prefer:", "note:", "")).mkString("\n")
    }
    (header.mkString("\n") +: cases).mkString("\n")
  }

  /** The labels a filled review file ([[render]]) holds, by turn, under the guide it names:
    * each case's label lines read, `shown <n>…` as the locators of the records numbered `n`; a
    * case whose every line is blank is left out. `Left` naming the case and field when the file
    * names no guide or one this build does not know, a line is not of its form or is given
    * twice, `prefer` is filled (a review of one reply has nothing to prefer), or `shown` names a
    * record its case did not show.
    */
  def labels(file: String): Either[String, Vector[(WorkflowId, ReplyLabel)]] = {
    // Lines a person's words fill are quoted: only the others are read.
    val lines = file.split("\n").toVector.map(_.stripSuffix("\r")).filterNot(_.startsWith(">"))
    val (head, body) = lines.span(!_.startsWith("## "))
    for {
      guide <- head.collectFirst {
        case l if l.startsWith("guide:") => l.stripPrefix("guide:").trim
      } match {
        case Some(name) => ReplyGuide.read(name)
        case None => Left("the file names no guide")
      }
      cases = split(body)
      read <- Fields.each(cases)((w: String, ls: Vector[String]) => one(guide, w, ls))
    } yield read.flatten
  }

  /** Each case's lines, under its workflow id. */
  private def split(body: Vector[String]): Vector[(String, Vector[String])] = {
    val heads = body.zipWithIndex.collect { case (l, i) if l.startsWith("## ") => (i, l) }
    heads.zip(heads.drop(1).map(_._1) :+ body.size).map { case ((from, head), until) =>
      head.stripPrefix("## ").trim -> body.slice(from + 1, until)
    }
  }

  private val Stub: Vector[String] = Vector("reply", "answer", "speak", "prefer", "note")

  private def one(
      guide: ReplyGuide,
      w: String,
      lines: Vector[String]
  ): Either[String, Option[(WorkflowId, ReplyLabel)]] = {
    def err(why: String) = s"$w: $why"
    // The records' locators, by the number each is shown under.
    val located = lines
      .foldLeft((Option.empty[Int], Vector.empty[(Int, String)])) { case ((at, found), l) =>
        if (l.startsWith("### record "))
          (l.stripPrefix("### record ").takeWhile(_.isDigit).toIntOption, found)
        else if (l.startsWith("locator:"))
          (None, found ++ at.map(_ -> l.stripPrefix("locator:").trim))
        else (at, found)
      }
      ._2
    val stub = lines.flatMap(l =>
      Stub.find(k => l.startsWith(s"$k:")).map(k => k -> l.stripPrefix(s"$k:").trim)
    )
    def value(k: String): Either[String, Option[String]] =
      stub.filter(_._1 == k).map(_._2) match {
        case Vector() => Right(None)
        case Vector(v) => Right(Option.when(v.nonEmpty)(v))
        case _ => Left(err(s"$k: given twice"))
      }
    for {
      locators <- Fields.each(located)((n, json) =>
        scala.util
          .Try(ujson.read(json))
          .toOption
          .toRight(err(s"record $n: its locator is not JSON"))
          .flatMap(Locator.read(_).left.map(err))
          .map(n -> _)
      )
      reply <- value("reply").flatMap(
        optional(_)(v => ReplyLabels.qualityNamed(v).toRight(err(s"reply: $v")))
      )
      answer <- value("answer").flatMap(optional(_)(found(_, locators.toMap).left.map(err)))
      speak <- value("speak").flatMap(optional(_) {
        case "yes" => Right(true)
        case "no" => Right(false)
        case v => Left(err(s"speak: $v"))
      })
      _ <- value("prefer").flatMap {
        case None => Right(())
        case Some(_) => Left(err("prefer: this review shows one reply, nothing to prefer"))
      }
    } yield Option.when(reply.isDefined || answer.isDefined || speak.isDefined)(
      WorkflowId(w) -> ReplyLabel(guide, reply, answer, speak)
    )
  }

  /** `read` of `o`'s value, when there is one. */
  private def optional[A](o: Option[String])(
      read: String => Either[String, A]
  ): Either[String, Option[A]] =
    o.fold(Right(None))(read(_).map(Some(_)))

  private def found(v: String, records: Map[Int, Locator]): Either[String, Found] =
    v.split("[\\s,]+").toList.filter(_.nonEmpty) match {
      case "shown" :: ns =>
        Fields
          .each(ns.toVector)(n =>
            n.toIntOption
              .flatMap(records.get)
              .toRight(s"answer: shown $n names no record shown")
          )
          .flatMap(_.distinct.toList match {
            case first :: rest => Right(Found.Shown(::(first, rest)))
            case Nil => Left("answer: shown names no record")
          })
      case List(name) => ReplyLabels.Placed.get(name).toRight(s"answer: $v")
      case _ => Left(s"answer: $v")
    }
}
