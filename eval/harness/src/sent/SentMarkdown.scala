package grit.eval.harness.sent

import java.time.ZoneOffset

import grit.assembly.estimate.CharEstimate
import grit.core.classify.{Answer, Request}
import grit.core.context.{SectionTag, Shown}
import grit.core.id.{
  ConversationId,
  EntryId,
  EntrySeq,
  KnowledgeSourceName,
  QuestionName,
  ToolCallId,
  TurnSeq
}
import grit.core.message.{AssistantBlock, Message, Tokens}
import grit.core.period.Probability
import grit.core.speech.SpeechJson
import grit.core.stitch.{Offered, Placed}
import grit.core.store.Nearby
import grit.core.tool.ToolSetId
import grit.core.triage.Tags
import grit.eval.harness.corpus.Spent
import grit.eval.harness.stats.Mills
import grit.turn.{Turn, TurnJudge, TurnOffer, TurnRecord}

/** How long a tool result the file shows may be. */
enum Cut {

  /** At most `chars` characters, the rest counted and left out. */
  case Within(chars: Int)

  /** Whole. */
  case Whole
}

object Cut {

  /** 4,000 characters: a page of a file. */
  val Default: Cut = Within(4000)
}

/** A recorded turn as markdown for a person to read, text and all. */
object SentMarkdown {

  /** `sent`, read from the database `from`, as markdown: a header (ids, room, when heard,
    * models, calls, its cost in m$, and that the requests are its recorded inputs through this
    * build's code, the epoch it ran under beside this build's, flagged when they differ); how
    * it got there (stitch, triage, speech, retrieval, every ledger row); the request (the
    * system prompt as sent, the tools offered, the last call's messages numbered by role);
    * each call, its usage and whether its rebuilt request's estimate agrees with its ledger
    * row or differs, with both; then the answer and the summary. Tool results are cut as
    * `cut` says; [[TurnRecord.Schemas.Recorded]] is said above the tools.
    */
  def render(sent: TurnSent, from: String, cut: Cut): String = {
    val calls = sent.calls
    val all = sent.requests
    val last = all.lastOption.map(_._3)
    val out = Vector.newBuilder[String]
    def add(lines: String*): Unit = lines.foreach(out += _)

    // Header.
    val cost = sent.spent.map(_._2.usage.costUsd)
    val total =
      if (cost.isEmpty || cost.exists(_.isEmpty)) "unpriced"
      else Mills.figure(cost.flatten.sum)
    val models = sent.spent.map(_._2.model).distinct
    add(
      s"# Turn ${TurnSeq.value(sent.turn.turnSeq)} of ${ConversationId.value(sent.turn.conversationId)}: the context it was sent",
      "",
      s"- Workflow `${grit.core.id.WorkflowId.value(sent.workflow)}`, root ${rootName(sent.root)}",
      s"- Room `${sent.room.written}`" + sent.heard.fold("")(t =>
        s"; first message ${t.atOffset(ZoneOffset.UTC).toLocalDateTime} UTC"
      ),
      s"- Model${if (models.size == 1) "" else "s"} ${models.map(m => s"`$m`").mkString(", ")}; " +
        s"${all.size} call${if (all.size == 1) "" else "s"} of its reply, ${sent.spent.size} ledger rows in all, $total for the turn",
      s"- Rebuilt from `$from`: the turn's recorded inputs (its prompt fragments, tool set, " +
        "pinned profile, window and entries) through this build's code, so texts this build " +
        "adds (labels, gap lines, the topic tag, the last-call note) are this build's.",
      s"- Ran under epoch `${sent.epoch}`; this build is epoch `${Turn.Epoch}`" +
        (if (sent.epoch == Turn.Epoch) "."
         else ": **they differ**, so this build's texts may not be those sent."),
      ""
    )

    // How it got here.
    add("## How it got here", "")
    sent.stitch match {
      case None => add("- Stitch: none recorded for its conversation's first message.")
      case Some(p) =>
        val head = p match {
          case f: Placed.Follows =>
            s"**follows** `${ConversationId.value(f.root)}` at p ${p2(f.p)} (`${f.model}`)"
          case b: Placed.Begins =>
            s"**begins**: its likeliest exchange at p ${p2(b.p)} (`${b.model}`)"
          case u: Placed.Unread => s"**unread**: ${u.why}"
        }
        add(s"- Stitch: $head; offered:")
        p.seen.offered.foreach(o =>
          add(
            s"  - `${ConversationId.value(o.root)}` (${why(o.why)}), p ${o.p.fold("unread")(p2)}"
          )
        )
        add("", "  The state the stitch classifier was sent:", "")
        add(fenced(ujson.write(p.seen.state, indent = 2), "json", "  ")*)
    }
    sent.tags match {
      case None => add("- Triage: no tags on its root.")
      case Some(Tags.Unanswered(why)) => add(s"- Triage: unanswered ($why).")
      case Some(Tags.Weighed(answers, model, _)) =>
        add(
          s"- Triage (`$model`): " + answers.toVector
            .map((q, a) => s"${QuestionName.value(q)} ${answered(a)}")
            .mkString(", ")
        )
    }
    add(s"- Speech: ${speech(sent)}")
    add(
      "- Retrieval: " + (if (sent.queries.isEmpty) "no query written"
                         else
                           sent.queries.map(q => s"query `${q.replace("`", "'")}`").mkString("; "))
    )
    sent.window.foreach(w =>
      add(
        s"- Window: ${w.entries.size} own entr${if (w.entries.size == 1) "y" else "ies"}" +
          s" (seqs ${w.entries.map(EntrySeq.value).mkString(", ")}), " +
          s"${w.recalled.size} recalled turns, ${w.documents.size} documents, " +
          s"${w.nearby.size} nearby sections" +
          (if (w.nearby.isEmpty) "" else ": " + w.nearby.map(nearby).mkString("; "))
      )
    )
    if (sent.givenUp.nonEmpty)
      add(
        "- Tool calls given up on: " + sent.givenUp.toVector
          .sortBy(_._1)
          .map { case ((n, j), g) =>
            s"round $n call $j ${g.toString.toLowerCase}"
          }
          .mkString(", ")
      )
    add(
      "",
      "| Ledger row | For | Model | In | Cached | Out | Cost | Estimate |",
      "|---|---|---|---|---|---|---|---|"
    )
    sent.spent.foreach((e, s) => add(row(e, s)))
    add("")

    // The request.
    add("## The request", "")
    last match {
      case None => add("No call of its reply is kept.", "")
      case Some(request) =>
        add(
          s"### System prompt (${sent.prompt.size} fragments: " +
            sent.prompt.map(f => s"${f.layer.key}/${f.source}").mkString(", ") + ")",
          ""
        )
        add(fenced(request.system, "text")*)
        add("")
        add(s"### Tools offered (recorded set `${ToolSetId.value(sent.toolSet)}`)", "")
        if (calls.schemas == TurnRecord.Schemas.Recorded)
          add(
            "The turn's pair enforced strict schemas, which a tool set does not record: each " +
              "tool below is shown as recorded, not as sent, and no estimate below is that of " +
              "what was sent.",
            ""
          )
        request.tools.foreach(t =>
          add(s"- `${t.name}`: ${t.description.linesIterator.nextOption().getOrElse("")}")
        )
        if (request.tools.nonEmpty) {
          add("", "<details><summary>Definitions as sent</summary>", "")
          add(
            fenced(
              ujson.write(
                ujson.Arr.from(
                  request.tools.map(t =>
                    ujson.Obj(
                      "name" -> t.name,
                      "description" -> t.description,
                      "parameters" -> t.parameters,
                      "strict" -> t.strict
                    )
                  )
                ),
                indent = 2
              ),
              "json"
            )*
          )
          add("", "</details>")
        }
        add("", s"### Messages (the last call's ${request.messages.size})", "")
        request.messages.zipWithIndex.foreach((m, i) => add(message(i + 1, m, cut)*))
    }

    // Each call.
    add("## Each call", "")
    all.foreach { (round, reply, request) =>
      val row = sent.row(reply)
      val rebuilt = CharEstimate.request(request)
      val estimate = (row, sent.agrees(reply, request)) match {
        case (Some(r), Some(true)) =>
          s"estimate agrees with its ledger row (${Tokens.value(r.estimated)})"
        case (Some(r), _) =>
          s"estimate differs (rebuilt ${Tokens.value(rebuilt)}, recorded ${Tokens.value(r.estimated)})"
        case (None, _) => s"no ledger row (rebuilt estimate ${Tokens.value(rebuilt)})"
      }
      val usage = row.fold("")(s =>
        s"`${s.model}`, ${Tokens.value(s.usage.input)} in (${Tokens.value(s.usage.cachedInput)} cached), " +
          s"${Tokens.value(s.usage.output)} out, ${s.usage.costUsd.fold("unpriced")(Mills.figure)}; "
      )
      val tools = request.use match {
        case grit.core.provider.ToolUse.Auto => "tools on"
        case grit.core.provider.ToolUse.Off => "tools off"
      }
      add(s"- **Call $round** (`${EntryId.value(reply)}`, $tools): $usage$estimate.")
      last.foreach { l =>
        val n = request.messages.size
        if (request.system != l.system || request.tools != l.tools)
          add("  Its system prompt or tools differ from the last call's; its request in full:")
        if (
          l.messages.take(
            n
          ) == request.messages && request.system == l.system && request.tools == l.tools
        )
          add(s"  It was sent messages 1–$n above.")
        else {
          add("  Its messages differ from the last call's; in full:", "")
          request.messages.zipWithIndex.foreach((m, i) =>
            add(message(i + 1, m, cut).map("  " + _)*)
          )
        }
      }
    }
    sent.picked.foreach {
      case Picked.Unsure(on, off, recorded) =>
        add(
          "",
          s"Which way the answering call was sent is unsure: with tools on its estimate is " +
            s"${Tokens.value(on)}, with them off (the budget's last call) ${Tokens.value(off)}, and " +
            s"its ledger row recorded ${recorded.fold("none")(r => Tokens.value(r).toString)}. " +
            "It is shown with tools on."
        )
      case Picked.On | Picked.Off => ()
    }
    add("")

    // The answer and the summary.
    sent.answer match {
      case None => add("## Answer", "", "No reply or draft is kept.", "")
      case Some(a) =>
        add(if (a.draft) "## Draft" else "## Reply", "")
        add(fenced(text(a.message), "text")*)
        add("")
    }
    sent.summary.foreach { s =>
      add("## Summary", "")
      add(fenced(s, "text")*)
      add("")
    }
    out.result().mkString("", "\n", "\n")
  }

  /** `heard`, read from the database `from`, as markdown: what Jev was asked of it and what
    * it answered. The stitch state is as recorded, its question as this build words it
    * (flagged when this build's state differs from the recorded one); triage's request is
    * rebuilt, not recorded, and its questions' names are set beside those triage's kept tags
    * answered, a difference flagged.
    */
  def heard(heard: HeardSent, from: String): String = {
    val out = Vector.newBuilder[String]
    def add(lines: String*): Unit = lines.foreach(out += _)
    val sources = heard.sources.all.map(s => s"`${KnowledgeSourceName.value(s.name)}`")
    add(
      s"# What Jev was asked of `${EntryId.value(heard.entry)}`",
      "",
      s"- Conversation `${ConversationId.value(heard.turn.conversationId)}`, turn ${TurnSeq.value(heard.turn.turnSeq)}; read from `$from`",
      "- The stitch request's state is as recorded with its placement; its question is as this " +
        "build words it.",
      "- Triage's request is **rebuilt, not recorded**: the shipped builder over the database as " +
        s"it stands now, worded for persona `${heard.persona.name}` with knowledge sources " +
        (if (sources.isEmpty) "none" else sources.mkString(", ")) +
        " (the deployment's, which the database does not keep).",
      ""
    )
    add("## Stitch", "")
    heard.stitch match {
      case None =>
        add(
          "No placement is kept: the message is not its conversation's first, or it was not stitched.",
          ""
        )
      case Some(p) =>
        val verdict = p match {
          case f: Placed.Follows =>
            s"**follows** `${ConversationId.value(f.root)}` at p ${p2(f.p)} (`${f.model}`)"
          case b: Placed.Begins =>
            s"**begins**: its likeliest exchange at p ${p2(b.p)} (`${b.model}`)"
          case u: Placed.Unread => s"**unread**: ${u.why}"
        }
        val t = p.seen.tuning
        add(
          s"Jev's answer: $verdict.",
          "",
          s"Tuning in force: follows at ${p2(t.followsAt)}, ${t.recent} recent and ${t.lexical} lexical offered, " +
            s"horizon ${t.horizon}.",
          "",
          "Each exchange offered, and the probability Jev gave it:",
          ""
        )
        p.seen.offered.foreach(o =>
          add(s"- `${ConversationId.value(o.root)}` (${why(o.why)}): p ${o.p.fold("unread")(p2)}")
        )
        add("", "The state Jev was sent, as recorded:", "")
        add(fenced(ujson.write(p.seen.state, indent = 2), "json")*)
        add("")
        heard.stitchAsked match {
          case None =>
            add("No stitch question is offered for it now, so its words are not shown.", "")
          case Some(r) =>
            add(
              heard.stitchSame match {
                case Some(true) => "This build rebuilds the same state."
                case _ =>
                  "**This build rebuilds a different state** (the room has moved on, or the builder changed); the recorded one above is what was sent."
              },
              "",
              "The question, as this build words it:",
              ""
            )
            add(fenced(ujson.write(Request.json(r)("questions"), indent = 2), "json")*)
            add("")
        }
    }
    add("## Triage", "")
    val answered = heard.tags match {
      case Some(Tags.Weighed(answers, model, _)) =>
        add(s"Jev's answers (`$model`):", "")
        answers.toVector.foreach((q, a) =>
          add(s"- `${QuestionName.value(q)}`: ${this.answered(a)}")
        )
        add("")
        answers.keys.toVector
      case Some(Tags.Unanswered(why)) =>
        add(s"Triage kept no answers: $why.", "")
        Vector.empty
      case None =>
        add("Triage kept no tags for it.", "")
        Vector.empty
    }
    if (answered.nonEmpty && answered.toSet != heard.asked.toSet)
      add(
        "**The rebuilt questions are not those triage answered**: only answered " +
          names((answered.toSet -- heard.asked).toVector) + "; only rebuilt " +
          names((heard.asked.toSet -- answered).toVector) +
          ". Pass the deployment's persona and knowledge sources (`--corpus`).",
        ""
      )
    heard.triage match {
      case Left(why) => add(s"The builder makes no triage request now: $why.", "")
      case Right(r) =>
        add("The request, rebuilt:", "")
        add(fenced(ujson.write(Request.json(r), indent = 2), "json")*)
        add("")
    }
    out.result().mkString("", "\n", "\n")
  }

  /** `text` in a fence of `info`, longer than any run of backticks in it, each line after
    * `indent`.
    */
  def fenced(text: String, info: String, indent: String = ""): Vector[String] = {
    val longest = "`+".r.findAllIn(text).map(_.length).maxOption.getOrElse(0)
    val fence = "`" * math.max(3, longest + 1)
    ((s"$fence$info" +: text.split("\n", -1).toVector) :+ fence).map(indent + _)
  }

  /** Message `n`, `m`, as a numbered block: its role and what it is, then its text. */
  private def message(n: Int, m: Message, cut: Cut): Vector[String] = m match {
    case Message.User(t) =>
      Vector("", s"**$n · user** (${kind(t)})") ++ fenced(t, "text")
    case Message.Assistant(blocks, _, _, _, _) =>
      val said = blocks.collect { case AssistantBlock.Text(t) => t }.mkString
      val calls = blocks.collect { case AssistantBlock.ToolCall(id, name, args) =>
        s"- calls `$name` (`${ToolCallId.value(id)}`) with `${ujson.write(args).replace("`", "'")}`"
      }
      Vector("", s"**$n · assistant**") ++
        (if (said.isEmpty) Vector.empty else fenced(said, "text")) ++ calls
    case Message.ToolResult(id, content, isError) =>
      val shown = cut match {
        // Counted in characters (code points), as a database counts them, never split.
        case Cut.Within(chars) if content.codePointCount(0, content.length) > chars =>
          val total = content.codePointCount(0, content.length)
          fenced(content.substring(0, content.offsetByCodePoints(0, chars)), "text") :+
            s"… $total chars, ${total - chars} not shown; `--full` shows all."
        case _ => fenced(content, "text")
      }
      Vector(
        "",
        s"**$n · tool result** for `${ToolCallId.value(id)}`${if (isError) ", an error" else ""}"
      ) ++ shown
  }

  /** What a user message the turn was sent is: the noun of the [[SectionTag]] it starts with, a heard message by the line [[Shown.of]] puts above it, else a message.
    */
  private def kind(text: String): String =
    SectionTag.values.find(l => text.startsWith(l.tag)) match {
      case Some(tag) => tag.noun
      case None =>
        if (text.linesIterator.nextOption().exists(_.endsWith(Shown.NotToYou))) "a heard message"
        else "a message"
    }

  private def text(m: Message.Assistant): String =
    m.blocks.collect { case AssistantBlock.Text(t) => t }.mkString

  private def rootName(root: TurnOffer.Root): String = root match {
    case TurnOffer.Root.Addressed => "addressed (said to grit)"
    case TurnOffer.Root.Heard => "heard (drafted unprompted)"
    case TurnOffer.Root.Named => "named (drafted, unjudged)"
    case TurnOffer.Root.ByName => "by name (heard, answered as said to grit)"
  }

  private def speech(sent: TurnSent): String = {
    val decided = sent.root match {
      case TurnOffer.Root.Addressed => "no speech decision (said to grit)"
      case TurnOffer.Root.ByName => "answering: put to grit by name, run as an addressed turn"
      case TurnOffer.Root.Heard => "drafting, judged before posting"
      case TurnOffer.Root.Named => "drafting, named, unjudged"
    }
    val outcome = sent.speech.fold("")(o => s"; outcome ${SpeechJson.outcomeName(o)}")
    val judged = sent.judged.fold("") {
      case TurnJudge.Judgement.Scored(j, _) =>
        s"; judged grounded ${p2(j.grounded)}, worth ${p2(j.worth)} (`${j.model}`)"
      case TurnJudge.Judgement.Passed => "; the draft passed, nothing judged"
      case TurnJudge.Judgement.NothingRecalled => "; nothing recalled, nothing judged"
      case TurnJudge.Judgement.Unjudged(why) => s"; unjudged ($why)"
    }
    decided + outcome + judged
  }

  private def nearby(n: Nearby): String = n match {
    case Nearby.Open(_, place, seqs) => s"open at `${place.written}` (${seqs.size} entries)"
    case Nearby.Along(_, place, seqs) => s"strand at `${place.written}` (${seqs.size} entries)"
    case Nearby.Asked(_, place, seqs) => s"asked at `${place.written}` (${seqs.size} entries)"
    case Nearby.Closed(_, place, _) => s"closed at `${place.written}` (its record)"
  }

  private def why(o: Offered): String = o match {
    case Offered.Recent(rank) => s"recent #$rank"
    case Offered.Lexical(score) => f"lexical $score%.2f"
  }

  private def answered(a: Answer): String = a match {
    case Answer.YesNo(yes) => f"$yes%.2f"
    case Answer.Choice(choice, weights, confidence) =>
      f"$choice (confidence $confidence%.2f; " +
        weights.map(w => f"${w.key} ${w.probability}%.2f").mkString(", ") + ")"
  }

  private def names(ns: Vector[QuestionName]): String =
    if (ns.isEmpty) "none" else ns.map(n => s"`${QuestionName.value(n)}`").mkString(", ")

  private def p2(p: Probability): String = f"${Probability.value(p)}%.2f"

  private def row(entry: EntryId, s: Spent): String = {
    val role = s.role.fold("—") {
      case TurnRecord.Role.Round(n) => s"call $n"
      case other => other.toString.toLowerCase
    }
    s"| `${EntryId.value(entry)}` | $role | `${s.model}` | ${Tokens.value(s.usage.input)} | " +
      s"${Tokens.value(s.usage.cachedInput)} | ${Tokens.value(s.usage.output)} | " +
      s"${s.usage.costUsd.fold("unpriced")(Mills.figure)} | ${Tokens.value(s.estimated)} |"
  }
}
