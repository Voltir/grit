package grit.tools

import grit.core.host.{Argv, Clipped}
import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message}
import grit.core.store.{CheckoutState, Entry, Payload, WentOn}
import grit.prose.words.Words

/** The last command a turn ran that failed, read from the answers its `run` and `tail` calls
  * recorded (ADR 0058).
  */
object FailedRuns {

  /** The last command among `entries`, a turn's, that a `run` or `tail` answer says ended in
    * failure (an exit code other than 0, or killed at its timeout) with a line that looks like a
    * failure as `run` flags one, its lines picked as `run` picks a long output's: each such line,
    * then each that looks like a warning, with the 3 after each, within
    * [[CheckoutState.FailedBytes]], a last line counting those left out. Picked from what the
    * answer showed: all of a short output; of a long one, the lines its answer flagged, with
    * those after each, and its tail, so a flagged line it counted as not shown stays out. A
    * failure a later answer says the same command passed (exit 0) after is passed over, as is
    * one of `except`'s command (the task's check, whose own output its note shows); `None` when
    * none is left. A `run` given a shell line rather than a program is not read.
    */
  def last(entries: Vector[Entry], except: Option[WentOn.Command]): Option[CheckoutState.Failed] = {
    val calls: Map[ToolCallId, AssistantBlock.ToolCall] = entries
      .flatMap(_.payload match {
        case Payload.Message(a: Message.Assistant) =>
          a.blocks.collect { case c: AssistantBlock.ToolCall => c.id -> c }
        case _ => Vector.empty
      })
      .toMap
    val runs = entries.flatMap(_.payload match {
      case Payload.Message(Message.ToolResult(id, content, _)) =>
        calls.get(id).flatMap(endedOf(_, content))
      case _ => None
    })
    runs.zipWithIndex.reverseIterator.collectFirst {
      case ((command, Some(failed)), i)
          if !except.exists(_.text == command) &&
            !runs.drop(i + 1).exists { case (c, outcome) => c == command && outcome.isEmpty } =>
        failed
    }
  }

  /** The command a call ran and, when its answer `content` says it ended, `None` when it passed
    * and its failure when it failed with a line looking like one; nothing when it did not end.
    */
  private def endedOf(
      call: AssistantBlock.ToolCall,
      content: String
  ): Option[(String, Option[CheckoutState.Failed])] = {
    val ran: Option[(String, String)] = call.name match {
      case "run" =>
        argv(call.arguments).map((_, content))
      case "tail" =>
        content.split("\n", 2).toList match {
          case first :: body :: Nil
              if first.startsWith("`") && first.endsWith("`") && first.length > 1 =>
            Some((first.drop(1).dropRight(1), body))
          case _ => None
        }
      case _ => None
    }
    ran.flatMap { (command, body) =>
      val exit =
        if (body.startsWith(Coding.ExitLead))
          body.drop(Coding.ExitLead.length).takeWhile(_.isDigit).toIntOption.map(Some(_))
        else if (body.startsWith(Coding.KilledLead)) Some(None)
        else None
      exit.map {
        case Some(0) => (command, None)
        case code => (command, failure(command, code, body))
      }
    }
  }

  /** The lines of the answer `body` `run` flags, when one looks like a failure. */
  private def failure(
      command: String,
      exit: Option[Int],
      body: String
  ): Option[CheckoutState.Failed] = {
    val shown = RunAnswer.shown(body)
    Option.when(shown.exists((_, l) => RunAnswer.Failing.findFirstIn(Clipped.plain(l)).nonEmpty)) {
      val (chosen, unshown) = RunAnswer.picked(shown, CheckoutState.FailedBytes)
      val more =
        if (unshown == 0) "" else s"\nNot shown: ${Words.counted(unshown, "more such line")}."
      CheckoutState.Failed.of(command, exit, RunAnswer.listed(chosen) + more)
    }
  }

  /** The program and arguments a `run` call was given, as a person would type them. */
  private def argv(arguments: ujson.Value): Option[String] = arguments match {
    case o: ujson.Obj =>
      for {
        program <- o.value.get("program").collect { case ujson.Str(p) => p }
        args <- o.value.get("args") match {
          case None | Some(ujson.Null) => Some(Vector.empty)
          case Some(ujson.Arr(items)) =>
            val words = items.toVector.collect { case ujson.Str(w) => w }
            Option.when(words.size == items.size)(words)
          case Some(_) => None
        }
        written <- Argv.of(program, args*).toOption.map(_.written)
      } yield written
    case _ => None
  }
}
