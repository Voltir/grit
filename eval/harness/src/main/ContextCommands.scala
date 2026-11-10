package grit.eval.harness.main

import java.nio.file.{Files, Path}

import grit.assembly.estimate.CharEstimate
import grit.assembly.linear.LinearAssembler
import grit.core.clock.Clock
import grit.core.id.{ConversationId, WorkflowId}
import grit.core.message.Tokens
import grit.core.persona.Persona
import grit.core.triage.Corpora
import grit.dbos.sql.DbConfig
import grit.eval.harness.capture.{KnowledgeJson, PersonaJson}
import grit.eval.harness.sent.{
  ConversationSent,
  Cut,
  HeardSent,
  Redrawn,
  SentMarkdown,
  TaskLines,
  TaskTurn,
  TurnSent
}
import grit.turn.Turn

import Cli.*

/** What a model was sent, for a person to read: `context`. */
private[main] object ContextCommands {

  /** `context`: one recorded turn's request as sent, text and all, written to `--out` for a
    * person to read, or with `--conversation` every turn of it ([[conversationContext]]);
    * prints counts only.
    */
  private[main] def context(
      f: Map[String, String],
      full: Boolean,
      clock: Clock^
  ): Either[String, Unit] =
    if (f.contains("entry")) heardContext(f)
    else if (f.contains("conversation")) conversationContext(f, full, clock)
    else turnContext(f, full)

  /** `context` of every turn of a conversation (`--conversation`), recorded before `clock`'s
    * now: what the task's turns held in the cache ([[TaskLines]]), printed and written to
    * `--out`; with `--redraw <tokens>`, each boundary's first call beside its window drawn
    * afresh by this build's linear assembler at a limit of that many tokens ([[Redrawn]]), its
    * table printed and each turn's redrawn request written; then each turn as
    * [[SentMarkdown.render]] writes it.
    */
  private def conversationContext(
      f: Map[String, String],
      full: Boolean,
      clock: Clock^
  ): Either[String, Unit] =
    for {
      url <- need(f, "url")
      from <- need(f, "from")
      conversation <- need(f, "conversation").map(ConversationId(_))
      out <- need(f, "out").map(Path.of(_))
      redraw <- f.get("redraw") match {
        case None => Right(None)
        case Some(raw) =>
          raw.toLongOption
            .filter(_ > 0)
            .map(n => Some(Tokens(n)))
            .toRight(s"--redraw is not a whole number above 0: $raw")
      }
      config <- DbConfig.fromEnv(sys.env.updated(DbConfig.UrlVar, url)).left.map(_.message)
      read <- opened[(ConversationSent, Vector[Redrawn])](config)(reader =>
        ConversationSent
          .read(reader, conversation, clock.now())
          .flatMap(sent =>
            redraw
              .fold[Either[String, Vector[Redrawn]]](Right(Vector.empty))(limit =>
                Redrawn.read(
                  reader,
                  sent,
                  new LinearAssembler(
                    reader.entries,
                    reader.periods,
                    reader.principals,
                    CharEstimate,
                    limit
                  )
                )
              )
              .map(sent -> _)
          )
      )
      sent = read._1
      redrawn: Vector[Redrawn] = read._2
      lines = TaskLines.lines(TaskTurn.all(sent))
      table = if (redraw.isEmpty) Vector.empty else Redrawn.table(redrawn)
      _ <- mkdirs(out.getParent)
      cut = if (full) Cut.Whole else Cut.Default
      _ <- write(
        out,
        (lines.map(l => s"- $l") ++ table.headOption.map(_ => table.mkString("\n")) ++
          redrawn.map(Redrawn.markdown(_).mkString("\n")) ++
          sent.turns.map(SentMarkdown.render(_, from, cut)))
          .mkString("\n\n")
      )
    } yield {
      println(s"written: $out")
      lines.foreach(println)
      table.foreach(println)
    }

  /** `context` of a heard message (`--entry`): what Jev was asked of it, its triage request
    * worded for the persona and corpora in `--declared` (a directory holding a hand-written
    * `persona.json` and `knowledge.json`), else grit's and none.
    */
  private def heardContext(f: Map[String, String]): Either[String, Unit] = {
    val declared = f.get("declared").map(Path.of(_))
    def file[A](name: String, none: A, parse: String => Either[String, A]): Either[String, A] =
      declared.map(_.resolve(name)).filter(Files.exists(_)) match {
        case Some(at) => read(at).flatMap(parse).left.map(why => s"$at: $why")
        case None => Right(none)
      }
    for {
      url <- need(f, "url")
      from <- need(f, "from")
      entry <- need(f, "entry").map(grit.core.id.EntryId(_))
      out <- need(f, "out").map(Path.of(_))
      persona <- file("persona.json", Persona.Grit, PersonaJson.read)
      sources <- file("knowledge.json", Corpora.Empty, KnowledgeJson.read)
      config <- DbConfig.fromEnv(sys.env.updated(DbConfig.UrlVar, url)).left.map(_.message)
      heard <- opened(config)(HeardSent.read(_, entry, persona, sources))
      _ <- mkdirs(out.getParent)
      _ <- write(out, SentMarkdown.heard(heard, from))
    } yield {
      println(s"written: $out")
      println(
        s"stitch: ${if (heard.stitch.isDefined) "placement kept" else "none"}; triage: " +
          s"${heard.triage.fold(_ => "not rebuilt", _ => "rebuilt")}, ${heard.asked.size} questions, " +
          s"tags ${if (heard.tags.isDefined) "kept" else "none"}"
      )
    }
  }

  /** `context` of a turn (`--workflow`), held against `--limit` input tokens when given. */
  private def turnContext(f: Map[String, String], full: Boolean): Either[String, Unit] =
    for {
      url <- need(f, "url")
      from <- need(f, "from")
      workflow <- need(f, "workflow").map(WorkflowId(_))
      out <- need(f, "out").map(Path.of(_))
      limit <- f.get("limit") match {
        case None => Right(None)
        case Some(raw) =>
          raw.toLongOption
            .filter(_ > 0)
            .map(n => Some(Tokens(n)))
            .toRight(s"--limit is not a whole number above 0: $raw")
      }
      config <- DbConfig.fromEnv(sys.env.updated(DbConfig.UrlVar, url)).left.map(_.message)
      sent <- opened(config)(TurnSent.read(_, workflow))
      _ <- mkdirs(out.getParent)
      _ <- write(out, SentMarkdown.render(sent, from, if (full) Cut.Whole else Cut.Default))
    } yield {
      val requests = sent.requests
      val agree = requests.count((_, reply, request) => sent.agrees(reply, request).contains(true))
      val shares = requests.flatMap((_, reply, request) => sent.share(reply, request))
      val range =
        if (shares.isEmpty) "no input counted"
        else s"estimate over input ${shares.min} to ${shares.max}"
      println(s"written: $out")
      println(
        s"calls: ${requests.size}, whose rebuilt estimate is the one its row recorded: $agree; " +
          s"$range; " +
          s"epoch ${sent.epoch}${
              if (Turn.epochOf(sent.epoch) == Turn.Epoch) "" else s" (this build: ${Turn.Epoch})"
            }"
      )
      SentMarkdown.efficiencyLines(sent).foreach(println)
      limit.foreach(l => SentMarkdown.limitLines(sent, l).foreach(println))
    }
}
