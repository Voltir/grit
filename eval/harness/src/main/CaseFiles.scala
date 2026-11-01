package grit.eval.harness.main

import java.nio.file.Path

import scala.util.Try

import grit.core.either.Traversal
import grit.core.id.WorkflowId
import grit.core.tool.ToolSet
import grit.dbos.internal.Reader
import grit.eval.harness.capture.{CaptureJson, Case, TurnCase, TurnJson}

import Cli.*

/** A capture's cases as the commands read them back. */
private[main] object CaseFiles {

  /** The entries of the set `t` draws its tools from ([[Offered.drawn]]), read through
    * `reader`; none when it recorded no offer.
    */
  private[main] def drawn(reader: Reader^, t: TurnCase): Either[String, Vector[ToolSet.Entry]] =
    t.offered.fold[Either[String, Vector[ToolSet.Entry]]](Right(Vector.empty))(o =>
      reader.all
        .read(reader.toolSets.get(o.drawn))
        .map(_.tools)
        .left
        .map(e => s"tool set of ${WorkflowId.value(t.workflow)} unread: ${e.kind}")
    )

  /** The turns of the capture in `dir`, from `turns.jsonl`. */
  private[main] def readTurns(dir: Path): Either[String, Vector[TurnCase]] =
    read(dir.resolve("turns.jsonl")).flatMap(text =>
      Traversal.traverse(text.linesIterator.filter(_.nonEmpty).toVector)(l =>
        Try(ujson.read(l)).toOption
          .toRight("turns.jsonl: a line is not JSON")
          .flatMap(TurnJson.read)
      )
    )

  /** The cases of the capture in `dir`. */
  private[main] def readCases(dir: Path): Either[String, Vector[Case]] =
    read(dir.resolve("cases.jsonl")).flatMap(text =>
      Traversal.traverse(text.linesIterator.filter(_.nonEmpty).toVector)(l =>
        Try(ujson.read(l)).toOption
          .toRight("cases.jsonl: a line is not JSON")
          .flatMap(CaptureJson.readCase)
      )
    )
}
