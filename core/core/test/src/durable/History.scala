package grit.core.durable

import scala.jdk.CollectionConverters.*

import grit.core.durable.InMemoryDurable.Outcome
import grit.core.id.WorkflowId

/** A recorded workflow history, as a fixture keeps it: which workflow, which run, under
  * which epoch, and its steps in order. Written by a recorder over [[InMemoryDurable]] or
  * captured from Postgres (`scripts/capture-history.sh`); both write this one shape. A
  * step's JSON has its `output`, or its `error`: the message, `null` for an exception
  * without one; a patch marker has neither. `kept` holds the content-addressed rows the
  * steps name by id (a turn's tool sets and prompt fragments), as the store keeps them
  * forever: a replay reads them back from there; written only when there are any.
  */
final case class History(
    workflow: String,
    id: WorkflowId,
    epoch: String,
    source: String,
    steps: Vector[InMemoryDurable.Step],
    kept: ujson.Obj = ujson.Obj()
)

object History {

  def write(h: History): ujson.Value =
    ujson.Obj(
      "workflow" -> h.workflow,
      "id" -> WorkflowId.value(h.id),
      "epoch" -> h.epoch,
      "source" -> h.source,
      "steps" -> ujson.Arr.from(h.steps.map { s =>
        val outcome = s.outcome match {
          case Outcome.Output(value) => Vector("output" -> ujson.Str(value))
          case Outcome.Threw(message) =>
            Vector("error" -> message.fold[ujson.Value](ujson.Null)(ujson.Str(_)))
          case Outcome.Marker => Vector.empty
        }
        ujson.Obj.from(("name" -> ujson.Str(s.name)) +: outcome)
      })
    ) match {
      case o if h.kept.value.isEmpty => o
      case o => o("kept") = h.kept; o
    }

  def read(v: ujson.Value): Either[String, History] = {
    def str(o: collection.Map[String, ujson.Value], key: String): Either[String, String] =
      o.get(key).flatMap(_.strOpt).toRight(s"expected a string '$key'")
    for {
      o <- v.objOpt.toRight("expected an object")
      workflow <- str(o, "workflow")
      id <- str(o, "id")
      epoch <- str(o, "epoch")
      source <- str(o, "source")
      raw <- o.get("steps").flatMap(_.arrOpt).toRight("expected an array 'steps'")
      steps <- raw.toVector
        .foldLeft[Either[String, Vector[InMemoryDurable.Step]]](Right(Vector.empty)) { (acc, s) =>
          for {
            done <- acc
            so <- s.objOpt.toRight("expected a step object")
            name <- str(so, "name")
            outcome <- (so.get("output"), so.get("error")) match {
              case (Some(ujson.Str(value)), None) => Right(Outcome.Output(value))
              case (None, Some(ujson.Str(message))) => Right(Outcome.Threw(Some(message)))
              case (None, Some(ujson.Null)) => Right(Outcome.Threw(None))
              case (None, None) => Right(Outcome.Marker)
              case _ =>
                Left(
                  s"step '$name': expected a string 'output', a string or null 'error', or neither"
                )
            }
          } yield done :+ InMemoryDurable.Step(name, outcome)
        }
      kept = o.get("kept").flatMap(_.objOpt).fold(ujson.Obj())(k => ujson.Obj.from(k))
    } yield History(workflow, WorkflowId(id), epoch, source, steps, kept)
  }

  /** Every `.json` history in `epoch`'s directory of the test module's recorded histories (the
    * `GRIT_HISTORIES` its build sets), by file name in name order, each as [[read]] reads it:
    * none when the epoch has no directory. Throws when `GRIT_HISTORIES` is unset or a file is
    * not JSON.
    */
  def recorded(epoch: String): Vector[(String, Either[String, History])] = {
    val dir = java.nio.file.Path.of(sys.env("GRIT_HISTORIES"), epoch)
    if (!java.nio.file.Files.isDirectory(dir)) Vector.empty
    else {
      val listed = java.nio.file.Files.list(dir)
      try {
        listed.toList.asScala.toVector
          .map(p => p.getFileName.toString -> p)
          .filter(_._1.endsWith(".json"))
          .sortBy(_._1)
          .map((name, p) => name -> read(ujson.read(java.nio.file.Files.readString(p))))
      } finally listed.close()
    }
  }
}
