package grit.core

/** A recorded workflow history, as a fixture keeps it: which workflow, which run, under
  * which epoch, and its steps in order. Written by a recorder over [[InMemoryDurable]] or
  * captured from Postgres (`scripts/capture-history.sh`); both write this one shape.
  */
final case class History(
    workflow: String,
    id: WorkflowId,
    epoch: String,
    source: String,
    steps: Vector[InMemoryDurable.Step]
)

object History {

  def write(h: History): ujson.Value =
    ujson.Obj(
      "workflow" -> h.workflow,
      "id" -> WorkflowId.value(h.id),
      "epoch" -> h.epoch,
      "source" -> h.source,
      "steps" -> ujson.Arr.from(h.steps.map { s =>
        val fields = Vector("name" -> ujson.Str(s.name)) ++
          s.output.map(o => "output" -> ujson.Str(o)) ++
          s.error.map(e => "error" -> ujson.Str(e))
        ujson.Obj.from(fields)
      })
    )

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
          } yield done :+ InMemoryDurable.Step(
            name,
            so.get("output").flatMap(_.strOpt),
            so.get("error").flatMap(_.strOpt)
          )
        }
    } yield History(workflow, WorkflowId(id), epoch, source, steps)
  }
}
