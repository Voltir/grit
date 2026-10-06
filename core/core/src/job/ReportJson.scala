package grit.core.job

/** A [[Report]]'s stored form, a schedule row's `report`: `{"kind":"kept"}`,
  * `{"kind":"posted","address":…}`.
  */
object ReportJson {

  def write(report: Report): ujson.Value = report match {
    case Report.Kept => ujson.Obj("kind" -> "kept")
    case Report.Posted(address) => ujson.Obj("kind" -> "posted", "address" -> address)
  }

  /** The report stored as `v` ([[write]]'s form), or why it is none. */
  def read(v: ujson.Value): Either[String, Report] = {
    def str(o: collection.Map[String, ujson.Value], key: String): Either[String, String] =
      o.get(key).flatMap(_.strOpt).toRight(s"a report has no $key")
    for {
      o <- v.objOpt.toRight("a report is not an object")
      kind <- str(o, "kind")
      report <- kind match {
        case "kept" => Right(Report.Kept)
        case "posted" => str(o, "address").map(Report.Posted(_))
        case other => Left(s"no report $other")
      }
    } yield report
  }
}
