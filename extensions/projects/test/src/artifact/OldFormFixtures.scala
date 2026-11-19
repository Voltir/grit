package grit.projects.artifact

/** Artifacts of the old form (ADR 0063), as the store held them before ids, for the suites of
  * the re-key and of a project re-keyed.
  */
object OldFormFixtures {

  /** The old form's data of a note, open, titled `title`, under `parent`, citing `cites` in its
    * head and linking each in its body, which reads `says` then the links then a full stop.
    */
  def data(
      title: String,
      parent: Option[String] = None,
      cites: Vector[String] = Vector.empty,
      says: String = "See "
  ): ujson.Value = {
    val spans = ujson.Obj("t" -> says) +: cites.zipWithIndex.map { (c, i) =>
      ujson.Obj("t" -> c.drop(c.indexOf('/') + 1), "n" -> ujson.Arr(i))
    } :+ ujson.Obj("t" -> ".")
    ujson.Obj(
      "head" -> ujson.Obj(
        "kind" -> "note",
        "state" -> "open",
        "title" -> title,
        "parent" -> parent.fold[ujson.Value](ujson.Null)(ujson.Str(_)),
        "cites" -> ujson.Arr.from(cites.map(ujson.Str(_))),
        "by" -> "nick"
      ),
      "body" -> ujson.Obj(
        "v" -> 1,
        "notes" -> ujson.Arr.from(cites.map(ujson.Str(_))),
        "blocks" -> ujson.Arr(ujson.Obj("type" -> "para", "text" -> ujson.Arr.from(spans)))
      )
    )
  }
}
