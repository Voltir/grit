package grit.eval.harness.label

import scala.util.Try

import grit.core.triage.Kind
import grit.eval.harness.corpus.{CaseId, Fields}

/** What a person labelled one case: each field `None` until labelled.
  *
  * @param fact
  *   whether it states a fact worth keeping (`some`) or not (`none`)
  * @param place
  *   for a thread's first message that was placed: what it should have become
  */
final case class Labelled(
    kind: Option[Kind],
    waiting: Option[Boolean],
    durable: Option[Boolean],
    helps: Option[Boolean],
    context: Option[Context],
    fact: Option[Boolean],
    place: Option[Place]
)

object Labelled {

  /** Nothing labelled. */
  val Blank: Labelled = Labelled(None, None, None, None, None, None, None)
}

/** Whether a careful person could answer correctly from exactly the input the classifier was
  * given: an error on an `Ok` case is the model's, on a `Short` one the input builder's.
  */
enum Context {
  case Ok, Short
}

object Context {

  /** `c`'s written name: `ok` or `short`. */
  def written(c: Context): String = c.toString.toLowerCase
}

/** Where a thread's first message belongs. */
enum Place {

  /** It begins an exchange of its own. */
  case Begins

  /** It continues the exchange the case `root` began. */
  case Follows(root: CaseId)
}

/** A corpus's labels, by case, and every label-guide version they were made under. */
final case class Labels(cases: Map[CaseId, Labelled], guides: Set[String]) {

  /** `id`'s labels; [[Labelled.Blank]] when it has none. */
  def of(id: CaseId): Labelled = cases.getOrElse(id, Labelled.Blank)
}

object Labels {

  /** No case labelled. */
  val Empty: Labels = Labels(Map.empty, Set.empty)

  /** The labels `text` holds, as the labelling tool writes `labels.json`: `{"cases": {<case
    * id>: {<field>: {"value", "guide", "at"}}}}`, the fields `kind` (a kind's written name),
    * `waiting`, `durable` and `helps` (booleans), `context` (`ok` or `short`), `fact` (`some` or
    * `none`) and `place` (`begins` or a case id). Why not, naming the case and field, when it
    * is not JSON of that form, or names another field.
    */
  def read(text: String): Either[String, Labels] =
    for {
      root <- Try(ujson.read(text)).toOption.toRight("labels.json: not JSON")
      cases <- Fields("labels.json", root).obj("cases")
      read <- Fields.each(cases.obj.toVector) { (written, fields) =>
        for {
          id <- CaseId.read(written).left.map(why => s"labels.json: $why")
          each <- Fields.each(fields.objOpt.fold(Vector.empty)(_.toVector)) { (name, v) =>
            val at = Fields(s"labels.json: $written: $name", v)
            for {
              value <- at.field("value")
              guide <- at.str("guide")
              set <- field(name, value).toRight(s"labels.json: $written: $name is not of its form")
            } yield (set, guide)
          }
        } yield (id, each.map(_._1).foldLeft(Labelled.Blank)(merge), each.map(_._2))
      }
    } yield Labels(read.map((id, l, _) => id -> l).toMap, read.flatMap(_._3).toSet)

  /** `Labelled` with only field `name` set, to `v`; `None` when `name` is no field's, or `v`
    * is not of its form.
    */
  private def field(name: String, v: ujson.Value): Option[Labelled] = {
    val b = Labelled.Blank
    def yesNo = v.boolOpt
    name match {
      case "kind" => v.strOpt.flatMap(Kind.read).map(k => b.copy(kind = Some(k)))
      case "waiting" => yesNo.map(y => b.copy(waiting = Some(y)))
      case "durable" => yesNo.map(y => b.copy(durable = Some(y)))
      case "helps" => yesNo.map(y => b.copy(helps = Some(y)))
      case "context" =>
        v.strOpt
          .flatMap(s => Context.values.find(Context.written(_) == s))
          .map(c => b.copy(context = Some(c)))
      case "fact" =>
        v.strOpt
          .collect { case "some" => true; case "none" => false }
          .map(f => b.copy(fact = Some(f)))
      case "place" =>
        v.strOpt
          .flatMap {
            case "begins" => Some(Place.Begins)
            case root => CaseId.read(root).toOption.map(Place.Follows(_))
          }
          .map(p => b.copy(place = Some(p)))
      case _ => None
    }
  }

  /** Each field of `a`, else of `b`. */
  private def merge(a: Labelled, b: Labelled): Labelled = Labelled(
    a.kind.orElse(b.kind),
    a.waiting.orElse(b.waiting),
    a.durable.orElse(b.durable),
    a.helps.orElse(b.helps),
    a.context.orElse(b.context),
    a.fact.orElse(b.fact),
    a.place.orElse(b.place)
  )
}
