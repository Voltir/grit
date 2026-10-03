package grit.eval.harness.reply

import scala.collection.immutable.VectorMap
import scala.util.Try

import grit.core.id.WorkflowId
import grit.core.place.Service
import grit.eval.harness.corpus.{Fields, Parts}
import grit.eval.harness.label.{Found, Locator, ReplyLabels}

/** What a turn's build is expected to do, by ids alone. */
enum Expect {

  /** Its window shows what `at` names ([[Locator.held]]). */
  case Holds(at: Locator)

  /** Its window shows none of what `at` names ([[Locator.anyHeld]]). */
  case Omits(at: Locator)

  /** It is offered a tool of `service`'s. */
  case Offers(service: Service)

  /** It is offered no tool of `service`'s. */
  case Withholds(service: Service)
}

/** How a turn's expectations stood under one variant. */
enum Judged {

  /** Every one held. */
  case Pass

  /** One did not. */
  case Fail

  /** None failed, and one could not be judged: it expects a window that was not rebuilt. */
  case Unjudged
}

/** Expectations of turns, by the workflow of each, in the order given. */
final case class Reference(turns: VectorMap[WorkflowId, Vector[Expect]]) {

  /** These, with `other`'s after them, a turn in both expected to do both. */
  def ++(other: Reference): Reference =
    Reference(other.turns.foldLeft(turns) { case (acc, (w, es)) =>
      acc.updated(w, acc.getOrElse(w, Vector.empty) ++ es)
    })
}

object Reference {

  /** No expectation. */
  val Empty: Reference = Reference(VectorMap.empty)

  /** What `labels` expect: of each turn whose answer a person found in records the review
    * showed, that its window holds each of them; in workflow id order.
    */
  def labelled(labels: ReplyLabels): Reference =
    Reference(
      VectorMap.from(labels.turns.toVector.sortBy((w, _) => WorkflowId.value(w)).flatMap { (w, l) =>
        l.answer.collect { case Found.Shown(at) => w -> at.map(Expect.Holds(_)).toVector }
      })
    )

  /** The expectations `text` holds: `{"turns": {<workflow id>: [{"holds": <locator>} |
    * {"omits": <locator>} | {"offers": <service>} | {"withholds": <service>}, …]}}`, a locator as
    * [[Locator.json]] writes it and a service by its name. Why not, naming the turn, when it
    * is not of that form.
    */
  def read(text: String): Either[String, Reference] =
    for {
      root <- Try(ujson.read(text)).toOption.toRight("reference: not JSON")
      turns <- Fields("reference", root).obj("turns")
      each <- Fields.each(turns.obj.toVector) { (w, v) =>
        val what = s"reference: $w"
        v.arrOpt
          .toRight("not a list")
          .flatMap(es =>
            Fields.each(es.toVector) { e =>
              e.objOpt.map(_.toVector) match {
                case Some(Vector(("holds", l))) => Locator.read(l).map(Expect.Holds(_))
                case Some(Vector(("omits", l))) => Locator.read(l).map(Expect.Omits(_))
                case Some(Vector(("offers", s))) =>
                  Fields.str("offers", s).flatMap(Service.of).map(Expect.Offers(_))
                case Some(Vector(("withholds", s))) =>
                  Fields.str("withholds", s).flatMap(Service.of).map(Expect.Withholds(_))
                case _ => Left("not one of holds, omits, offers or withholds")
              }
            }
          )
          .left
          .map(why => s"$what: $why")
          .map(WorkflowId(w) -> _)
      }
    } yield Reference(VectorMap.from(each))

  /** How `expected` stands of a turn whose window is `window` (`None` when it was not
    * rebuilt), offered a tool of each of `offered`.
    */
  def judge(expected: Vector[Expect], window: Option[Parts], offered: Set[Service]): Judged = {
    val each = expected.map {
      case Expect.Holds(at) => window.map(w => Locator.held(at, w.parts))
      case Expect.Omits(at) => window.map(w => !Locator.anyHeld(at, w.parts))
      case Expect.Offers(s) => Some(offered.contains(s))
      case Expect.Withholds(s) => Some(!offered.contains(s))
    }
    if (each.contains(Some(false))) Judged.Fail
    else if (each.contains(None)) Judged.Unjudged
    else Judged.Pass
  }
}
