package grit.core.triage

import grit.core.id.QuestionName
import grit.core.period.Probability

/** The stored JSON forms of what a gate reads and found: a [[Reading]], a [[Gate.Failed]] and
  * a [[Gate.Checked]]. A row and a workflow's journal read back what an earlier build wrote
  * (ADR 0004), so a form once written is read by every later build.
  */
object GateJson {

  /** `{"reads", "name"}`, with its `key` beside them for a `key` or `chosen` reading. */
  def writeReading(r: Reading): ujson.Value = r match {
    case Reading.Yes(name) => ujson.Obj("reads" -> "yes", "name" -> QuestionName.value(name))
    case Reading.Key(name, key) =>
      ujson.Obj("reads" -> "key", "name" -> QuestionName.value(name), "key" -> key)
    case Reading.Chosen(name, key) =>
      ujson.Obj("reads" -> "chosen", "name" -> QuestionName.value(name), "key" -> key)
  }

  def readReading(v: ujson.Value): Either[String, Reading] =
    for {
      o <- obj(v)
      name <- str(o, "name").flatMap(QuestionName.read)
      reading <- str(o, "reads").flatMap {
        case "yes" => Right(Reading.Yes(name))
        case "key" => str(o, "key").map(Reading.Key(name, _))
        case "chosen" => str(o, "key").map(Reading.Chosen(name, _))
        case other => Left(s"reading: unknown reads $other")
      }
    } yield reading

  /** `{"reading", "bound", "p", "read"}`: what the bound reads, `at_least` or `below` `p`, and
    * what it read.
    */
  def writeFailed(f: Gate.Failed): ujson.Value = {
    val (bound, on, p) = f.bound match {
      case Bound.AtLeast(on, p) => ("at_least", on, p)
      case Bound.Below(on, p) => ("below", on, p)
    }
    ujson.Obj("reading" -> writeReading(on), "bound" -> bound, "p" -> num(p), "read" -> num(f.read))
  }

  def readFailed(v: ujson.Value): Either[String, Gate.Failed] =
    for {
      o <- obj(v)
      on <- o.get("reading").toRight("failed: no reading").flatMap(readReading)
      p <- probability(o, "p")
      bound <- str(o, "bound").flatMap {
        case "at_least" => Right(Bound.AtLeast(on, p))
        case "below" => Right(Bound.Below(on, p))
        case other => Left(s"failed: unknown bound $other")
      }
      read <- probability(o, "read")
    } yield Gate.Failed(bound, read)

  /** `"passes"`, `{"fails": [failed, ...]}` (each as [[writeFailed]] writes it, in order), or
    * `{"unread": reading}`.
    */
  def writeChecked(c: Gate.Checked): ujson.Value = c match {
    case Gate.Checked.Passes => ujson.Str("passes")
    case Gate.Checked.Fails(first, rest) =>
      ujson.Obj("fails" -> ujson.Arr.from((first +: rest).map(writeFailed)))
    case Gate.Checked.Unread(reading) => ujson.Obj("unread" -> writeReading(reading))
  }

  /** What [[writeChecked]] wrote; `Left` for anything else, a `fails` with none among them. */
  def readChecked(v: ujson.Value): Either[String, Gate.Checked] = v match {
    case ujson.Str("passes") => Right(Gate.Checked.Passes)
    case o: ujson.Obj =>
      (o.value.get("fails"), o.value.get("unread")) match {
        case (Some(ujson.Arr(items)), None) =>
          items.toVector
            .foldLeft[Either[String, Vector[Gate.Failed]]](Right(Vector.empty))((acc, f) =>
              acc.flatMap(done => readFailed(f).map(done :+ _))
            )
            .flatMap {
              case first +: rest => Right(Gate.Checked.Fails(first, rest))
              case _ => Left("checked: fails names no bound")
            }
        case (None, Some(reading)) => readReading(reading).map(Gate.Checked.Unread(_))
        case _ => Left("checked: expected {fails} or {unread}")
      }
    case _ => Left("checked: expected \"passes\", {fails} or {unread}")
  }

  private def num(p: Probability): ujson.Value = ujson.Num(Probability.value(p))

  private def obj(v: ujson.Value): Either[String, collection.Map[String, ujson.Value]] =
    v.objOpt.toRight("expected an object")

  private def str(o: collection.Map[String, ujson.Value], k: String): Either[String, String] =
    o.get(k).flatMap(_.strOpt).toRight(s"$k: expected a string")

  private def probability(
      o: collection.Map[String, ujson.Value],
      k: String
  ): Either[String, Probability] =
    o.get(k).flatMap(_.numOpt).flatMap(Probability.of).toRight(s"$k: expected a probability")
}
