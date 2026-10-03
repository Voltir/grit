package grit.core.speech

import scala.concurrent.duration.*

import grit.core.id.{EntryId, PrincipalId, TurnRef, WorkflowId}
import grit.core.message.Cost
import grit.core.period.Probability
import grit.core.spend.{DailyCap, Spend}
import grit.core.store.PayloadJson
import grit.core.triage.{Bound, Gate, GateJson, Kind, Reading, Tags}

/** The stored JSON forms of a [[Decision]], a [[Silence]] and an [[Outcome]]: the same in a
  * `grit.speech` row and in a workflow's journal. Written by hand and read totally, as
  * [[PayloadJson]] is: a workflow in flight must read back what an earlier build wrote, so
  * change these only with the workflow's epoch (ADR 0004).
  */
object SpeechJson {

  /** `{"drafting": turn's workflow id}` or `{"held": silence}`. */
  def writeDecision(d: Decision): ujson.Value = d match {
    case Decision.Drafting(turn) => ujson.Obj("drafting" -> WorkflowId.value(turn.workflowId))
    case Decision.Held(why) => ujson.Obj("held" -> writeSilence(why))
  }

  def readDecision(v: ujson.Value): Either[String, Decision] =
    obj(v).flatMap { o =>
      (o.get("drafting"), o.get("held")) match {
        case (Some(ujson.Str(id)), None) =>
          TurnRef
            .fromWorkflowId(WorkflowId(id))
            .map(Decision.Drafting(_))
            .toRight(s"decision: $id is not a turn")
        case (None, Some(why)) => readSilence(why).map(Decision.Held(_))
        case _ => Left("decision: expected {drafting} or {held}")
      }
    }

  /** `{"kind": its name}`, with its detail beside it; a `Stale` age in whole seconds, its
    * fraction dropped.
    */
  def writeSilence(s: Silence): ujson.Value = s match {
    case Silence.Off => kind("off")
    case Silence.NoAddress => kind("no_address")
    case Silence.Stale(age) => kind("stale", "seconds" -> ujson.Num(age.toSeconds.toDouble))
    case Silence.Unweighed(why) => kind("unweighed", "why" -> ujson.Str(why))
    case Silence.Gated(first, rest) =>
      kind("gated", "failed" -> ujson.Arr.from((first +: rest).map(GateJson.writeFailed)))
    case Silence.Unasked(reading) => kind("unasked", "reading" -> GateJson.writeReading(reading))
    case Silence.AskedOf(other) => kind("asked_of", "other" -> ujson.Str(PrincipalId.value(other)))
    case Silence.Unanswered(previous) =>
      kind("unanswered", "previous" -> ujson.Str(WorkflowId.value(previous.workflowId)))
    case Silence.Thread(n) => kind("thread", "n" -> ujson.Num(n))
    case Silence.Room(n) => kind("room", "n" -> ujson.Num(n))
    case Silence.Deployment(n) => kind("deployment", "n" -> ujson.Num(n))
    case Silence.OverSpeechCap(spent, cap) =>
      kind("over_speech_cap", "spent" -> writeSpend(spent), "cap" -> ujson.Str(cap.usd.toString))
    case Silence.OverBudget => kind("over_budget")
  }

  /** A silence as [[writeSilence]] writes it; an earlier build's `chatter` reads as `Gated` on
    * `kind` chosen `chatter`, and `below` as `Gated` on `helps` at least `helps_at`.
    */
  def readSilence(v: ujson.Value): Either[String, Silence] =
    obj(v).flatMap { o =>
      str(o, "kind").flatMap {
        case "off" => Right(Silence.Off)
        case "no_address" => Right(Silence.NoAddress)
        case "stale" => long(o, "seconds").map(s => Silence.Stale(s.seconds))
        case "unweighed" => str(o, "why").map(Silence.Unweighed(_))
        case "gated" =>
          o.get("failed").flatMap(_.arrOpt).map(_.toVector) match {
            case Some(first +: rest) =>
              for {
                f <- GateJson.readFailed(first)
                r <- rest.foldLeft[Either[String, Vector[Gate.Failed]]](Right(Vector.empty))(
                  (acc, v) => acc.flatMap(fs => GateJson.readFailed(v).map(fs :+ _))
                )
              } yield Silence.Gated(f, r)
            case _ => Left("failed: expected a non-empty array")
          }
        case "unasked" =>
          o.get("reading")
            .toRight("unasked: no reading")
            .flatMap(GateJson.readReading)
            .map(Silence.Unasked(_))
        // Stored before a hold named the bound it failed: v1's gate's, with what it read. A
        // chatter hold was kind's most weighted key, which reads 1.
        case "chatter" =>
          val chatter = Reading.Chosen(Tags.V1.kind, Kind.written(Kind.Chatter))
          Right(
            Silence.Gated(
              Gate.Failed(Bound.Below(chatter, Probability.clamped(0.5)), Probability.clamped(1)),
              Vector.empty
            )
          )
        case "below" =>
          for {
            helps <- probability(o, "helps")
            at <- probability(o, "helps_at")
          } yield Silence.Gated(
            Gate.Failed(Bound.AtLeast(Reading.Yes(Tags.V1.helps), at), helps),
            Vector.empty
          )
        case "asked_of" => str(o, "other").map(p => Silence.AskedOf(PrincipalId(p)))
        case "unanswered" => turn(o, "previous").map(Silence.Unanswered(_))
        case "thread" => int(o, "n").map(Silence.Thread(_))
        case "room" => int(o, "n").map(Silence.Room(_))
        case "deployment" => int(o, "n").map(Silence.Deployment(_))
        case "over_speech_cap" =>
          for {
            spent <- o.get("spent").toRight("silence: no spent").flatMap(readSpend)
            cap <- str(o, "cap").flatMap(DailyCap.of)
          } yield Silence.OverSpeechCap(spent, cap)
        case "over_budget" => Right(Silence.OverBudget)
        case other => Left(s"silence: unknown kind $other")
      }
    }

  /** `{"kind": its name}`, with its detail beside it: a judgement as `{"grounded", "worth",
    * "model", "usage"}`.
    */
  def writeOutcome(out: Outcome): ujson.Value = {
    val detail: Vector[(String, ujson.Value)] = out match {
      case Outcome.Passed | Outcome.NothingRecalled | Outcome.Withdrawn => Vector.empty
      case Outcome.Spoken(by) => Vector("by" -> ujson.Str(EntryId.value(by)))
      case Outcome.Unjudged(why) => Vector("why" -> ujson.Str(why))
      case Outcome.Below(j, at) => Vector("judged" -> writeJudged(j), "post_at" -> num(at))
      case Outcome.Shadowed(j) => Vector("judged" -> writeJudged(j))
      case Outcome.Posted(j) => Vector("judged" -> writeJudged(j))
      case Outcome.Failed(why) => Vector("why" -> ujson.Str(why))
    }
    kind(outcomeName(out), detail*)
  }

  def readOutcome(v: ujson.Value): Either[String, Outcome] =
    obj(v).flatMap { o =>
      def judged = o.get("judged").toRight("outcome: no judged").flatMap(readJudged)
      str(o, "kind").flatMap {
        case "passed" => Right(Outcome.Passed)
        case "nothing_recalled" => Right(Outcome.NothingRecalled)
        // Stored as answered while a person's reply held a draft (before ADR 0023).
        case "spoken" | "answered" => str(o, "by").map(b => Outcome.Spoken(EntryId(b)))
        case "withdrawn" => Right(Outcome.Withdrawn)
        case "unjudged" => str(o, "why").map(Outcome.Unjudged(_))
        case "below" =>
          for {
            j <- judged
            at <- probability(o, "post_at")
          } yield Outcome.Below(j, at)
        case "shadowed" => judged.map(Outcome.Shadowed(_))
        case "posted" => judged.map(Outcome.Posted(_))
        case "failed" => str(o, "why").map(Outcome.Failed(_))
        case other => Left(s"outcome: unknown kind $other")
      }
    }

  /** The judgement an outcome carries; `None` for one that was not judged. */
  def judgedOf(out: Outcome): Option[Judged] = out match {
    case Outcome.Below(j, _) => Some(j)
    case Outcome.Shadowed(j) => Some(j)
    case Outcome.Posted(j) => Some(j)
    case Outcome.Passed | Outcome.NothingRecalled | Outcome.Spoken(_) | Outcome.Withdrawn |
        Outcome.Unjudged(_) | Outcome.Failed(_) =>
      None
  }

  /** The stored name of `out`'s kind, under `kind` in [[writeOutcome]]'s form. */
  def outcomeName(out: Outcome): String = out match {
    case Outcome.Passed => "passed"
    case Outcome.NothingRecalled => "nothing_recalled"
    case Outcome.Spoken(_) => "spoken"
    case Outcome.Withdrawn => "withdrawn"
    case Outcome.Unjudged(_) => "unjudged"
    case Outcome.Below(_, _) => "below"
    case Outcome.Shadowed(_) => "shadowed"
    case Outcome.Posted(_) => "posted"
    case Outcome.Failed(_) => "failed"
  }

  /** `{"grounded", "worth", "model", "usage"}`. */
  def writeJudged(j: Judged): ujson.Value =
    ujson.Obj(
      "grounded" -> num(j.grounded),
      "worth" -> num(j.worth),
      "model" -> j.model,
      "usage" -> PayloadJson.writeUsage(j.usage)
    )

  /** A judgement as [[writeJudged]] writes it; an `"adds"` beside it, which judgements
    * recorded before the judge dropped that question carry, is ignored.
    */
  def readJudged(v: ujson.Value): Either[String, Judged] =
    for {
      o <- obj(v)
      grounded <- probability(o, "grounded")
      worth <- probability(o, "worth")
      model <- str(o, "model")
      usage <- o.get("usage").toRight("judged: no usage").flatMap(PayloadJson.readUsage)
    } yield Judged(grounded, worth, model, usage)

  private def writeSpend(s: Spend): ujson.Value = s.cost match {
    case Cost.Exact(usd) => ujson.Obj("calls" -> s.calls, "usd" -> usd.toString)
    case Cost.AtLeast(usd) =>
      ujson.Obj("calls" -> s.calls, "usd" -> usd.toString, "at_least" -> true)
  }

  private def readSpend(v: ujson.Value): Either[String, Spend] =
    for {
      o <- obj(v)
      calls <- int(o, "calls")
      raw <- str(o, "usd")
      usd <- scala.util.Try(BigDecimal(raw)).toOption.toRight(s"spend: $raw is not a number")
      atLeast = o.get("at_least").flatMap(_.boolOpt).getOrElse(false)
    } yield Spend(calls, if (atLeast) Cost.AtLeast(usd) else Cost.Exact(usd))

  private def kind(name: String, rest: (String, ujson.Value)*): ujson.Value =
    ujson.Obj.from(("kind" -> ujson.Str(name)) +: rest)

  private def num(p: Probability): ujson.Value = ujson.Num(Probability.value(p))

  private def obj(v: ujson.Value): Either[String, collection.Map[String, ujson.Value]] =
    v.objOpt.toRight("expected an object")

  private def str(o: collection.Map[String, ujson.Value], k: String): Either[String, String] =
    o.get(k).flatMap(_.strOpt).toRight(s"$k: expected a string")

  private def long(o: collection.Map[String, ujson.Value], k: String): Either[String, Long] =
    o.get(k)
      .flatMap(_.numOpt)
      .filter(_.isWhole)
      .map(_.toLong)
      .toRight(s"$k: expected a whole number")

  private def int(o: collection.Map[String, ujson.Value], k: String): Either[String, Int] =
    long(o, k).flatMap(n => Either.cond(n.isValidInt, n.toInt, s"$k: expected a whole number"))

  private def probability(
      o: collection.Map[String, ujson.Value],
      k: String
  ): Either[String, Probability] =
    o.get(k).flatMap(_.numOpt).flatMap(Probability.of).toRight(s"$k: expected a probability")

  private def turn(o: collection.Map[String, ujson.Value], k: String): Either[String, TurnRef] =
    str(o, k).flatMap(id =>
      TurnRef.fromWorkflowId(WorkflowId(id)).toRight(s"$k: $id is not a turn")
    )
}
