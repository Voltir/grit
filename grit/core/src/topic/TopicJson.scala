package grit.core.topic

import grit.core.id.TurnSeq

/** The stored JSON form of [[TopicEvent]]s. Written by hand: it is persisted data, so a
  * rename in Scala must not change it, and reading it is total.
  */
object TopicJson {

  def write(e: TopicEvent): ujson.Value = e match {
    case TopicEvent.Opened(t) => ujson.Obj("event" -> "opened", "topic" -> TopicId.value(t))
    case TopicEvent.Placed(turn, weights, by) =>
      ujson.Obj(
        "event" -> "placed",
        "turn" -> TurnSeq.value(turn).toDouble,
        "weights" -> ujson.Arr.from(
          weights.byTopic.map(s => ujson.Arr(TopicId.value(s.topic), s.weight))
        ),
        "elsewhere" -> weights.elsewhere,
        "by" -> placement(by)
      )
    case TopicEvent.Described(t, name, summary) =>
      ujson.Obj(
        "event" -> "described",
        "topic" -> TopicId.value(t),
        "name" -> name,
        "summary" -> summary
      )
  }

  private def placement(p: Placement): ujson.Value = p match {
    case Placement.First => ujson.Obj("kind" -> "first")
    case Placement.Unclassified(reason) => ujson.Obj("kind" -> "unclassified", "reason" -> reason)
    case Placement.Classified(pSame, outcome) =>
      val choice = outcome match {
        case Placement.Outcome.Changed(choice) => choice
        case Placement.Outcome.Same | Placement.Outcome.Uncertain => Vector.empty
      }
      ujson.Obj(
        "kind" -> "classified",
        "pSame" -> pSame,
        "band" -> bandKey(outcome.band),
        "choice" -> ujson.Arr.from(choice.map { c =>
          ujson.Arr(
            c.topic.fold[ujson.Value](ujson.Null)(id => ujson.Str(TopicId.value(id))),
            c.probability
          )
        })
      )
    case Placement.Asked(verdict, anomaly) =>
      val o = ujson.Obj("kind" -> "asked", "verdict" -> writeVerdict(verdict))
      anomaly.foreach(a => o("anomaly") = a)
      o
  }

  private def bandKey(b: Band): String = b match {
    case Band.Same => "same"
    case Band.Uncertain => "uncertain"
    case Band.Changed => "changed"
  }

  private def writeVerdict(v: Verdict): ujson.Value = v match {
    case Verdict.Current => ujson.Obj("about" -> "current")
    case Verdict.Earlier(name) => ujson.Obj("about" -> "earlier", "name" -> name)
    case Verdict.New(name) =>
      val o = ujson.Obj("about" -> "new")
      name.foreach(n => o("name") = n)
      o
    case Verdict.Unreadable(why) => ujson.Obj("about" -> "unreadable", "why" -> why)
  }

  /** The event `v` encodes, or why it encodes none. */
  def read(v: ujson.Value): Either[String, TopicEvent] =
    for {
      o <- v.objOpt.toRight("a topic event is not an object")
      kind <- str(o, "event")
      e <- kind match {
        case "opened" => str(o, "topic").map(t => TopicEvent.Opened(TopicId(t)))
        case "placed" =>
          for {
            turn <- o
              .get("turn")
              .collect {
                case ujson.Num(n) if n.isWhole && n >= 0 => TurnSeq(n.toLong)
              }
              .toRight("placed: bad turn")
            weights <- o
              .get("weights")
              .flatMap(_.arrOpt)
              .toRight("placed: no weights")
              .flatMap(ws =>
                traverse(ws.toVector) {
                  case ujson.Arr(pair) =>
                    pair.toVector match {
                      case Vector(ujson.Str(t), ujson.Num(w)) => Right(Weights.Share(TopicId(t), w))
                      case _ => Left("placed: a weight is not [topic, number]")
                    }
                  case _ => Left("placed: a weight is not [topic, number]")
                }
              )
            elsewhere <- num(o, "elsewhere")
            valid <- Weights.of(weights, elsewhere).left.map(e => s"placed: ${whyNot(e)}")
            by <- o.get("by").toRight("placed: no by").flatMap(readPlacement)
          } yield TopicEvent.Placed(turn, valid, by)
        case "described" =>
          for {
            t <- str(o, "topic")
            name <- str(o, "name")
            summary <- str(o, "summary")
          } yield TopicEvent.Described(TopicId(t), name, summary)
        case other => Left(s"unknown topic event: $other")
      }
    } yield e

  private def readPlacement(v: ujson.Value): Either[String, Placement] =
    for {
      o <- v.objOpt.toRight("a placement is not an object")
      kind <- str(o, "kind")
      p <- kind match {
        case "first" => Right(Placement.First)
        case "unclassified" => str(o, "reason").map(Placement.Unclassified(_))
        case "classified" =>
          for {
            pSame <- num(o, "pSame")
            band <- str(o, "band")
            choice <- o
              .get("choice")
              .flatMap(_.arrOpt)
              .toRight("classified: no choice")
              .flatMap(cs =>
                traverse(cs.toVector) {
                  case ujson.Arr(pair) =>
                    pair.toVector match {
                      case Vector(ujson.Str(t), ujson.Num(q)) =>
                        Right(Placement.Chance(Some(TopicId(t)), q))
                      case Vector(ujson.Null, ujson.Num(q)) => Right(Placement.Chance(None, q))
                      case _ => Left("classified: an option is not [topic or null, number]")
                    }
                  case _ => Left("classified: an option is not [topic or null, number]")
                }
              )
            outcome <- (band, choice) match {
              case ("same", Vector()) => Right(Placement.Outcome.Same)
              case ("uncertain", Vector()) => Right(Placement.Outcome.Uncertain)
              case ("changed", _) => Right(Placement.Outcome.Changed(choice))
              case ("same" | "uncertain", _) => Left(s"classified: a choice in band $band")
              case (other, _) => Left(s"unknown band: $other")
            }
          } yield Placement.Classified(pSame, outcome)
        case "asked" =>
          for {
            verdict <- o.get("verdict").toRight("asked: no verdict").flatMap(readVerdict)
            anomaly <- o.get("anomaly") match {
              case None => Right(None)
              case Some(ujson.Str(a)) => Right(Some(a))
              case Some(_) => Left("asked: the anomaly is not a string")
            }
          } yield Placement.Asked(verdict, anomaly)
        case other => Left(s"unknown placement: $other")
      }
    } yield p

  private def readVerdict(v: ujson.Value): Either[String, Verdict] =
    for {
      o <- v.objOpt.toRight("a verdict is not an object")
      about <- str(o, "about")
      verdict <- about match {
        case "current" => Right(Verdict.Current)
        case "earlier" => str(o, "name").map(Verdict.Earlier(_))
        case "new" => Right(Verdict.New(o.get("name").flatMap(_.strOpt)))
        case "unreadable" => str(o, "why").map(Verdict.Unreadable(_))
        case other => Left(s"unknown verdict: $other")
      }
    } yield verdict

  private def whyNot(e: WeightsError): String = e match {
    case WeightsError.NoTopic => "no topic weighed"
    case WeightsError.Repeated(t) => s"${TopicId.value(t)} weighed twice"
    case WeightsError.Negative(t) => s"${share(t)} is negative"
    case WeightsError.NotFinite(t) => s"${share(t)} is not finite"
    case WeightsError.SumOff(sum) => s"the weights sum to $sum"
  }

  private def share(t: Option[TopicId]): String =
    t.fold("elsewhere")(id => s"the weight of ${TopicId.value(id)}")

  private def str(o: collection.Map[String, ujson.Value], key: String): Either[String, String] =
    o.get(key).flatMap(_.strOpt).toRight(s"missing string $key")

  private def num(o: collection.Map[String, ujson.Value], key: String): Either[String, Double] =
    o.get(key).flatMap(_.numOpt).toRight(s"missing number $key")

  private def traverse[A, B](as: Vector[A])(f: A => Either[String, B]): Either[String, Vector[B]] =
    as.foldLeft[Either[String, Vector[B]]](Right(Vector.empty))((acc, a) =>
      acc.flatMap(bs => f(a).map(bs :+ _))
    )
}
