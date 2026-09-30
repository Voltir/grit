package grit.core.stitch

import scala.concurrent.duration.*

import grit.core.id.ConversationId
import grit.core.message.Tokens
import grit.core.period.Probability
import grit.core.store.PayloadJson

/** The stored form of a [[Placed]]: kept in `grit.stitches` and in the steps' journals. */
object StitchJson {

  // Stored names: the `kind` of a placement and of why an exchange was offered.
  private val Follows = "follows"
  private val Begins = "begins"
  private val Unread = "unread"
  private val Recent = "recent"
  private val Lexical = "lexical"

  /** `{"kind": "follows", "root", "p", "model", "usage", "seen"}`, `{"kind": "begins", "p",
    * "model", "usage", "seen"}` or `{"kind": "unread", "why", "seen"}`.
    */
  def write(placed: Placed): ujson.Value = placed match {
    case Placed.Follows(root, p, seen, model, usage) =>
      ujson.Obj(
        "kind" -> Follows,
        "root" -> ConversationId.value(root),
        "p" -> Probability.value(p),
        "model" -> model,
        "usage" -> PayloadJson.writeUsage(usage),
        "seen" -> writeSeen(seen)
      )
    case Placed.Begins(p, seen, model, usage) =>
      ujson.Obj(
        "kind" -> Begins,
        "p" -> Probability.value(p),
        "model" -> model,
        "usage" -> PayloadJson.writeUsage(usage),
        "seen" -> writeSeen(seen)
      )
    case Placed.Unread(why, seen) =>
      ujson.Obj("kind" -> Unread, "why" -> why, "seen" -> writeSeen(seen))
  }

  /** The placement `v` stores in [[write]]'s form, or why it stores none. */
  def read(v: ujson.Value): Either[String, Placed] =
    for {
      o <- obj(v)
      kind <- str(o, "kind")
      seen <- o.get("seen").toRight("placed: no seen").flatMap(readSeen)
      placed <- kind match {
        case Follows =>
          for {
            root <- str(o, "root")
            p <- probability(o, "p")
            model <- str(o, "model")
            usage <- o.get("usage").toRight("placed: no usage").flatMap(PayloadJson.readUsage)
          } yield Placed.Follows(ConversationId(root), p, seen, model, usage)
        case Begins =>
          for {
            p <- probability(o, "p")
            model <- str(o, "model")
            usage <- o.get("usage").toRight("placed: no usage").flatMap(PayloadJson.readUsage)
          } yield Placed.Begins(p, seen, model, usage)
        case Unread => str(o, "why").map(Placed.Unread(_, seen))
        case other => Left(s"placed: no kind $other")
      }
    } yield placed

  /** The stored name of `placed`'s kind: `follows`, `begins` or `unread`. */
  def kindOf(placed: Placed): String = placed match {
    case _: Placed.Follows => Follows
    case _: Placed.Begins => Begins
    case _: Placed.Unread => Unread
  }

  private def writeSeen(s: Seen): ujson.Value = ujson.Obj(
    "state" -> s.state,
    "offered" -> ujson.Arr.from(s.offered.map { o =>
      ujson.Obj(
        "root" -> ConversationId.value(o.root),
        "why" -> (o.why match {
          case Offered.Recent(rank) => ujson.Obj("kind" -> Recent, "rank" -> rank)
          case Offered.Lexical(score) => ujson.Obj("kind" -> Lexical, "score" -> score)
        }),
        "p" -> o.p.fold[ujson.Value](ujson.Null)(p => ujson.Num(Probability.value(p)))
      )
    }),
    "tuning" -> ujson.Obj(
      "horizon_seconds" -> s.tuning.horizon.toSeconds.toDouble,
      "recent" -> s.tuning.recent,
      "lexical" -> s.tuning.lexical,
      "follows_at" -> Probability.value(s.tuning.followsAt),
      "window_tokens" -> Tokens.value(s.tuning.windowTokens).toDouble,
      "strand_chars" -> s.tuning.strandChars
    )
  )

  private def readSeen(v: ujson.Value): Either[String, Seen] =
    for {
      o <- obj(v)
      state <- o.get("state").toRight("seen: no state")
      offered <- o
        .get("offered")
        .flatMap(_.arrOpt)
        .toRight("seen: offered: expected an array")
        .flatMap(_.toVector.foldLeft[Either[String, Vector[Seen.Offer]]](Right(Vector.empty)) {
          (acc, x) => acc.flatMap(done => readOffer(x).map(done :+ _))
        })
      t <- o.get("tuning").toRight("seen: no tuning").flatMap(obj)
      horizon <- long(t, "horizon_seconds")
      recent <- long(t, "recent")
      lexical <- long(t, "lexical")
      followsAt <- probability(t, "follows_at")
      window <- long(t, "window_tokens")
      chars <- long(t, "strand_chars")
    } yield Seen(
      state,
      offered,
      Tuning(horizon.seconds, recent.toInt, lexical.toInt, followsAt, Tokens(window), chars.toInt)
    )

  private def readOffer(v: ujson.Value): Either[String, Seen.Offer] =
    for {
      o <- obj(v)
      root <- str(o, "root")
      w <- o.get("why").toRight("offer: no why").flatMap(obj)
      why <- str(w, "kind").flatMap {
        case Recent => long(w, "rank").map(r => Offered.Recent(r.toInt))
        case Lexical =>
          w.get("score")
            .flatMap(_.numOpt)
            .toRight("score: expected a number")
            .map(Offered.Lexical(_))
        case other => Left(s"offer: no why $other")
      }
      p <- o.get("p") match {
        case None | Some(ujson.Null) => Right(None)
        case Some(x) =>
          x.numOpt.flatMap(Probability.of).toRight("p: expected a probability").map(Some(_))
      }
    } yield Seen.Offer(ConversationId(root), why, p)

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

  private def probability(
      o: collection.Map[String, ujson.Value],
      k: String
  ): Either[String, Probability] =
    o.get(k).flatMap(_.numOpt).flatMap(Probability.of).toRight(s"$k: expected a probability")
}
