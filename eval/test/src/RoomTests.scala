package grit.eval

import java.nio.file.{Files, Paths}

import scala.concurrent.duration.*
import scala.io.Source
import scala.util.Using

import utest.*

/** [[Room]]: a synthetic room read from its file, and what placing each opening should give. */
object RoomTests extends TestSuite {

  private def message(
      id: String,
      minute: Int,
      who: String,
      topic: Option[String] = None,
      thread: Option[String] = None,
      second: Int = 0
  ): ujson.Value = {
    val m = ujson.Obj(
      "id" -> id,
      "at" -> f"2026-10-12T09:$minute%02d:$second%02dZ",
      "who" -> who,
      "text" -> s"text of $id"
    )
    topic.foreach(t => m("topic") = t)
    thread.foreach(t => m("thread") = t)
    m
  }

  private def room(messages: ujson.Value*): Either[String, Room] =
    Room.read(
      "r",
      ujson.Obj("team" -> "T", "channel" -> "C", "messages" -> ujson.Arr(messages*)).render()
    )

  private def truth(messages: ujson.Value*): Vector[(String, Option[String])] =
    room(messages*)
      .fold(e => throw new java.lang.AssertionError(e), Room.truth)
      .map(t => t.opening.id -> t.follows)

  private def card(id: String, minute: Int, topic: Option[String] = None): ujson.Value = {
    val c = ujson.Obj(
      "id" -> id,
      "at" -> f"2026-10-12T10:$minute%02d:00Z",
      "title" -> s"File $id",
      "text" -> s"text of $id"
    )
    topic.foreach(t => c("topic") = t)
    c
  }

  private def carded(cards: ujson.Value*): Either[String, Room] =
    Room.read(
      "r",
      ujson
        .Obj(
          "team" -> "T",
          "channel" -> "C",
          "messages" -> ujson.Arr(message("m1", 0, "Ana", Some("ci"))),
          "cards" -> ujson.Arr(cards*)
        )
        .render()
    )

  val tests = Tests {
    test("a room's cards are read in the order written, each with its topic or none") {
      (
        carded(card("c2", 20), card("c1", 10, Some("ci"))).map(
          _.cards.map(c => (c.id, c.title, c.topic))
        ),
        room(message("m1", 0, "Ana")).map(_.cards)
      ) ==> (
        Right(Vector(("c1", "File c1", Some("ci")), ("c2", "File c2", None))),
        Right(Vector())
      )
    }

    test("a card whose id is a message's, or another card's, is refused") {
      (
        carded(card("m1", 10)),
        carded(card("c1", 10), card("c1", 20))
      ) ==> (Left("r: id m1 repeats"), Left("r: id c1 repeats"))
    }

    test("a topic's later openings follow its first; chatter and a topic's first begin") {
      truth(
        message("m3", 20, "Ana", Some("ci")),
        message("m1", 0, "Ana", Some("ci")),
        message("m2", 10, "Ben"),
        message("r1", 11, "Ben", thread = Some("m1")),
        message("m4", 30, "Ben", Some("csv")),
        message("m5", 40, "Dev", Some("ci"))
      ) ==> Vector(
        "m1" -> None,
        "m2" -> None,
        "m3" -> Some("m1"),
        "m4" -> None,
        "m5" -> Some("m1")
      )
    }

    test(
      "a burst is its author's own previous opening within two minutes, two minutes exactly included, continuing on its topic"
    ) {
      val bursts = room(
        message("m1", 0, "Ana", Some("ci")),
        message("m2", 1, "Ana", Some("ci")),
        message("m3", 2, "Ana", Some("csv"), second = 30),
        message("m4", 5, "Ana", Some("csv")),
        message("m5", 6, "Ana"),
        message("m6", 7, "Ana"),
        message("m7", 10, "Ana"),
        message("b1", 10, "Ben", second = 30),
        message("m8", 11, "Ana"),
        message("m9", 13, "Ana"),
        message("m10", 15, "Ana", second = 1)
      ).fold(e => throw new java.lang.AssertionError(e), Room.truth)
        .map(t => t.opening.id -> t.burst)
      bursts ==> Vector(
        "m1" -> None,
        "m2" -> Some(Room.Burst("m1", 1.minute, continues = true)),
        "m3" -> Some(Room.Burst("m2", 90.seconds, continues = false)),
        "m4" -> None,
        "m5" -> Some(Room.Burst("m4", 1.minute, continues = false)),
        "m6" -> Some(Room.Burst("m5", 1.minute, continues = false)),
        "m7" -> None,
        "b1" -> None,
        "m8" -> Some(Room.Burst("m7", 1.minute, continues = false)),
        "m9" -> Some(Room.Burst("m8", Room.BurstGap, continues = false)),
        "m10" -> None
      )
    }

    test("a reply under no earlier opening, a reply with a topic, and a repeated id are refused") {
      room(message("r1", 0, "Ana", thread = Some("m1")), message("m1", 1, "Ben")) ==>
        Left("r: r1's thread is no opening said before it")
      room(message("m1", 0, "Ana"), message("r1", 1, "Ben", Some("ci"), Some("m1"))) ==>
        Left("r: r1 is a reply with a topic")
      room(message("m1", 0, "Ana"), message("m1", 1, "Ben")) ==> Left("r: id m1 repeats")
    }

    test("every room the eval holds reads, each with openings that follow and that begin") {
      val names = Option(getClass.getResource("/rooms"))
        .map(u => Using.resource(Files.list(Paths.get(u.toURI)))(_.toArray.toVector))
        .getOrElse(Vector.empty)
        .map(_.toString.split('/').last)
        // A room is a visible JSON file: a tool may leave a dotfile there mid-run.
        .collect { case f if f.endsWith(".json") && !f.startsWith(".") => f.stripSuffix(".json") }
        .sorted
      val read = names.map { name =>
        val text = Using.resource(Source.fromResource(s"rooms/$name.json"))(_.mkString)
        name -> Room.read(name, text).map(Room.truth).map { t =>
          (t.exists(_.follows.isDefined), t.exists(_.follows.isEmpty))
        }
      }
      (names.size >= 2, read.filterNot(_._2 == Right((true, true)))) ==> (true, Vector())
    }
  }
}
