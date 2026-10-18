package grit.eval

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

  val tests = Tests {
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

    test("a burst is its author's previous opening within two minutes, continuing on its topic") {
      val bursts = room(
        message("m1", 0, "Ana", Some("ci")),
        message("m2", 1, "Ana", Some("ci")),
        message("m3", 2, "Ana", Some("csv"), second = 30),
        message("m4", 5, "Ana", Some("csv")),
        message("m5", 6, "Ana"),
        message("m6", 7, "Ana")
      ).fold(e => throw new java.lang.AssertionError(e), Room.truth)
        .map(t => t.opening.id -> t.burst)
      bursts ==> Vector(
        "m1" -> None,
        "m2" -> Some(Room.Burst("m1", 1.minute, continues = true)),
        "m3" -> Some(Room.Burst("m2", 90.seconds, continues = false)),
        "m4" -> None,
        "m5" -> Some(Room.Burst("m4", 1.minute, continues = false)),
        "m6" -> Some(Room.Burst("m5", 1.minute, continues = false))
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
      val read = Vector("room-a", "room-b").map { name =>
        val text = Using.resource(Source.fromResource(s"rooms/$name.json"))(_.mkString)
        name -> Room.read(name, text).map(Room.truth).map { t =>
          (t.exists(_.follows.isDefined), t.exists(_.follows.isEmpty))
        }
      }
      read ==> Vector("room-a" -> Right((true, true)), "room-b" -> Right((true, true)))
    }
  }
}
