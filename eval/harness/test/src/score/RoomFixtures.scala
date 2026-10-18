package grit.eval.harness.score

import grit.eval.Room

/** A five-opening room and one way of placing it, shared by the room scoring suites. */
object RoomFixtures {

  def message(
      id: String,
      minute: Int,
      who: String,
      topic: Option[String],
      ambiguous: Boolean = false
  ): ujson.Value = {
    val m = ujson.Obj(
      "id" -> id,
      "at" -> f"2026-10-12T09:$minute%02d:00Z",
      "who" -> who,
      "text" -> s"text of $id"
    )
    topic.foreach(t => m("topic") = t)
    if (ambiguous) m("ambiguous") = true
    m
  }

  def roomOf(messages: ujson.Value*): Room = Room
    .read(
      "r",
      ujson.Obj("team" -> "T", "channel" -> "C", "messages" -> ujson.Arr(messages*)).render()
    )
    .fold(e => throw new java.lang.AssertionError(e), identity)

  // Said: m1 (0), m3 (1, Ana's burst after m1), m2 (10), m4 (11, Ben's burst after m2), m5 (20).
  val room: Room = roomOf(
    message("m1", 0, "Ana", Some("ci")),
    message("m3", 1, "Ana", Some("ci")),
    message("m2", 10, "Ben", Some("csv")),
    message("m4", 11, "Ben", Some("lunch")),
    message("m5", 20, "Dev", Some("csv"))
  )

  def offer(root: String, p: Double, recent: Boolean = true): RoomPlacing.Offer =
    RoomPlacing.Offer(root, recent, Some(p))

  val placings: Vector[RoomPlacing] = Vector(
    RoomPlacing("m1", RoomPlacing.Made.Unoffered, Vector.empty),
    RoomPlacing(
      "m3",
      RoomPlacing.Made.Follows("m1", 0.8),
      Vector(offer("m1", 0.8, recent = false))
    ),
    RoomPlacing("m2", RoomPlacing.Made.Begins(0.2), Vector(offer("m1", 0.2))),
    RoomPlacing("m4", RoomPlacing.Made.Follows("m2", 0.7), Vector(offer("m2", 0.7))),
    RoomPlacing("m5", RoomPlacing.Made.Begins(0.5), Vector(offer("m2", 0.5), offer("m1", 0.1)))
  )

  val strands: Vector[Vector[String]] = Vector(Vector("m1", "m3"), Vector("m2", "m4"))

  def counts(p: grit.eval.harness.stats.Proportion): (Long, Long) = (p.hits, p.n)
}
