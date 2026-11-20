package grit.knowledge.topic

import java.time.{LocalDate, LocalTime, ZoneOffset}

import grit.core.message.Cut
import grit.core.visibility.{Integrity, Label}
import grit.knowledge.topic.Meeting.{State, When}
import grit.knowledge.topic.TestMeetings.{Kept, meeting, subject, text}

import utest.*

/** A turn's lookup of the meetings a topics plugin keeps, over documents kept as its runs keep
  * them ([[TestMeetings]]). Monday 2026-11-16 is the lookup's first day unless a test says.
  */
object MeetingLookupTests extends TestSuite {

  private def day(d: Int): LocalDate = LocalDate.of(2026, 11, d)

  private def lookup(
      from: LocalDate = day(16),
      to: Option[LocalDate] = None,
      about: Option[String] = None,
      most: Option[Int] = None,
      latestFirst: Boolean = false
  ): MeetingLookup =
    MeetingLookup
      .of(from, to, ZoneOffset.UTC, about, most, latestFirst)
      .fold(sys.error, identity)

  private def planned(d: Int, start: Option[LocalTime] = None): State =
    State.Planned(When(day(d), start))

  private def at(name: String, t: Topic, integrity: Integrity = Integrity.Member) =
    (name, t, Label.Public, integrity)

  /** The leads among `leads` the answer shows, in the order it shows them. */
  private def shown(answer: String, leads: String*): Vector[String] =
    leads.toVector.filter(answer.contains).sortBy(answer.indexOf)

  val tests = Tests {
    test(
      "the answer is a header naming the days and their zone, then each meeting's text, a rule between"
    ) {
      val a = meeting(planned(18, Some(LocalTime.of(9, 30))), Vector("Ana", "Ben"), "Engine sync.")
      val b = meeting(State.Held(day(16)), Vector("Ana"), "Retro.", day(16))
      lookup().answer(Kept(at("a", a), at("b", b)).read()).text ==>
        "Meetings from Monday 2026-11-16 to Sunday 2026-11-29 (UTC time), soonest first, as of the last write-up:" +
        s"\n\n${text(b)}\n\n---\n\n${text(a)}"
    }

    test(
      "dated meetings from `from` to `to` come soonest first, by day then start, none after a start; a day outside is not listed"
    ) {
      val kept = Kept(
        at("a", meeting(planned(18, Some(LocalTime.of(9, 30))), Vector.empty, "Engine sync.")),
        at("b", meeting(planned(18), Vector.empty, "Budget.")),
        at("c", meeting(State.Held(day(16)), Vector.empty, "Retro.")),
        at("d", meeting(planned(29, Some(LocalTime.of(14, 0))), Vector.empty, "Planning.")),
        at("e", meeting(planned(30), Vector.empty, "Late.")),
        at("f", meeting(State.Held(day(15)), Vector.empty, "Before."))
      )
      val answer = lookup(most = Some(8)).answer(kept.read()).text
      shown(answer, "Engine sync.", "Budget.", "Retro.", "Planning.", "Late.", "Before.") ==>
        Vector("Retro.", "Engine sync.", "Budget.", "Planning.")
    }

    test(
      "latest first reverses the order, and with most 1 over three held meetings shows the newest, counting the rest"
    ) {
      val kept = Kept(
        at("a", meeting(State.Held(day(2)), Vector.empty, "Sync one.")),
        at("b", meeting(State.Held(day(9)), Vector.empty, "Sync two.")),
        at("c", meeting(State.Held(day(16)), Vector.empty, "Sync three."))
      )
      val answer =
        lookup(from = day(1), to = Some(day(19)), most = Some(1), latestFirst = true)
          .answer(kept.read())
          .text
      (
        shown(answer, "Sync one.", "Sync two.", "Sync three."),
        answer.linesIterator.toVector.lastOption
      ) ==> (
        Vector("Sync three."),
        Some("2 more match; give fewer days, more words in about, or a larger most.")
      )
      answer.linesIterator.nextOption() ==> Some(
        "Meetings from Sunday 2026-11-01 to Thursday 2026-11-19 (UTC time), latest first, as of the last write-up:"
      )
    }

    test(
      "about keeps a meeting only where each word starts a word of its text, in any case: ed matches no planned meeting without Ed"
    ) {
      val kept = Kept(
        at("a", meeting(planned(18), Vector("Ed Moss"), "Engine sync.")),
        at("b", meeting(planned(19), Vector("Ana"), "Budget, planned with Cassie.")),
        at("c", meeting(planned(20), Vector("Ana"), "Engine review."))
      )
      shown(
        lookup(about = Some("ed")).answer(kept.read()).text,
        "Engine sync.",
        "Budget",
        "Engine review."
      ) ==>
        Vector("Engine sync.")
      shown(
        lookup(about = Some("ENGINE ana")).answer(kept.read()).text,
        "Engine sync.",
        "Budget",
        "Engine review."
      ) ==>
        Vector("Engine review.")
      lookup(about = Some("cass")).answer(kept.read()).text.linesIterator.nextOption() ==> Some(
        "Meetings from Monday 2026-11-16 to Sunday 2026-11-29 (UTC time) that mention \"cass\", soonest first, as of the last write-up:"
      )
    }

    test(
      "one proposed with no day, or a day before from, talked of within Lately days before from, follows the dated under its header; one talked of earlier does not"
    ) {
      val kept = Kept(
        at("a", meeting(planned(18), Vector.empty, "Dated.")),
        at("b", meeting(State.Proposed(None), Vector.empty, "Undated lately.", day(2))),
        at("c", meeting(State.Proposed(None), Vector.empty, "Undated long ago.", day(1))),
        at(
          "d",
          meeting(State.Proposed(Some(When(day(12), None))), Vector.empty, "Passed.", day(14))
        )
      )
      val answer = lookup().answer(kept.read()).text
      shown(
        answer,
        "Dated.",
        MeetingLookup.UndatedHeader,
        "Passed.",
        "Undated lately.",
        "Undated long ago."
      ) ==>
        Vector("Dated.", MeetingLookup.UndatedHeader, "Passed.", "Undated lately.")
    }

    test("each meeting is shown cut to ShownChars") {
      val long = meeting(planned(18), Vector.empty, "Engine " * 400)
      val answer = lookup().answer(Kept(at("a", long)).read()).text
      (
        answer.contains(Cut.head(text(long), MeetingLookup.ShownChars)),
        answer.contains(text(long))
      ) ==>
        (true, false)
    }

    test(
      "with no meeting matched it says none has been written up, naming about when given, believed at nothing"
    ) {
      val kept = Kept(at("a", meeting(planned(30), Vector.empty, "Late.")))
      lookup().answer(kept.read()) ==> MeetingLookup.Answer(
        "No meeting this conversation may see from Monday 2026-11-16 to Sunday 2026-11-29 has been written up.",
        None
      )
      lookup(about = Some("cassie")).answer(kept.read()).text ==>
        "No meeting this conversation may see from Monday 2026-11-16 to Sunday 2026-11-29 mentions \"cassie\"."
    }

    test("it is believed at the meet of every meeting matched, one only counted among them") {
      val kept = Kept(
        at("a", meeting(planned(17), Vector.empty, "First."), Integrity.Member),
        at("b", meeting(planned(18), Vector.empty, "Second."), Integrity.Outside)
      )
      lookup(most = Some(1)).answer(kept.read()).believed ==> Some(Integrity.Outside)
    }

    test("a subject is passed over") {
      lookup().answer(Kept(at("a", subject("Engine.", day(18)))).read()) ==> MeetingLookup.Answer(
        "No meeting this conversation may see from Monday 2026-11-16 to Sunday 2026-11-29 has been written up.",
        None
      )
    }

    test(
      "of: to is 13 days after from when not given; to before from, or most outside 1 to 8, is refused"
    ) {
      lookup().to ==> day(29)
      MeetingLookup.of(day(16), Some(day(15)), ZoneOffset.UTC, None, None, false) ==>
        Left("`to` is before `from`; give a day on or after it.")
      MeetingLookup.of(day(16), None, ZoneOffset.UTC, None, Some(9), false) ==>
        Left("`most` is a whole number from 1 to 8.")
    }
  }
}
