package grit.lifecycle.close

import grit.core.period.{Balance, Edit, Section, TestClosings}

import TestClosings.{balance, line}
import utest.*

object ClosingSummaryTests extends TestSuite {
  import CloseFixtures.replyOf

  private val backup = line(Section.Open, "How often should the laptop backup run?", 1, 1)
  private val drive = line(Section.Open, "Which drive holds the off-site copy?", 1, 2)
  private val exiftool = line(Section.Standing, "Photos are renamed with exiftool", 1, 1)
  private val topic = line(Section.Topics, "Photo Rename", 1, 1)
  private val known: Balance = balance(backup, drive, exiftool, topic)

  private def read(text: String, asked: Asked = Asked.Every, from: Balance = known) =
    ClosingSummary.read(replyOf(text), from, asked)

  private def written(prose: String, outcome: Option[String], edits: Edit*) =
    Some(ClosingSummary.Written(prose, outcome, edits.toVector))

  val tests = Tests {
    test("the writer sees open lines as o1…, standing as s1…, in the balance's order; no topics") {
      ClosingSummary.labels(known) ==> Vector("o1" -> backup, "o2" -> drive, "s1" -> exiftool)
    }

    test("each part is read: adds, and resolves, drops and touches by the labels shown") {
      read(
        """Summary: We set up the backup.
          |It runs nightly.
          |Outcome: backups are scheduled
          |Open:
          |- Is the NAS at 192.168.1.20 reachable from the laptop?
          |Standing:
          |* Backups run daily at 02:00 with restic
          |Resolved:
          |- o1: daily at 02:00
          |Dropped:
          |- [s1]: the photos go to Pillow now
          |Touched:
          |- o2""".stripMargin
      ) ==> written(
        "We set up the backup. It runs nightly.",
        Some("backups are scheduled"),
        Edit.Add(Section.Open, "Is the NAS at 192.168.1.20 reachable from the laptop?"),
        Edit.Add(Section.Standing, "Backups run daily at 02:00 with restic"),
        Edit.Resolve(backup.id, "daily at 02:00"),
        Edit.Drop(exiftool.id, "the photos go to Pillow now"),
        Edit.Touch(drive.id)
      )
    }

    test("an item naming no shown label, or none at all, is unread") {
      read("Summary: x\nResolved:\n- o9: done\nTouched:\n- the backup line\n- t1") ==> written(
        "x",
        None,
        Edit.Unread("o9: done", "names no line"),
        Edit.Unread("the backup line", "names no line"),
        Edit.Unread("t1", "names no line")
      )
    }

    test("only the parts asked for are kept; none is no outcome") {
      read(
        "Summary: We talked.\nOutcome: none\nOpen:\n- a question\nStanding:\n- a fact\nResolved:\n- o1: yes",
        Asked(outcome = true, open = false, standing = true, settled = false)
      ) ==> written("We talked.", None, Edit.Add(Section.Standing, "a fact"))
    }

    test("labels in markdown emphasis or as headings, in any case, are read") {
      read(
        """**Summary:** We talked.
          |## standing:
          |- ship it
          |__OUTCOME__: shipped""".stripMargin
      ) ==> written("We talked.", Some("shipped"), Edit.Add(Section.Standing, "ship it"))
    }

    test("text before the first label is the prose when Summary is missing; unlabelled, all") {
      read("We talked about knots.\nStanding:\n- the bowline holds") ==>
        written("We talked about knots.", None, Edit.Add(Section.Standing, "the bowline holds"))
      read("Just some text\nover two lines.") ==> written("Just some text over two lines.", None)
    }

    test("a reply with no prose is none") {
      read("") ==> None
      read("Standing:\n- ship it") ==> None
    }

    test("the request shows what is known by label, then the transcript's end") {
      val r =
        ClosingSummary.request("x" * 50_000, known, Vector.empty, Asked(true, true, false, true))
      r.system ==>
        (ClosingSummary.System + "\n" +
          "Summary: two to four sentences: what was asked, and what came of it.\n" +
          "Outcome: one line: what the conversation came to.\n" +
          "Open: then one line per item, each starting with \"- \": each question left unanswered, task left unfinished or thing not known, that is not already known.\n" +
          "Resolved: then one line per item, each starting with \"- \": each known line this stretch answered, finished or overturned, as its label, a colon and how (\"- o1: done on Monday\").\n" +
          "Dropped: then one line per item, each starting with \"- \": each known line that no longer holds and was not resolved, as its label, a colon and why.\n" +
          "Touched: then one line per item, each starting with \"- \": each known line this stretch relied on or confirmed, as its label (\"- s3\").\n" +
          "Write none under a part with nothing in it.")
      r.messages.map(_.toString) ==> Vector(
        "User(Already known:\nOpen:\n[o1] How often should the laptop backup run?\n" +
          "[o2] Which drive holds the off-site copy?\nStanding:\n[s1] Photos are renamed with exiftool" +
          s"\n\nTranscript:\n${"x" * 40_000})"
      )
      ClosingSummary.request(
        "t",
        Balance.empty,
        Vector.empty,
        Asked(false, false, false, false)
      ) ==>
        grit.core.provider.ModelRequest(
          ClosingSummary.System + "\nSummary: two to four sentences: what was asked, and what came of it.",
          Vector(grit.core.message.Message.User("Already known: nothing.\n\nTranscript:\nt"))
        )
    }

    test("what the period was shown from elsewhere is known, before the transcript; never new") {
      val r = ClosingSummary.request(
        "t",
        Balance.empty,
        Vector("[fs:/home/nick/api] Assistant: Pin TZ=UTC."),
        Asked(false, false, false, false)
      )
      r.messages ==> Vector(
        grit.core.message.Message.User(
          "Already known: nothing.\n\n" +
            "Known elsewhere (shown from other places; never record it here):\n" +
            "[fs:/home/nick/api] Assistant: Pin TZ=UTC.\n\nTranscript:\nt"
        )
      )
      assert(
        ClosingSummary.System.contains(
          "Lines known elsewhere were shown from the person's other conversations: an Open or " +
            "Standing item that restates one is not new, like a line already known."
        )
      )
    }

    test("the prompt carries the rules: a line reads alone, only what is new, absence is open") {
      assert(
        ClosingSummary.System.contains("Every line you add must read alone"),
        ClosingSummary.System.contains("Add only what this stretch newly established"),
        ClosingSummary.System.contains(
          "Something not known, not found or not recorded is an Open item (what to find out), " +
            "never a Standing fact."
        ),
        ClosingSummary.System.contains(
          "never record a recap, a lookup, or a list of earlier activity"
        )
      )
    }
  }
}
