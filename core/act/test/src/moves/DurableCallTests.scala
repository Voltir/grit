package grit.act.moves

import grit.core.act.Called
import grit.core.act.MovesFixtures.{Judging, shown}
import grit.core.flow.{TestTainted, Trusted}
import grit.core.place.Place
import grit.core.tool.Outcome
import grit.core.visibility.{
  Accounted,
  Cap,
  DeclaredLabel,
  Facts,
  Integrity,
  Label,
  Labelled,
  Level,
  Provenance,
  Readers,
  ResultPart,
  RoomLabels
}

import utest.*

/** [[DurableMoves]]' calls, as an edge's answer accounts for its items and the places they were
  * kept at: what no scripted world
  * can show, since a scripted call's answer is the world's to give.
  */
object DurableCallTests extends TestSuite {
  import MovesContract.*

  private def under(name: String): Place =
    Place.read(s"${Probe.place.written}/$name").fold(sys.error, identity)

  val tests = Tests {
    test(
      "a call's answer is believed at its items as its service declares, met with its arguments"
    ) {
      val (vetted, open) = (under("vetted"), under("open"))
      val world = new MovesWorld(
        broken = false,
        Floor,
        capped = Vector(Cap(Probe.place, Integrity.Member), Cap(vetted, Integrity.Outside)),
        answer = Outcome.Done(
          Read,
          None,
          Accounted.of(
            Vector(Provenance(vetted, Integrity.Authority), Provenance(open, Integrity.Authority))
          )
        )
      )
      val got = world.run(limits(0, 2), Vector(), new Judging()) { m =>
        Seen(
          Vector(Integrity.Authority, Integrity.Untrusted).zipWithIndex.map((i, n) =>
            m.call(name(s"c$n"), Probe, Tool, TestTainted.at(Args, i)).map {
              case Called.Done(text, _) => Integrity.written(text.integrity)
              case other => shown(other)
            }
          )
        )
      }
      got ==> Seen(Vector(Right("outside"), Right("untrusted")))
    }

    test(
      "a call's answer holds nothing above its service's place's label joined with its kept parts' places"
    ) {
      val staff = under("staff")
      val internal = Label.at(Level.Internal)
      def at(labelled: Vector[(Place, DeclaredLabel)], answer: Outcome): Seen = {
        val rooms = RoomLabels
          .of(labelled, Labelled.Mapped(Label.Public))
          .fold(p => throw new java.lang.AssertionError(p.written), identity)
        val world = new MovesWorld(
          broken = false,
          Label.at(Level.Confidential),
          answer = answer,
          rooms = rooms
        )
        world.run(limits(0, 1), Vector(), new Judging())(m => Seen(Vector(called(m, "c"))))
      }
      val cut = Outcome.Done(
        "Found:",
        None,
        Accounted.of(Vector.empty),
        Some(Vector(ResultPart(Provenance(staff, Integrity.Member), Readers.AtPlace, "rota.md")))
      )
      Vector(
        at(Vector(Probe.place -> internal), Outcome.Done(Read)),
        at(Vector(staff -> internal), cut)
      ) ==> Vector(
        Seen(Vector(Right(s"Done($Read,$internal)"))),
        Seen(Vector(Right(s"Done(Found:\nrota.md,$internal)")))
      )
    }

    test("a call's answer hands over its kept parts' facts, and its own") {
      val staff = under("staff")
      def facts(name: String) = Facts.of(ujson.Obj("name" -> name))
      def part(text: String, facts: Option[Facts]) =
        ResultPart(Provenance(staff, Integrity.Member), Readers.AtPlace, text, facts)
      val world = new MovesWorld(
        broken = false,
        Floor,
        answer = Outcome.Done(
          "Found:",
          None,
          Accounted.of(Vector.empty),
          Some(
            Vector(
              part("rota.md", Some(facts("rota"))),
              part("notes.md", None),
              part("plan.md", Some(facts("plan")))
            )
          ),
          Some(facts("listing"))
        )
      )
      val got = world.run(limits(0, 1), Vector(), new Judging()) { m =>
        Seen(
          Vector(m.call(name("c"), Probe, Tool, TestTainted.at(Args, Integrity.Authority)).map {
            case Called.Done(answer, _) =>
              val stated = Trusted.asIs(answer).extract.stated
              s"${stated.answer.map(_.json.render())} ${stated.parts.map(_.json.render())}"
            case other => shown(other)
          })
        )
      }
      got ==> Seen(
        Vector(Right("""Some({"name":"listing"}) Vector({"name":"rota"}, {"name":"plan"})"""))
      )
    }
  }
}
