package grit.act.moves

import grit.core.act.Called
import grit.core.act.MovesFixtures.{Judging, shown}
import grit.core.flow.TestTainted
import grit.core.place.Place
import grit.core.tool.Outcome
import grit.core.visibility.{Accounted, Cap, Integrity, Provenance}

import utest.*

/** [[DurableMoves]]' calls, as an edge's answer accounts for its items: what no scripted world
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
  }
}
