package grit.app.main

import grit.kit.deployment.Offered
import grit.models.CacheMarks

import utest.*

/** Which repeated prefixes a run's calls to Anthropic's models mark (`GRIT_CACHE_MARKS`). */
object CacheMarksChoiceTests extends TestSuite {

  val tests: Tests = Tests {
    test(
      "GRIT_CACHE_MARKS: unset marks the stable messages; stable, task or established choose; any other word is an error"
    ) {
      def declared(env: Map[String, String]): Either[String, CacheMarks.Upto] =
        Deploying
          .deployment(env, Offered.Read, Vector.empty, Vector.empty, java.time.ZoneOffset.UTC)
          .map(_.cacheMarks)
      Vector(
        Map.empty[String, String],
        Map("GRIT_CACHE_MARKS" -> "stable"),
        Map("GRIT_CACHE_MARKS" -> " task "),
        Map("GRIT_CACHE_MARKS" -> "established"),
        Map("GRIT_CACHE_MARKS" -> "drawn")
      ).map(declared) ==> Vector(
        Right(CacheMarks.Upto.Stable),
        Right(CacheMarks.Upto.Stable),
        Right(CacheMarks.Upto.Task),
        Right(CacheMarks.Upto.Established),
        Left("GRIT_CACHE_MARKS is none of stable, task, established")
      )
    }
  }
}
