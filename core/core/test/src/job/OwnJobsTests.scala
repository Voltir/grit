package grit.core.job

import grit.core.id.{JobName, PluginName}

import utest.*

/** [[OwnJobs]]: what a plugin's tools may book. */
object OwnJobsTests extends TestSuite {
  import JobTests.Counting

  private val remind = PluginName.of("remind").getOrElse(throw new java.lang.AssertionError())

  val tests = Tests {
    test("a plugin books its own jobs, by name, and is refused another's, naming both") {
      val own = OwnJobs.over(remind, Vector(new Counting("remind"), new Counting("nudge")))
      Vector(new Counting("nudge", 2), new Counting("standup")).map(j =>
        own
          .of(j)
          .map(b => JobName.value(b.job.name))
          .left
          .map(n => (n.plugin, JobName.value(n.job)))
      ) ==> Vector(Right("nudge"), Left((remind, "standup")))
    }
  }
}
