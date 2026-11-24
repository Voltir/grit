package grit.mcp.searchconsole

import java.time.LocalDate

import grit.core.clock.Clock

/** [[ConsoleRestContract]] over [[FakeConsole]]: `sites-none` as Google answered a service
  * account that is a user of no property, the other fixtures made by hand from the API's
  * reference.
  */
object ConsoleRestRecordedTests extends ConsoleRestContract {

  private val fake = FakeConsole.start()
  fake.sharing()

  override def utestAfterAll(): Unit = fake.stop()

  protected def rest: ConsoleRest^ = fake.rest(Clock.system())
  protected def site: SiteUrl = FakeConsole.site("sc-domain:example.com")
  protected def today: LocalDate = LocalDate.parse("2026-11-24")
}
