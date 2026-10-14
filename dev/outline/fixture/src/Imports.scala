package grit.outline.fixture

import grit.outline.fixture.store.{Classy, MemStore as Kept}
// format: off
import grit.outline.fixture.store.{
  Store,
  StoreSuite => Suite
}
// format: on

/** Names reached through braced imports, one split across lines. */
object Imports {
  def n: Int = Classy.n
  def kept: Option[Kept] = None
  def store: Option[Store] = None
  def suite: Suite.type = Suite
}
