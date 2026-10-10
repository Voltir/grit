package grit.outline.fixture.store

/** Keeps strings by key. */
trait Store {

  /** The value at `key`, if any. */
  def get(key: String): Option[String]

  /** `value` kept at `key`. */
  def put(key: String, value: String): Store
}

/** A store in memory. */
final case class MemStore(values: Map[String, String]) extends Store {
  def get(key: String): Option[String] = values.get(key)
  def put(key: String, value: String): Store = MemStore(values + (key -> value))
}

/** A store that keeps nothing. */
object NoStore extends Store {
  def get(key: String): Option[String] = None
  def put(key: String, value: String): Store = this
}

/** What every store does. */
abstract class StoreContract {

  /** A fresh store. */
  protected def fresh(): Store

  /** Whether `put` then `get` returns the value. */
  def keepsWhatItIsGiven(): Boolean = fresh().put("k", "v").get("k") == Some("v")
}

/** The contract over [[MemStore]]. */
object MemStoreContract extends StoreContract {
  protected def fresh(): Store = MemStore(Map.empty)
}

/** Another thing named get, unrelated. */
object Unrelated {
  def get(key: String): Option[String] = None
}

/** Calls through a lambda. */
object Caller {
  def all(stores: Vector[Store]): Vector[Option[String]] = stores.map(s => s.get("k"))

  def split(s: Store): Option[String] = s
    .get("k")
}

/** A second Tx, so a short name is ambiguous. */
trait Tx

/** Vals bound by one pattern. */
object Pairs {
  val (left, right) = (1, 2)
}

/** A suite in the shape test frameworks use. */
object StoreSuite {
  private def test(name: String)(body: => Unit): Unit = { val _ = name; body }

  /** A fresh store for each test. */
  def fresh(): Store = MemStore(Map.empty)

  def run(): Unit = {
    test("put then get returns the value") {
      assert(fresh().put("k", "v").get("k") == Some("v"))
    }
    test("get of an absent key is None") {
      assert(fresh().get("x").isEmpty)
    }
  }
}

/** A second Quiver, under another owner than the one in the package above. */
trait Quiver

object Twin
