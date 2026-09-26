package grit.dbos.sql

import scala.io.Source
import scala.util.Using

import utest.*

/** Every table in `schema.sql` declares what bounds it: `journal` (the raw window), `ledger`
  * (the closings' window), `cache` (rebuilt from closings, deleted with them) or `kept`
  * (settings and catalogs, bounded by what people change). A table nothing bounds is how a
  * database grows without end.
  */
object SchemaRetentionTests extends TestSuite {

  private val Classes = Set("journal", "ledger", "cache", "kept")

  private def schema: Vector[String] =
    Using.resource(
      Option(getClass.getResourceAsStream("/schema.sql")).getOrElse(sys.error("no schema.sql"))
    )(Source.fromInputStream(_).getLines().toVector)

  /** Every line that creates a table, in whatever form. */
  private val Creates = """(?i)\s*CREATE\b[^;]*\bTABLE\b.*""".r

  /** Each table, and the class its comment block declares, if any. */
  private def declared: Vector[(String, Option[String])] = {
    val lines = schema
    val Table = """CREATE TABLE IF NOT EXISTS grit\.(\w+).*""".r
    val Declared = """-- Retention: (\w+)\b.*""".r
    lines.indices.toVector.collect {
      case i if Table.matches(lines(i)) =>
        val name = lines(i) match { case Table(n) => n; case _ => "" }
        val comments = lines.take(i).reverse.takeWhile(_.startsWith("--"))
        name -> comments.collectFirst { case Declared(c) => c }
    }
  }

  val tests = Tests {
    test("every table declares one of the retention classes") {
      val tables = declared
      // Every table is read: one created in another form (UNLOGGED, no schema) is not missed.
      tables.size ==> schema.count(Creates.matches)
      tables.filterNot(_._2.exists(Classes)) ==> Vector()
    }
  }
}
