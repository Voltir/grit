package grit.dbos.engine

import scala.util.Using
import scala.util.control.NonFatal

import grit.core.store.Tx
import grit.core.visibility.Integrity.Level
import grit.core.visibility.{Compartment, Integrity}
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}

import utest.*

/** The integrity lattice twice, as Scala's [[Integrity]] and as `schema.sql`'s `grit.integrity`
  * functions, bound over one generated case set: whatever one says of a case, the other says
  * too. A case is stored as its level's rank and its compartments' names.
  */
object IntegrityLatticeContract extends TestSuite {

  private lazy val config: DbConfig = {
    val c = TestPostgres.freshDatabase("integrity_contract")
    LiveEngine.open(c, "test").close()
    c
  }

  private def compartment(name: String): Compartment =
    Compartment.of(name).fold(e => throw new java.lang.AssertionError(e), identity)

  /* Names whose bytewise order and a natural-language collation's disagree. */
  private val names: Vector[String] = Vector("trial", "ab", "a-b", "b1", "acme")

  /** Every case over the four levels and the five compartments: a rank and names. */
  private val cases: Vector[(Int, Vector[String])] =
    for {
      level <- Level.values.toVector
      mask <- (0 until 32).toVector
    } yield (
      level.ordinal,
      names.zipWithIndex.collect { case (c, i) if (mask & (1 << i)) != 0 => c }.sorted
    )

  private def integrity(rank: Int, compartments: Vector[String]): Integrity =
    Integrity.at(Level.fromOrdinal(rank), compartments.map(compartment)*)

  private def json(c: (Int, Vector[String])): ujson.Value =
    ujson.Arr(c._1, ujson.Arr.from(c._2.map(ujson.Str(_))))

  /** A case as an SQL expression over its JSON form at `at`. */
  private def sqlCase(at: String): String =
    s"ROW(($at ->> 0)::smallint, ARRAY(SELECT e FROM jsonb_array_elements_text($at -> 1) " +
      "WITH ORDINALITY AS t(e, i) ORDER BY i))::grit.integrity"

  /** An integrity read back from its level and its compartments as JSON text. */
  private def read(rank: Int, compartments: String): Integrity =
    integrity(rank, ujson.read(compartments).arr.map(_.str).toVector)

  /** Runs `sql` with `bind`, each row read by `row`. */
  private def query[A](sql: String)(bind: java.sql.PreparedStatement => Unit)(
      row: java.sql.ResultSet => A
  ): Vector[A] =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(conn.prepareStatement(sql)) { ps =>
        bind(ps)
        Using.resource(ps.executeQuery()) { rs =>
          val out = Vector.newBuilder[A]
          while (rs.next()) out += row(rs)
          out.result()
        }
      }
    }

  /** The constraint `sql` violates, run in a transaction of its own; `None` when it runs. */
  private def refusal(sql: String): Option[String] = {
    val Violated = """(?s).*violates (?:check|unique) constraint "(\w+)".*""".r
    try {
      LiveDb.transaction(config) { (tx: Tx^) ?=>
        val conn: java.sql.Connection^{tx} = Tx.connection(tx)
        Using.resource(conn.createStatement())(_.execute(sql))
      }
      None
    } catch {
      case NonFatal(e) =>
        Some(Option(e.getMessage).collect { case Violated(name) => name }.getOrElse(e.toString))
    }
  }

  private def intern(rank: Int, compartments: String): Either[String, Int] =
    try
      Right(
        query(s"SELECT grit.intern_integrity(ROW($rank, '$compartments')::grit.integrity)")(_ =>
          ()
        )(_.getInt(1)).headOption.getOrElse(-1)
      )
    catch { case NonFatal(e) => Left(Option(e.getMessage).getOrElse(e.toString)) }

  val tests = Tests {
    test(
      "dominance and meet in SQL say what Integrity's say of every pair over four levels and five compartments"
    ) {
      val pairs = for { a <- cases; b <- cases } yield (a, b)
      val sql =
        s"""SELECT ord, grit.integrity_dominates(a, b),
           |       (grit.integrity_meet(a, b)).level, to_jsonb((grit.integrity_meet(a, b)).compartments)::text
           |  FROM (SELECT ord, ${sqlCase("c -> 0")} AS a, ${sqlCase("c -> 1")} AS b
           |          FROM jsonb_array_elements(?::jsonb) WITH ORDINALITY AS t(c, ord)) AS cases
           | ORDER BY ord""".stripMargin
      val inSql = query(sql)(
        _.setString(1, ujson.Arr.from(pairs.map((a, b) => ujson.Arr(json(a), json(b)))).render())
      )(rs => (rs.getBoolean(2), read(rs.getInt(3), rs.getString(4))))
      val inScala = pairs.map { (a, b) =>
        val (x, y) = (integrity.tupled(a), integrity.tupled(b))
        (x.dominates(y), x.meet(y))
      }
      inSql.size ==> pairs.size
      pairs.indices.find(i => inSql(i) != inScala(i)).map(i => (pairs(i), inSql(i), inScala(i))) ==>
        None
    }

    test("integrity_meet_agg is Integrity's meet folded, and NULL over none") {
      // Runs of 0 to 4 cases from across the case set.
      val lists = (0 until 200).toVector.map(i =>
        (0 until i % 5).toVector.map(j => cases((i * 7 + j * 31) % cases.size))
      )
      val sql =
        s"""SELECT ord, (agg).level, to_jsonb((agg).compartments)::text
           |  FROM (SELECT ord, grit.integrity_meet_agg(${sqlCase(
            "e"
          )}) FILTER (WHERE e IS NOT NULL) AS agg
           |          FROM jsonb_array_elements(?::jsonb) WITH ORDINALITY AS t(list, ord)
           |          LEFT JOIN LATERAL jsonb_array_elements(list) AS e ON true
           |         GROUP BY ord) AS folded
           | ORDER BY ord""".stripMargin
      val inSql = query(sql)(
        _.setString(1, ujson.Arr.from(lists.map(l => ujson.Arr.from(l.map(json)))).render())
      )(rs => Option(rs.getString(3)).map(compartments => read(rs.getInt(2), compartments)))
      inSql ==> lists.map(l => l.map(integrity.tupled).reduceOption(_.meet(_)))
    }

    test(
      "intern_integrity gives untrusted id 1 and member id 2, the same id for the same integrity, and refuses one not canonical"
    ) {
      val first = intern(3, "{acme,trial}")
      (
        intern(0, "{}"),
        intern(2, "{}"),
        intern(3, "{acme,trial}") == first,
        intern(1, "{acme}") == first
      ) ==> (Right(1), Right(2), true, false)
      assert(
        intern(3, "{trial,acme}").left.exists(_.contains("integrity_labels_compartments_check"))
      )
      query(
        "SELECT count(*) FROM grit.integrity_labels WHERE level = 3 AND compartments = '{acme,trial}'"
      )(_ => ())(
        _.getInt(1)
      ) ==> Vector(1)
    }

    test(
      "an integrity row is refused unless its compartments are canonical and its level is one of the four, and each integrity is one row"
    ) {
      def insert(level: String, compartments: String): Option[String] =
        refusal(
          s"INSERT INTO grit.integrity_labels (level, compartments) VALUES ($level, '$compartments')"
        )
      Vector(
        insert("1", "{trial,acme}"),
        insert("1", "{acme,NULL}"),
        insert("4", "{}"),
        insert("-1", "{}"),
        insert("2", "{}")
      ) ==> Vector(
        Some("integrity_labels_compartments_check"),
        Some("integrity_labels_compartments_check"),
        Some("integrity_labels_level_check"),
        Some("integrity_labels_level_check"),
        Some("integrity_labels_level_compartments_key")
      )
    }
  }
}
