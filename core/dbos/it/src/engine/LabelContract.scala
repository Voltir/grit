package grit.dbos.engine

import java.sql.Connection
import java.util.concurrent.{CountDownLatch, TimeUnit}

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Using
import scala.util.control.NonFatal

import grit.core.store.Tx
import grit.core.visibility.{Compartment, Label, LabelParts, Level}
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}

import utest.*

/** The lattice twice, as Scala's [[Label]] and as `schema.sql`'s `grit.label` functions, bound
  * over one generated case set: whatever one says of a case, the other says too.
  */
object LabelContract extends TestSuite {

  private lazy val config: DbConfig = {
    val c = TestPostgres.freshDatabase("label_contract")
    LiveEngine.open(c, "test").close()
    c
  }

  private def compartment(name: String): Compartment =
    Compartment.of(name).fold(e => throw new java.lang.AssertionError(e), identity)

  /* Names whose bytewise order and a natural-language collation's disagree: a hyphen and a
   * digit sort before letters bytewise, and some collations skip the hyphen. */
  private val names: Vector[Compartment] =
    Vector("trial", "ab", "a-b", "b1", "acme").map(compartment)

  /** Every label over the four levels and the five compartments. */
  private val labels: Vector[Label] =
    for {
      level <- Level.values.toVector
      mask <- (0 until 32).toVector
    } yield Label.at(
      level,
      names.zipWithIndex.collect { case (c, i) if (mask & (1 << i)) != 0 => c }*
    )

  private def json(l: Label): ujson.Value =
    ujson.Arr(LabelParts.rank(l), ujson.Arr.from(LabelParts.compartments(l).map(ujson.Str(_))))

  /** `l` as an SQL expression over its JSON form at `at`. */
  private def label(at: String): String =
    s"ROW(($at ->> 0)::smallint, ARRAY(SELECT e FROM jsonb_array_elements_text($at -> 1) " +
      "WITH ORDINALITY AS t(e, i) ORDER BY i))::grit.label"

  /** `l`'s parts read back from a `grit.label` column pair: its level and its compartments as
    * JSON text.
    */
  private def parts(rank: Int, compartments: String): (Int, Vector[String]) =
    (rank, ujson.read(compartments).arr.map(_.str).toVector)

  private def stored(l: Label): (Int, Vector[String]) =
    (LabelParts.rank(l), LabelParts.compartments(l))

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

  val tests = Tests {
    test(
      "dominance, join and meet in SQL say what Label's say of every pair of labels over four levels and five compartments"
    ) {
      val pairs = for { a <- labels; b <- labels } yield (a, b)
      val sql =
        s"""SELECT ord, grit.label_dominates(a, b),
           |       (grit.label_join(a, b)).level, to_jsonb((grit.label_join(a, b)).compartments)::text,
           |       (grit.label_meet(a, b)).level, to_jsonb((grit.label_meet(a, b)).compartments)::text
           |  FROM (SELECT ord, ${label("c -> 0")} AS a, ${label("c -> 1")} AS b
           |          FROM jsonb_array_elements(?::jsonb) WITH ORDINALITY AS t(c, ord)) AS cases
           | ORDER BY ord""".stripMargin
      val inSql = query(sql)(
        _.setString(1, ujson.Arr.from(pairs.map((a, b) => ujson.Arr(json(a), json(b)))).render())
      )(rs =>
        (
          rs.getBoolean(2),
          parts(rs.getInt(3), rs.getString(4)),
          parts(rs.getInt(5), rs.getString(6))
        )
      )
      val inScala = pairs.map((a, b) => (a.dominates(b), stored(a.join(b)), stored(a.meet(b))))
      inSql.size ==> pairs.size
      pairs.indices.find(i => inSql(i) != inScala(i)).map(i => (pairs(i), inSql(i), inScala(i))) ==>
        None
    }

    test("label_join_agg is Label's join folded from public, over no labels included") {
      // Runs of 0 to 4 labels from across the case set.
      val lists = (0 until 200).toVector.map(i =>
        (0 until i % 5).toVector.map(j => labels((i * 7 + j * 31) % labels.size))
      )
      val sql =
        s"""SELECT ord, (agg).level, to_jsonb((agg).compartments)::text
           |  FROM (SELECT ord, grit.label_join_agg(${label(
            "e"
          )}) FILTER (WHERE e IS NOT NULL) AS agg
           |          FROM jsonb_array_elements(?::jsonb) WITH ORDINALITY AS t(list, ord)
           |          LEFT JOIN LATERAL jsonb_array_elements(list) AS e ON true
           |         GROUP BY ord) AS folded
           | ORDER BY ord""".stripMargin
      val inSql = query(sql)(
        _.setString(1, ujson.Arr.from(lists.map(l => ujson.Arr.from(l.map(json)))).render())
      )(rs => parts(rs.getInt(2), rs.getString(3)))
      inSql ==> lists.map(l => stored(l.foldLeft(Label.Public)(_.join(_))))
    }

    test(
      "canonical_compartments sorts as LabelParts does, and drops repeats, NULLs and level names"
    ) {
      val sql =
        """SELECT to_jsonb(grit.canonical_compartments(
          |         ARRAY(SELECT e FROM jsonb_array_elements_text(c) WITH ORDINALITY AS t(e, i)
          |                ORDER BY i)))::text
          |  FROM jsonb_array_elements(?::jsonb) WITH ORDINALITY AS t(c, ord)
          | ORDER BY ord""".stripMargin
      // Each label's compartments reversed and repeated.
      val shuffled =
        labels.map(l => LabelParts.compartments(l).reverse ++ LabelParts.compartments(l))
      query(sql)(_.setString(1, ujson.Arr.from(shuffled.map(cs => ujson.Arr.from(cs))).render()))(
        rs => ujson.read(rs.getString(1)).arr.map(_.str).toVector
      ) ==> labels.map(LabelParts.compartments)
      query(
        "SELECT to_jsonb(grit.canonical_compartments(ARRAY['trial', NULL, 'internal', 'restricted', 'a-b']))::text"
      )(_ => ())(rs => rs.getString(1)) ==> Vector("""["a-b", "trial"]""")
    }

    test(
      "a label row is refused unless its compartments are canonical and its level is one of the four, and each label is one row"
    ) {
      def insert(level: String, compartments: String): Option[String] =
        refusal(
          s"INSERT INTO grit.labels (level, compartments) VALUES ($level, '$compartments')"
        )
      Vector(
        insert("1", "{trial,acme}"),
        insert("1", "{acme,acme}"),
        insert("1", "{acme,NULL}"),
        insert("1", "{internal}"),
        insert("4", "{}"),
        insert("-1", "{}"),
        insert("0", "{}")
      ) ==> Vector(
        Some("labels_compartments_check"),
        Some("labels_compartments_check"),
        Some("labels_compartments_check"),
        Some("labels_compartments_check"),
        Some("labels_level_check"),
        Some("labels_level_check"),
        Some("labels_level_compartments_key")
      )
    }

    test(
      "intern_label gives public id 1, the same id for the same label, and refuses a label not canonical"
    ) {
      def intern(level: Int, compartments: String): Either[String, Int] =
        try
          Right(
            query(s"SELECT grit.intern_label(ROW($level, '$compartments')::grit.label)")(_ => ())(
              _.getInt(1)
            ).headOption.getOrElse(-1)
          )
        catch { case NonFatal(e) => Left(Option(e.getMessage).getOrElse(e.toString)) }
      val first = intern(2, "{acme,trial}")
      (intern(0, "{}"), intern(2, "{acme,trial}") == first, intern(1, "{acme}") == first) ==>
        (Right(1), true, false)
      assert(intern(2, "{trial,acme}").left.exists(_.contains("labels_compartments_check")))
    }

    test(
      "two transactions interning one new label at once get one row, the second waiting on the first"
    ) {
      val sql = "SELECT grit.intern_label(ROW(3, '{b1}')::grit.label)"
      def interned(conn: Connection): Int =
        Using.resource(conn.createStatement()) { st =>
          Using.resource(st.executeQuery(sql)) { rs =>
            rs.next()
            rs.getInt(1)
          }
        }
      def connect(): Connection = {
        val c = java.sql.DriverManager.getConnection(config.jdbcUrl, config.user, config.password)
        c.setAutoCommit(false)
        c
      }
      Using.resources(connect(), connect()) { (first, second) =>
        val firstId = interned(first)
        val asked = new CountDownLatch(1)
        given ExecutionContext = ExecutionContext.global
        val secondId = Future {
          asked.countDown()
          val id = interned(second)
          second.commit()
          id
        }
        asked.await(5, TimeUnit.SECONDS)
        // The second is waiting on the first's uncommitted row until it commits.
        Thread.sleep(300)
        assert(!secondId.isCompleted)
        first.commit()
        (
          Await.result(secondId, 10.seconds),
          query(
            "SELECT count(*) FROM grit.labels WHERE level = 3 AND compartments = '{b1}'"
          )(_ => ())(_.getInt(1))
        ) ==> (firstId, Vector(1))
      }
    }
  }
}
