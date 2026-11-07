package grit.databases.edge

import scala.concurrent.duration.FiniteDuration

import grit.core.edge.{Route, ToolRequest}
import grit.core.host.{Clipped, Kept}
import grit.core.tool.{
  Args,
  ArgsError,
  Believed,
  Field,
  Gate,
  Hosted,
  Outcome,
  Retry,
  ToolName,
  ToolSet,
  ToolSpec,
  Toolbox
}
import grit.databases.connect.{Clip, DatabaseReader, OneStatement, QueryRefusal, Relation, Rows}
import grit.databases.declare.Database
import grit.edge.{Run, Tools}
import grit.prose.form.{Block, Cell, Column, Doc, Text}
import grit.prose.markdown.Markdown
import grit.prose.words.Words

/** The two tools of `database`'s edge, as `described` describes them, run over `reader`: what
  * the role may read, and one read-only statement's rows. Each answer is at most
  * `database.limits.bytes` bytes of UTF-8 cut at a whole line, then a line saying what was cut
  * ([[Clipped]]). A refusal is [[Outcome.Failed]], one sentence naming the database ("the leads
  * database"); `said` hears a failure's SQLSTATE and message, which the model is never told.
  */
private[edge] final class DatabaseTools(
    database: Database,
    described: DatabaseTools.Described,
    reader: DatabaseReader^
) extends Tools {

  def run(route: Route, request: ToolRequest, said: String => Unit): Outcome = {
    val tables = described.tables.over { table =>
      reader.readable() match {
        case Left(r) =>
          DatabaseTools.logged(r).foreach(why => said(s"${described.tablesName}: $why"))
          Outcome.Failed(DatabaseTools.listingRefused(database, r))
        case Right(relations) =>
          table.fold(Right(DatabaseTools.listing(database, relations)))(
            DatabaseTools.relation(database, relations, _)
          ) match {
            case Right(text) => Outcome.Done(text)
            case Left(why) => Outcome.Failed(why)
          }
      }
    }
    val query = described.query.over { statement =>
      reader.query(statement) match {
        case Left(r) =>
          DatabaseTools.logged(r).foreach(why => said(s"${described.queryName}: $why"))
          Outcome.Failed(DatabaseTools.refused(database, r))
        case Right(rows) => Outcome.Done(DatabaseTools.answer(database, rows))
      }
    }
    Toolbox.of[caps.CapSet^{reader, said}](tables, query) match {
      case Right(box) => Run.request(request, box)
      // Their names differ by suffix.
      case Left(repeated) =>
        Outcome.Failed(s"two database tools are named ${ToolName.value(repeated.name)}.")
    }
  }
}

private[edge] object DatabaseTools {

  /** The two tools of a database, described. */
  final case class Described(tables: Hosted[Option[String]], query: Hosted[OneStatement]) {

    /** The two, as the edge advertises them. */
    def offered: ToolSet =
      ToolSet.of(Vector(tables.entry, query.entry)).getOrElse(ToolSet.Empty)

    def tablesName: String = ToolName.value(tables.name)
    def queryName: String = ToolName.value(query.name)
  }

  /** `{service}_tables` and `{service}_query` for `database`: the first taking an optional
    * `table` (`schema.name`, or `name` alone, matched in every schema, either ignoring case);
    * the second `sql`, refused as an argument unless it is one statement
    * ([[OneStatement.of]]). Both free, read-only and rerun when their edge dies running them.
    * `Left` when the service's name is over 57 characters, since a tool's is at most 64.
    */
  def described(database: Database): Either[String, Described] = {
    val service = database.service.name
    val db = named(database)
    val limits = database.limits
    for {
      tablesName <- ToolName.of(s"${service}_tables")
      queryName <- ToolName.of(s"${service}_query")
    } yield Described(
      new Hosted(
        ToolSpec(
          tablesName,
          s"List the tables and views of $db that you may read. With no table, one line for " +
            "each: its name, whether it is a table or a view, and the first line of its " +
            "description. With a table's name, its columns (name, type and description) and the " +
            "tables its columns refer to. The descriptions say what a column means, such as " +
            "which of two ids is which: read them before writing a query.",
          Args
            .of(
              (table =
                Field
                  .text(
                    "A table's or view's name, as schema.name or the name alone, to see its columns."
                  )
                  .optional
              )
            )
            .refine(a => Right(a.table.map(_.trim).filter(_.nonEmpty))),
          Retry.Rerun
        ),
        Gate.Free,
        table => table.getOrElse("its tables"),
        Believed.Default
      ),
      new Hosted(
        ToolSpec(
          queryName,
          s"Run one read-only SQL statement (Postgres) on $db and get its rows back as a table. " +
            "List the tables first: you may read only the tables and columns that listing " +
            "shows, and anything else is refused. Send a single statement with no ; inside it. " +
            s"At most ${limits.rows} rows and ${size(limits.bytes)} come back, and a statement " +
            s"still running after ${duration(limits.timeout)} is stopped, so filter, aggregate " +
            "and LIMIT in the SQL. NULL is shown as NULL; text that is empty or reads NULL is " +
            "shown in single quotes.",
          Args
            .of((sql = Field.text("One SQL statement that only reads.")))
            .refine(a => OneStatement.of(a.sql).toRight(ArgsError.Refused("sql", OneOnly))),
          Retry.Rerun
        ),
        Gate.Free,
        statement => OneStatement.text(statement).take(Hosted.ShownMax),
        Believed.Default
      )
    )
  }

  /** What a statement that is not one is refused with. */
  val OneOnly: String = "Send one SQL statement, with no ; inside it."

  /** "the leads database", for `database`'s service `leads`. */
  def named(database: Database): String = s"the ${database.service.name} database"

  /** A line for each of `relations`: `schema.name (kind)`, then `: ` and its comment's first
    * line when it has one; cut as the class says.
    */
  def listing(database: Database, relations: Vector[Relation]): String =
    if (relations.isEmpty) s"You may read no table or view in ${named(database)}."
    else {
      val lines = relations.map { r =>
        s"- ${r.schema}.${r.name} (${kind(r.kind)})" +
          r.comment.flatMap(firstLine).fold("")(c => s": $c")
      }
      val text = (s"The tables and views you may read in ${named(database)}:" +: lines)
        .mkString("\n")
      Clipped
        .head(
          text,
          _.map(k =>
            s"The first ${math.max(0, k.lines - 1)} of them: the rest ${tooLong(database, k)}. " +
              "Name one to see its columns."
          ),
          database.limits.bytes
        )
        .show
    }

  /** Each of `relations` named `table`, as the class says `table` matches: its kind and
    * comment, a table of its columns, and a line for each reference; `Left`, the sentence the
    * model is told, when none is.
    */
  def relation(
      database: Database,
      relations: Vector[Relation],
      table: String
  ): Either[String, String] = {
    val matched = relations.filter { r =>
      table.equalsIgnoreCase(s"${r.schema}.${r.name}") || table.equalsIgnoreCase(r.name)
    }
    if (matched.isEmpty)
      Left(
        s"${named(database).capitalize} has no table or view named $table that you may read: " +
          "list the tables, naming none, to see them."
      )
    else {
      val text = matched.map(shown).mkString("\n\n")
      Right(
        Clipped
          .head(
            text,
            _.map(k => s"The first ${k.lines} lines of it: the rest ${tooLong(database, k)}."),
            database.limits.bytes
          )
          .show
      )
    }
  }

  /** One relation: its name and kind, its comment, its columns, its references. */
  private def shown(r: Relation): String = {
    val head = s"${r.schema}.${r.name} (${kind(r.kind)})" + r.comment.fold("")(c => s": $c")
    val columns =
      if (r.columns.isEmpty) "You may read none of its columns."
      else
        table(
          Vector("column", "type", "description"),
          r.columns.map(c => Vector(c.name, c.typeName, c.comment.getOrElse("")))
        )
    val references = r.references.map { ref =>
      s"- (${ref.pairs.map(_._1).mkString(", ")}) references ${ref.schema}.${ref.table} " +
        s"(${ref.pairs.map(_._2).mkString(", ")})"
    }
    (Vector(head, columns) ++ Option.when(references.nonEmpty)(
      ("References:" +: references).mkString("\n")
    )).mkString("\n\n")
  }

  /** What a query answered: its rows as a markdown table, a value as Postgres writes it, NULL
    * written `NULL` and a text that is empty or reads `null` in any case in single quotes, then
    * "No rows." when it has none; that it returned no result when `rows` is `None`. Cut as the
    * class says, else said where the reader clipped it ([[Clip]]).
    */
  def answer(database: Database, rows: Option[Rows]): String = rows match {
    case None => "The statement ran and returned no result."
    case Some(result) if result.columns.isEmpty =>
      s"The statement returned ${result.rows.size} rows with no columns."
    case Some(result) =>
      val written = table(result.columns, result.rows.map(_.map(cell)))
      val text = if (result.rows.isEmpty) s"$written\n\nNo rows." else written
      val bytes = size(database.limits.bytes)
      val clipped = Clipped.head(
        text,
        _.map { k =>
          val kept = math.max(0, k.lines - 2)
          if (kept == 0) s"No row fits in $bytes: select fewer columns."
          else s"The first $kept rows: the rest ${tooLong(database, k)}."
        },
        database.limits.bytes
      )
      val said = clipped.hint.orElse(result.clip.map {
        case Clip.Rows => s"The first ${result.rows.size} rows of more."
        case Clip.Bytes if result.rows.isEmpty => s"No row fits in $bytes: select fewer columns."
        case Clip.Bytes => s"The first ${result.rows.size} rows: the next would go over $bytes."
      })
      said.fold(clipped.text)(h =>
        if (clipped.text.isEmpty) s"[$h]" else s"${clipped.text}\n\n[$h]"
      )
  }

  /** What a query refused for `r` is answered, in one sentence. */
  def refused(database: Database, r: QueryRefusal): String = {
    val db = named(database)
    r match {
      case QueryRefusal.NotPermitted(f) =>
        s"${db.capitalize} does not allow reading that: ${bare(f.message)}."
      case QueryRefusal.Rejected(f) => s"${db.capitalize} refused the query: ${bare(f.message)}."
      case QueryRefusal.Writes =>
        s"${db.capitalize} is only read here, and that statement would change it, so it was not run."
      case QueryRefusal.TimedOut =>
        s"The query ran past ${duration(database.limits.timeout)} and was stopped: narrow it, or add a LIMIT."
      case QueryRefusal.TooLarge =>
        "The answer was too large to read: select fewer columns or rows."
      case QueryRefusal.Unreachable(_) => s"${db.capitalize} could not be reached just now."
      case QueryRefusal.RoleChanged(_) => s"${db.capitalize} could not be read just now."
    }
  }

  /** What a listing refused for `r` is answered: as [[refused]] says, but for a timeout and an
    * answer too large, which say the listing.
    */
  def listingRefused(database: Database, r: QueryRefusal): String = r match {
    case QueryRefusal.TimedOut =>
      s"${named(database).capitalize} took longer than ${duration(database.limits.timeout)} " +
        "to list its tables."
    case QueryRefusal.TooLarge => "The list of tables was too large to read."
    case other => refused(database, other)
  }

  /** What the operator is told of `r`: its SQLSTATE and message, or the roles a session was;
    * `None` for a refusal the model's own statement explains.
    */
  def logged(r: QueryRefusal): Option[String] = r match {
    case QueryRefusal.NotPermitted(f) => Some(failure(f.state, f.message))
    case QueryRefusal.Rejected(f) => Some(failure(f.state, f.message))
    case QueryRefusal.Unreachable(f) => Some(failure(f.state, f.message))
    case QueryRefusal.RoleChanged(seen) =>
      Some(
        s"the session was ${seen.current} (session ${seen.session}), not the declared role; " +
          "its connection is closed"
      )
    case QueryRefusal.TooLarge =>
      Some("an answer passed the driver's buffer; its connection is closed")
    case QueryRefusal.Writes | QueryRefusal.TimedOut => None
  }

  private def failure(state: Option[String], message: String): String =
    state.fold(message)(s => s"$s $message")

  /** A value as a cell's text: NULL as `NULL`, a text that would read as it, or as nothing, in
    * single quotes.
    */
  private def cell(value: Option[String]): String = value match {
    case None => "NULL"
    case Some(s) if s.isEmpty || s.equalsIgnoreCase("null") => s"'$s'"
    case Some(s) => s
  }

  /** `columns` and `rows` as a markdown table, a row a line. */
  private def table(columns: Vector[String], rows: Vector[Vector[String]]): String =
    Markdown.write(
      Doc(
        Vector(
          Block.table(
            columns.map(c => Column(Text.plain(c))),
            rows.map(_.map(v => Cell.Words(Text.plain(v))))
          )
        )
      )
    )

  /** Why a cut that kept `k` stopped: past [[Clipped.MaxLines]] lines, or `database`'s bytes. */
  private def tooLong(database: Database, k: Kept): String =
    if (k.lines >= Clipped.MaxLines) s"would go over ${Clipped.MaxLines} lines"
    else s"would go over ${size(database.limits.bytes)}"

  private def kind(k: Relation.Kind): String = k match {
    case Relation.Kind.Table => "table"
    case Relation.Kind.View => "view"
    case Relation.Kind.MaterializedView => "materialized view"
    case Relation.Kind.ForeignTable => "foreign table"
  }

  private def firstLine(s: String): Option[String] =
    s.linesIterator.map(_.trim).find(_.nonEmpty)

  /** A message without a trailing full stop, to end a sentence of its own. */
  private def bare(message: String): String = message.trim.stripSuffix(".")

  /** `bytes` as "32 KB", or "500 bytes" when not whole kilobytes. */
  private def size(bytes: Int): String =
    if (bytes % 1024 == 0) s"${bytes / 1024} KB" else s"$bytes bytes"

  /** `d` as "10 seconds", or "200 milliseconds" when not whole seconds. */
  private def duration(d: FiniteDuration): String = {
    val ms = d.toMillis
    if (ms % 1000 == 0) Words.counted(ms / 1000, "second") else Words.counted(ms, "millisecond")
  }

}
