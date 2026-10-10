package grit.outline.query

/** A query's positional operand: its form in the usage line, its MCP parameter name, and what it takes. */
final case class Operand(form: String, parameter: String, sentence: String)

/** One flag a query's parser accepts. `argument` is its value's form, empty for a flag that takes none;
  * `parameter` is the MCP parameter it is offered as, none for a flag MCP does not offer.
  */
final case class Flag(name: String, argument: String, parameter: Option[String], sentence: String)

/** One example: the task it serves, and the CLI arguments that run it (after `scripts/outline`). */
final case class Example(task: String, args: String)

/** A query's help: its purpose in task terms, its examples and its flags. The CLI's help and the MCP tool's
  * description both read it.
  */
final case class Entry(
    query: String,
    operand: Operand,
    purpose: String,
    examples: Vector[Example],
    flags: Vector[Flag]
)

/** The five queries' help, as plain data: nothing here reads a file or runs a query. */
object Help {

  private val cap = Flag(
    "--cap",
    "BYTES",
    None,
    "cut the answer at this many bytes, a number; default 80000."
  )

  private val root = Flag(
    "--root",
    "DIR",
    Some("root"),
    "a checkout or worktree to read, from its compiled TASTy; default the repository above the working directory, and for the MCP server its start-up root."
  )

  private val common: Vector[Flag] = Vector(cap, root)

  val show: Entry = Entry(
    query = "show",
    operand = Operand(
      "Sym[,Sym…]",
      "symbols",
      "symbols to show, each Name, Name.member or fully qualified; comma-separated on the CLI."
    ),
    purpose =
      "To read or call a definition: its signature and Scaladoc, with one-line outlines of the project types it names. Pass every symbol you need in one call. After them, a Tests section lists the tests in test sources that exercise the symbols: a `## tests exercising <Sym>` line, then per file a `== file  package` line and entries `NN-MM <Suite>: <test name>`, or `NN-MM <Suite>: helper <def>` for a use in a suite's helper. Those lines are not file text.",
    examples = Vector(
      Example("read one move's signature and doc", "show Moves.ask"),
      Example(
        "read two private members you are about to edit",
        "show Render --body oneLine,caseItem --private"
      )
    ),
    flags = Vector(
      Flag(
        "--depth",
        "0|1|2",
        Some("depth"),
        "how many levels of project types to outline under the named ones: 0, 1 or 2; default 1."
      ),
      Flag(
        "--body",
        "m[,m…]",
        Some("body"),
        "member names, comma-separated, of the definitions shown (their simple names); there is no `all` or `full`."
      ),
      Flag(
        "--private",
        "",
        Some("private"),
        "include private members. Ask for them only to edit them or to understand an implementation; otherwise trust the public interface and referential transparency to understand the code. Name the members you need with --body; --private on a whole type with no --body lists every internal helper."
      ),
      Flag(
        "--no-tests",
        "",
        Some("tests"),
        "whether the Tests section lists the tests in test sources that exercise the named symbols; true by default, and --no-tests (MCP tests: false) drops it."
      )
    ) ++ common
  )

  val family: Entry = Entry(
    query = "family",
    operand = Operand("Trait", "trait", "the trait or abstract class, by name."),
    purpose = "To implement or change a trait: its implementations, contracts and their suites.",
    examples = Vector(
      Example(
        "see one member of Inbox across its implementations and contracts",
        "family Inbox --member hear"
      )
    ),
    flags = Vector(
      Flag(
        "--member",
        "m",
        Some("member"),
        "only this member, in the trait and in each implementation: a member name as the trait declares it."
      ),
      Flag(
        "--body",
        "",
        Some("body"),
        "with --member, include that member's body too; takes no value."
      )
    ) ++ common
  )

  val uses: Entry = Entry(
    query = "uses",
    operand = Operand(
      "Sym[,Sym…]",
      "symbols",
      "symbols whose references to find: Name, Name.member or fully qualified; a Name with no qualifier shorter than 4 characters, such as of or get, is refused: qualify it with at least its owner, as in MoveLimits.of; comma-separated on the CLI."
    ),
    purpose = "To find who calls or names a definition before changing it.",
    examples = Vector(
      Example("find the references to Inbox.hear outside core", "uses Inbox.hear --outside core"),
      Example("find the calls to a short member name, qualified by its owner", "uses MoveLimits.of")
    ),
    flags = Vector(
      Flag("--in", "PREFIX", Some("in"), "only files whose repo-relative path starts with PREFIX."),
      Flag(
        "--outside",
        "PREFIX",
        Some("outside"),
        "drop files whose repo-relative path starts with PREFIX."
      )
    ) ++ common
  )

  val area: Entry = Entry(
    query = "area",
    operand = Operand(
      "name[,name…]",
      "names",
      "declared area names, as the project's .outline.conf declares them; comma-separated on the CLI."
    ),
    purpose = "To get a map of a part of the project.",
    examples = Vector(
      Example("map the storage area", "area storage")
    ),
    flags = Vector(
      Flag(
        "--level",
        "0|1",
        Some("level"),
        "0 for one line per package, 1 for each definition with its doc's first sentence; default 1."
      )
    ) ++ common
  )

  val tests: Entry = Entry(
    query = "tests",
    operand = Operand("Suite", "suite", "the suite's class or object, by name or fully qualified."),
    purpose =
      "To add or change a test in a suite: its helpers and its tests' names with line ranges; `--test NAME` prints the body of each test whose name starts with NAME.",
    examples = Vector(
      Example(
        "read the bodies of OneLineTests' tests named \"a trait\"",
        "tests OneLineTests --test \"a trait\""
      )
    ),
    flags = Vector(
      Flag(
        "--test",
        "NAME",
        Some("test"),
        "print the verbatim body of each test whose name starts with NAME."
      ),
      Flag(
        "--body",
        "h[,h…]",
        Some("body"),
        "print each named helper's own lines, its doc, signature and body, after its signature line; a name matching no helper is named in a note with the helpers' names."
      )
    ) ++ common
  )

  /** Every query, in the order the summary lists them. */
  val entries: Vector[Entry] = Vector(show, family, uses, area, tests)

  /** Which text show, family and tests print is the file's own, for an Edit to match; in the help and descriptions of those three and in the server's instructions. */
  val fileText: String =
    "File text: in show and family, each definition's lines after its NN-MM header are the file's own lines, unchanged: its doc and signature, and with --body its body (family: with --member and --body). In tests --test, each printed test is the file's own lines. Not file text: the ##, == and -- lines, the NN-MM header lines, the → lines, the [N KB] line, and tests' helper and test-name lines; a helper's --body lines are file text. A family line that gives an implementation, a contract or a member its collapsed signature after the NN-MM header is not file text. An Edit may take file text as old_string without a Read first; Read the line range only when an Edit fails to match. Exceptions: a file tagged [stale: ...] may print lines that no longer match its source; a cap cuts whole entries, never inside one; byte-exactness is tested for LF files only."

  /** The queries whose output is file text, and so carry `fileText` in their help and descriptions. */
  private val readsFiles: Set[String] = Set("show", "family", "tests")

  /** The entry for `query`, none when no query is named so. */
  def entry(query: String): Option[Entry] = entries.find(_.query == query)

  /** The sentence the MCP parameter `parameter` of `e` is described by, none when `e` offers no such parameter. */
  def sentence(e: Entry, parameter: String): Option[String] =
    if (e.operand.parameter == parameter) Some(e.operand.sentence)
    else e.flags.find(_.parameter.contains(parameter)).map(_.sentence)

  /** The one-line usage of `e`, as the CLI prints it on an error. */
  def usage(e: Entry): String =
    (Vector(e.query, e.operand.form) ++ e.flags.map(f => flagForm(f, bracketed = true))).mkString(
      "usage: ",
      " ",
      ""
    )

  /** The MCP tool description of `e`: its purpose, then its examples as the CLI writes them, then `fileText` when `e` prints file text. */
  def description(e: Entry): String =
    (Vector(e.purpose, "", "Examples, as the CLI writes them:") ++
      e.examples.map(x => s"  scripts/outline ${x.args}  # ${x.task}")).mkString("\n") + fileTail(e)

  /** The text `fileText` adds after an entry's own help, none for a query that prints no file text. */
  private def fileTail(e: Entry): String =
    if (readsFiles.contains(e.query)) s"\n\n$fileText" else ""

  /** The full help of `e`, which `scripts/outline <query> --help` prints. */
  def help(e: Entry): String =
    (Vector(usage(e), "", e.purpose, "", "examples:") ++
      e.examples.flatMap(x => Vector(s"  ${x.task}", s"    scripts/outline ${x.args}")) ++
      Vector("", "arguments and flags:") ++
      Vector(s"  ${e.operand.form}", s"      ${e.operand.sentence}") ++
      e.flags.flatMap(f => Vector(s"  ${flagForm(f, bracketed = false)}", s"      ${f.sentence}")))
      .mkString("\n") + fileTail(e)

  /** One line per query with its purpose, which `scripts/outline --help` prints. */
  def summary: String =
    (Vector(
      "usage: scripts/outline <query> [arguments]    (scripts/outline <query> --help for its help)",
      ""
    ) ++
      entries.map(e => s"  ${e.query.padTo(7, ' ')} ${e.purpose}")).mkString("\n")

  private def flagForm(f: Flag, bracketed: Boolean): String = {
    val spelled = if (f.argument.isEmpty) f.name else s"${f.name} ${f.argument}"
    if (bracketed) s"[$spelled]" else spelled
  }
}
