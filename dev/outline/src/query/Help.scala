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

/** The queries' help, as plain data: nothing here reads a file or runs a query. */
object Help {

  /** The KB an answer is cut at when no `--max-kb` is given. */
  val defaultMaxKb: Int = 12

  private val maxKb = Flag(
    "--max-kb",
    "N",
    Some("max_kb"),
    s"cut the answer at N KB (1024 bytes), a whole number from 1; default $defaultMaxKb. It cuts whole entries, never inside one, and a cut answer ends with `-- cut at N KB: M more entries, ~X KB; rerun with --max-kb X or narrow`, X the KB every entry needs. In show, a type of two members or more is cut between its members, its head first, then the members --body names, then the rest in file order; what --body names prints whatever N. A cut type's answer ends with `-- cut at N KB after Turn.route: M more members, ~R KB; rerun with --max-kb X, or --body <member> for one` (the hint only without --body), R the KB of what was cut; its closing lines print only when no member was cut."
  )

  private val root = Flag(
    "--root",
    "DIR",
    Some("root"),
    "a checkout or worktree to read, from its compiled TASTy; default the repository above the working directory, and for the MCP server its start-up root."
  )

  private val common: Vector[Flag] = Vector(maxKb, root)

  val show: Entry = Entry(
    query = "show",
    operand = Operand(
      "Sym[,Sym…]",
      "symbols",
      "symbols to show, each Name, Name.member or fully qualified; comma-separated on the CLI."
    ),
    purpose =
      "To read or call a definition: its signature and Scaladoc, with one-line outlines of the project types it names. Pass every symbol you need in one call. Each definition's NN-MM header, and each outline, ends with its size, `[N.N KB]`; a class, trait, object or enum shown without its body ends with its closing lines, an anchor for an Edit adding a member to it. After them, one line per named symbol counts the tests in test sources that exercise it: `-- tests of <Sym>: N in M suites (nearest: A, B, C); --tests to list`, N the tests holding a reference to it or to a member, M the suites holding one in a test or a helper, and the nearest at most three of those suites, the ones under the symbol's own module directory first, then those holding the most references; `-- tests of <Sym>: none` when no suite holds one. With --tests, a Tests section lists them instead: a `## tests exercising <Sym>` line, then per file a `== file  package` line and entries `NN-MM <Suite>: <test name>`, or `NN-MM <Suite>: helper <def>` for a helper holding a reference itself. Those lines are not file text. A name main sources lack is looked up in test sources, under a `-- in test sources: <full name>` note, with no tests line.",
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
        "member names, comma-separated, of the definitions shown (their simple names); there is no `all` or `full`. A path reaches a nested member (`Settling.began`), printed whole under its owner-qualified name; a bare name none of a shown type's own members has is looked for in its nested owners, printed when one owner has it and named with each owner when several do."
      ),
      Flag(
        "--private",
        "",
        Some("private"),
        "include private members. Ask for them only to edit them or to understand an implementation; otherwise trust the public interface and referential transparency to understand the code. Name the members you need with --body; --private on a whole type with no --body lists every internal helper."
      ),
      Flag(
        "--docs",
        "",
        Some("docs"),
        "print every member's doc whole; without it, a member of a shown type not named in --body prints its doc's first sentence only."
      ),
      Flag(
        "--tests",
        "",
        Some("tests"),
        "list every test and helper exercising the named symbols, in place of the count line; --no-tests drops even the count. MCP tests: \"summary\" (the default, the count line), \"list\" or \"none\"."
      ),
      Flag(
        "--no-tests",
        "",
        None,
        "print neither the tests count line nor the list (MCP tests: \"none\")."
      )
    ) ++ common
  )

  val family: Entry = Entry(
    query = "family",
    operand = Operand("Trait", "trait", "the trait or abstract class, by name."),
    purpose =
      "To implement or change a trait: its implementations, contracts and their suites, each definition's line ending with its size, `[N.N KB]`; the trait, and with --member its member shown without its body when that member is a type, ends with its closing lines.",
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
      "symbols whose references to find: Name, Name.member or fully qualified; a Name with no qualifier shorter than 4 characters, such as of or get, is refused: qualify it with at least its owner, as in MoveLimits.of; comma-separated on the CLI. X.apply or X.new, for a class or enum case X with no apply in source, finds X's construction sites, X(…) and new X(…). X.unapply, for a class or enum case X with no unapply in source, finds its pattern sites, case X(…) and case _: X (a trait or a plain class: case _: X alone). X alone includes case X(…) patterns and X(…) calls with its other uses."
    ),
    purpose =
      "To find who calls or names a definition before changing it. Sites are grouped by file (`== file`), then by the definition holding them: an `in <owner>` line (the member around the site, or `<Suite>: <test name>` for a site in a test) ending with the owner's size, `[N.N KB]`, then each site as `<line>  <source line>`, and last `[N sites in M files]`. Over 20 sites in all, each file prints as `== file  N sites` and each owner once as `in <owner>  N sites`, with no site lines, ending `-- N sites in M files; --sites to list them all, or --in/--outside to narrow`.",
    examples = Vector(
      Example("find the references to Inbox.hear outside core", "uses Inbox.hear --outside core"),
      Example("find where a case class is built", "uses EdgeStores.apply"),
      Example("find the calls to a short member name, qualified by its owner", "uses MoveLimits.of")
    ),
    flags = Vector(
      Flag("--in", "PREFIX", Some("in"), "only files whose repo-relative path starts with PREFIX."),
      Flag(
        "--outside",
        "PREFIX",
        Some("outside"),
        "drop files whose repo-relative path starts with PREFIX."
      ),
      Flag(
        "--sites",
        "",
        Some("sites"),
        "list every site, however many there are; without it, over 20 sites print as counts per file and owner."
      )
    ) ++ common
  )

  val area: Entry = Entry(
    query = "area",
    operand = Operand(
      "[name[,name…]]",
      "names",
      "declared area names, as the project's .outline.conf declares them; comma-separated on the CLI; none to list the declared areas, each with its purpose. A name no area declares, such as a module's (slack), gives the one area whose symbols lie under a package of that name, or else every package of that name, under a note saying so."
    ),
    purpose =
      "To get a map of a part of the project, or with no name the list of the declared areas.",
    examples = Vector(
      Example("list the declared areas and what each is for", "area"),
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

  val find: Entry = Entry(
    query = "find",
    operand = Operand(
      "pattern",
      "pattern",
      "a name pattern: dot-separated segments matched against the last segments of each definition's full name, each whole, with * for any run of characters within one segment; quote it in the shell."
    ),
    purpose =
      "To find a definition when you don't know its name: one line per definition in main sources, members and constructor vals included, with its full name, file, lines, collapsed signature and size, `[N.N KB]`. When main sources hold no match, test sources' matches follow a note saying so; when neither does, a renaming import (import a.X as Y, or a braced selector X as Y or X => Y) giving the name is named, with X's definition. A pattern whose owner is a method says find lists no definition local to one, and names the show that prints its body.",
    examples = Vector(
      Example("find every type whose name holds Shelf", "find '*Shelf*'"),
      Example("find the member placed of any owner", "find '*.placed'"),
      Example("find a private helper whose name holds Source", "find '*Source*' --private")
    ),
    flags = Flag(
      "--private",
      "",
      Some("private"),
      "include private members. Without it, a pattern only private names match says how many, under a `-- N private matches: add --private` note."
    ) +: common
  )

  val tests: Entry = Entry(
    query = "tests",
    operand = Operand("Suite", "suite", "the suite's class or object, by name or fully qualified."),
    purpose =
      "To add or change a test in a suite: its file's imports and its own loose lines as file text, its head and closing lines as file text, its helpers' signatures, the test-source fixtures it uses with the members it names, and its tests' names with line ranges, each line naming a definition or a test ending with its size, `[N.N KB]`. `--test NAME` prints each test whose name starts with NAME; `--body h[,h…]` prints a helper's or a used fixture member's own lines; `--fixtures` lists the used members' signatures. What is named is printed first when the answer is cut.",
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
        "print each named helper's own lines, its doc, signature and body, after its signature line; a name no helper has prints the used fixture member of that name, after the fixtures; a name matching neither is named in a note with the names there are."
      ),
      Flag(
        "--fixtures",
        "",
        Some("fixtures"),
        "list each used fixture member's collapsed signature, with its line range and size, in place of its name."
      )
    ) ++ common
  )

  val file: Entry = Entry(
    query = "file",
    operand = Operand(
      "Path|Name",
      "target",
      "a repo-relative path, a path suffix only one file ends with (TurnEnv.scala), or a definition's name, resolved as show resolves it, main sources first, then test sources."
    ),
    purpose =
      "To edit a file's imports or place a new definition: its loose lines (package, imports, comments outside every definition) as file text, each top-level definition and its members as `NN-MM kind name [N KB]` lines, and its last lines under `NN-MM end of file`, file text. A target several files match is NoMatch naming each; a file with no .tasty fails, naming the command that compiles it.",
    examples = Vector(
      Example(
        "see where a file's imports end and its definitions lie",
        "file core/turn/src/TurnEnv.scala"
      ),
      Example("the same, by a name the file defines", "file TurnEnv")
    ),
    flags = common
  )

  /** Every query, in the order the summary lists them. */
  val entries: Vector[Entry] = Vector(show, family, uses, area, find, tests, file)

  /** Which text show, family and tests print is the file's own, for an Edit to match; in the help and descriptions of those three and in the server's instructions. */
  val fileText: String =
    "File text: in show and family, each definition's lines after its NN-MM header are the file's own lines, unchanged: its doc and signature, and with --body its body (family: with --member and --body). A header is `NN-MM <kind> <Owner.name>  [N.N KB]`, or `NN-MM  [N.N KB]` alone when the definition is top-level or shown under its type and the file text under it shows its kind and name (`def at`); a member shown alone (show Owner.member, family --member) keeps its name. A class, trait, object or enum printed without its body ends, after its members' entries, with its closing lines under an `NN-MM end of <name>` header line: its last line, or its last two when the last holds only closing brackets; those lines are file text, for an Edit that adds a member before them; a type whose head already reaches its last line has none, and no other definition has any. In show, a member of a shown type not named in --body has its doc cut to its first sentence, unless --docs: a doc whose first sentence is its whole first line prints that line, file text, then a `-- … N more doc lines` line; any other cut doc prints as one `-- <first sentence> …` line in its place; a doc of one line prints whole; the member's lines after its doc, from its signature on, are file text. In tests, the lines under each bare `NN-MM` line (the file's and the suite's loose lines, and the suite's head) and under `NN-MM end of <Suite>` are the file's own lines, and with --test each printed test is. In file, the lines under each bare `NN-MM` line (loose lines, outside the definitions there) and under `NN-MM end of file` are the file's own lines; its `NN-MM <kind> <name>` lines are not. Not file text: the ##, == and -- lines, the NN-MM header lines, the → lines, the [N KB] line, and tests' helper, fixture and test-name lines; a helper's or a fixture member's --body lines are file text. A size tag, `[N.N KB]` at the end of a definition's header or of a line naming one, is the bytes of the definition's whole text, doc included (what --body prints for it), to one decimal and at least 0.2; no file-text line carries one, and a definition past the end of its source has none. A family line that gives an implementation, a contract or a member its collapsed signature after the NN-MM header is not file text. An Edit may take file text as old_string without a Read first; Read the line range only when an Edit fails to match. Exceptions: a file tagged [stale: ...] may print lines that no longer match its source, and an answer that read any such file says so after its ## root line, in a `-- stale since the last compile:` line naming each module, its stale files and the command that compiles them; a file is tagged when it changed since its .tasty, as the number and length of its lines tell, so an edit that keeps every line's length is not tagged though its lines may not match; a --max-kb budget cuts whole entries, never inside one; byte-exactness is tested for LF files only."

  /** The queries whose output is file text, and so carry `fileText` in their help and descriptions. */
  private val readsFiles: Set[String] = Set("show", "family", "tests", "file")

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
