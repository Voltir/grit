# grit.formats

Implements `Format` (`grit.core.files`), the readers a deployment reads files with
([`docs/extending.md`](../../docs/extending.md#extension-points)), and `Defaults`, the choice
of them a deployment makes unless it makes its own (ADR 0044).

A shipped extension (ADR 0021): it names a generic format and no organisation, depends on
`grit.core` alone, and is named only by a deployment (`formats-names-core-alone`). Each reader
makes `grit.prose`'s tree from one file, says what of the file it covers and what it saw and
did not read, and stops at each of the `ReadLimits` it is handed with what it has read. None
throws for a file it cannot read: a malformed file is `Unread.Damaged`, or a tree that covers
only what came before the damage.

In dependency order:

- **`limits`**: what every reader stops at: `Guard.until` runs one read with its counts (the
  characters read, the bytes unpacked, the deadline checked every 1024 steps) and answers the
  limit that stopped it; a zip part's stream counted as it is unpacked; XML read with no DTD and
  no external entity. The JDK alone. Imports nothing in formats.
- **`ooxml`**: Word documents (`Docx`) and Excel workbooks (`Xlsx`), transitional or strict,
  read over the JDK's `ZipFile` and StAX by their parts' local names. `ZipFile`, not a stream,
  since a stream unpacks every entry it skips outside the count. The JDK alone. ← `limits`
- **`choice`**: `Defaults`, which format reads which kind. ← `ooxml`

The deadline is cooperative: a reader checks it between steps, so a single slow step inside a
library is not interrupted, and only the byte limit bounds it.

No source file sits at the module's root. The test tree mirrors it; `choice.Fixtures` writes the
files the suites read, each built part by part in the test that reads it, bombs included.
