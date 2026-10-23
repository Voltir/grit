# 0044. A file is read by the first of a deployment's formats for its kind, into prose noted by where it lies; never silently wrong

Status: accepted (2026-10-22)

Context: an edge is handed files grit cannot read as text today: PDFs, Word documents, Excel
workbooks and Google Slides decks. Reading them is a choice of library and of shape. Measured and turned down:
pandoc as the reader (an xlsx's numbers and dates come out wrong, a bomb leaves no partial
text, ZIP64 is refused); Apache POI, Tika, docx4j or fastexcel in the JVM (each silently drops
or keeps the wrong text somewhere, and several run out of memory on a crafted file); a
structure library for PDFs in the JVM (OpenDataLoader, docling: hundreds of megabytes to
gigabytes for headings the bookmarks already give). In shape, turned down: one module with a
fixed set of readers (a deployment may need a format grit does not ship); a pure reader over
the file's bytes in memory (a zip reader needs random access to a file, or it inflates every
entry it skips); a reader chosen per occasion, a turn's or a sync's (one list serves both until
a slower reader exists); and a page as a block around its paragraphs (a paragraph crossing a
page break belongs to both).

Decision:

- **Formats are an extension point.** `grit.core.files.Format` reads one `OpenFile`, a
  capability valid within the block that opened it, under `ReadLimits` (bytes, characters,
  glyphs, inflated bytes, zip entries, time: the defaults a deployment may only lower). A
  `Format` holds no capability; a deployment's `Formats` gives, for each `Kind`, the formats
  tried in order, and the first that does not answer `Unread.NotMine` wins.
- **A file's kind is its bytes'.** `Kind.sniff` reads its first bytes, and a zip's entries'
  names; the type its sender claims breaks only a tie: a zip holding both a document and a
  workbook, or text claimed as a native file's API answer (a Google Slides deck's JSON).
- **Read into prose, noted by where it lies.** A reader makes a `Doc[Locus]` (ADR 0043): a
  span on a page carries `Locus.Page`, so a paragraph crossing a page break keeps both pages.
  Footnotes and endnotes are an end section, referenced as `[n]`.
- **Never silently wrong.** Every `Understood` says what of the file it `covers` (all of it, or
  the first pages or sheets read and the limit that stopped it) and what it `missed`: what it
  saw and did not read, as comments or a chart. The model's text of a read (`ModelText`)
  states both beside the pages or characters it shows. A reader that cannot read a file says
  why (`Unread`): a kind not read here, a password, a scan with no text, damage, too slow, too
  large; its message names no setting.

Consequences: a new format is one `Format` value in a deployment's `Formats`, which every
deployment names (`Deployment.formats`, no default) and its file area holds (ADR 0045), so an
edge that puts a file and `file_read` read it with the same ones, through `OpenFile.read`. Each read
parses the file again; nothing caches a tree. The deadline is cooperative: a reader checks it
between steps, so one slow step inside a library is not interrupted, and only the byte limit
bounds it. A format holding a capability, or a choice of formats capturing one, does not
compile (a probe, `FormatCaptureTests`); what a range reads and says is pinned by
`ModelTextTests`.
