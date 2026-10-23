# 0046. A file's pieces are kept with their trees in their own table and index, cut under heading paths, bounded in bytes, capped per file in the window

Status: accepted (2026-10-22)

Context: a card says what a file holds in sixty words, so a question answered by one clause
of a long policy or standard finds the card at best, and the model must then read the file
range by range. Retrieval needs the passages themselves. Turned down: passages as documents
(`grit.documents` is one row per key and label, a summary each, its bound shared with the
plugin's cards; a 300-page standard would crowd every card out); a summary per passage (a
model call per 200 tokens of every file, on every revision); a search over the file area at
turn time (files are kept a day, and a turn cannot afford to read them); text alone, not the
tree (the model's text and the index's text differ: a table row as `column: value`, the
heading path first, identifiers with their aliases).

Decision:

- **A piece is a tree under a heading path.** `Pieces.of` cuts a file's tree (ADR 0044)
  greedily to about `Pieces.Target`, 200 tokens, the one constant the judge's brief is sized
  by: whole paragraphs under one heading path, a long one cut at sentences, a list by items,
  code by lines, a table by rows with its columns kept; at most `Pieces.PerFile` (2,000) a
  file, the rest counted as cut. A piece's identity across revisions is its fingerprint, the
  hash of its normalised heading path and text.
- **Their own tables and index.** `grit.piece_sets` (one row per plugin, file and label: name,
  revision, format and version, the pointer that reads it, place, labels, when written,
  superseded and last placed) and `grit.pieces` (each piece's tree as prose JSON, fingerprint,
  anchor, index text and size), searched
  by their own BM25 index over `PieceIndex.of`: the file name's words, then `IndexText`, each
  identifier also as its alias (`AC-2(3)` also `AC2x3`), the same aliases added to a query.
  The model reads a piece rendered from its tree (`Shown.piece`), never a stored text.
- **Kept by a plugin's keeping job as it keeps a card.** A piece move (`piece:{name}`, a keep
  by kind) admits the file a read kept in the area, opens and reads it holding no
  transaction, and keeps its pieces at the run's floor in a write of its own, believed at no
  more than what it pieces from; a rerun keeps the same pieces again. A keep replaces the
  file's set at that label, and an unchanged piece keeps its id. The knowledge sync pieces
  each file whose read kept it whole and was believed at `Outside` or above, moves a file's
  pieces with its card when the card is relabelled (each keeping its id and when it was last
  placed), and withdraws them when its card is gone. A file read with none of it kept (no
  whole file kept, a read below `Outside`, no format reads it) is marked unread, an empty set
  saying why, so its earlier pieces stop being current and no run reads it again for them
  until it changes. Each run first finds the current files with neither pieces nor a mark at
  its floor, or with pieces a format no longer read at that version made, and reads them again
  for their pieces alone, within its reads: files carded before their pieces were kept, or
  before a reader's version changed, are pieced without waiting for an edit.
- **Bounded in bytes, forgotten whole.** A plugin's pieces take at most `PieceKeeper.Bound`
  (128 MiB) counted over every label; past it, whole files are deleted, those superseded
  first, then the least recently placed, never the one written. Superseded and withdrawn sets
  are collected after the plugin's retention (`Target.PieceSet`).
- **A piece is cited by an anchor its pointer follows.** Each piece keeps an `Anchor` (its
  first 8 words, 32 characters either side, its position; under 170 characters), and the
  window shows its file's pointer (its card's) with `near` set to it. Every reading tool's
  `near` re-reads the file and finds the passage exactly by context, else nearest its
  position, else at least 0.8 alike, reading the page it lies on, or says it is gone. Turned
  down: a citation by offset or page alone (any edit before it moves it), and following by
  the area's handle (kept a day; a piece lives as long as its card).
- **At most eight of one file in a window.** The pieces search answers at most
  `PieceSearch.PerFile` of one file and how many more matched, and the window says so ("n
  more parts of this file matched"), so one long file cannot fill the judged candidates.

Consequences: a question about a clause of a kept file finds the clause, cited by file,
heading path and pages, at the cost of one search per turn that holds any pieces and no model
call to keep them. A file's pieces live only as long as the sync keeps its card. Enforced by
the pieces contract (`PieceContract`, over the in-memory and SQL stores) and the moves
contract (`MovesContract`) for the piece move; the turn's replay gate holds the window's
encoding.
