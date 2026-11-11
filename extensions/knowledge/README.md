# grit.knowledge

Cards: one short document per unit of a source (a repository's files and directories), each a
summary with search terms and a pointer (ADR 0028) to the hosted call that reads the unit, so a
window can show what a source holds without holding it. Names `grit.core` and `grit.prose`
alone: a source is read through the MCP tools a deployment offers, never a client of its own.
Beside the cards, a second plugin keeps what each closed conversation settled as a document
(`StandingDocuments`, over `standing`).

Strand documents: by a third plugin in the same module, one short document per strand of a
room's threads (ADR 0060), summarised from what its members said, so a window and the
relevance judge see a topic spread over threads as one thing.

Packages, in one-way order (each names only those before it):

- **`ref`**: the names knowledge refers by: `Ref` (a strand, a topic, a Drive file, a GitHub
  path or any other link, core's `Href`), `CardKey`, a unit's key (the SHA-256 of its corpus and
  path), and `TopicKey`, kept as core's `DocRef`; and the unit a link or a card's pointer
  names. Names core alone.
- **`card`**: what a card is, whatever its source: its version (a git SHA, or when a file last
  changed), its title, its summary, and its document's key, text and data; the names of a
  sync run's moves (`{phase}/{key}`, `held`, `drop`) and a dreaming run's numbered ones
  (`{kind}-{i}`); the tail of a run's reply naming what it did not write; and `CardShelf`, cards as another plugin
  reads them through `Knowledge`'s export. Imports `ref`.
- **`sync`**: a sync run over any source. A `Source` says how a run reads it: the tool and
  arguments of each list and read, what an answer holds, what of a listing is checked, and its
  `Walk`, the run's plan, which is pure: from the cards a run holds and each step's outcome, the
  next step (list, read, summarise, write, relabel, and a last drop). `Walking` is every
  source's walk: it reads, summarises, writes, relabels, tallies and ends, while a source's
  `Walking.Routine` says what each of its tasks does, which tasks left untaken count, and what
  it drops. `Sync`, the keeping job `knowledge-sync`, makes each step as a move: a call, a
  summary ask, a check, or a keep; a card held at another label is withdrawn in the keep that
  replaces it. A file whose read the file area kept whole is kept as pieces beside its card, by
  a piece move after its card's keep, and one read with none kept is marked unread; its pieces
  move with its card and go when it goes, and a current file with neither pieces nor a mark,
  or pieced by a reader since revised, is read again for them alone. A
  listing or a file answered below `Outside` is checked by the integrity tree before anything is made from it (ADR 0036),
  since a card is kept only from `Outside` or above. Imports `ref` and `card`.
- **`github`**: a repository as a card source, read through GitHub's MCP server's
  `get_file_contents`, and its walk (a `Walking.Routine`), depth first from the repository's
  root. A unit's pointer is `{server}_get_file_contents` with `owner`, `repo` and `path` and no
  `ref`. A file's card is written only at its blob SHA; a file answered without one is not
  carded. Imports `ref`, `card` and `sync`; another source is a package beside it, and neither
  names the other.
- **`drive`**: Drive's shared drives as a card source, read through grit's own Drive tools
  (`drive_list`, `drive_read`) by the facts their answers carry, which it decodes itself since
  it may not name grit.mcp; and its walk (a `Walking.Routine`), the drives, each drive and the
  folders directly in it. A source declares a chain of labels, one run per label, and a file is carded by the run
  of the least label dominating what its listing was read at. A file's pointer is `drive_read`
  with its id; its card is written at when it last changed, titled with its name. Imports
  `ref`, `card` and `sync`, never `github`.
- **`strand`**: a strand's document (ADR 0042), one per strand root of three threads or more
  with enough said, kept at its room's label: its key, text and data, and `StrandDream`, the
  dreaming job `strands-dream`, which scans the strands of its run's rooms by ids and counts,
  has core summarise each changed one from what its members said, and keeps or withdraws
  their documents. Its code never holds a message's text. Imports `card`.
- **`standing`**: `Standing`, what each earned closing settled as a document: the decisions
  and facts a person or a tool established at a conversation's close (its Standing lines),
  one document per close, with no model call; its terms, posting and data. Names core alone.
- **`topic`**: a topic (`Topic`, `Passage`, `Lineage`, `TopicNote`, `Claim`): a lead and dated
  passages in prose, whose notes are its links and claims, and its data and text; and
  `TopicDream`, the dreaming job `topics-dream`, which scans topics, strands and cards by ids,
  asks once where the unplaced ones go, remembering those it sets aside (`Aside`, one document
  per label), and once per topic touched for its new lead and the day's passage, and keeps it
  with refs to what it was written from and links; and a topic's review as an edge posts it
  (`TopicPrompt`: its lead, whether the day opened it, that day's strands by root and its
  cards' titles) with the four verdicts a rater gives it (`TopicVerdict`). Its code never
  holds a message's or a card's text. Imports `ref`, `card` and `grit.prose`; never `strand`,
  `sync` or a source.
- **`declassify`**: `CardDeclassify`, the dreaming job `cards-declassify`, which reads the
  current cards by their pointers and revisions, asks for each the application in force,
  withdraws in its scan every own document but its withholding that is not a rewrite of a card's
  current version and revision that stands under the versions in force
  (`Declassified.Made.stands`), and has core rewrite its file's
  text and check it for each card due (`Dreaming.declassify`), keeping the rewrite below its floor or
  remembering it withheld (`Withheld`, one document per label). A card is rewritten without a
  compartment only when an operator applies that compartment's policy to its pointer; see
  `grit.declassification_applications` in schema.sql. Its code never holds a card's or a
  rewrite's text. Imports `card`.
- **`plugin`**: wiring only. `Knowledge`, the plugin over a deployment's sources: its
  documents' terms, the `Sync` job, each source's sync schedules (the keys in `Knowledge`'s
  doc), and its export, a `CardShelf`; `StrandDocuments`, the strands plugin: the
  `StrandDream` job on a daily schedule per label it is declared at, and its documents' terms;
  `TopicDocuments`, the topics plugin, the same for `TopicDream`, needing the plugin that
  exports its cards when given one (`DreamSchedules` holds what the two share);
  `DeclassifiedCards`, the plugin of `CardDeclassify` on one schedule, needing that plugin,
  whose policies and their applications an operator sets by the SQL beside
  `grit.declassification_policies` and `grit.declassification_applications` in `schema.sql`; and
  `StandingDocuments`, a plugin of its own over `standing`; and `Leaves`, which documents say
  again what a topic was written from, read by the eval's duplication measure alone, since
  the three plugins' key forms meet only here. Each is its own plugin because a
  plugin's documents share one label and one bound, and a sync holds every current document
  to be a card. Imports every package above.

## A sync run

A file's card is written from one summary of its first 8 KB; any other unit's as its walk makes
it, at no model cost, one whose read found it unreadable (`Fetched.Unreadable`) among them, so it
is read again only once it changes. An unchanged unit's card held at another label is copied up
to the run's floor, and one kept elsewhere to its source's place (`Source.at`).

Of a source that keeps files, after a file's card is kept, `Sync` keeps the pieces of the file
when its read kept all of it in the file area (`Source.file`) believed at `Outside` or above, as
its card's key's at its version, believed as its card (`Keeping.piece`), and otherwise marks the
file unread, saying why, so no earlier revision's pieces stay current; a file's pieces move where
its card is copied, and are withdrawn where it is withdrawn. A file whose card is current at the
floor but which has neither pieces there nor a mark, or whose pieces a format the area no longer
reads at that version made, is read again for its pieces alone, within the run's `perRun` reads,
and counted unchanged.

A card is believed at the meet of the run and what it read on its unit's way
(`Step.Fetching.way`: everything read before it for `Way.Run`; for `Way.Through`, the listings it
was reached through, whose labels its own joins), its source's results (each as
`Visibility.believed` believes it, so a repository's cap counts, not only its service's) and the
summaries asked of them; its call's arguments and its summary's ask are made from the same. A
listing or file answered below `Outside` is checked its paths or its head a window at a time:
raised, it is believed as raised; otherwise its unit is left unwritten, saying why (it holds
hidden or encoded text, it looks written to steer its reader, part of it looks out of place, it
could not be checked, or it is over `Sync.MostWindows` windows), and nothing beneath a withheld
directory is reached.

## A topics run

`TopicDream` scans its topics, the strands of rooms at its floor spoken in within `recent`, and
the cards changed within `cardRecent` (`TopicParams.SeedReach` on a night up to its
`seedUntil`). It withdraws from a topic each passage naming a strand it no longer reads at its
floor (its room at another label now, or its messages gone), the topic itself when none is left.
It attaches each strand or card a topic already refers to (a card by the unit its pointer reads,
which a topic refers to when it was written from that card, links it, or a strand of it links
it); pairs each card a strand it takes up links (`TopicScan.Linked`, read whatever its age or
whether it was set aside) with that strand, to go wherever the strand goes, the aside included,
never shown to the attach ask; and asks (`attach`) where the others go but a directory's cards
and those kept below its floor, which attach only by ref or a strand's link, at most `perRun`
strands and `TopicScan.Cards` cards past those already placed, each numbered in the order shown,
as are the topics shown (those attached to, and at most `TopicScan.Leads` more, newest first,
each by its lead, its text's head within `TopicDream.LeadChars`).

Its reply groups them by the topic they go to: one shown, or a new one, keyed by the group's
first strand; groups naming one topic are one. A new group holding no strand is set aside, so no
card alone opens a topic; shown no strand, the ask is offered neither discussions nor a new topic
to name, and with no topic to show either it is not made, its cards left for a later run. Those
it puts in none are set aside (`aside`), in a document of its floor under `topic:aside` holding
no topic, while within `recent` or `cardRecent` at the digest or version set aside, and for at
most `asideFor`. A card a reply placing anything names nowhere is set aside too; a discussion it
names nowhere is counted unnamed and offered again. A number or topic not shown is dropped from
the reply, its topic's group with it, and a strand or card it names in two groups, or in a group
and the aside, is dropped from both, so unnamed; each counted dropped. A reply that places
nothing it was shown is asked once more (`attach-again`), and when that one does not either, all
are left.

Each topic touched is written (`write-{i}`) from the strands and cards attached, the current
cards of at most `TopicScan.Holds` more units it refers to, and its current text, and kept
(`keep-{i}`): the day's passage replacing one the topic holds for that day, its lineage joined.
A topic's version refers to each strand it was written from, each card's unit and each card unit
its strands link (newest first), each topic it links and the unit of each other link; a ref
`Ref.written` refuses, or one past `DocRef.Most` (the oldest units first), is left out and
counted. A strand or card already in a topic's lineage or set aside at the same digest or
version touches nothing.

## A declassify run

In its scan `CardDeclassify` withdraws every one of its plugin's own documents but its
withholding (`Withheld`) whose data does not read as a rewrite of a card's current version and
revision that stands under the application in force for it (`Declassified.Made.stands`: made
under its policy version in force, or, until that version withholds the card, an earlier one it
was published to keep), whatever wrote it, and forgets each entry it withheld that is not at the
versions in force.

Before any card, it benches each version over its cards' applications, in force or the latest
draft (a compartment's first version included), that has no bench of its checks as they are now
(`Dreaming.bench`; `Declassifying.unbenched`), so a card is rewritten under a version in force in
the same run once its bench passes, and a draft is benched before it is published.

A card is rewritten from its file's text at its revision. A rewrite made replaces its card's
earlier one (`DocumentKeeper.declassified`); one the check kept back, or the model withheld, is
remembered so it is not asked again at those versions, and every earlier rewrite of it withdrawn
in the same step (`CardDeclassify.withheld`); one whose file is not yet pieced at its revision is
tried again next run.

## What a deployment declares

A deployment builds each source with `GitHubCards.of` (its corpus, which names it and supplies
its service; the name the deployment gave GitHub's MCP server; the repository, ref and what of
it becomes cards, and those files in a few plain words, which its directories' cards say they
hold; at most how many cards; its label; the place its cards are kept at, drawn on
where a conversation's scope holds it; and its `SyncSchedule`s), or with `DriveCards.of` (the
same, but Drive's two tools, a chain of labels in place of the repository and one label, and
the deployment's formats, so that a PDF, Word or Excel file, or a Slides deck as the PDF Drive
exports it to, is carded only where one reads it),
and the plugin with
`Knowledge.of(name, sources, visibility)`, under the visibility it declares. It also serves an
edge hosting the MCP server's `get_file_contents` at the source's service, or every run's first
call fails and its reply says so. The reference deployment's `GRIT_PLUGINS=knowledge` is one
such declaration (`deployments/app/README.md`).

## The label rule

A source is kept at one label: the one it declares, or its service place's when it declares
none. It may be above the service's (a confidential repository behind an internal service),
never below, and never above what the deployment clears the service for: `Knowledge.of`
refuses either, naming the source, since its runs could then not read the service or send it
a call. Its schedules run at that label, so a run reads and keeps at it, and every answer a
run's calls and asks bring back is at it: no card is kept where the next run cannot read it
back. Whether a turn reads a card is its label's business alone: a turn in a room below the
source's label holds none of its cards.

A Drive source is kept at each of its labels, and every one of them is held to that rule: each
declares its own schedules, whose runs card what was read at most at that label and above the
one below it.

## Data handling

A sync run sends the head of each file it summarises (up to 8 KB) to the catalog's summary
model, a third party, whatever the source's label. Declaring a source is the deployment's
consent for that source's included files. A turn that reads the same file already sends all of
it to the reply model. For a source whose results are believed below `Outside`, a run also
sends each listing's paths and each file's head to the deployment's classifier, when it is
cleared for the source's label, to check them.

The test tree mirrors it. `test/resources/github/` holds `tools/call` results GitHub's hosted
read-only server (`https://api.githubcopilot.com/mcp/readonly`) answered on 2026-10-11 (in an
event stream) to `get_file_contents` for a public repository, `github/github-mcp-server`:
`get-file-contents-directory.json` (its `docs` directory, every field),
`get-file-contents-directory-git-url.json` (the same, asking for `type`, `path`, `sha` and
`git_url`), `get-file-contents-match.json` (a path naming no file, `testing.md`, answered with
the paths that end with it) and `get-file-contents-file.json` (the file `docs/testing.md`).
Each has its server's icons removed, the repository set to `octocat/Hello-World` and its commit
to a placeholder in every URL and URI; the file's text is replaced by a short neutral text, the
SHA in its text block by that text's git blob SHA, and its `structuredContent` (a copy of the
text) removed. Keys, their order and every other value are as sent.

`test/resources/drive/facts/` holds the facts Drive's tools answer, as a reader keeping every
part states them (`{"answer"?:…,"parts":[…]}`, the keys `grit.mcp.drive.DriveFacts`'): the shared
drives (`drives.json`), a drive's and a folder's listing (`listing-drive-a.json`,
`listing-folder-3.json`) and a text file's read (`read-file-text.json`). Knowledge may not name
grit.mcp, so `grit.app`'s `DriveFactsPinTests` holds them equal to what grit.mcp answers over
its recordings.
