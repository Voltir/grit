# 0066. A dreaming job reads its rooms' strands by ids, counts and a digest, and core summarises and sorts what they said for it

Status: accepted (2026-11-19)

Context: a nightly job that keeps knowledge from what a room said (the topics job, ADR 0065)
must read raw messages while they are kept, yet a plugin's code must never hold a message's
text (ADR 0027). This record carries the dreaming-job decisions of the record it replaces,
whose title, one strand document per root, stopped being true when that plugin was retired;
its revision of 2026-11-19 (core sorts for a dreaming job) is adopted with it. Turned down:
summarising the members' closings (a closing exists only after its period lapses, a heard one
is prose alone, and a summary of summaries loses what the messages said); the engine writing
the documents itself (it owns no documents, and a plugin is where derived knowledge lives); a
step returning the members' text for a plain ask (the text would sit in a step's record, and
come back believed at the run's own integrity, laundering a guest's words up to grit's); every
keeping job reading strands (a job that never needs raw messages would hold the read).

Decision:

- **A dreaming job is a third kind of plugin job** (`DreamingJob`, handed `Dreaming`). Its
  keeps may also read the strands of the rooms its transaction reads, by ids, counts and a
  content digest of their messages (`StrandShelf`, `Strand.Digested`): nothing a model would
  read leaves a scan.
- **Core summarises for it.** `Dreaming.summarise` is an ask whose step reads the members'
  messages itself (`StrandExcerpts`), shows them as a reader's excerpt within the summary's
  characters, and records only the JSON ask's own form under a digest of the strands' ids. Its
  reply is believed at the meet of every message read, so a guest's message caps what is
  written. When the messages are not the ones the scan digested, it asks nothing. One summary
  may show several strands and documents, the job naming documents by version: each is read
  in the same step, under its plugin's label, only when it is the job's plugin's own or a
  need's and the transaction reads it (one that is not is left out, and the job told which
  were shown), and the reply is believed at the meet of everything read.
- **Core sorts for it, too.** Where a job must place a discussion or a document among its
  plugin's documents, `Dreaming.sort` is a judgment whose step reads the item and each
  candidate itself and asks the deployment's classifier: one choice over the candidates and
  the job's fixed criteria (`Sorting.choice`), or a yes/no for each of the nearest by their
  words (`Sorting.each`, at most 40). The job names the candidates by version, those it must
  show and a pool the step fills from by the documents' index, never reading every
  candidate's text; it is handed back its own values for them, each with a probability. A
  request never passes the classifier's limit: each candidate's text is cut evenly only when
  it would. Its record holds ids, a digest and the answers, and its answer is believed at the
  meet of everything read. A judgment costs a fraction of a model's ask and reads no prompt a
  model could be steered by, so placing moves off the summary model.
- **A run reads at one label**: its schedule's clearance, and only rooms whose label in force
  is its floor; a deployment declares one schedule per label.
- **Summaries are asked of the policy's `dream` ladder**, a role of its own, so the nightly
  passes may run on a stronger pair than a turn's summary; a catalog naming none uses the
  summary's.
- **Members are read as raw messages until they age out.** Nothing re-links strands: a strand
  is stitched, a thread, or a run its scan cuts (ADR 0065).

Consequences:

- A plugin's code never holds a message's text, only the links in it as `Href`s
  (`StrandShelf.links`); ADR 0034's moves gain a read inside a keep and an ask core frames. A
  planted link can place a card the plugin already reads into a write already capped by the
  guest's strand, which is the influence a sort's placing has.
- A sort's classifier reads the guest's words as its state, and they can move a probability:
  a job sets the marks a probability must pass, and a placement only decides which document is
  written, never what it is believed at.
- What a job keeps is never believed above its least-believed message, nor labelled below its
  room: the keeper joins with the floor, and the run reads no room above it.
- Enforced by the types (only `Dreaming` offers `scan`, `summarise` and `sort`;
  `Strand.Digested` has no text; a sort's candidates are pure values) and capture probes
  (`ActCaptureTests`, `SortCaptureTests`), and by `MovesContract` against the moves and their
  fake.
