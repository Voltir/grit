# 0039. Drive is read over REST as a service account, each folder an invited place people label, each file believed at the meet over its editors

Status: accepted (2026-10-16)

Context: a deployment's documents live in shared drives that a person shares with grit's
service account, folder by folder. Two questions had to be settled: who may read what grit
reads there (ADR 0030), and how far to believe it (ADR 0036). Drive's own sharing says who can
open a file, not how confidential it is, and its folders are made and renamed by people
without a deploy. Turned down:

- Drive's MCP server: it lists its tools to a service account but refuses every call to one,
  so grit would need a person's token, and a person's reach.
- Labels declared in the deployment's code or environment, a list of folder ids: each new
  folder would wait on a deploy, and the people who know what a folder holds are its members.
- Labelling a folder by its parent's label when it has none (inheritance): a folder shared
  more narrowly than its parent would read at the parent's label. Deferred, not adopted.
- Believing a file at the integrity of its owner alone, or of the service: in a shared drive
  a file has no owner, and the service relays whatever its editors wrote.
- A lookup per search hit to read each hit's editors: a search would cost a request per hit.

Decision:

- **Drive is read over its v3 REST API**, read-only, as a service account the deployment
  holds the key of (`grit.mcp.drive`). Three tools: a listing, of the shared drives or of what
  lies directly in one folder or drive (its first page of 100, each file saying when it last
  changed), a search by the words a file holds, and a read of one file's text (a Doc as
  markdown, a Sheet's first sheet as CSV, a text file as it is); any other type is refused in
  words. A listing, like a search, always says that files in folders no one has labelled are
  not shown, so an empty answer tells nothing of what a folder holds.
- **Each folder is a place** under the service's: a file is kept at the place of the folder
  or shared drive it lies in. The service's own place is **invited** (`Visibility.invites`):
  every place strictly within it is read as an invited room no edge reported, so labelled
  `unmapped` until a person labels it. The service's place keeps its declared label, and
  holds folders' names.
- **People label a folder from Slack**, by its link or id: `label <level> [compartment…] at
  <folder>`, `label at` and `unlabel at`, decided by ADR 0033's one rule as if asked in it.
  A place no invited place holds is refused as not shared. The deployment's edge reads a
  folder's link into its place; core knows only places.
- **Who reads an item**: a file in a shared drive, whoever reads its place; one shared from a
  person's own drive, only that person when their address is in the deployment's Drive
  domain; any other file, no one.
- **How far an item is believed**: a search hit or a listed file, at member under its place's
  cap (a folder or shared drive itself at the service's place), its title shown only when it
  is a plain line; a file read, at the meet, over everyone who may edit it,
  of each one's level: a person or a domain grant in the deployment's Drive domain, member; an
  address the deployment declares a writer (a service's, never a person's), the level it
  declares; anyone else, a group, or another domain, outside; anyone at all, untrusted; the
  service account itself is skipped. Grants not read whole, or no editor left, are untrusted.
- **Drive is cleared to be sent queries** at the deployment's top label, since it holds the
  files already.
- **Each answer carries facts** beside its text, the text rendered from them
  (docs/extending.md), their keys in one home (`DriveFacts`): a listed file's, a drive's and a
  read's, its id, kind, name when shown, and change time (a read's with the text it kept); a
  listing's own, whether it is complete, false when Drive had more or its parts were cut to
  fit. A search carries none. Code acts on them, never on the text.

Consequences: labelling is a person's act at human rate, and a new folder shows nothing until
someone labels it; the model is told so, and what to type, in plain words. A read costs three
requests (the file, its grants, its text), a search two (the hits, the drives' names) and a
listing one.
A workflow's steps record nothing of Drive beyond a tool result and the facts a call's answer
kept, a read's text a second time at most. Enforced by the transaction's reads (`Tx.access`, `Tx.roomLabel`), ADR 0033's rule,
and the law (`mcp-generic-names-no-github`, which keeps Drive out of the generic MCP client);
`DriveItemsTests` and `AdministrationContract`.
