# 0045. Files live in a transient area under handles: a catalog in Postgres, bytes on a filesystem, admitted in a transaction and opened outside one

Status: accepted (2026-10-22)

Context: a file an edge fetches (a PDF from Drive) is read a range at a time, often over
several calls and turns, and a later job may read it again. Fetching it afresh for each range
costs a download per call; a step output holding its bytes would put megabytes in every
recorded history. Turned down: bytes in Postgres (`BYTEA` or large objects: every file through
the database's WAL and backups, for data kept a day); a per-edge scratch directory (nothing
else can read what it holds, and a crash leaves copies only that edge's next start can find);
an object store now (no deployment has one, and `Blobs` leaves room for it); a cleanup workflow
(the disk is the machine's, recovery may run elsewhere, and an edge names no workflow).

Decision:

- **A file is a handle.** `FileHandle`, written `f41`, names one put of one snapshot of a
  file's bytes; a step output, a tool's answer and a fact carry the handle, never the bytes.
- **A catalog in Postgres, bytes on a filesystem.** One row of `grit.files` per put (its
  name, size, digest, origin, and the item it is kept at: place, integrity, readers), and the
  bytes once per digest in `Blobs` (`LocalBlobs`: a directory on this machine). The catalog
  and the blobs meet only in `FileArea`; an edge reaches it as `Intake`, a turn or a job as
  `Files`, and both read a file with the deployment's formats, which the area holds.
- **A put is idempotent by its call.** `Intake.put` is keyed by `PutKey`, a hosted call's
  slot: run again after a crash, it answers the file the first run kept without fetching it.
- **Admitted in a transaction, opened outside one.** `Files.admit` reads the row in the
  caller's transaction and lets it through only when that transaction reads the file's item
  (`Tx.readsItem`, as a result part is read); an expired or unreadable file is answered
  alike, `Gone`. `Admitted` has no stored form, so no step records it; `Files.open` reads the
  bytes holding no transaction, the `OpenFile` valid only within its block.
- **Kept a while, under a cap.** A file is kept 24 hours after its put (a deployment may set
  another expiry), at most 100 MiB, and the files kept at most 2 GiB together, each blob once.
  A put past either is refused; nothing is evicted.
- **Swept both ways.** The area's sweep marks for deletion each row expired or whose blob is
  missing (a new machine, a lost disk), removes each blob no row names, and clears a download
  abandoned for an hour; the collector deletes a marked row (`Target.File`), and the next
  sweep its blob. All of it runs under one advisory lock with every put.

Consequences: a file read once is read further by handle (`file_read`) without fetching it
again, and the knowledge sync can read what an edge fetched. ADR 0002 holds with one addition:
an edge and the engine share the area's blobs, reached only through `Intake` and `Files`, so
an edge and the engine serving one database share a machine, or a filesystem, for files.
Bytes are on one machine's disk: an engine moved elsewhere answers `Gone` for files put
before, until they are fetched again. Enforced by the compiler (an open file kept, returned
or captured past its block, and an `Admitted` built outside grit, are rejected:
`OpenFileCaptureTests`) and by the file area's contract (`FilesContract`), kept over the
in-memory and SQL catalogs.
