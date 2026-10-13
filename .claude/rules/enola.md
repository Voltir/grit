<!-- Written by `enola install`. Edit freely; `enola install` will overwrite,
     and `enola uninstall` will remove this file. Trimmed for grit: after a reinstall,
     trim it again to what follows. -->

## enola — the architecture gate, and module structure on demand

enola maps grit's structure from source. Its import edges and module facts are sound; its
symbol facts guide exploration only (CLAUDE.md, "The architecture gate (enola)").

- **The gate:** `bash scripts/enola-law.sh`. It regenerates the snapshot, checks every rule in
  `enola-intent.yaml`, and fails on any cycle between production packages and on its own
  greps. `scripts/check` runs it.
- **After changing the law, `enola-intent.yaml`, or where a module's folder is:**
  `scripts/enola-plant`, which must report every plant caught.
- **What a change did to the structure:** before editing, run the law and copy `.enola` to a
  scratch directory; after, run the law again, then
  `tools/enola check --baseline <scratch>/.enola --detail`. Same binary as the gate, and the
  pinned baseline is left alone.
- **Never call MCP `set_baseline` or `generate_snapshot`.** The law owns `.enola/baseline`, and
  the MCP server may be an older binary than `tools/enola` (it keeps the one it started with).
- **Module-level questions** (what depends on this package, how two are wired): MCP
  `impact_analysis`, `find_path`, `traverse`, or `explore`. For symbols, read through
  `scripts/outline`; confirm any symbol-level answer from enola there before stating it.

enola's stop hook reports the architectural delta at the end of a session when it finds
something worth reading. A finding nothing enforced is a report, not a broken build: show it
to the user, say that nothing enforced it, and ask whether to accept it, change it, or add a
rule. Do not revert work over it on your own initiative, and do not describe the session as
clean without mentioning it. When it says it could not grade (the baseline is not comparable),
that is no verdict at all; the remedy is the re-pin the law prints.
