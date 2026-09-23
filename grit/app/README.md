# grit.app — placeholder

The composition root: wires implementations into the seams. The only module that depends
on every other one, and so the only place quarantine modules meet. Because Mill's
`moduleDeps` are transitive, DBOS is on its classpath — enola's `only-dbos-imports-dbos-*`
rules are what keep it out.

The phase-0 proof run (`grit.dbos.Main`) moves here once `grit.dbos` exposes a grit-style
durability API instead of raw DBOS.
