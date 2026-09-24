# grit.app

The composition root: wires implementations into the seams. The only module that depends
on every other one, and so the only place quarantine modules meet. Because Mill's
`moduleDeps` are transitive, DBOS is on its classpath — enola's `only-dbos-imports-dbos-*`
rules are what keep it out; it reaches Postgres only through `grit.dbos.Engine`.

`Main` is the M0 run: the real turn, the stub provider and the linear assembler over the
Postgres named by `GRIT_DATABASE_*`. TUI mode replaces it.
