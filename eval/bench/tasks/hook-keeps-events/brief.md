# The hook keeps events the edge did not take, and sends them again

Found on the laptop, 2026-11-22. The Claude Code hook posts each line once (`--connect-timeout 0.3`), and on failure says "dropped: the edge at … did not answer" on stderr and moves on. A session on a machine whose path to the edge is down (an SSH forward that died, a lid closed) loses its events silently: its counters, its commits for `lately`, and the events that keep its claims' leases.

Proposal: keep undelivered lines beside the hook (its `sessions/` directory) and send them ahead of the next post. A replay is already harmless: an event whose `n` the log holds is ignored (`Sessions.record`). Bound the spool's size and age (a line older than `Session.Unended` is moot).

Done already, outside the code: the laptop's forward runs as a systemd user unit (`grit-edge-tunnel.service`, `Restart=always`), so it returns after a drop. This note is what remains: the events lost while it is down.
