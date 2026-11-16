# What the join and the hook both check, which the adapter puts before each.

# The edge's address: http:// or https://, a host (a name, or an IPv6 address in brackets) and an
# optional port, nothing after; and the loopback addresses plain http may name. The hook checks
# GRIT_EDGE with them, as it comes from the session's environment, past every check the join made.
address='^https?://(\[[0-9a-f:]+\]|[A-Za-z0-9.-]+)(:[0-9]+)?$'
loopback='^http://(localhost|127\.[0-9]+\.[0-9]+\.[0-9]+|\[::1\])(:[0-9]+)?$'
