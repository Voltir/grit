# What the join and the hook both check, which the adapter puts before each.

# The edge's address: http:// or https://, a host (a name, or an IPv6 address in brackets) and an
# optional port, nothing after; and the loopback addresses plain http may name. The hook checks
# GRIT_EDGE with them, as it comes from the session's environment, past every check the join made.
address='^https?://(\[[0-9a-f:]+\]|[A-Za-z0-9.-]+)(:[0-9]+)?$'
loopback='^http://(localhost|127\.[0-9]+\.[0-9]+\.[0-9]+|\[::1\])(:[0-9]+)?$'

# The programs the join and the hook run, but bash and `claude`, which the join checks for itself,
# and `realpath`, which only --retire needs: the hook's watchdog is `timeout`, and an undo alone
# runs `rmdir`. Prints those PATH lacks, comma-separated; nothing when it has them all.
missing() {
  local p lacks=''
  for p in git jq curl cat chmod cp grep mkdir mktemp mv readlink rm rmdir stat timeout tr; do
    command -v "$p" > /dev/null 2>&1 || lacks+=${lacks:+, }$p
  done
  printf '%s' "$lacks"
}
