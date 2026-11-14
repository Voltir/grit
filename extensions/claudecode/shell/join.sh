# grit's join script for Claude Code, served by the commands edge at /join/<harness> after the
# lines setting `harness` and `hook` (the hook's text). A person pipes it into bash in a checkout:
#
#   curl -fsS <edge>/join/claude-code | bash -s -- <edge> [--mcp-name <name>] [--retire <dir>]...
#   curl -fsS <edge>/join/claude-code | bash -s -- --undo [--mcp-name <name>]
#
# What it refuses, writes and prints: README.md. bash runs a script it reads as it arrives, one
# command at a time, and a top-level word cut short is a command (`program` cut to `pro` runs
# one), so the adapter wraps this body in one `{ … }` group, which bash runs only once its closing
# brace has arrived. Inside it everything defines but the last line, and `set` waits for `main`;
# `main` runs with its input from /dev/null, so nothing it starts reads the rest of the script.

refuse() {
  printf 'grit %s: %s\n' "$harness" "$1" >&2
  exit 1
}

# $1 single-quoted, as any POSIX shell reads it back.
sq() {
  local q="'\\''"
  printf "'%s'" "${1//"'"/$q}"
}

# Whether $1 holds a control character (a newline among them).
controlled() {
  case $1 in
    *[[:cntrl:]]*) return 0 ;;
  esac
  return 1
}

# Writes $3 to $1 with mode $2, through a temporary file beside it moved into place.
put() {
  local tmp
  mkdir -p -- "${1%/*}"
  tmp=$(mktemp -- "${1%/*}/.grit-join.XXXXXX")
  printf '%s' "$3" > "$tmp"
  chmod "$2" -- "$tmp"
  mv -f -- "$tmp" "$1"
}

# The program a command line runs: its first word, one layer of quotes removed, after an `exec`;
# `$CLAUDE_PROJECT_DIR` and `~` at its start expanded. Empty for a word with no `/`.
program() {
  local s=$1 w
  s=${s#"${s%%[![:space:]]*}"}
  case $s in
    exec[[:space:]]*) s=${s#exec} && s=${s#"${s%%[![:space:]]*}"} ;;
  esac
  case $s in
    '"$CLAUDE_PROJECT_DIR"'*) s=$top${s#'"$CLAUDE_PROJECT_DIR"'} ;;
    '$CLAUDE_PROJECT_DIR'*) s=$top${s#'$CLAUDE_PROJECT_DIR'} ;;
    '~/'*) s=$HOME/${s#'~/'} ;;
  esac
  case $s in
    "'"*) s=${s#"'"} && w=${s%%"'"*} ;;
    '"'*) s=${s#'"'} && w=${s%%'"'*} ;;
    *) w=${s%%[[:space:]]*} ;;
  esac
  case $w in
    */*) printf '%s' "$w" ;;
  esac
}

# Whether the command line $1 runs a program under a retired directory.
retired() {
  local p c d
  p=$(program "$1")
  [ -n "$p" ] || return 1
  c=$(realpath -m -- "$p") || return 1
  for d in "${retire[@]+"${retire[@]}"}"; do
    case $c in
      "$d"/*) return 0 ;;
    esac
  done
  return 1
}

# Whether the git hook $1 is ours: it holds the join's marker line.
ours_file() {
  [ -f "$1" ] && grep -qxF -- "$marker" "$1"
}

# Whether every command line of the git hook $1 runs a program under a retired directory.
retired_file() {
  local line any=1
  [ -f "$1" ] && [ "${#retire[@]}" -gt 0 ] || return 1
  while IFS= read -r line || [ -n "$line" ]; do
    case $line in
      '' | '#'*) continue ;;
    esac
    retired "$line" || return 1
    any=0
  done < "$1"
  return "$any"
}

# The settings' commands, each ending in a NUL.
commands_of() {
  jq -j '[(.hooks // {})[][] | (.hooks // [])[] | .command? | strings] | unique | .[] | . + "\u0000"' "$1"
}

# Whether the settings $1 hold a command of ours.
names_hook() {
  [ -f "$1" ] && jq -e --arg ours "$ours" \
    '[(.hooks // {})[][] | (.hooks // [])[] | .command? | strings | select(startswith($ours))] | length > 0' \
    "$1" > /dev/null 2>&1
}

# The settings $1 with our handlers and the commands of the JSON array $2 removed, and the groups
# of the JSON array $3 appended, each to its event. Groups and events left empty by a removal go.
rewrite_settings() {
  jq --arg ours "$ours" --argjson drop "$2" --argjson add "$3" '
    def gone: (.command? // null) as $c
      | ($c | type) == "string" and (($c | startswith($ours)) or any($drop[]; . == $c));
    has("hooks") as $had
    | (.hooks // {}) as $was
    | .hooks = ($was | with_entries(.value |= map(
        if (.hooks | type) == "array" then
          (.hooks | length) as $n | .hooks |= map(select(gone | not))
          | select((.hooks | length) > 0 or $n == 0)
        else . end)))
    | reduce $add[] as $a (.; .hooks[$a.event] = ((.hooks[$a.event] // []) + [$a.group]))
    | .hooks |= with_entries(select((.value | length) > 0 or (($was[.key] // []) | length) == 0))
    | if (.hooks | length) == 0 and ($had | not) then del(.hooks) else . end' "$1"
}

# Our groups, one per event, as a JSON array of {event, group}.
our_groups() {
  jq -n --arg h "$(sq "$H")" --arg m "$tools_matcher" '
    def c($s): {type: "command", command: ($h + " " + $s), timeout: 5};
    def a($s): c($s) + {async: true};
    [ {event: "SessionStart", group: {hooks: [c("brief"), c("opening"), a("event")]}},
      {event: "SubagentStart", group: {hooks: [c("brief"), c("event")]}},
      {event: "PostToolUse", group: {matcher: $m, hooks: [a("event")]}},
      {event: "PostToolUseFailure", group: {matcher: $m, hooks: [a("event")]}},
      {event: "Stop", group: {hooks: [a("event")]}},
      {event: "PostCompact", group: {hooks: [a("event")]}},
      {event: "SubagentStop", group: {hooks: [a("event")]}},
      {event: "SessionEnd", group: {hooks: [c("event") + {timeout: 2}]}} ]'
}

main() {
  set -euo pipefail
  # The tools a Claude Code hook event is posted for: an exact match, as a matcher holding `.`
  # or `*` is an unanchored regular expression. The MCP ones are read-only tools, posted as reads.
  tools_matcher='^(Read|Edit|Write|MultiEdit|NotebookEdit|Bash|mcp__scout__.*)$'
  marker="# grit $harness join"
  if [ -z "${BASH_VERSINFO-}" ] || [ "${BASH_VERSINFO[0]}" -lt 5 ]; then
    refuse "needs bash 5 or later; this is bash ${BASH_VERSION-unknown}"
  fi
  local tool
  for tool in git jq curl; do
    command -v "$tool" > /dev/null 2>&1 || refuse "needs $tool on PATH"
  done

  local edge='' undo=0 mcp=grit dir r
  retire=()
  while [ "$#" -gt 0 ]; do
    case $1 in
      --undo) undo=1 ;;
      --mcp-name)
        [ "$#" -ge 2 ] || refuse "--mcp-name needs a name"
        mcp=$2
        shift
        ;;
      --retire)
        [ "$#" -ge 2 ] || refuse "--retire needs a directory"
        dir=$2
        shift
        r=$(realpath -e -- "$dir" 2> /dev/null) && [ -d "$r" ] || refuse "--retire $dir is not a directory"
        if [ "$r" = / ] || [ "$r" = "$(realpath -m -- "$HOME")" ]; then
          refuse "--retire $dir would retire every program under it; name the directory of the old hooks"
        fi
        retire+=("$r")
        ;;
      -*) refuse "unknown option $1" ;;
      *)
        [ -z "$edge" ] || refuse "one address, not two: $edge and $1"
        edge=$1
        ;;
    esac
    shift
  done
  local named='^[A-Za-z0-9_-]{1,64}$'
  local address='^https?://(\[[0-9a-f:]+\]|[A-Za-z0-9.-]+)(:[0-9]+)?$'
  local loopback='^http://(localhost|127\.[0-9]+\.[0-9]+\.[0-9]+|\[::1\])(:[0-9]+)?$'
  [[ $mcp =~ $named ]] || refuse "--mcp-name $mcp is not 1 to 64 letters, digits, _ or -"
  if [ "$undo" = 1 ]; then
    [ -z "$edge" ] || refuse "--undo takes no address"
    [ "${#retire[@]}" = 0 ] || refuse "--undo takes no --retire: nothing retired is restored"
  else
    [ -n "$edge" ] || refuse "usage: bash -s -- <edge's address> [--mcp-name <name>] [--retire <dir>]... | --undo [--mcp-name <name>]"
    [[ $edge =~ $address ]] ||
      refuse "$edge is not an address: http:// or https://, a host and a port, nothing after"
    if [[ $edge == http://* ]] && ! [[ $edge =~ $loopback ]]; then
      refuse "$edge is plain http beyond this machine, where a hook's posts would be read in clear; use https"
    fi
  fi

  top=$(git rev-parse --show-toplevel 2> /dev/null) || refuse "not inside a git checkout; run the join in one"
  local common hooks
  common=$(git rev-parse --path-format=absolute --git-common-dir) || refuse "git cannot name this checkout's repository"
  hooks=$(git rev-parse --path-format=absolute --git-path hooks) || refuse "git cannot name this checkout's hooks"
  H=$common/grit/$harness/hook
  if controlled "$top" || controlled "$H" || controlled "$hooks"; then
    refuse "this checkout's path holds a control character, which a hook command cannot carry"
  fi
  ours="$(sq "$H") "

  local settings=$top/.claude/settings.local.json
  if [ -L "$settings" ]; then
    refuse "$settings is a symbolic link; the join writes no file it does not own"
  fi
  if [ -e "$settings" ] && ! jq -e '
      type == "object"
      and ((.hooks // {}) | type == "object")
      and all((.hooks // {})[];
        type == "array" and all(.[]; type == "object" and ((.hooks // []) | type == "array")))' \
    "$settings" > /dev/null 2>&1; then
    refuse "$settings is not a JSON object whose hooks are lists of groups; nothing was changed"
  fi

  if [ "$undo" = 1 ]; then
    undo_join "$settings" "$hooks" "$mcp"
  else
    join "$settings" "$hooks" "$edge" "$mcp"
  fi
}

# Joins the checkout: the hook, its handlers in the settings $1, git's hooks in $2, and the MCP
# entry $4 at the edge $3.
join() {
  local settings=$1 hooks=$2 edge=$3 mcp=$4 c groups drop='[]' f
  local -a removed=() lines=() written=() foreign=()

  if [ -e "$settings" ]; then
    while IFS= read -r -d '' c; do
      if [[ $c != "$ours"* ]] && retired "$c"; then
        removed+=("$c")
        drop=$(jq -c --arg c "$c" '. + [$c]' <<< "$drop")
      fi
    done < <(commands_of "$settings")
  fi

  for f in post-commit post-rewrite; do
    if [ ! -e "$hooks/$f" ] || ours_file "$hooks/$f"; then
      written+=("$f")
    elif retired_file "$hooks/$f"; then
      while IFS= read -r c || [ -n "$c" ]; do
        case $c in '' | '#'*) continue ;; esac
        lines+=("$f: $c")
      done < "$hooks/$f"
      written+=("$f")
    else
      foreign+=("$f")
    fi
  done

  put "$H" 0755 "#!/usr/bin/env bash
harness=$(sq "$harness")
edge=$(sq "$edge")
edge=\${GRIT_EDGE:-\$edge}
$hook"

  groups=$(our_groups)
  local before='{}'
  [ -e "$settings" ] && before=$(cat -- "$settings")
  put "$settings" 0644 "$(rewrite_settings <(printf '%s' "$before") "$drop" "$groups")
"

  for f in "${written[@]+"${written[@]}"}"; do
    if [ "$f" = post-commit ]; then
      put "$hooks/$f" 0755 "#!/usr/bin/env bash
$marker
exec $(sq "$H") commit
"
    else
      put "$hooks/$f" 0755 "#!/usr/bin/env bash
$marker
exec $(sq "$H") rewrite \"\$@\"
"
    fi
  done

  printf 'grit %s: joined %s to %s\n' "$harness" "$top" "$edge"
  printf '  hook      %s, shared by this repository'\''s worktrees\n' "$H"
  printf '  settings  .claude/settings.local.json: %s handlers of ours\n' \
    "$(jq '[.[].group.hooks | length] | add' <<< "$groups")"
  for c in "${removed[@]+"${removed[@]}"}" "${lines[@]+"${lines[@]}"}"; do
    printf '  removed   %s\n' "$c"
  done
  case ${#written[@]} in
    2) printf '  git       post-commit and post-rewrite in %s, shared by this repository'\''s worktrees\n' "$hooks" ;;
    1) printf '  git       %s in %s, shared by this repository'\''s worktrees\n' "${written[0]}" "$hooks" ;;
  esac
  for f in "${foreign[@]+"${foreign[@]}"}"; do
    if [ "$f" = post-commit ]; then c="exec $(sq "$H") commit"; else c="exec $(sq "$H") rewrite \"\$@\""; fi
    printf '  git       %s/%s is not ours and is left as it is; add the line: %s\n' "$hooks" "$f" "$c"
  done

  local add="claude mcp add --transport http --scope local $mcp $edge/mcp" old=''
  if command -v claude > /dev/null 2>&1; then
    if [ -f "$HOME/.claude.json" ]; then
      old=$(jq -r --arg p "$top" --arg n "$mcp" \
        '.projects[$p].mcpServers[$n] | if type == "object" then (.url // .command // "") else "" end' \
        "$HOME/.claude.json" 2> /dev/null) || old=''
    fi
    claude mcp remove --scope local "$mcp" > /dev/null 2>&1 || true
    if ! claude mcp add --transport http --scope local "$mcp" "$edge/mcp" > /dev/null 2>&1; then
      if [ -n "$old" ]; then
        printf 'grit %s: claude mcp add failed, and %s at %s is removed; run: %s\n' \
          "$harness" "$mcp" "$old" "$add" >&2
      else
        printf 'grit %s: claude mcp add failed; run: %s\n' "$harness" "$add" >&2
      fi
      exit 1
    fi
    if [ -n "$old" ]; then
      printf '  mcp       %s at %s/mcp, local scope, replacing %s at %s\n' "$mcp" "$edge" "$mcp" "$old"
    else
      printf '  mcp       %s at %s/mcp, local scope\n' "$mcp" "$edge"
    fi
  else
    printf '  mcp       claude not found; run: %s\n' "$add"
  fi
  printf 'Sessions pick the hooks up at once; run /mcp in a running session to connect %s.\n' "$mcp"
  printf 'Undo: curl -fsS %s/join/%s | bash -s -- --undo --mcp-name %s\n' "$edge" "$harness" "$mcp"
}

# Undoes the join of the checkout: its handlers in the settings $1 and its MCP entry $3; and,
# unless another worktree's settings still name the hook, the hook and git's hooks in $2.
undo_join() {
  local settings=$1 hooks=$2 mcp=$3 n=0 f w other='' before
  local -a gone=()
  if [ -e "$settings" ]; then
    n=$(jq --arg ours "$ours" \
      '[(.hooks // {})[][] | (.hooks // [])[] | .command? | strings | select(startswith($ours))] | length' \
      "$settings")
  fi
  if [ "$n" -gt 0 ]; then
    before=$(cat -- "$settings")
    put "$settings" 0644 "$(rewrite_settings <(printf '%s' "$before") '[]' '[]')
"
  fi

  while IFS= read -r -d '' f; do
    case $f in
      'worktree '*)
        w=${f#worktree }
        if [ "$w" != "$top" ] && [ -z "$other" ] && names_hook "$w/.claude/settings.local.json"; then
          other=$w
        fi
        ;;
    esac
  done < <(git worktree list --porcelain -z)

  printf 'grit %s: undid the join in %s\n' "$harness" "$top"
  printf '  settings  .claude/settings.local.json: %s handlers of ours removed\n' "$n"
  if [ -n "$other" ]; then
    printf '  hook      %s kept: the worktree %s still names it\n' "$H" "$other"
    printf '  git       post-commit and post-rewrite kept, as the hook is\n'
  else
    rm -f -- "$H"
    rmdir -- "${H%/*}" 2> /dev/null || true
    rmdir -- "${H%/*/*}" 2> /dev/null || true
    for f in post-commit post-rewrite; do
      if ours_file "$hooks/$f"; then
        rm -f -- "$hooks/$f"
        gone+=("$f")
      fi
    done
    printf '  hook      %s removed\n' "$H"
    case ${#gone[@]} in
      2) printf '  git       post-commit and post-rewrite removed\n' ;;
      1) printf '  git       %s removed\n' "${gone[0]}" ;;
      0) printf '  git       none of ours to remove\n' ;;
    esac
  fi
  if command -v claude > /dev/null 2>&1; then
    if claude mcp remove --scope local "$mcp" > /dev/null 2>&1; then
      printf '  mcp       %s removed from local scope\n' "$mcp"
    else
      printf '  mcp       no entry %s in local scope\n' "$mcp"
    fi
  else
    printf '  mcp       claude not found; run: claude mcp remove --scope local %s\n' "$mcp"
  fi
  printf 'Nothing retired was restored.\n'
}

</dev/null main "$@"
