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

# As put, but a file that exists keeps its mode (settings may hold tokens); $2 is a new one's.
put_kept() {
  local tmp
  [ -e "$1" ] || {
    put "$@"
    return
  }
  tmp=$(mktemp -- "${1%/*}/.grit-join.XXXXXX")
  cp -p -- "$1" "$tmp"
  printf '%s' "$3" > "$tmp"
  mv -f -- "$tmp" "$1"
}

# The program a command line runs: its first word after an `exec`, as a shell reads it with its
# quotes removed, and `$CLAUDE_PROJECT_DIR` or `${CLAUDE_PROJECT_DIR}` at its start expanded
# unless single-quoted, `~/` unless quoted. Empty for a word with no `/`.
program() {
  local s=$1 w='' q='' c i lead=''
  s=${s#"${s%%[![:space:]]*}"}
  case $s in
    exec[[:space:]]*) s=${s#exec} && s=${s#"${s%%[![:space:]]*}"} ;;
  esac
  for ((i = 0; i < ${#s}; i++)); do
    c=${s:i:1}
    if [ -n "$q" ]; then
      if [ "$c" = "$q" ]; then q=''; else w+=$c; fi
    else
      case $c in
        [[:space:]]) break ;;
        "'" | '"')
          q=$c
          [ "$i" -gt 0 ] || lead=$c
          ;;
        *) w+=$c ;;
      esac
    fi
  done
  case $lead$w in
    '$CLAUDE_PROJECT_DIR/'* | '"$CLAUDE_PROJECT_DIR/'*) w=$top${w#'$CLAUDE_PROJECT_DIR'} ;;
    '${CLAUDE_PROJECT_DIR}/'* | '"${CLAUDE_PROJECT_DIR}/'*) w=$top${w#'${CLAUDE_PROJECT_DIR}'} ;;
    '~/'*) w=$HOME/${w#'~/'} ;;
  esac
  case $w in
    */*) printf '%s' "$w" ;;
  esac
}

# Whether the command line $1 runs a program under a retired directory.
retired() {
  local p c d
  [ "${#retire[@]}" -gt 0 ] || return 1
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

# Whether the directory $1 is $2 or an ancestor of it, both canonical.
covers() {
  [ "$1" = / ] && return 0
  case $2/ in
    "$1"/*) return 0 ;;
  esac
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

# Whether the settings $1 hold a command of ours; refused when jq cannot read them.
names_hook() {
  local rc=0
  [ -f "$1" ] || return 1
  jq -e --arg ours "$ours" \
    '[(.hooks // {})[][] | (.hooks // [])[] | .command? | strings | select(startswith($ours))] | length > 0' \
    "$1" > /dev/null 2>&1 || rc=$?
  case $rc in
    0) return 0 ;;
    1) return 1 ;;
    *) refuse "jq could not read $1; nothing was changed" ;;
  esac
}

# Whether $1 is the text of one JSON object.
object() {
  [ -n "$1" ] && jq -e 'type == "object"' <<< "$1" > /dev/null 2>&1
}

# The settings on stdin with our handlers and the commands of the JSON array $1 removed, and the
# groups of the JSON array $2 appended, each to its event. Groups and events left empty by a
# removal go, and so does a `hooks` left empty.
rewrite_settings() {
  jq --arg ours "$ours" --argjson drop "$1" --argjson add "$2" '
    def gone: (.command? // null) as $c
      | ($c | type) == "string" and (($c | startswith($ours)) or any($drop[]; . == $c));
    (.hooks // {}) as $was
    | .hooks = ($was | with_entries(.value |= map(
        if (.hooks | type) == "array" then
          (.hooks | length) as $n | .hooks |= map(select(gone | not))
          | select((.hooks | length) > 0 or $n == 0)
        else . end)))
    | reduce $add[] as $a (.; .hooks[$a.event] = ((.hooks[$a.event] // []) + [$a.group]))
    | .hooks |= with_entries(select((.value | length) > 0 or (($was[.key] // []) | length) == 0))
    | if (.hooks | length) == 0 then del(.hooks) else . end'
}

# The checkout's local MCP entry $1 in ~/.claude.json, as `<ours><where>`: <ours> is 1 when it
# is an edge's /mcp, an address the join would write, else 0; <where> is its URL's scheme, host
# and path alone (no credentials, query or fragment), or `the command <program>`. Empty when
# there is none; fails when jq cannot read the file.
mcp_entry() {
  [ -f "$HOME/.claude.json" ] || return 0
  jq -r --arg p "$top" --arg n "$1" --arg ours "${address%'$'}/mcp\$" '
    .projects[$p].mcpServers[$n] as $e
    | def shown: gsub("[[:cntrl:]]"; "?");
    if ($e | type) != "object" then empty
    elif ($e.url | type) == "string" then
      ([$e.url | capture("^(?<s>[A-Za-z][A-Za-z0-9+.-]*://)([^@/?#]*@)?(?<h>[^/?#]*)(?<p>[^?#]*)")]
        | first // null) as $m
      | (if ($e.url | test($ours)) then "1" else "0" end)
        + (if $m then ($m.s + $m.h + $m.p | shown) else "an address that is not a URL" end)
    elif ($e.command | type) == "string" then "0the command " + ($e.command | shown)
    else "0an entry of another kind" end' "$HOME/.claude.json"
}

# How git treats the settings, `.claude/settings.local.json` under the checkout's top, where
# info/exclude holds $1: `tracked`; `ours` when the join's lines in info/exclude ignore it;
# `ignored` by any other rule; or `open`. Fails when git cannot say.
settings_kept() {
  local listed rc=0
  listed=$(git ls-files -- .claude/settings.local.json) || return 1
  if [ -n "$listed" ]; then
    printf tracked
    return 0
  fi
  case $1 in
    *"$marker"$'\n'"$excluded"$'\n'*)
      printf ours
      return 0
      ;;
  esac
  git check-ignore -q -- .claude/settings.local.json || rc=$?
  case $rc in
    0) printf ignored ;;
    1) printf open ;;
    *) return 1 ;;
  esac
}

# Sets `text` to the file $1's bytes, every one kept (it holds no NUL), empty when there is none;
# refused when it cannot be read.
text_of() {
  text=''
  [ -e "$1" ] || return 0
  [ -f "$1" ] && [ -r "$1" ] || refuse "cannot read $1; nothing was changed"
  IFS= read -r -d '' text < "$1" || true # read fails at the end of a file with no NUL
}

# Our groups, one per event, as a JSON array of {event, group}.
our_groups() {
  local h
  h=$(sq "$H")
  jq -n --arg h "$h" --arg m "$tools_matcher" '
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
  # The line, after the marker, that keeps the settings out of `git add -A` in every worktree.
  excluded=/.claude/settings.local.json
  if [ -z "${BASH_VERSINFO-}" ] || [ "${BASH_VERSINFO[0]}" -lt 5 ]; then
    refuse "needs bash 5 or later; this is bash ${BASH_VERSION-unknown}"
  fi
  local tool gnu=0
  # The hook's tools too: its watchdog is `timeout`, and it posts commits through jq's --args.
  for tool in git jq curl timeout; do
    command -v "$tool" > /dev/null 2>&1 || refuse "needs $tool on PATH"
  done
  [ "$(jq -nr '$ARGS.positional[0]' --args ok 2> /dev/null)" = ok ] ||
    refuse "needs jq 1.6 or later, whose --args the hook uses; this is $(jq --version 2> /dev/null | tr -cd '[:print:]')"
  # Only --retire needs GNU realpath's -e and -m; BSD's and older busybox's lack them.
  if realpath -e -- / > /dev/null 2>&1 && realpath -m -- /nowhere/x > /dev/null 2>&1; then gnu=1; fi

  local edge='' undo=0 mcp=grit dir r home
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
        [ "$gnu" = 1 ] || refuse "--retire needs GNU realpath, for its -e and -m, which this one lacks"
        home=$(realpath -m -- "$HOME") || refuse "cannot resolve HOME, $HOME"
        r=$(realpath -e -- "$dir" 2> /dev/null) && [ -d "$r" ] || refuse "--retire $dir is not a directory"
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
  local named='^[A-Za-z0-9][A-Za-z0-9_.-]{0,63}$'
  address='^https?://(\[[0-9a-f:]+\]|[A-Za-z0-9.-]+)(:[0-9]+)?$'
  local loopback='^http://(localhost|127\.[0-9]+\.[0-9]+\.[0-9]+|\[::1\])(:[0-9]+)?$'
  [[ $mcp =~ $named ]] || refuse "--mcp-name $mcp is not 1 to 64 letters, digits, _, . or -, starting with a letter or digit"
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
  # From here on, as a hook runs: a relative program in the settings is read from the checkout,
  # and `claude mcp --scope local` keys its entry by it.
  cd -- "$top" || refuse "cannot enter $top"
  for r in "${retire[@]+"${retire[@]}"}"; do
    if covers "$r" "$home" || covers "$r" "$top"; then
      refuse "--retire $r holds HOME or this checkout, and would retire every program in it; name the directory of the old hooks"
    fi
  done
  git worktree list --porcelain -z > /dev/null 2>&1 ||
    refuse "needs git 2.36 or later, whose git worktree list takes -z"
  local common hooks
  common=$(git rev-parse --path-format=absolute --git-common-dir) || refuse "git cannot name this checkout's repository"
  hooks=$(git rev-parse --path-format=absolute --git-path hooks) || refuse "git cannot name this checkout's hooks"
  exclude=$(git rev-parse --path-format=absolute --git-path info/exclude) ||
    refuse "git cannot name this checkout's info/exclude"
  H=$common/grit/$harness/hook
  if controlled "$top" || controlled "$H" || controlled "$hooks" || controlled "$exclude"; then
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
    # core.hooksPath may name a directory other repositories share, or one the checkout tracks.
    local own=0
    if [ "$hooks" = "$common/hooks" ]; then own=1; fi
    join "$settings" "$hooks" "$edge" "$mcp" "$own"
  fi
}

# Joins the checkout: the hook, its handlers in the settings $1, git's hooks in $2 when $5 is 1
# (they are the repository's own, not core.hooksPath's), and the MCP entry $4 at the edge $3.
join() {
  local settings=$1 hooks=$2 edge=$3 mcp=$4 own=$5 c groups drop='[]' f new count qh qe qH old
  local -a removed=() lines=() written=() foreign=() commands=()

  # Everything that can fail is worked out before the first file is written.
  if [ -e "$settings" ]; then
    mapfile -d '' -t commands < <(commands_of "$settings")
    wait "$!" || refuse "jq could not read $settings; nothing was changed"
  fi
  for c in "${commands[@]+"${commands[@]}"}"; do
    if [[ $c != "$ours"* ]] && retired "$c"; then
      removed+=("$c")
      drop=$(jq -c --arg c "$c" '. + [$c]' <<< "$drop") || refuse "jq failed; nothing was changed"
    fi
  done

  for f in post-commit post-rewrite; do
    if [ "$own" != 1 ]; then
      continue
    elif [ ! -e "$hooks/$f" ] || ours_file "$hooks/$f"; then
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

  groups=$(our_groups) || refuse "jq failed; nothing was changed"
  if [ -e "$settings" ]; then
    new=$(rewrite_settings "$drop" "$groups" < "$settings") || refuse "jq could not rewrite $settings; nothing was changed"
  else
    new=$(rewrite_settings "$drop" "$groups" <<< '{}') || refuse "jq could not write the settings; nothing was changed"
  fi
  object "$new" || refuse "jq wrote no settings object; nothing was changed"
  count=$(jq '[.[].group.hooks | length] | add' <<< "$groups") || refuse "jq failed; nothing was changed"
  old=$(mcp_entry "$mcp") || refuse "jq could not read ~/.claude.json; nothing was changed"
  old=${old#?}
  # The settings are the person's own, and may hold tokens: `git add -A` must not stage them.
  local kept text excluding=''
  text_of "$exclude"
  kept=$(settings_kept "$text") ||
    refuse "git could not say whether .claude/settings.local.json is tracked or ignored; nothing was changed"
  if [ "$kept" = open ]; then
    [ ! -L "$exclude" ] || refuse "$exclude is a symbolic link; the join writes no file it does not own"
    [ -z "$text" ] || [[ $text == *$'\n' ]] || text+=$'\n'
    excluding=$text$marker$'\n'$excluded$'\n'
  fi
  qh=$(sq "$harness")
  qe=$(sq "$edge")
  qH=$(sq "$H")

  put "$H" 0755 "#!/usr/bin/env bash
harness=$qh
edge=$qe
edge=\${GRIT_EDGE:-\$edge}
$hook"
  put_kept "$settings" 0644 "$new
"
  [ -z "$excluding" ] || put_kept "$exclude" 0644 "$excluding"
  for f in "${written[@]+"${written[@]}"}"; do
    if [ "$f" = post-commit ]; then
      put "$hooks/$f" 0755 "#!/usr/bin/env bash
$marker
exec $qH commit
"
    else
      put "$hooks/$f" 0755 "#!/usr/bin/env bash
$marker
exec $qH rewrite \"\$@\"
"
    fi
  done

  printf 'grit %s: joined %s to %s\n' "$harness" "$top" "$edge"
  printf '  hook      %s, shared by this repository'\''s worktrees\n' "$H"
  printf '  settings  .claude/settings.local.json: %s handlers of ours\n' "$count"
  case $kept in
    tracked) printf '  ignore    .claude/settings.local.json is tracked, so git add -A stages what the join wrote; it is left so\n' ;;
    ignored) printf '  ignore    .claude/settings.local.json is ignored already\n' ;;
    *) printf '  ignore    .claude/settings.local.json in %s, shared by this repository'\''s worktrees\n' "$exclude" ;;
  esac
  for c in "${removed[@]+"${removed[@]}"}" "${lines[@]+"${lines[@]}"}"; do
    printf '  removed   %s\n' "$c"
  done
  case ${#written[@]} in
    2) printf '  git       post-commit and post-rewrite in %s, shared by this repository'\''s worktrees\n' "$hooks" ;;
    1) printf '  git       %s in %s, shared by this repository'\''s worktrees\n' "${written[0]}" "$hooks" ;;
  esac
  # A line to add has no `exec`: in a hook of its own, it would end that hook at the line.
  for f in "${foreign[@]+"${foreign[@]}"}"; do
    if [ "$f" = post-commit ]; then c="$qH commit"; else c="$qH rewrite \"\$@\""; fi
    printf '  git       %s/%s is not ours and is left as it is; add the line: %s\n' "$hooks" "$f" "$c"
  done
  if [ "$own" != 1 ]; then
    printf '  git       %s is core.hooksPath, which the join does not write; add these lines\n' "$hooks"
    printf '  git       post-commit: %s commit\n' "$qH"
    printf '  git       post-rewrite: %s rewrite "$@"\n' "$qH"
  fi

  local add="claude mcp add --transport http --scope local $mcp $edge/mcp"
  if command -v claude > /dev/null 2>&1; then
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
  local settings=$1 hooks=$2 mcp=$3 n=0 f w other='' new entry
  local -a gone=() entries=()

  # Everything that can fail is worked out before the first file is changed.
  if [ -e "$settings" ]; then
    n=$(jq --arg ours "$ours" \
      '[(.hooks // {})[][] | (.hooks // [])[] | .command? | strings | select(startswith($ours))] | length' \
      "$settings") || refuse "jq could not read $settings; nothing was changed"
  fi
  if [ "$n" -gt 0 ]; then
    new=$(rewrite_settings '[]' '[]' < "$settings") || refuse "jq could not rewrite $settings; nothing was changed"
    object "$new" || refuse "jq wrote no settings object; nothing was changed"
  fi
  entry=$(mcp_entry "$mcp") || refuse "jq could not read ~/.claude.json; nothing was changed"
  mapfile -d '' -t entries < <(git worktree list --porcelain -z)
  wait "$!" || refuse "git worktree list failed; nothing was changed"
  for f in "${entries[@]+"${entries[@]}"}"; do
    case $f in
      'worktree '*)
        w=${f#worktree }
        if [ "$w" != "$top" ] && [ -z "$other" ] && names_hook "$w/.claude/settings.local.json"; then
          other=$w
        fi
        ;;
    esac
  done

  # The join's two lines in info/exclude, the first of them if a person copied them.
  local text excluding=''
  text_of "$exclude"
  case $text in
    *"$marker"$'\n'"$excluded"$'\n'*) excluding=${text/"$marker"$'\n'"$excluded"$'\n'/} ;;
    *) text='' ;;
  esac

  if [ "$n" -gt 0 ]; then
    put_kept "$settings" 0644 "$new
"
  fi

  printf 'grit %s: undid the join in %s\n' "$harness" "$top"
  printf '  settings  .claude/settings.local.json: %s handlers of ours removed\n' "$n"
  if [ -n "$other" ]; then
    printf '  hook      %s kept: the worktree %s still names it\n' "$H" "$other"
    printf '  git       post-commit and post-rewrite kept, as the hook is\n'
    if [ -n "$text" ]; then
      printf '  ignore    .claude/settings.local.json kept in %s, as the hook is\n' "$exclude"
    fi
  else
    if [ -n "$text" ]; then
      put_kept "$exclude" 0644 "$excluding"
    fi
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
    if [ -n "$text" ]; then
      printf '  ignore    .claude/settings.local.json'\''s lines removed from %s\n' "$exclude"
    fi
    for f in post-commit post-rewrite; do
      if [ -f "$hooks/$f" ] && grep -qF -- "$H" "$hooks/$f"; then
        printf '  git       %s/%s is not ours and still names %s; remove that line\n' "$hooks" "$f" "$H"
      fi
    done
  fi
  # Only an entry the join would have written goes: a checkout never joined may hold another.
  if [ -z "$entry" ]; then
    printf '  mcp       no entry %s in local scope\n' "$mcp"
  elif [ "${entry:0:1}" != 1 ]; then
    printf '  mcp       %s at %s is not an edge'\''s /mcp and is left as it is\n' "$mcp" "${entry#?}"
  elif ! command -v claude > /dev/null 2>&1; then
    printf '  mcp       claude not found; run: claude mcp remove --scope local %s\n' "$mcp"
  elif claude mcp remove --scope local "$mcp" > /dev/null 2>&1; then
    printf '  mcp       %s at %s removed from local scope\n' "$mcp" "${entry#?}"
  else
    printf '  mcp       %s at %s: claude mcp remove failed; run: claude mcp remove --scope local %s\n' \
      "$mcp" "${entry#?}" "$mcp"
  fi
  printf 'Nothing retired was restored.\n'
}

</dev/null main "$@"
