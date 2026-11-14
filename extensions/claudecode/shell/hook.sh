# grit's hook for Claude Code: one program, a subcommand per role, that the join script installs
# after a shebang and the lines setting `harness` and `edge` (GRIT_EDGE, when set, overriding
# `edge`). Each role is a hook registered in the checkout's .claude/settings.local.json or git's
# post-commit and post-rewrite, or the holder a Bash tool asks for:
#
#   hook brief | opening | event          a Claude Code hook, the event's JSON on stdin
#   hook commit | rewrite <amend|rebase>  a git hook
#   hook holder [<type> --dir <dir> --session <id>]
#
# What each reads, asks, posts and prints: README.md. A hook never fails a session: every role
# but `holder` exits 0 whatever happens, saying on stderr what it could not do, and the whole run
# is bounded by one deadline, 3 s from its start, past which a watchdog stops it (a SessionEnd's
# requests, 1.2 s; a commit's and a rewrite's, 1 s). Only `brief` and `opening` print, and only
# Claude Code's hook JSON.

set -uo pipefail

say() {
  printf 'grit %s hook: %s\n' "$harness" "${1//[[:cntrl:]]/?}" >&2
}

# The edge's address, as the join checks it: GRIT_EDGE comes from the session's environment, past
# every check the join made.
address='^https?://(\[[0-9a-f:]+\]|[A-Za-z0-9.-]+)(:[0-9]+)?$'
loopback='^http://(localhost|127\.[0-9]+\.[0-9]+\.[0-9]+|\[::1\])(:[0-9]+)?$'

# The bound on a whole run, in seconds.
bound=3

# The watchdog: the first run starts the hook again under `timeout`, which stops it and every
# program it started at the bound, and exits 0 (`holder`, with its status).
if [ -z "${GRIT_HOOK_STARTED-}" ]; then
  failed=0
  [ "${1-}" = holder ] && failed=1
  if [ -z "${BASH_VERSINFO-}" ] || [ "${BASH_VERSINFO[0]}" -lt 5 ]; then
    say "needs bash 5 or later; it did nothing"
    exit "$failed"
  fi
  for tool in jq curl git timeout; do
    command -v "$tool" > /dev/null 2>&1 || {
      say "needs $tool on PATH; it did nothing"
      exit "$failed"
    }
  done
  if ! [[ $edge =~ $address ]]; then
    say "the edge's address${GRIT_EDGE:+ in GRIT_EDGE} is not http:// or https://, a host and a port, nothing after; it did nothing"
    exit "$failed"
  fi
  if [[ $edge == http://* ]] && ! [[ $edge =~ $loopback ]]; then
    say "the edge's address${GRIT_EDGE:+ in GRIT_EDGE} is plain http beyond this machine, where its posts would be read in clear; it did nothing"
    exit "$failed"
  fi
  self=${BASH_SOURCE[0]}
  [[ $self == /* ]] || self=$PWD/$self
  GRIT_HOOK_STARTED=${EPOCHREALTIME//[!0-9]/} GRIT_HOOK_SELF=$self GRIT_EDGE=$edge \
    timeout -k 0.2 "$bound" bash -- "$self" "$@"
  rc=$?
  case $rc in
    0) exit 0 ;;
    124 | 137) say "${1-} ran past its bound and was stopped" ;;
  esac
  exit "$failed"
fi

started=$GRIT_HOOK_STARTED
self=$GRIT_HOOK_SELF
unset GRIT_HOOK_STARTED GRIT_HOOK_SELF
export TZ=UTC
deadline=$((10#$started + bound * 1000000))
# The session and agent ids the hook sends; one that does not match is never sent. An agent's type
# is sent as the payload has it, as a query parameter or a JSON string.
id='^[A-Za-z0-9_-][A-Za-z0-9._-]{0,99}$'
# A loopback edge is asked directly: a proxy the environment names would take its posts off the
# machine.
direct=()
[[ ! $edge =~ $loopback ]] || direct=(--noproxy '*')

# $1 single-quoted, as any POSIX shell reads it back.
sq() {
  local q="'\\''"
  printf "'%s'" "${1//"'"/$q}"
}

# Sets `secs` to the seconds left before the deadline, less a margin for what follows a request;
# fails when too few are left to ask anything.
left() {
  local now=${EPOCHREALTIME//[!0-9]/} us
  us=$((deadline - 10#$now - 250000))
  ((us >= 50000)) || return 1
  printf -v secs '%d.%06d' $((us / 1000000)) $((us % 1000000))
}

# GET $1 with the query parameters $2…, each `name=value`, URL-encoded: sets `status` to the HTTP
# status and `answer` to the body; fails when the edge did not answer.
ask() {
  local path=$1 out kv secs
  shift
  local -a query=()
  for kv in "$@"; do query+=(--data-urlencode "$kv"); done
  left || return 1
  out=$(curl -q -sG "${direct[@]+"${direct[@]}"}" --connect-timeout 0.3 --max-time "$secs" -w '\n%{http_code}' "${query[@]}" -- "$edge$path") ||
    return 1
  status=${out##*$'\n'}
  answer=${out%$'\n'*}
}

# Claude Code's hook JSON adding $2 to the window, for the event $1, on stdout.
context() {
  local out
  out=$(jq -nc --arg e "$1" --arg c "$2" '{hookSpecificOutput: {hookEventName: $e, additionalContext: $c}}') &&
    printf '%s\n' "$out"
}

# Reads the hook's event from stdin into `input`, and from it `event`, `session`, `agent`, `type`,
# `cwd`, `source` and `file` (a tool's file), each empty when missing, not a string, or holding a
# control character; and `dir`, the starting session's directory. Fails, saying why, on input that
# is no JSON object, or a session or agent id the hook cannot send.
payload() {
  local fields
  local -a f=()
  input=$(cat) || {
    say "could not read its input"
    return 1
  }
  fields=$(jq -r '
    def word: if type == "string" and (test("[[:cntrl:]]") | not) then . else "" end;
    if type != "object" then error("no object") else . end
    | (.tool_input | if type == "object" then .file_path // .notebook_path else null end) as $file
    | .hook_event_name, .session_id, .agent_id, .agent_type, .cwd, .source, $file
    | word' <<< "$input" 2> /dev/null) || {
    say "its input is no JSON object; it did nothing"
    return 1
  }
  mapfile -t f <<< "$fields"
  event=${f[0]-} session=${f[1]-} agent=${f[2]-} type=${f[3]-} cwd=${f[4]-} source=${f[5]-} file=${f[6]-}
  if ! [[ $session =~ $id ]]; then
    say "${event:-an event} names no session id it can send; it did nothing"
    return 1
  fi
  if [ -n "$agent" ] && ! [[ $agent =~ $id ]]; then
    say "$event names an agent id it cannot send; it did nothing"
    return 1
  fi
  dir=${CLAUDE_PROJECT_DIR:-$cwd}
  [[ $dir == /* ]] || {
    say "$event has no absolute directory; it did nothing"
    return 1
  }
}

# The brief of the session or agent starting: fresh at a new window (startup, clear, an agent),
# resumed where the window goes on (resume, compact, fork).
brief() {
  local start who=session
  local -a more=()
  payload || return 0
  case $event in
    SessionStart)
      case $source in
        startup | clear) start=fresh ;;
        *) start=resume ;;
      esac
      ;;
    SubagentStart)
      # An agent no type names is none this session started (a compaction's).
      [ -n "$agent" ] && [ -n "$type" ] || return 0
      start=fresh who=agent
      more=("agent=$agent" "type=$type")
      ;;
    *) return 0 ;;
  esac
  if ! ask /brief "harness=$harness" "session=$session" "dir=$dir" "start=$start" "${more[@]+"${more[@]}"}"; then
    say "the edge at $edge did not answer for the brief"
    context "$event" "grit: grit's server at $edge did not answer. This $who starts without its brief, and what it does is not recorded until the server answers."
  elif [ "$status" != 200 ]; then
    say "the edge at $edge answered $status for the brief: ${answer%%$'\n'*}"
    context "$event" "grit: grit's server at $edge answered $status when asked for this $who's brief, so it starts without one."
  elif [ -n "$answer" ]; then
    context "$event" "$answer"
  fi
}

# What a session opens with besides its brief: the claim holder it works as, how an agent it
# starts learns its own, and after a compaction or a clear the digest of what it did lately.
opening() {
  local holder text part=''
  payload || return 0
  [ "$event" = SessionStart ] || return 0
  if ! ask /holder "harness=$harness" "dir=$dir"; then
    say "the edge at $edge did not answer for the holder"
    return 0
  elif [ "$status" != 200 ]; then
    say "the edge at $edge answered $status for the holder: ${answer%%$'\n'*}"
    return 0
  fi
  holder=$answer
  # TYPE, not <type>: pasted into a shell, `<type>` would be a redirect.
  text="grit: this session's claim holder is $holder.
grit: an agent this session starts has its own claim holder. This prints it, with the agent's type in place of TYPE: $(sq "$self") holder TYPE --dir $(sq "$dir") --session $(sq "$session")"
  case $source in
    compact) part=resume ;;
    clear) part=fresh ;;
  esac
  if [ -n "$part" ]; then
    if ! ask /brief "harness=$harness" "session=$session" "dir=$dir" "start=$part" part=digest; then
      say "the edge at $edge did not answer for the digest"
    elif [ "$status" != 200 ]; then
      say "the edge at $edge answered $status for the digest: ${answer%%$'\n'*}"
    elif [ -n "$answer" ]; then
      text+=$'\n\n'$answer
    fi
  fi
  context SessionStart "$text"
}

# The claim holder of the session in --dir (the checkout's top by default), or of an agent of
# <type> that the session --session (the Bash tool's own by default) starts, on stdout; exit 1,
# saying why, when the edge did not tell it.
holder() {
  local kind='' d=${CLAUDE_PROJECT_DIR-} s=${CLAUDE_CODE_SESSION_ID-}
  local -a more=()
  while [ "$#" -gt 0 ]; do
    case $1 in
      --dir | --session)
        [ "$#" -ge 2 ] || {
          say "holder: $1 needs a value"
          return 1
        }
        if [ "$1" = --dir ]; then d=$2; else s=$2; fi
        shift
        ;;
      -*)
        say "holder: unknown option $1"
        return 1
        ;;
      *)
        [ -z "$kind" ] || {
          say "holder: one agent type, not two: $kind and $1"
          return 1
        }
        kind=$1
        ;;
    esac
    shift
  done
  if [ -z "$d" ]; then
    d=$(git rev-parse --show-toplevel 2> /dev/null) || d=$PWD
  fi
  if [ -n "$kind" ]; then
    [[ $s =~ $id ]] || {
      say "holder $kind: no session id; run it in the Bash tool of the session that starts the agent, or pass --session"
      return 1
    }
    more=("session=$s" "type=$kind")
  fi
  if ! ask /holder "harness=$harness" "dir=$d" "${more[@]+"${more[@]}"}"; then
    say "holder: the edge at $edge did not answer"
    return 1
  elif [ "$status" != 200 ]; then
    say "holder: the edge at $edge answered $status: ${answer%%$'\n'*}"
    return 1
  fi
  printf '%s\n' "$answer"
}

# Sets `n` and `at` from one reading of the clock: `n` its microseconds times 1000 plus this
# process's id modulo 1000, `at` the instant.
stamp() {
  local us=${EPOCHREALTIME//[!0-9]/}
  us=$((10#$us))
  n=$((us * 1000 + BASHPID % 1000))
  printf -v at '%(%Y-%m-%dT%H:%M:%S)T.%06dZ' $((us / 1000000)) $((us % 1000000))
}

# The top of the checkout the absolute directory $1 is in; fails when it is in none.
toplevel() {
  [[ $1 == /* ]] && git -C "$1" rev-parse --show-toplevel 2> /dev/null
}

# Posts the event lines $1 to the edge, each line's fate said on stderr when it was not kept, $2
# naming them and $3 its verb, `was` or `were`. Nothing is kept to resend.
send() {
  local out code answer secs why
  if ! left; then
    say "$2 $3 dropped: no time was left for its post"
    return 0
  fi
  out=$(curl -q -s "${direct[@]+"${direct[@]}"}" --connect-timeout 0.3 --max-time "$secs" -H 'Content-Type: application/x-ndjson' \
    --data-binary @- -w '\n%{http_code}' -- "$edge/events?harness=$harness" <<< "$1") || {
    say "$2 $3 dropped: the edge at $edge did not answer"
    return 0
  }
  code=${out##*$'\n'} answer=${out%$'\n'*}
  case $code in
    200)
      why=$(jq -r '.refused[]? | "line \(.line) refused: \(.why)"' <<< "$answer" 2> /dev/null) || why=''
      [ -z "$why" ] || while IFS= read -r out; do say "$2: $out"; done <<< "$why"
      ;;
    413) say "$2 $3 dropped: the edge at $edge took its post as too long (413)" ;;
    *) say "the edge at $edge answered $code to $2, keeping nothing: ${answer%%$'\n'*}" ;;
  esac
}

# The event line of a Claude Code hook, in grit's words, from the payload `input` and the shell's
# `session`, `agent`, `type`, `dir`, `w` (the worktree it ran in), `fw` (its file's), `n` and `at`:
# nothing for an event or tool the edge is told nothing of. Strings are cut to the line's bounds
# (/events): a name to its last or first characters within so many UTF-8 bytes, control
# characters written `?`, a summary to 2,000 UTF-16 characters, as grit counts them.
lined() {
  jq -c --arg session "$session" --arg agent "$agent" --arg type "$type" --arg dir "$dir" \
    --arg w "$w" --arg fw "$fw" --arg n "$n" --arg at "$at" '
    def str: if type == "string" then . else "" end;
    def clean: str | gsub("[[:cntrl:]]"; "?");
    def fits($k): (tojson | utf8bytelength) <= $k + 2;
    def head($k): clean | .[:$k] | until(fits($k); .[:-1]);
    def tail($k): clean | .[-$k:] | until(fits($k); .[1:]);
    def said: str | explode | .[:2000] as $c
      | (reduce $c[] as $x ({u: 0, k: 0};
          if .u < 0 then .
          else (.u + (if $x > 65535 then 2 else 1 end)) as $u
            | if $u <= 2000 then {u: $u, k: (.k + 1)} else {u: -1, k: .k} end
          end)) as $r
      | $c[:$r.k] | implode;
    def rel($cwd):
      if $cwd != "" and startswith($cwd + "/") then .[($cwd | length) + 1:] else (split("/") | last // "") end;
    def placed($cwd):
      str as $p
      | if $fw != "" and ($p | startswith($fw + "/")) and ($fw == $w or ($fw | utf8bytelength) <= 100)
        then {path: ($p[($fw | length) + 1:] | tail(120))} + (if $fw == $w then {} else {worktree: $fw} end)
        else {path: ($p | rel($cwd) | tail(120))}
        end;
    def program:
      str | .[:400] | sub("^\\s*(cd(\\s+[^;&|\\s]+)?\\s*(&&|;)\\s*)+"; "")
      | [splits("\\s+") | select(length > 0 and (contains("=") | not))]
      | (.[0] // "") | (split("/") | last // "") | head(40);
    def answer:
      if type == "string" then .
      elif type == "array" then map(select(type == "object") | .text | strings) | join("")
      elif type == "object" and (.content | type) == "array" then .content | answer
      else ""
      end;
    .hook_event_name as $e
    | (.tool_name | str) as $tool
    | ($e == "PostToolUse") as $ok
    | (.cwd | str) as $cwd
    | (.tool_input | if type == "object" then . else {} end) as $in
    | (if $ok then .tool_response else null end) as $out
    | (if $e == "PostToolUse" or $e == "PostToolUseFailure" then
         if $tool == "Read" then
           {event: "read",
            bytes: ([$out | objects | .file | objects | .content | strings | utf8bytelength] | add // 0),
            shape: (($in.file_path | placed($cwd)) + {ok: $ok})}
         elif $tool == "Edit" or $tool == "Write" or $tool == "MultiEdit" or $tool == "NotebookEdit" then
           {event: "edited", shape: ((($in.file_path // $in.notebook_path) | placed($cwd)) + {ok: $ok})}
         elif $tool == "Bash" then {event: "ran", shape: {program: ($in.command | program), ok: $ok}}
         elif ($tool | startswith("mcp__")) then
           ($tool | split("__") | last) as $t
           | {event: "read", bytes: ($out | answer | utf8bytelength),
              shape: ((if $t == "find" or $t == "area" then {}
                       else {named: ($in
                         | [(.symbols | if type == "array" then .[0] else null end), .suite, .target]
                         | map(strings) | (.[0] // "") | head(120))}
                       end) + {tool: ($t | head(40)), ok: $ok})}
         else empty
         end
       elif $e == "SessionStart" then {event: "started", shape: {source: (.source | head(20))}}
       elif $e == "SubagentStart" and $agent != "" then {event: "started"}
       elif $e == "Stop" then
         (.last_assistant_message | said) as $t | if $t == "" then empty else {event: "closing", text: $t} end
       elif $e == "PostCompact" then
         (.compact_summary | said) as $t
         | if $t == "" then empty else {event: "closing", text: $t, shape: {trigger: (.trigger | head(20))}} end
       elif $e == "SubagentStop" and $agent != "" then {event: "ended"}
       elif $e == "SessionEnd" then {event: "ended", shape: {reason: (.reason | head(40))}}
       else empty
       end) as $line
    | {session: $session} + (if $agent != "" then {agent: $agent, type: $type} else {} end)
      + {dir: $dir, worktree: $w, n: $n, at: $at} + $line' <<< "$input"
}

# A Claude Code hook's event, posted as one line.
event() {
  local w fw='' line d
  payload || return 0
  # Claude Code gives SessionEnd's hooks 1.5 s by default.
  [ "$event" != SessionEnd ] || deadline=$((10#$started + 1200000))
  # An agent no type names is none this session started: a compaction's (recorded).
  [ -z "$agent" ] || [ -n "$type" ] || return 0
  w=$(toplevel "$cwd") || w=$cwd
  [[ $w == /* ]] || w=$dir
  if [[ $file == /* ]]; then
    d=${file%/*}
    fw=$(toplevel "${d:-/}") || fw=''
  fi
  stamp
  line=$(lined) || {
    say "could not read $event as an event; it was dropped"
    return 0
  }
  [ -z "$line" ] || send "$line" "$event" was
}

# Whether a rebase is in progress in this worktree.
rebasing() {
  local merge apply
  merge=$(git rev-parse --git-path rebase-merge 2> /dev/null) &&
    apply=$(git rev-parse --git-path rebase-apply 2> /dev/null) &&
    { [ -d "$merge" ] || [ -d "$apply" ]; }
}

# Posts, for the session `session` in this checkout, the commits $@ in order, each
# `<sha>[ <replaced sha>…]`, with ordinals one apart, in posts of at most 256 KiB, a quarter of
# the edge's 1 MiB bound on a body. One line is far smaller: its strings are cut but `dir` and
# `worktree`, an environment variable (128 KiB at most, as Linux passes one) and a path.
commits() {
  local top subjects lines line post='' i k=0 what were
  local -a shas=() subject=() args=()
  # git returns from a commit only once its hooks end, so their requests end 1 s after the start.
  deadline=$((10#$started + 1000000))
  top=$(git rev-parse --show-toplevel 2> /dev/null) || return 0
  dir=${CLAUDE_PROJECT_DIR:-$top}
  for i in "$@"; do shas+=("${i%% *}"); done
  counted "$#" "$#"
  subjects=$(git log --no-show-signature --no-walk=unsorted --format=%s "${shas[@]}" --) || {
    say "$what $were not posted: git log failed"
    return 0
  }
  mapfile -t subject <<< "$subjects"
  stamp
  for ((i = 0; i < $#; i++)); do
    args+=("${shas[i]}" "${@:i+1:1}" "${subject[i]-}" "$((n + i))")
  done
  # Each post's lines, then an empty line: a line is a JSON object, never empty.
  lines=$(jq -nr --arg session "$session" --arg dir "$dir" --arg w "$top" --arg at "$at" '
    def head($k): if type == "string" then gsub("[[:cntrl:]]"; "?") | .[:$k] else "" end;
    reduce ($ARGS.positional | range(0; length; 4) as $i | .[$i:$i + 4] as [$sha, $was, $subject, $n]
      | ($was | split(" ") | .[1:] | map(select(length > 0) | .[:12]) | .[:4]) as $replaces
      | {session: $session, dir: $dir, worktree: $w, n: $n, at: $at, event: "commit",
         shape: ({sha: $sha, subject: ($subject | head(100))}
           + (if $replaces == [] then {} else {replaces: $replaces} end))}
      | tojson) as $l ({posts: [], size: 0};
        ($l | utf8bytelength + 1) as $k
        | if .posts == [] or .size + $k > 262144 then .posts += [[$l]] | .size = $k
          else .posts[(.posts | length) - 1] += [$l] | .size += $k end)
    | .posts[] | (.[], "")' --args "${args[@]}") || {
    say "$what $were not posted: jq could not write their event lines"
    return 0
  }
  while IFS= read -r line; do
    if [ -n "$line" ]; then
      post+=$line$'\n'
      k=$((k + 1))
    elif [ -n "$post" ]; then
      counted "$k" "$#"
      send "$post" "$what" "$were"
      post='' k=0
    fi
  done <<< "$lines"
  # The command substitution dropped the last post's empty line.
  [ -z "$post" ] || { counted "$k" "$#" && send "$post" "$what" "$were"; }
}

# Sets `what` to the $1 of the $2 commits made, as a line on stderr names them, and `were` to its
# verb.
counted() {
  if [ "$1" != "$2" ]; then
    what="$1 of the $2 commits made"
  elif [ "$1" = 1 ]; then
    what='the commit made'
  else
    what="the $1 commits made"
  fi
  if [ "$1" = 1 ]; then were=was; else were=were; fi
}

# git's post-commit: the commit just made, for the Claude Code session whose Bash tool made it.
commit() {
  local sha
  session=${CLAUDE_CODE_SESSION_ID-}
  # Outside a Claude Code session a commit is no session's; mid-rebase, the rewrite posts it.
  [[ $session =~ $id ]] || return 0
  ! rebasing || return 0
  sha=$(git rev-parse HEAD 2> /dev/null) || return 0
  commits "$sha"
}

# git's post-rewrite, $1 `amend` or `rebase`, stdin a line `<old> <new>` per commit rewritten:
# each commit the rewrite left, once, in order, with those it replaces. An amend inside a rebase
# posts nothing, the rebase's own list holding it.
rewrite() {
  local list old new rest
  local -A olds=()
  local -a order=() made=()
  session=${CLAUDE_CODE_SESSION_ID-}
  [[ $session =~ $id ]] || return 0
  case ${1-} in
    rebase) ;;
    amend) ! rebasing || return 0 ;;
    *) return 0 ;;
  esac
  list=$(cat) || return 0
  while read -r old new rest; do
    [[ $old =~ ^[0-9a-f]{40,64}$ && $new =~ ^[0-9a-f]{40,64}$ ]] || continue
    [ -n "${olds[$new]+x}" ] || order+=("$new")
    olds[$new]="${olds[$new]-} $old"
  done <<< "$list"
  [ "${#order[@]}" -gt 0 ] || return 0
  for new in "${order[@]}"; do made+=("$new${olds[$new]}"); done
  commits "${made[@]}"
}

role=${1-}
shift || true
case $role in
  brief) brief ;;
  opening) opening ;;
  event) event ;;
  commit) commit ;;
  rewrite) rewrite "$@" ;;
  holder)
    holder "$@"
    exit
    ;;
  *) say "no role ${role:-given}; it did nothing" ;;
esac
exit 0
