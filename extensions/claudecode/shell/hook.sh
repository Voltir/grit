# grit's hook for Claude Code: one program, a subcommand per role, that the join script installs
# after a shebang, the lines setting `harness`, `edge` and either `credential`, the file holding
# the credential every request bears, or no credential and GRIT_EDGE, when set, overriding `edge`,
# then the adapter's line setting `unended`, the seconds after its last event a session counts as
# ended, and shared.sh.
# Each role is a hook registered in the checkout's .claude/settings.local.json or git's
# post-commit and post-rewrite, or what a Bash tool or Claude Code's MCP entry asks for:
#
#   hook brief | opening | event          a Claude Code hook, the event's JSON on stdin
#   hook commit | rewrite <amend|rebase>  a git hook
#   hook holder [<type> --dir <dir> --session <id>]
#   hook header                           the MCP entry's headersHelper
#
# What each reads, asks, posts and prints: README.md. A hook never fails a session: every role
# but `holder` and `header` exits 0 whatever happens, saying on stderr what it could not do, and
# the whole run is bounded by one deadline, 3 s from its start, past which a watchdog stops it (a
# SessionEnd's requests, 1.2 s; a commit's and a rewrite's, 1 s). Only `brief` and `opening`
# print, and only Claude Code's hook JSON; `header` prints the credential's header as JSON.

set -uo pipefail

say() {
  printf 'grit %s hook: %s\n' "$harness" "${1//[[:cntrl:]]/?}" >&2
}

# The bound on a whole run, in seconds.
bound=3
# The bounds on a commit's and a rewrite's requests, and on a SessionEnd's, in microseconds from
# the start: git returns from a commit only once its hooks end, and Claude Code gives SessionEnd's
# hooks 1.5 s by default.
committed=1000000
ended=1200000

# The watchdog: the first run starts the hook again under `timeout`, which stops it and every
# program it started at the bound, and exits 0 (`holder`, with its status).
if [ -z "${GRIT_HOOK_STARTED-}" ]; then
  failed=0
  case ${1-} in holder | header) failed=1 ;; esac
  if [ -z "${BASH_VERSINFO-}" ] || [ "${BASH_VERSINFO[0]}" -lt 5 ]; then
    say "needs bash 5 or later; it did nothing"
    exit "$failed"
  fi
  if ! [[ ${unended-} =~ ^[0-9]{1,9}$ ]]; then
    say "names no count of seconds after which a kept event is moot; it did nothing"
    exit "$failed"
  fi
  lacks=$(missing)
  if [ -n "$lacks" ]; then
    say "needs $lacks on PATH; it did nothing"
    exit "$failed"
  fi
  if ! [[ $edge =~ $address ]]; then
    say "the edge's address${GRIT_EDGE:+ in GRIT_EDGE} is not http:// or https://, a host and a port, nothing after; it did nothing"
    exit "$failed"
  fi
  if [[ $edge == http://* ]] && ! [[ $edge =~ $loopback ]]; then
    say "the edge's address${GRIT_EDGE:+ in GRIT_EDGE} is plain http beyond this machine, where its posts would be read in clear; it did nothing"
    exit "$failed"
  fi
  if [ -n "${credential-}" ] && [ -n "${GRIT_EDGE-}" ] && [ "$GRIT_EDGE" != "$edge" ]; then
    say "GRIT_EDGE names $GRIT_EDGE, but this checkout's credential is sent only to the edge it was joined to, $edge; it did nothing"
    exit "$failed"
  fi
  self=${BASH_SOURCE[0]}
  [[ $self == /* ]] || self=$PWD/$self
  GRIT_HOOK_STARTED=${EPOCHREALTIME//[!0-9]/} GRIT_HOOK_SELF=$self GRIT_HOOK_PARENT=$PPID GRIT_EDGE=$edge \
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
# The process that ran the hook: Claude Code's `sh -c`, or Claude Code itself.
parent=${GRIT_HOOK_PARENT-}
unset GRIT_HOOK_STARTED GRIT_HOOK_SELF GRIT_HOOK_PARENT
# Where /proc is read: GRIT_HOOK_PROC, which tests alone set, names a directory standing in for it.
proc=${GRIT_HOOK_PROC:-/proc}
# The sessions' files, one a session from its start until its end is recorded or kept, beside the
# hook. Every entry in it that is no session's file starts with a dot, which `sweep`'s `*` skips.
lives=${self%/*}/sessions
# The events the edge did not take, kept to send later: a queue (a directory) for each session,
# named by its id, and one for each agent of it, `<session>+<agent>`, holding a file a line, named
# by its `n`.
kept=$lives/.kept
# What a queue holds at most, in lines and in bytes: a queue is posted whole, in a post at most a
# quarter of the edge's 1 MiB bound on a body; a line is moot `unended` seconds after its `n`.
most_lines=1000
most_bytes=262144
export TZ=UTC
deadline=$((10#$started + bound * 1000000))
# The session and agent ids the hook sends; one that does not match is never sent. An agent's type
# is sent as the payload has it, as a query parameter or a JSON string.
id='^[A-Za-z0-9_-][A-Za-z0-9._-]{0,99}$'
# A loopback edge is asked directly: a proxy the environment names would take its posts off the
# machine.
direct=()
[[ ! $edge =~ $loopback ]] || direct=(--noproxy '*')
# The credential's text, read once; never on a command line, but written to curl through a pipe.
credential=${credential-}
token=''
if [ -n "$credential" ]; then
  { token=$(< "$credential"); } 2> /dev/null && [[ $token =~ ^[A-Za-z0-9._~+/-]+=*$ ]] || {
    say "cannot read the credential in $credential; it did nothing"
    exit 1
  }
fi

# The request header bearing the credential, as curl's -H @file reads it: none without one.
authorization() {
  [ -z "$token" ] || printf 'Authorization: Bearer %s\n' "$token"
}

# Says the edge's answer $2, not 200, to the request for $1, its body $3, and then $4; a 401 says
# what to do about the credential.
answered() {
  local first=${3%%$'\n'*}
  if [ "$2" != 401 ]; then
    say "the edge at $edge answered $2 for $1: $first${4-}"
  elif [ -n "$credential" ]; then
    say "the edge at $edge refused this machine's credential (401) for $1: $first; ask whoever runs the edge for this credential and put it in $credential${4-}"
  else
    say "the edge at $edge needs a credential (401) for $1: $first; ask whoever runs the edge for one, and join again with --credential <file>${4-}"
  fi
}

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
  out=$(curl -q -sG "${direct[@]+"${direct[@]}"}" --connect-timeout 0.3 --max-time "$secs" -H @<(authorization) -w '\n%{http_code}' "${query[@]}" -- "$edge$path") ||
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
    context "$event" "grit: grit's server at $edge did not answer. This $who starts without its brief; what it does is kept on this machine and recorded once the server answers."
  elif [ "$status" != 200 ]; then
    answered "the brief" "$status" "$answer"
    context "$event" "grit: grit's server at $edge answered $status when asked for this $who's brief, so it starts without one."
  elif [ -n "$answer" ]; then
    context "$event" "$answer"
  fi
}

# Sets `comm` to the name of the process $1 and `stat` to the fields of its stat after the name,
# the third first, read after the last `)` as a name may hold one; fails when there is no such
# process.
stat_of() {
  local text
  [[ $1 =~ ^[1-9][0-9]*$ ]] || return 1
  { text=$(< "$proc/$1/stat"); } 2> /dev/null && [[ $text == *') '* ]] || return 1
  comm=${text#*\(} comm=${comm%)*}
  read -ra stat <<< "${text##*) }"
}

# The harness's process, on stdout: the nearest process above the hook that is none of the shells
# and the watchdog it runs under; fails when one on the way cannot be read.
harnessed() {
  local p=$parent k comm
  local -a stat=()
  for ((k = 0; k < 8; k++)); do
    stat_of "$p" || return 1
    case $comm in
      sh | dash | bash | timeout) p=${stat[1]-} ;;
      *)
        printf '%s\n' "$p"
        return 0
        ;;
    esac
  done
  return 1
}

# Sets `host`, `boot` and `ns` to this machine's name, its boot's id and the pid namespace the hook
# runs in; fails when one cannot be read.
here() {
  { host=$(< "$proc/sys/kernel/hostname") && boot=$(< "$proc/sys/kernel/random/boot_id"); } 2> /dev/null &&
    ns=$(readlink -- "$proc/self/ns/pid") && [ -n "$host" ] && [ -n "$boot" ] && [ -n "$ns" ]
}

# Keeps the file of the session `session` in `dir`, `lives/<session>`, for a later start to tell
# whether it died: one JSON object of strings, `host`, `boot` and `ns` as `here` reads them, `pid`
# the harness's process, `start` that process's start in clock ticks since boot (field 22 of its
# stat), and `dir`. Says why when it cannot.
alive() {
  local pid host boot ns comm tmp=''
  local -a stat=()
  if ! pid=$(harnessed) || ! stat_of "$pid" || ! [[ ${stat[19]-} =~ ^[0-9]+$ ]] || ! here; then
    say "could not read which process runs this session, so a later start cannot end it should it stop without saying so"
    return 0
  fi
  if ! mkdir -p -- "$lives" || ! tmp=$(mktemp -- "$lives/.$session.XXXXXX") ||
    ! jq -nc --arg host "$host" --arg boot "$boot" --arg ns "$ns" --arg pid "$pid" --arg start "${stat[19]-}" --arg dir "$dir" \
      '{host: $host, boot: $boot, ns: $ns, pid: $pid, start: $start, dir: $dir}' > "$tmp" ||
    ! mv -f -- "$tmp" "$lives/$session"; then
    [ -z "$tmp" ] || rm -f -- "$tmp"
    say "could not keep this session's file in $lives, so a later start cannot end it should it stop without saying so"
  fi
}

# Ends each other session whose file says its harness died on this machine, a boot since or its
# process gone or another in its place, each in a post of its own, the file removed once the edge
# keeps the end; a session of another machine's name, or of another pid namespace on this boot,
# is left as it is. A file it cannot read is removed, saying so. Sets `swept` to how many it ended.
sweep() {
  local f name fields why line n at host boot ns comm
  local -a v=() stat=()
  swept=0
  here || return 0
  for f in "$lives"/*; do
    [ -f "$f" ] || continue
    name=${f##*/}
    [ "$name" != "$session" ] || continue
    fields=$(jq -r '
      def word: if type == "string" and . != "" and (test("[[:cntrl:]]") | not) then . else error("no word") end;
      .host, .boot, .ns, .pid, .start, .dir | word' "$f" 2> /dev/null) || fields=''
    mapfile -t v <<< "$fields"
    if ! [[ $name =~ $id && ${#v[@]} == 6 && ${v[3]} =~ ^[1-9][0-9]*$ && ${v[4]} =~ ^[0-9]+$ ]]; then
      rm -f -- "$f"
      say "removed the session file $f, which it could not read"
      continue
    fi
    [ "${v[0]}" = "$host" ] || continue
    if [ "${v[1]}" != "$boot" ]; then
      why=rebooted
    elif [ "${v[2]}" != "$ns" ]; then
      continue
    elif ! stat_of "${v[3]}"; then
      why=gone
    elif [ "${stat[19]-}" != "${v[4]}" ]; then
      why=reused
    else
      continue
    fi
    stamp
    line=$(jq -nc --arg s "$name" --arg d "${v[5]}" --arg n "$n" --arg at "$at" --arg why "$why" \
      '{session: $s, dir: $d, n: $n, at: $at, event: "ended", shape: {reason: "stopped", found: $why}}') || continue
    deliver "$name" "the end of the stopped session $name" was "$n" "$line"
    case $fate in
      taken)
        rm -f -- "$f"
        swept=$((swept + 1))
        ;;
      # The end is in the session's queue, which a later start sends.
      kept) rm -f -- "$f" ;;
    esac
  done
}

# Removes each entry under `kept` whose name holds an `n` past `unended`, or none, kept files'
# temporary ones among them, and the queues it leaves empty; a line on stderr a queue that lost
# lines.
aged() {
  local d f name cut
  local -i k
  moot
  for d in "$kept"/*; do
    if ! [ -d "$d" ]; then
      [ ! -e "$d" ] || rm -f -- "$d"
      continue
    fi
    k=0
    for f in "$d"/* "$d"/.*; do
      name=${f##*/}
      [ -e "$f" ] && [ "$name" != . ] && [ "$name" != .. ] || continue
      [[ $name =~ ^\.?([0-9]{19})(\..*)?$ ]] && ((10#${BASH_REMATCH[1]} >= cut)) && continue
      rm -f -- "$f"
      [[ $name == .* ]] || k+=1
    done
    rmdir -- "$d" 2> /dev/null
    ((k == 0)) || dropped "${d##*/}" "$k" "older than $((unended / 3600)) hours"
  done
}

# Sends what is kept, at a start once the edge has answered: the session's own queues first, then
# each other queue, a dead session's with its end (`sweep`), a post each while the edge answers
# and time is left.
resend() {
  local d
  aged
  fate=taken
  family ''
  [ "$fate" = taken ] || [ "$fate" = refused ] || return 0
  sweep
  for d in "$kept"/*; do
    [ -d "$d" ] || continue
    case ${d##*/} in "$session" | "$session"+*) continue ;; esac
    deliver "${d##*/}" '' '' || [ "$fate" = refused ] || return 0
  done
}

# What a session opens with besides its brief: the claim holder it works as, how an agent it
# starts learns its own, and after a compaction the digest of what it did lately. A session that
# starts afresh (a clear among them) has none: its brief is the handover.
opening() {
  local holder text swept=0
  payload || return 0
  [ "$event" = SessionStart ] || return 0
  alive
  if ! ask /holder "harness=$harness" "dir=$dir"; then
    say "the edge at $edge did not answer for the holder"
    return 0
  elif [ "$status" != 200 ]; then
    answered "the holder" "$status" "$answer"
    return 0
  fi
  holder=$answer
  resend
  # TYPE, not <type>: pasted into a shell, `<type>` would be a redirect.
  text="grit: this session's claim holder is $holder, and its session is $harness:$session.
grit: an agent this session starts has its own claim holder. This prints it, with the agent's type in place of TYPE: $(sq "$self") holder TYPE --dir $(sq "$dir") --session $(sq "$session")"
  case $swept in
    0) ;;
    1) text+=$'\ngrit: ended 1 session of this checkout that had stopped without saying so.' ;;
    *) text+=$'\n'"grit: ended $swept sessions of this checkout that had stopped without saying so." ;;
  esac
  if [ "$source" = compact ]; then
    if ! ask /brief "harness=$harness" "session=$session" "dir=$dir" start=resume part=digest; then
      say "the edge at $edge did not answer for the digest"
    elif [ "$status" != 200 ]; then
      answered "the digest" "$status" "$answer"
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
  elif [ "$status" = 401 ]; then
    answered "the holder" "$status" "$answer"
    return 1
  elif [ "$status" != 200 ]; then
    say "holder: the edge at $edge answered $status: ${answer%%$'\n'*}"
    return 1
  fi
  printf '%s\n' "$answer"
}

# The header bearing the credential, as Claude Code's headersHelper reads it, on stdout; exit 1,
# printing nothing, when the checkout was joined with none.
header() {
  [ -n "$token" ] || {
    say "header: this checkout was joined with no credential"
    return 1
  }
  printf '{"Authorization":"Bearer %s"}\n' "$token"
}

# Sets `n` and `at` from one reading of the clock: `n` its microseconds times 1000 plus this
# process's id modulo 1000, 19 digits, `at` the instant. A kept line's age is read from its `n`.
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

# Who the queue $1 is kept for, as a line on stderr names it.
whose() {
  if [[ $1 == *+* ]]; then
    printf 'the agent %s of the session %s' "${1#*+}" "${1%%+*}"
  else
    printf 'the session %s' "$1"
  fi
}

# `1 event` or `<k> events`, for the count $1.
events() {
  if [ "$1" = 1 ]; then printf '1 event'; else printf '%d events' "$1"; fi
}

# Says that the $2 oldest lines kept for the queue $1 were dropped, for the reason $3.
dropped() {
  if [ "$2" = 1 ]; then
    say "dropped the oldest event kept for $(whose "$1"): $3"
  else
    say "dropped the $2 oldest events kept for $(whose "$1"): $3"
  fi
}

# Sets `cut` to the ordinal below which a kept line is past `unended`: a line's `n` is a reading
# of the clock (`stamp`), so its age is read from its name.
moot() {
  local now=${EPOCHREALTIME//[!0-9]/}
  cut=$(((10#$now - unended * 1000000) * 1000))
}

# The length of $1 in bytes, in `size`.
bytesof() {
  local LC_ALL=C
  # shellcheck disable=SC2034 # read by the caller
  size=${#1}
}

# Keeps the new lines `fresh` (each `<n>` and its line, in turn) in the queue $1, each in a file
# of its own named by its `n`, written whole before it is named; sets `fate` to `kept`, or, saying
# so with $2 naming them and $3 its verb, to `dropped` when they could not be kept.
stash() {
  local q=$kept/$1 i tmp
  for ((i = 0; i < ${#fresh[@]}; i += 2)); do
    tmp=''
    # A start removes the queues it leaves empty: one removed after mkdir is made again.
    if ! [[ ${fresh[i]} =~ ^[0-9]{19}$ ]] ||
      ! { { mkdir -p -- "$q" && tmp=$(mktemp -- "$q/.${fresh[i]}.XXXXXX"); } 2> /dev/null ||
        { mkdir -p -- "$q" && tmp=$(mktemp -- "$q/.${fresh[i]}.XXXXXX"); }; } ||
      ! printf '%s' "${fresh[i + 1]}" > "$tmp" || ! mv -f -- "$tmp" "$q/${fresh[i]}"; then
      [ -z "$tmp" ] || rm -f -- "$tmp"
      say "$2 $3 dropped: it could not be kept in $q"
      fate=dropped
      return 0
    fi
  done
  fate=kept
}

# Posts, as one post, the lines kept for the queue $1 in their ordinals' order, then the new lines
# $4… (each `<n>` and its line, in turn), $2 naming the new lines and $3 its verb, `was` or
# `were`. Every post of an event goes through it. A line is posted after every line its queue held
# when this read it, so it never reaches the edge ahead of them. Before the post, the queue's
# lines past `unended`, then its oldest past `most_lines` lines or `most_bytes` bytes with the new lines, are
# dropped, a line on stderr a reason. Sets `fate`: `taken` when the edge kept every line, `refused`
# when it kept all but those it named (refused again as they are, so never kept), `kept` when the
# new lines were kept to send later (no answer, no time left, a 401, 403 or 5xx), `dropped` when
# they were not; and `took` to the edge's 200 body, empty for any other answer. A kept line leaves
# its queue when the edge answered 200 or refused the post whole (400, 413) for a post holding it,
# or for a bound. Fails unless `fate` is `taken`.
deliver() {
  local key=$1 what=$2 verb=$3 q=$kept/$1 f name line body='' out code answer secs why size cut i
  local -a files=() lines=() fresh=()
  local -i count total=0 old=0 over=0 big=0 from=0 k
  shift 3
  fresh=("$@")
  # shellcheck disable=SC2034 # the edge's 200 body, for a caller to read
  took='' fate=taken
  moot
  for f in "$q"/*; do
    name=${f##*/}
    [[ -f $f && $name =~ ^[0-9]{19}$ ]] || continue
    if ((10#$name < cut)); then
      rm -f -- "$f"
      old+=1
      continue
    fi
    line=''
    IFS= read -r -d '' line < "$f"
    files+=("$f") lines+=("$line")
    bytesof "$line"
    total=$((total + size + 1))
  done
  for ((i = 1; i < ${#fresh[@]}; i += 2)); do
    bytesof "${fresh[i]}"
    total=$((total + size + 1))
  done
  count=$((${#files[@]} + ${#fresh[@]} / 2))
  while ((from < ${#files[@]})) && ((count > most_lines || total > most_bytes)); do
    if ((count > most_lines)); then over+=1; else big+=1; fi
    bytesof "${lines[from]}"
    total=$((total - size - 1)) count=$((count - 1))
    rm -f -- "${files[from]}"
    from+=1
  done
  ((old == 0)) || dropped "$key" "$old" "older than $((unended / 3600)) hours"
  ((over == 0)) || dropped "$key" "$over" "more than $most_lines were kept"
  ((big == 0)) || dropped "$key" "$big" "more than $((most_bytes / 1024)) KiB were kept"
  k=$((${#files[@]} - from))
  for ((i = from; i < ${#files[@]}; i++)); do body+=${lines[i]}$'\n'; done
  for ((i = 1; i < ${#fresh[@]}; i += 2)); do body+=${fresh[i]}$'\n'; done
  [ -n "$body" ] || return 0
  if [ "${#fresh[@]}" = 0 ]; then
    what="the $(events "$k") kept earlier for $(whose "$key")"
    verb=were
    [ "$k" != 1 ] || verb=was
  fi
  if ! left; then
    fate=kept
    [ "${#fresh[@]}" = 0 ] || {
      stash "$key" "$what" "$verb"
      [ "$fate" != kept ] || say "$what $verb kept to send later: no time was left for its post"
    }
    return 1
  fi
  out=$(curl -q -s "${direct[@]+"${direct[@]}"}" --connect-timeout 0.3 --max-time "$secs" -H @<(authorization) -H 'Content-Type: application/x-ndjson' \
    --data-binary @- -w '\n%{http_code}' -- "$edge/events?harness=$harness" <<< "${body%$'\n'}") || {
    fate=kept
    [ "${#fresh[@]}" = 0 ] || {
      stash "$key" "$what" "$verb"
      [ "$fate" != kept ] || say "$what $verb kept to send later: the edge at $edge did not answer"
    }
    return 1
  }
  code=${out##*$'\n'} answer=${out%$'\n'*}
  case $code in
    200 | 400 | 413)
      for ((i = from; i < ${#files[@]}; i++)); do rm -f -- "${files[i]}"; done
      ;;
  esac
  case $code in
    200)
      # shellcheck disable=SC2034 # the edge's 200 body, for a caller to read
      took=$answer
      ((k == 0)) || say "sent $(events "$k") kept earlier for $(whose "$key")"
      why=$(jq -r '.refused[]? | "line \(.line) refused: \(.why)"' <<< "$answer" 2> /dev/null) || why=''
      [ -z "$why" ] || {
        while IFS= read -r out; do say "$what: $out"; done <<< "$why"
        fate=refused
        return 1
      }
      return 0
      ;;
    400 | 413)
      if [ "${#fresh[@]}" != 0 ] && ((k > 0)); then
        what="$what and the $(events "$k") kept before it" verb=were
      fi
      if [ "$verb" = was ]; then out=it; else out=they; fi
      say "the edge at $edge refused $what whole ($code), so $out $verb dropped: ${answer%%$'\n'*}"
      fate=dropped
      return 1
      ;;
  esac
  fate=kept
  [ "${#fresh[@]}" = 0 ] || stash "$key" "$what" "$verb"
  if [ "$fate" != kept ]; then
    :
  elif [ "$code" = 401 ]; then
    answered "$what" 401 "$answer" "; $what $verb kept to send later"
  else
    say "the edge at $edge answered $code to $what, which $verb kept to send later: ${answer%%$'\n'*}"
  fi
  return 1
}

# Posts each queue of the session `session`, its own and its agents', but the queue $1, a post
# each, while the edge answers and time is left.
family() {
  local d
  for d in "$kept/$session" "$kept/$session"+*; do
    [ -d "$d" ] && [ "${d##*/}" != "$1" ] || continue
    deliver "${d##*/}" '' '' || [ "$fate" = refused ] || return 0
  done
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
  local w fw='' line d key
  payload || return 0
  [ "$event" != SessionEnd ] || deadline=$((10#$started + ended))
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
  [ -n "$line" ] || return 0
  key=$session${agent:++$agent}
  deliver "$key" "$event" was "$n" "$line"
  # A session whose end is recorded, or kept to send later, needs its file no more: a later start
  # sends what is kept, the end among it.
  case $event:$fate in
    SessionEnd:taken | SessionEnd:kept) rm -f -- "$lives/$session" ;;
  esac
  case $fate in
    taken | refused) family "$key" ;;
  esac
}

# Whether a rebase is in progress in this worktree.
rebasing() {
  local merge apply
  merge=$(git rev-parse --git-path rebase-merge 2> /dev/null) &&
    apply=$(git rev-parse --git-path rebase-apply 2> /dev/null) &&
    { [ -d "$merge" ] || [ -d "$apply" ]; }
}

# Posts, for the session `session` in this checkout, the commits $@ in order, each
# `<sha>[ <replaced sha>…]`, with ordinals one apart, in posts of at most 256 KiB of commits (and
# the session's queue ahead of them), a quarter of the edge's 1 MiB bound on a body. One line is far smaller: its strings are cut but `dir` and
# `worktree`, an environment variable (128 KiB at most, as Linux passes one) and a path.
commits() {
  local top subjects lines line i j=0 what were sent='' down=''
  local -a shas=() subject=() args=() post=()
  deadline=$((10#$started + committed))
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
      post+=("$((n + j))" "$line")
      j=$((j + 1))
    elif [ "${#post[@]}" -gt 0 ]; then
      posted "$#"
      post=()
    fi
  done <<< "$lines"
  # The command substitution dropped the last post's empty line.
  [ "${#post[@]}" = 0 ] || posted "$#"
  [ -z "$sent" ] || family "$session"
}

# Posts `post`'s commits, of the $1 made, to the session's queue; once a post was not answered,
# keeps the rest without asking. Sets `sent` when one was taken.
posted() {
  local -a fresh=()
  counted "$((${#post[@]} / 2))" "$1"
  if [ -n "$down" ]; then
    fresh=("${post[@]}")
    stash "$session" "$what" "$were"
    [ "$fate" != kept ] || say "$what $were kept to send later, as the post before it was"
    return 0
  fi
  deliver "$session" "$what" "$were" "${post[@]}"
  case $fate in
    taken | refused) sent=1 ;;
    kept) down=1 ;;
  esac
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
  header)
    header
    exit
    ;;
  *) say "no role ${role:-given}; it did nothing" ;;
esac
exit 0
