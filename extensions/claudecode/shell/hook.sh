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
# is bounded by one deadline, 3 s from its start, past which a watchdog stops it. Only `brief` and
# `opening` print, and only Claude Code's hook JSON. Not yet written: `event`, `commit` and
# `rewrite` do nothing.

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
    say "the edge's address $edge is not http:// or https://, a host and a port, nothing after; it did nothing"
    exit "$failed"
  fi
  if [[ $edge == http://* ]] && ! [[ $edge =~ $loopback ]]; then
    say "the edge's address $edge is plain http beyond this machine, where its posts would be read in clear; it did nothing"
    exit "$failed"
  fi
  self=${BASH_SOURCE[0]}
  [[ $self == /* ]] || self=$PWD/$self
  GRIT_HOOK_STARTED=${EPOCHREALTIME//[!0-9]/} GRIT_HOOK_SELF=$self GRIT_EDGE=$edge \
    timeout -k 0.2 "$bound" bash -- "$self" "$@"
  rc=$?
  case $rc in
    124 | 137) say "${1-} ran past its bound and was stopped" ;;
  esac
  [ "$failed" = 1 ] && exit "$rc"
  exit 0
fi

started=$GRIT_HOOK_STARTED
self=$GRIT_HOOK_SELF
unset GRIT_HOOK_STARTED GRIT_HOOK_SELF
export TZ=UTC
deadline=$((10#$started + bound * 1000000))
# A session's and an agent's id, and an agent's type, as the hook sends them.
id='^[A-Za-z0-9_-][A-Za-z0-9._-]{0,99}$'

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
  out=$(curl -sG --connect-timeout 0.3 --max-time "$secs" -w '\n%{http_code}' "${query[@]}" -- "$edge$path") ||
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
    context "$event" "grit: the edge at $edge did not answer; this $who opens without its brief."
  elif [ "$status" != 200 ]; then
    say "the edge at $edge answered $status for the brief: ${answer%%$'\n'*}"
    context "$event" "grit: the edge at $edge answered $status for the brief; this $who opens without it."
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
  text="grit: this session works as the claim holder $holder.
grit: an agent of type <type> that this session starts works as the claim holder this command prints: $(sq "$self") holder <type> --dir $(sq "$dir") --session $(sq "$session")"
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

role=${1-}
shift || true
case $role in
  brief) brief ;;
  opening) opening ;;
  holder)
    holder "$@"
    exit
    ;;
  event | commit | rewrite) ;;
  *) say "no role ${role:-given}; it did nothing" ;;
esac
exit 0
