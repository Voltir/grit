#!/usr/bin/env bash
# Installs the pinned native scalafmt into tools/scalafmt, for scripts/format-check: a native
# image starts in milliseconds, where Mill's checkFormatAll pays a JVM and Mill's own start on
# every run. Project-local, like scripts/fetch-enola.sh. Re-run to re-fetch.
#
# The version is .scalafmt.conf's, which Mill's reformatAll also reads: this refuses to install
# when the two differ, since a native image formats only as its own version does. A bump changes
# both, and the digest below, read from the new release asset.
set -euo pipefail

SCALAFMT_VERSION=3.11.5
SHA256=4a29c3651f64fbaf9adcca01c6ac2ba0e8281a3d20e08c12b152582139fae290 # scalafmt-linux-glibc
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
dest="$repo_root/tools"

configured=$(sed -nE 's/^version *= *"([^"]+)".*/\1/p' "$repo_root/.scalafmt.conf")
if [ "$configured" != "$SCALAFMT_VERSION" ]; then
  echo ".scalafmt.conf names scalafmt $configured; this script pins $SCALAFMT_VERSION" >&2
  exit 1
fi
if [ "$(uname -s)-$(uname -m)" != Linux-x86_64 ]; then
  echo "only scalafmt-linux-glibc (x86_64) is pinned; this is $(uname -s)-$(uname -m)" >&2
  exit 1
fi

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
curl -fsSL -o "$work/scalafmt" \
  "https://github.com/scalameta/scalafmt/releases/download/v${SCALAFMT_VERSION}/scalafmt-linux-glibc"
echo "$SHA256  $work/scalafmt" | sha256sum -c - >/dev/null
mkdir -p "$dest"
install -m 0755 "$work/scalafmt" "$dest/scalafmt"
"$dest/scalafmt" --version
