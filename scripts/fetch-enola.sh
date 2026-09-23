#!/usr/bin/env bash
# Installs the pinned enola binary into tools/enola -- project-local rather than on
# PATH, because grit has no devcontainer and no CI to carry a system install. Adapted
# from ../Spikes/capture-spike-enola/scripts/fetch-enola.sh. Re-run to re-fetch.
#
# TRAP: two things drift out from under this pin. `enola upgrade` replaces the binary in
# place, so anyone who runs it diverges from what this script installed. And a release
# carries an EXTRACTOR version of its own (`enola --version --json`) which decides how
# facts are read -- so a bump here can move the snapshot with no repository code
# changing. Re-pin the baseline afterwards and expect the first check to report noise.
#
# TRAP: the pinned version is NOT a free upgrade decision. enola's Scala extractor
# silently degrades on capture-checking syntax, which grit uses project-wide, and the
# damage is invisible in the receipt (`parse_errors: 0`). Before bumping this number,
# re-run the measurement in ../Spikes/capture-spike-enola -- its cap/ and plain/ trees
# are a controlled fixture with a documented symbol baseline. See the 2026-09-07 rows in
# roadmap/decisions.md.
set -euo pipefail

ENOLA_VERSION=0.4.22
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
dest="$repo_root/tools"

case "$(uname -m)" in
  x86_64) arch=amd64 ;;
  aarch64 | arm64) arch=arm64 ;;
  *) echo "unsupported arch: $(uname -m)" >&2; exit 1 ;;
esac

base="https://github.com/enola-labs/enola/releases/download/v${ENOLA_VERSION}"
name="enola-${ENOLA_VERSION}-linux-${arch}"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

curl -fsSL -o "$work/enola.tar.gz" "${base}/${name}.tar.gz"
curl -fsSL -o "$work/enola.sha256" "${base}/${name}.sha256"

# Published checksums, so they are checked. The file names the release asset while the
# download is a temporary path, so only the digest is reused.
echo "$(cut -d' ' -f1 "$work/enola.sha256")  $work/enola.tar.gz" | sha256sum -c - >/dev/null

tar -xzf "$work/enola.tar.gz" -C "$work" "$name"
mkdir -p "$dest"
install -m 0755 "$work/$name" "$dest/enola"
"$dest/enola" --version
